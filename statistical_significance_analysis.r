# STATISTICAL SIGNIFICANCE AND MODEL VALIDATION ANALYSIS
# Proper hypothesis testing and cross-validation to assess model validity

library(dplyr)
library(nnet)
library(car)
library(broom)

# Load and prepare data
data_raw <- read.csv("Explore Exploit Dataset.csv")
data_raw$outcome_clean <- ifelse(grepl("explore", data_raw$OUTCOME, ignore.case = TRUE), "explore",
                               ifelse(grepl("exploit", data_raw$OUTCOME, ignore.case = TRUE), "exploit", 
                                     ifelse(data_raw$OUTCOME %in% c("NONE", "none", "stop"), "none", "other")))

data_valid <- data_raw[!is.na(data_raw$outcome_clean) & data_raw$outcome_clean != "other", ]

monkey_order <- c("FRAN", "CHOCOLAT", "DALI", "ICE", "EBI", "ANEMONE")
monkey_info <- data.frame(
  monkey = monkey_order,
  sex = c("Male", "Female", "Male", "Female", "Male", "Female"),
  rank = c("Dominant", "Dominant", "Intermediate", "Intermediate", "Subordinate", "Subordinate"),
  absolute_rank = c(1, 2, 3, 4, 5, 6)
)

data_analysis <- merge(data_valid, monkey_info, by = "monkey", all.x = TRUE)

cat("=== STATISTICAL SIGNIFICANCE AND MODEL VALIDATION ===\n")

# Prepare data for modeling with proper validation
data_for_models <- data_analysis %>%
  filter(!is.na(RELATIVE_RANK) & !is.na(expected_explore)) %>%
  mutate(
    social_complexity = as.numeric(factor(CONDITION, levels = c("solo", "duo", "trio"))) - 1,
    sex_numeric = as.numeric(sex == "Male"),
    rank_numeric = 4 - RELATIVE_RANK,
    expected_explore_scaled = as.numeric(scale(expected_explore)),
    monkey_factor = as.factor(monkey)
  )

cat("Sample size for analysis:", nrow(data_for_models), "trials\n")
cat("Number of monkeys:", length(unique(data_for_models$monkey)), "\n")
cat("Outcome distribution:\n")
print(table(data_for_models$outcome_clean))

# 1. CROSS-VALIDATION SETUP
set.seed(123)  # For reproducibility
n_trials <- nrow(data_for_models)
train_indices <- sample(1:n_trials, size = floor(0.7 * n_trials))
train_data <- data_for_models[train_indices, ]
test_data <- data_for_models[-train_indices, ]

cat("\nCROSS-VALIDATION SETUP:\n")
cat("Training set:", nrow(train_data), "trials\n")
cat("Test set:", nrow(test_data), "trials\n")

# 2. FIT MODELS ON TRAINING DATA
cat("\n=== FITTING MODELS ON TRAINING DATA ===\n")

# Model 1: Null model
model_null <- multinom(outcome_clean ~ 1, data = train_data, trace = FALSE)

# Model 2: Social complexity only
model_social <- multinom(outcome_clean ~ social_complexity, data = train_data, trace = FALSE)

# Model 3: Social + Sex
model_social_sex <- multinom(outcome_clean ~ social_complexity + sex_numeric, data = train_data, trace = FALSE)

# Model 4: Social + Sex + Rank
model_social_sex_rank <- multinom(outcome_clean ~ social_complexity + sex_numeric + rank_numeric, 
                                 data = train_data, trace = FALSE)

# Model 5: Full model
model_full <- multinom(outcome_clean ~ social_complexity + sex_numeric + rank_numeric + expected_explore_scaled, 
                      data = train_data, trace = FALSE)

# Model 6: Full model with random effects (mixed model approach)
# Note: Using monkey as fixed effect since nnet doesn't support random effects
model_full_monkey <- multinom(outcome_clean ~ social_complexity + sex_numeric + rank_numeric + 
                             expected_explore_scaled + monkey_factor, 
                             data = train_data, trace = FALSE)

# 3. STATISTICAL SIGNIFICANCE TESTS
cat("\n=== STATISTICAL SIGNIFICANCE TESTS ===\n")

# Likelihood ratio tests for nested models
cat("LIKELIHOOD RATIO TESTS (p-values):\n")

# Test social complexity effect
lrt_social <- anova(model_null, model_social, test = "Chisq")
cat("Social complexity effect: p =", format(lrt_social$`Pr(>Chi)`[2], scientific = TRUE), "\n")

# Test sex effect
lrt_sex <- anova(model_social, model_social_sex, test = "Chisq")
cat("Sex effect: p =", format(lrt_sex$`Pr(>Chi)`[2], scientific = TRUE), "\n")

# Test rank effect
lrt_rank <- anova(model_social_sex, model_social_sex_rank, test = "Chisq")
cat("Rank effect: p =", format(lrt_rank$`Pr(>Chi)`[2], scientific = TRUE), "\n")

# Test value effect
lrt_value <- anova(model_social_sex_rank, model_full, test = "Chisq")
cat("Expected value effect: p =", format(lrt_value$`Pr(>Chi)`[2], scientific = TRUE), "\n")

# Test individual differences
lrt_monkey <- anova(model_full, model_full_monkey, test = "Chisq")
cat("Individual monkey effects: p =", format(lrt_monkey$`Pr(>Chi)`[2], scientific = TRUE), "\n")

# 4. COEFFICIENT SIGNIFICANCE (Z-tests)
cat("\n=== COEFFICIENT SIGNIFICANCE (Full Model) ===\n")
model_summary <- summary(model_full)
coefficients <- model_summary$coefficients
std_errors <- model_summary$standard.errors

# Calculate z-scores and p-values
z_scores <- coefficients / std_errors
p_values <- 2 * (1 - pnorm(abs(z_scores)))

cat("EXPLORE vs NONE coefficients:\n")
for(i in 1:ncol(coefficients)) {
  var_name <- colnames(coefficients)[i]
  coef_val <- coefficients[1, i]
  p_val <- p_values[1, i]
  significance <- ifelse(p_val < 0.001, "***", ifelse(p_val < 0.01, "**", ifelse(p_val < 0.05, "*", "")))
  cat(sprintf("%s: β = %.3f, p = %.4f %s\n", var_name, coef_val, p_val, significance))
}

cat("\nEXPLOIT vs NONE coefficients:\n")
for(i in 1:ncol(coefficients)) {
  var_name <- colnames(coefficients)[i]
  coef_val <- coefficients[2, i]
  p_val <- p_values[2, i]
  significance <- ifelse(p_val < 0.001, "***", ifelse(p_val < 0.01, "**", ifelse(p_val < 0.05, "*", "")))
  cat(sprintf("%s: γ = %.3f, p = %.4f %s\n", var_name, coef_val, p_val, significance))
}

# 5. CROSS-VALIDATION PERFORMANCE
cat("\n=== CROSS-VALIDATION PERFORMANCE ===\n")

# Predict on test set
test_predictions <- predict(model_full, newdata = test_data, type = "probs")
test_pred_classes <- predict(model_full, newdata = test_data)

# Calculate accuracy
accuracy <- mean(test_pred_classes == test_data$outcome_clean)
cat("Test set accuracy:", round(accuracy * 100, 2), "%\n")

# Baseline accuracy (most frequent class)
baseline_accuracy <- max(table(test_data$outcome_clean)) / nrow(test_data)
cat("Baseline accuracy (majority class):", round(baseline_accuracy * 100, 2), "%\n")
cat("Improvement over baseline:", round((accuracy - baseline_accuracy) * 100, 2), "percentage points\n")

# Observed vs Predicted on TEST set (not training set!)
observed_test <- table(test_data$outcome_clean) / nrow(test_data) * 100
predicted_test <- colMeans(test_predictions) * 100

cat("\nTEST SET - OBSERVED vs PREDICTED RATES:\n")
comparison_test <- data.frame(
  Outcome = names(observed_test),
  Observed = as.numeric(observed_test),
  Predicted = predicted_test[names(observed_test)],
  Difference = abs(as.numeric(observed_test) - predicted_test[names(observed_test)])
)
print(comparison_test)

# 6. EFFECT SIZES WITH CONFIDENCE INTERVALS
cat("\n=== EFFECT SIZES WITH CONFIDENCE INTERVALS ===\n")

# Convert log-odds to odds ratios for interpretation
odds_ratios <- exp(coefficients)
ci_lower <- exp(coefficients - 1.96 * std_errors)
ci_upper <- exp(coefficients + 1.96 * std_errors)

cat("ODDS RATIOS (95% CI) for EXPLORE vs NONE:\n")
for(i in 2:ncol(coefficients)) {  # Skip intercept
  var_name <- colnames(coefficients)[i]
  or_val <- odds_ratios[1, i]
  ci_low <- ci_lower[1, i]
  ci_high <- ci_upper[1, i]
  cat(sprintf("%s: OR = %.3f (95%% CI: %.3f - %.3f)\n", var_name, or_val, ci_low, ci_high))
}

# 7. MODEL DIAGNOSTICS
cat("\n=== MODEL DIAGNOSTICS ===\n")

# Residual deviance
null_deviance <- model_null$deviance
model_deviance <- model_full$deviance
pseudo_r2 <- (null_deviance - model_deviance) / null_deviance

cat("Null deviance:", round(null_deviance, 2), "\n")
cat("Model deviance:", round(model_deviance, 2), "\n")
cat("Pseudo R-squared (McFadden):", round(pseudo_r2, 3), "\n")

# AIC comparison with proper validation
aic_train <- AIC(model_full)
# Calculate AIC on test set (approximation)
test_log_lik <- sum(log(test_predictions[cbind(1:nrow(test_predictions), 
                                               match(test_data$outcome_clean, colnames(test_predictions)))]))
aic_test <- -2 * test_log_lik + 2 * length(coef(model_full))

cat("Training AIC:", round(aic_train, 2), "\n")
cat("Test AIC (approximate):", round(aic_test, 2), "\n")
cat("AIC difference (test - train):", round(aic_test - aic_train, 2), "\n")

if(aic_test - aic_train > 10) {
  cat("WARNING: Large AIC difference suggests potential overfitting!\n")
} else {
  cat("AIC difference suggests acceptable generalization.\n")
}

# 8. SIMPLE EFFECT TESTS
cat("\n=== SIMPLE EFFECT TESTS (Chi-square) ===\n")

# Test social complexity effect with simple chi-square
social_table <- table(data_for_models$CONDITION, data_for_models$outcome_clean)
social_chisq <- chisq.test(social_table)
cat("Social complexity chi-square: χ² =", round(social_chisq$statistic, 3), 
    ", p =", format(social_chisq$p.value, scientific = TRUE), "\n")

# Test sex effect
sex_table <- table(data_for_models$sex, data_for_models$outcome_clean)
sex_chisq <- chisq.test(sex_table)
cat("Sex difference chi-square: χ² =", round(sex_chisq$statistic, 3), 
    ", p =", format(sex_chisq$p.value, scientific = TRUE), "\n")

# Test rank effect
rank_table <- table(data_for_models$rank, data_for_models$outcome_clean)
rank_chisq <- chisq.test(rank_table)
cat("Rank effect chi-square: χ² =", round(rank_chisq$statistic, 3), 
    ", p =", format(rank_chisq$p.value, scientific = TRUE), "\n")

# 9. POWER ANALYSIS
cat("\n=== POWER ANALYSIS ===\n")
cat("Sample sizes per group:\n")
cat("Social contexts:", table(data_for_models$CONDITION), "\n")
cat("Sex groups:", table(data_for_models$sex), "\n")
cat("Rank groups:", table(data_for_models$rank), "\n")

# Calculate effect sizes (Cohen's w for chi-square)
social_w <- sqrt(social_chisq$statistic / sum(social_table))
sex_w <- sqrt(sex_chisq$statistic / sum(sex_table))
rank_w <- sqrt(rank_chisq$statistic / sum(rank_table))

cat("Effect sizes (Cohen's w):\n")
cat("Social complexity: w =", round(social_w, 3), 
    ifelse(social_w > 0.5, "(large)", ifelse(social_w > 0.3, "(medium)", "(small)")), "\n")
cat("Sex difference: w =", round(sex_w, 3), 
    ifelse(sex_w > 0.5, "(large)", ifelse(sex_w > 0.3, "(medium)", "(small)")), "\n")
cat("Rank effect: w =", round(rank_w, 3), 
    ifelse(rank_w > 0.5, "(large)", ifelse(rank_w > 0.3, "(medium)", "(small)")), "\n")

# 10. SAVE RESULTS
significance_results <- data.frame(
  Effect = c("Social_Complexity", "Sex", "Rank", "Expected_Value", "Individual_Differences"),
  Chi_Square = c(lrt_social$Deviance[2], lrt_sex$Deviance[2], lrt_rank$Deviance[2], 
                 lrt_value$Deviance[2], lrt_monkey$Deviance[2]),
  p_value = c(lrt_social$`Pr(>Chi)`[2], lrt_sex$`Pr(>Chi)`[2], lrt_rank$`Pr(>Chi)`[2],
              lrt_value$`Pr(>Chi)`[2], lrt_monkey$`Pr(>Chi)`[2]),
  Significant = c(lrt_social$`Pr(>Chi)`[2] < 0.05, lrt_sex$`Pr(>Chi)`[2] < 0.05, 
                  lrt_rank$`Pr(>Chi)`[2] < 0.05, lrt_value$`Pr(>Chi)`[2] < 0.05,
                  lrt_monkey$`Pr(>Chi)`[2] < 0.05)
)

validation_results <- data.frame(
  Metric = c("Test_Accuracy", "Baseline_Accuracy", "Improvement", "Pseudo_R2", "Training_AIC", "Test_AIC"),
  Value = c(accuracy, baseline_accuracy, accuracy - baseline_accuracy, pseudo_r2, aic_train, aic_test)
)

write.csv(significance_results, "statistical_significance_results.csv", row.names = FALSE)
write.csv(validation_results, "model_validation_results.csv", row.names = FALSE)
write.csv(comparison_test, "test_set_predictions.csv", row.names = FALSE)

cat("\n=== SUMMARY ===\n")
cat("Statistical significance tests completed.\n")
cat("Cross-validation performed to check for overfitting.\n")
cat("Results saved to CSV files for detailed review.\n")

# Create visualization
pdf("statistical_validation_plots.pdf", width = 16, height = 12)
layout(matrix(1:6, nrow = 2, ncol = 3, byrow = TRUE))

# Plot 1: Significance tests
barplot(-log10(significance_results$p_value), 
        names.arg = significance_results$Effect,
        main = "Statistical Significance\n-log10(p-value)", 
        ylab = "-log10(p-value)",
        col = ifelse(significance_results$Significant, "darkgreen", "red"),
        las = 2)
abline(h = -log10(0.05), col = "red", lty = 2)
text(3, -log10(0.05) + 0.5, "p = 0.05", col = "red")

# Plot 2: Cross-validation accuracy
barplot(c(baseline_accuracy, accuracy) * 100,
        names.arg = c("Baseline\n(Majority Class)", "Model\n(Cross-Validation)"),
        main = "Model Performance\nTest Set Accuracy",
        ylab = "Accuracy %",
        col = c("gray", "darkgreen"))

# Plot 3: Observed vs Predicted (Test Set)
barplot(rbind(comparison_test$Observed, comparison_test$Predicted),
        beside = TRUE, names.arg = comparison_test$Outcome,
        main = "Test Set Validation\nObserved vs Predicted",
        ylab = "Percentage", col = c("blue", "red"),
        legend.text = c("Observed", "Predicted"))

# Plot 4: Effect sizes
barplot(c(social_w, sex_w, rank_w),
        names.arg = c("Social", "Sex", "Rank"),
        main = "Effect Sizes\n(Cohen's w)",
        ylab = "Effect Size",
        col = "orange")
abline(h = 0.3, col = "blue", lty = 2)
abline(h = 0.5, col = "red", lty = 2)
text(2, 0.35, "Medium", col = "blue")
text(2, 0.55, "Large", col = "red")

# Plot 5: Residuals check
residuals_pearson <- residuals(model_full, type = "pearson")
plot(fitted(model_full), residuals_pearson,
     main = "Residuals Check\nPearson Residuals vs Fitted",
     xlab = "Fitted Values", ylab = "Pearson Residuals")
abline(h = 0, col = "red")

# Plot 6: AIC comparison
barplot(c(aic_train, aic_test),
        names.arg = c("Training", "Test"),
        main = "AIC Comparison\nOverfitting Check",
        ylab = "AIC Value",
        col = c("lightblue", "orange"))

dev.off()

cat("Generated statistical_validation_plots.pdf with validation visualizations!\n") 