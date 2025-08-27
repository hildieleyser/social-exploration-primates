# STATISTICAL SIGNIFICANCE TESTS - BASE R VERSION
# Proper hypothesis testing and validation without external packages

# Load and prepare data
data_raw <- read.csv("Explore Exploit Dataset.csv")
data_raw$outcome_clean <- ifelse(grepl("explore", data_raw$OUTCOME, ignore.case = TRUE), "explore",
                               ifelse(grepl("exploit", data_raw$OUTCOME, ignore.case = TRUE), "exploit", 
                                     ifelse(data_raw$OUTCOME %in% c("NONE", "none", "stop"), "none", "other")))

data_valid <- data_raw[!is.na(data_raw$outcome_clean) & data_raw$outcome_clean != "other", ]

# Add monkey characteristics
monkey_order <- c("FRAN", "CHOCOLAT", "DALI", "ICE", "EBI", "ANEMONE")
data_valid$sex <- ifelse(data_valid$monkey %in% c("FRAN", "DALI", "EBI"), "Male", "Female")
data_valid$rank <- ifelse(data_valid$monkey %in% c("FRAN", "CHOCOLAT"), "Dominant",
                         ifelse(data_valid$monkey %in% c("DALI", "ICE"), "Intermediate", "Subordinate"))

# Filter to complete cases
data_analysis <- data_valid[!is.na(data_valid$RELATIVE_RANK) & !is.na(data_valid$expected_explore), ]

cat("=== STATISTICAL SIGNIFICANCE TESTS ===\n")
cat("Sample size:", nrow(data_analysis), "trials\n")
cat("Number of monkeys:", length(unique(data_analysis$monkey)), "\n")
cat("\nOutcome distribution:\n")
print(table(data_analysis$outcome_clean))

# 1. CHI-SQUARE TESTS FOR INDEPENDENCE
cat("\n=== CHI-SQUARE INDEPENDENCE TESTS ===\n")

# Test 1: Social complexity effect
social_table <- table(data_analysis$CONDITION, data_analysis$outcome_clean)
social_chisq <- chisq.test(social_table)
cat("SOCIAL COMPLEXITY EFFECT:\n")
cat("Chi-square =", round(social_chisq$statistic, 3), "\n")
cat("p-value =", format(social_chisq$p.value, scientific = TRUE), "\n")
cat("Effect size (Cramér's V) =", round(sqrt(social_chisq$statistic / (sum(social_table) * (min(dim(social_table)) - 1))), 3), "\n")
if(social_chisq$p.value < 0.001) {
  cat("*** HIGHLY SIGNIFICANT (p < 0.001)\n")
} else if(social_chisq$p.value < 0.01) {
  cat("** SIGNIFICANT (p < 0.01)\n")
} else if(social_chisq$p.value < 0.05) {
  cat("* SIGNIFICANT (p < 0.05)\n")
} else {
  cat("NOT SIGNIFICANT (p ≥ 0.05)\n")
}

# Test 2: Sex effect
sex_table <- table(data_analysis$sex, data_analysis$outcome_clean)
sex_chisq <- chisq.test(sex_table)
cat("\nSEX DIFFERENCE:\n")
cat("Chi-square =", round(sex_chisq$statistic, 3), "\n")
cat("p-value =", format(sex_chisq$p.value, scientific = TRUE), "\n")
cat("Effect size (Cramér's V) =", round(sqrt(sex_chisq$statistic / (sum(sex_table) * (min(dim(sex_table)) - 1))), 3), "\n")
if(sex_chisq$p.value < 0.001) {
  cat("*** HIGHLY SIGNIFICANT (p < 0.001)\n")
} else if(sex_chisq$p.value < 0.01) {
  cat("** SIGNIFICANT (p < 0.01)\n")
} else if(sex_chisq$p.value < 0.05) {
  cat("* SIGNIFICANT (p < 0.05)\n")
} else {
  cat("NOT SIGNIFICANT (p ≥ 0.05)\n")
}

# Test 3: Hierarchy effect
rank_table <- table(data_analysis$rank, data_analysis$outcome_clean)
rank_chisq <- chisq.test(rank_table)
cat("\nHIERARCHY EFFECT:\n")
cat("Chi-square =", round(rank_chisq$statistic, 3), "\n")
cat("p-value =", format(rank_chisq$p.value, scientific = TRUE), "\n")
cat("Effect size (Cramér's V) =", round(sqrt(rank_chisq$statistic / (sum(rank_table) * (min(dim(rank_table)) - 1))), 3), "\n")
if(rank_chisq$p.value < 0.001) {
  cat("*** HIGHLY SIGNIFICANT (p < 0.001)\n")
} else if(rank_chisq$p.value < 0.01) {
  cat("** SIGNIFICANT (p < 0.01)\n")
} else if(rank_chisq$p.value < 0.05) {
  cat("* SIGNIFICANT (p < 0.05)\n")
} else {
  cat("NOT SIGNIFICANT (p ≥ 0.05)\n")
}

# 2. CROSS-VALIDATION SETUP
cat("\n=== CROSS-VALIDATION ===\n")
set.seed(123)  # For reproducibility
n_trials <- nrow(data_analysis)
train_indices <- sample(1:n_trials, size = floor(0.7 * n_trials))
train_data <- data_analysis[train_indices, ]
test_data <- data_analysis[-train_indices, ]

cat("Training set:", nrow(train_data), "trials\n")
cat("Test set:", nrow(test_data), "trials\n")

# 3. SIMPLE LOGISTIC REGRESSION FOR EXPLORE vs NOT-EXPLORE
cat("\n=== LOGISTIC REGRESSION (Explore vs Not-Explore) ===\n")

# Convert to binary outcome
data_analysis$explore_binary <- as.numeric(data_analysis$outcome_clean == "explore")
train_data$explore_binary <- as.numeric(train_data$outcome_clean == "explore")
test_data$explore_binary <- as.numeric(test_data$outcome_clean == "explore")

# Create numeric predictors
train_data$social_numeric <- as.numeric(factor(train_data$CONDITION, levels = c("solo", "duo", "trio"))) - 1
train_data$sex_numeric <- as.numeric(train_data$sex == "Male")
train_data$rank_numeric <- 4 - train_data$RELATIVE_RANK

test_data$social_numeric <- as.numeric(factor(test_data$CONDITION, levels = c("solo", "duo", "trio"))) - 1
test_data$sex_numeric <- as.numeric(test_data$sex == "Male")
test_data$rank_numeric <- 4 - test_data$RELATIVE_RANK

# Fit logistic regression models
model_null <- glm(explore_binary ~ 1, data = train_data, family = "binomial")
model_social <- glm(explore_binary ~ social_numeric, data = train_data, family = "binomial")
model_social_sex <- glm(explore_binary ~ social_numeric + sex_numeric, data = train_data, family = "binomial")
model_full <- glm(explore_binary ~ social_numeric + sex_numeric + rank_numeric + scale(expected_explore), 
                 data = train_data, family = "binomial")

# Likelihood ratio tests
cat("\nLIKELIHOOD RATIO TESTS:\n")

lrt_social <- anova(model_null, model_social, test = "LRT")
cat("Social complexity: Chi-square =", round(lrt_social$Deviance[2], 3), 
    ", p =", format(lrt_social$`Pr(>Chi)`[2], scientific = TRUE), "\n")

lrt_sex <- anova(model_social, model_social_sex, test = "LRT")
cat("Sex effect: Chi-square =", round(lrt_sex$Deviance[2], 3), 
    ", p =", format(lrt_sex$`Pr(>Chi)`[2], scientific = TRUE), "\n")

lrt_full <- anova(model_social_sex, model_full, test = "LRT")
cat("Rank + Value effects: Chi-square =", round(lrt_full$Deviance[2], 3), 
    ", p =", format(lrt_full$`Pr(>Chi)`[2], scientific = TRUE), "\n")

# Coefficient significance
cat("\nCOEFFICIENT TESTS (Full Model):\n")
summary_full <- summary(model_full)
coeffs <- summary_full$coefficients
for(i in 1:nrow(coeffs)) {
  var_name <- rownames(coeffs)[i]
  estimate <- coeffs[i, 1]
  p_value <- coeffs[i, 4]
  significance <- ifelse(p_value < 0.001, "***", ifelse(p_value < 0.01, "**", ifelse(p_value < 0.05, "*", "")))
  cat(sprintf("%s: β = %.3f, p = %.4f %s\n", var_name, estimate, p_value, significance))
}

# 4. MODEL VALIDATION
cat("\n=== MODEL VALIDATION ===\n")

# Predict on test set
test_predictions <- predict(model_full, newdata = test_data, type = "response")
test_pred_binary <- as.numeric(test_predictions > 0.5)

# Calculate accuracy
accuracy <- mean(test_pred_binary == test_data$explore_binary)
baseline_accuracy <- max(table(test_data$explore_binary)) / nrow(test_data)

cat("Test set accuracy:", round(accuracy * 100, 2), "%\n")
cat("Baseline accuracy:", round(baseline_accuracy * 100, 2), "%\n")
cat("Improvement:", round((accuracy - baseline_accuracy) * 100, 2), "percentage points\n")

# Observed vs Predicted on test set
observed_explore_rate <- mean(test_data$explore_binary) * 100
predicted_explore_rate <- mean(test_predictions) * 100

cat("\nTest Set Validation:\n")
cat("Observed exploration rate:", round(observed_explore_rate, 2), "%\n")
cat("Predicted exploration rate:", round(predicted_explore_rate, 2), "%\n")
cat("Difference:", round(abs(observed_explore_rate - predicted_explore_rate), 2), "percentage points\n")

if(abs(observed_explore_rate - predicted_explore_rate) < 5) {
  cat("Good model fit (difference < 5%)\n")
} else {
  cat("Potential overfitting (large difference)\n")
}

# 5. EFFECT SIZES IN REAL UNITS
cat("\n=== EFFECT SIZES (Real Units) ===\n")

# Calculate exploration rates by group
solo_rate <- mean(data_analysis$explore_binary[data_analysis$CONDITION == "solo"]) * 100
duo_rate <- mean(data_analysis$explore_binary[data_analysis$CONDITION == "duo"]) * 100  
trio_rate <- mean(data_analysis$explore_binary[data_analysis$CONDITION == "trio"]) * 100

male_rate <- mean(data_analysis$explore_binary[data_analysis$sex == "Male"]) * 100
female_rate <- mean(data_analysis$explore_binary[data_analysis$sex == "Female"]) * 100

dominant_rate <- mean(data_analysis$explore_binary[data_analysis$rank == "Dominant"]) * 100
subordinate_rate <- mean(data_analysis$explore_binary[data_analysis$rank == "Subordinate"]) * 100

cat("SOCIAL COMPLEXITY EFFECT:\n")
cat("Solo:", round(solo_rate, 1), "% exploration\n")
cat("Duo:", round(duo_rate, 1), "% exploration\n")
cat("Trio:", round(trio_rate, 1), "% exploration\n")
cat("Total reduction (solo → trio):", round(solo_rate - trio_rate, 1), "percentage points\n")

cat("\nSEX DIFFERENCE:\n")
cat("Males:", round(male_rate, 1), "% exploration\n")
cat("Females:", round(female_rate, 1), "% exploration\n")
cat("Male advantage:", round(male_rate - female_rate, 1), "percentage points\n")

cat("\nHIERARCHY EFFECT:\n")
cat("Dominant:", round(dominant_rate, 1), "% exploration\n")
cat("Subordinate:", round(subordinate_rate, 1), "% exploration\n")
cat("Dominance advantage:", round(dominant_rate - subordinate_rate, 1), "percentage points\n")

# 6. POWER ANALYSIS
cat("\n=== POWER ANALYSIS ===\n")
cat("Sample sizes:\n")
print(table(data_analysis$CONDITION))
print(table(data_analysis$sex))
print(table(data_analysis$rank))

# Calculate Cohen's w effect sizes
social_w <- sqrt(social_chisq$statistic / sum(social_table))
sex_w <- sqrt(sex_chisq$statistic / sum(sex_table))
rank_w <- sqrt(rank_chisq$statistic / sum(rank_table))

cat("\nEffect sizes (Cohen's w):\n")
cat("Social complexity:", round(social_w, 3), 
    ifelse(social_w > 0.5, "(large)", ifelse(social_w > 0.3, "(medium)", "(small)")), "\n")
cat("Sex difference:", round(sex_w, 3), 
    ifelse(sex_w > 0.5, "(large)", ifelse(sex_w > 0.3, "(medium)", "(small)")), "\n")
cat("Hierarchy:", round(rank_w, 3), 
    ifelse(rank_w > 0.5, "(large)", ifelse(rank_w > 0.3, "(medium)", "(small)")), "\n")

# 7. CREATE VISUALIZATION
pdf("statistical_tests_results.pdf", width = 16, height = 12)
par(mfrow = c(2, 3))

# Plot 1: P-values
p_values <- c(social_chisq$p.value, sex_chisq$p.value, rank_chisq$p.value)
effect_names <- c("Social", "Sex", "Hierarchy")
colors <- ifelse(p_values < 0.05, "darkgreen", "red")

barplot(-log10(p_values), names.arg = effect_names,
        main = "Statistical Significance\n-log10(p-value)", 
        ylab = "-log10(p-value)", col = colors)
abline(h = -log10(0.05), col = "red", lty = 2)
text(2, -log10(0.05) + 0.5, "p = 0.05", col = "red")

# Plot 2: Effect sizes
effect_sizes <- c(social_w, sex_w, rank_w)
barplot(effect_sizes, names.arg = effect_names,
        main = "Effect Sizes\n(Cohen's w)", ylab = "Effect Size", col = "orange")
abline(h = 0.3, col = "blue", lty = 2)
abline(h = 0.5, col = "red", lty = 2)

# Plot 3: Cross-validation
barplot(c(baseline_accuracy, accuracy) * 100,
        names.arg = c("Baseline", "Model"),
        main = "Cross-Validation\nTest Accuracy", ylab = "Accuracy %",
        col = c("gray", "darkgreen"))

# Plot 4: Social complexity effect
barplot(c(solo_rate, duo_rate, trio_rate),
        names.arg = c("Solo", "Duo", "Trio"),
        main = "Social Complexity Effect", ylab = "Exploration %",
        col = c("lightblue", "orange", "red"))

# Plot 5: Sex difference
barplot(c(female_rate, male_rate),
        names.arg = c("Female", "Male"),
        main = "Sex Difference", ylab = "Exploration %",
        col = c("pink", "lightblue"))

# Plot 6: Hierarchy effect
barplot(c(subordinate_rate, dominant_rate),
        names.arg = c("Subordinate", "Dominant"),
        main = "Hierarchy Effect", ylab = "Exploration %",
        col = c("brown", "gold"))

dev.off()

# 8. SAVE RESULTS
results_summary <- data.frame(
  Effect = c("Social_Complexity", "Sex_Difference", "Hierarchy"),
  Chi_Square = c(social_chisq$statistic, sex_chisq$statistic, rank_chisq$statistic),
  p_value = c(social_chisq$p.value, sex_chisq$p.value, rank_chisq$p.value),
  Effect_Size_w = c(social_w, sex_w, rank_w),
  Significant = c(social_chisq$p.value < 0.05, sex_chisq$p.value < 0.05, rank_chisq$p.value < 0.05)
)

validation_summary <- data.frame(
  Metric = c("Test_Accuracy", "Baseline_Accuracy", "Improvement"),
  Value = c(accuracy, baseline_accuracy, accuracy - baseline_accuracy)
)

write.csv(results_summary, "statistical_test_results.csv", row.names = FALSE)
write.csv(validation_summary, "cross_validation_results.csv", row.names = FALSE)

cat("\n=== SUMMARY ===\n")
cat("All statistical tests completed using base R.\n")
cat("Results show which effects are statistically significant.\n")
cat("Cross-validation performed to check model validity.\n")
cat("Files saved: statistical_test_results.csv, cross_validation_results.csv\n")
cat("Visualization: statistical_tests_results.pdf\n") 