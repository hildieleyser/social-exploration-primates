# FINAL COMPREHENSIVE TRINOMIAL MODEL
# All specified variables: y10, y02, y03, y04, y05, y06 with grouping factors

library(nnet)

# Load and prepare data
data_raw <- read.csv("Explore Exploit Dataset.csv")

cat("=== COMPREHENSIVE TRINOMIAL MODEL WITH ALL VARIABLES ===\n")

# Clean outcome variable (trinomial: explore/exploit/none)
data_raw$outcome_clean <- ifelse(grepl("explore", data_raw$OUTCOME, ignore.case = TRUE), "explore",
                               ifelse(grepl("exploit", data_raw$OUTCOME, ignore.case = TRUE), "exploit", 
                                     ifelse(data_raw$OUTCOME %in% c("NONE", "none", "stop"), "none", "other")))

# Filter to valid data
data_valid <- data_raw[!is.na(data_raw$outcome_clean) & data_raw$outcome_clean != "other", ]

# Add monkey characteristics
monkey_info <- data.frame(
  monkey = c("FRAN", "CHOCOLAT", "DALI", "ICE", "EBI", "ANEMONE"),
  sex = c("Male", "Female", "Male", "Female", "Male", "Female"),
  hierarchy = c("Dominant", "Dominant", "Intermediate", "Intermediate", "Subordinate", "Subordinate")
)

data_analysis <- merge(data_valid, monkey_info, by = "monkey", all.x = TRUE)

# Create complete dataset with all your variables
model_data <- data_analysis[
  !is.na(data_analysis$CONDITION) &           # y10
  !is.na(data_analysis$PAIRED_WITH) &        # y02
  !is.na(data_analysis$RELATIVE_RANK) &      # y03
  !is.na(data_analysis$SUBJECTIVE_CHOSEN_VALUE) &  # y04
  !is.na(data_analysis$subjective_exploit) & # y05
  !is.na(data_analysis$expected_explore), ]  # y06

cat("Dataset with all variables:\n")
cat("Sample size:", nrow(model_data), "trials\n")
cat("Variables included:\n")
cat("y10 - CONDITION:", table(model_data$CONDITION), "\n")
cat("y02 - PAIRED_WITH: 13 unique partners\n")
cat("y03 - RELATIVE_RANK:", table(model_data$RELATIVE_RANK), "\n")
cat("y04 - SUBJECTIVE_CHOSEN_VALUE: continuous\n")
cat("y05 - subjective_exploit: continuous\n") 
cat("y06 - expected_explore: continuous\n")

# Prepare modeling variables
model_data$y10_condition <- factor(model_data$CONDITION, levels = c("solo", "duo", "trio"))
model_data$y02_partner_group <- ifelse(model_data$PAIRED_WITH == "", "NONE", "PARTNERED")
model_data$y02_partner_group <- factor(model_data$y02_partner_group)
model_data$y03_rel_rank <- model_data$RELATIVE_RANK
model_data$y04_chosen_value <- scale(model_data$SUBJECTIVE_CHOSEN_VALUE)[,1]
model_data$y05_exploit_value <- scale(model_data$subjective_exploit)[,1]
model_data$y06_explore_expect <- scale(model_data$expected_explore)[,1]

# Grouping factors
model_data$sex_group <- factor(model_data$sex)
model_data$hierarchy_group <- factor(model_data$hierarchy, levels = c("Subordinate", "Intermediate", "Dominant"))
model_data$monkey_group <- factor(model_data$monkey)

# Outcome factor
model_data$outcome_factor <- factor(model_data$outcome_clean, levels = c("none", "explore", "exploit"))

cat("\nOutcome distribution:\n")
print(table(model_data$outcome_factor))

# Cross-validation split
set.seed(42)
n_trials <- nrow(model_data)
train_indices <- sample(1:n_trials, size = floor(0.7 * n_trials))
train_data <- model_data[train_indices, ]
test_data <- model_data[-train_indices, ]

cat("Training:", nrow(train_data), "trials\n")
cat("Testing:", nrow(test_data), "trials\n")

# Model 1: All main effects
cat("\n=== MODEL 1: ALL MAIN EFFECTS ===\n")
model_all_main <- multinom(outcome_factor ~ y10_condition + y02_partner_group + y03_rel_rank + 
                          y04_chosen_value + y05_exploit_value + y06_explore_expect,
                          data = train_data, trace = FALSE)

cat("AIC:", AIC(model_all_main), "\n")

# Model 2: Main effects + grouping
cat("\n=== MODEL 2: MAIN EFFECTS + GROUPING ===\n")
model_with_groups <- multinom(outcome_factor ~ y10_condition + y02_partner_group + y03_rel_rank + 
                             y04_chosen_value + y05_exploit_value + y06_explore_expect +
                             sex_group + hierarchy_group,
                             data = train_data, trace = FALSE)

cat("AIC:", AIC(model_with_groups), "\n")

# Model 3: Full model with individual monkeys
cat("\n=== MODEL 3: WITH INDIVIDUAL MONKEYS ===\n")
model_full_individuals <- multinom(outcome_factor ~ y10_condition + y02_partner_group + y03_rel_rank + 
                                  y04_chosen_value + y05_exploit_value + y06_explore_expect +
                                  monkey_group,
                                  data = train_data, trace = FALSE)

cat("AIC:", AIC(model_full_individuals), "\n")

# Select best model
models <- list(
  "All Main Effects" = model_all_main,
  "With Groups" = model_with_groups,
  "With Individuals" = model_full_individuals
)

aic_values <- sapply(models, AIC)
best_model <- models[[which.min(aic_values)]]
best_name <- names(models)[which.min(aic_values)]

cat("\n=== MODEL COMPARISON ===\n")
for(i in 1:length(aic_values)) {
  cat(sprintf("%s: AIC = %.1f\n", names(aic_values)[i], aic_values[i]))
}
cat("Best model:", best_name, "\n")

# Statistical significance testing
cat("\n=== STATISTICAL SIGNIFICANCE TESTS ===\n")

# Manual likelihood ratio tests
null_model <- multinom(outcome_factor ~ 1, data = train_data, trace = FALSE)
social_model <- multinom(outcome_factor ~ y10_condition, data = train_data, trace = FALSE)
values_model <- multinom(outcome_factor ~ y10_condition + y04_chosen_value + y05_exploit_value + y06_explore_expect, 
                        data = train_data, trace = FALSE)

# Chi-square tests
lrt_social <- 2 * (logLik(social_model) - logLik(null_model))
lrt_values <- 2 * (logLik(values_model) - logLik(social_model))
lrt_full <- 2 * (logLik(best_model) - logLik(values_model))

df_social <- 4  # 3 conditions - 1 reference
df_values <- 3  # 3 value variables
df_full <- length(coef(best_model)) - length(coef(values_model))

cat("Social complexity effect: χ² =", round(as.numeric(lrt_social), 3), 
    ", df =", df_social, ", p =", format(1 - pchisq(as.numeric(lrt_social), df_social), scientific = TRUE), "\n")
cat("Value effects: χ² =", round(as.numeric(lrt_values), 3), 
    ", df =", df_values, ", p =", format(1 - pchisq(as.numeric(lrt_values), df_values), scientific = TRUE), "\n")
cat("Additional factors: χ² =", round(as.numeric(lrt_full), 3), 
    ", df =", df_full, ", p =", format(1 - pchisq(as.numeric(lrt_full), df_full), scientific = TRUE), "\n")

# Coefficient analysis
cat("\n=== COEFFICIENT ANALYSIS (Best Model) ===\n")
summary_best <- summary(best_model)
coeffs <- summary_best$coefficients
std_errors <- summary_best$standard.errors

# Calculate z-scores and p-values
z_scores <- coeffs / std_errors
p_values <- 2 * (1 - pnorm(abs(z_scores)))

cat("EXPLORE vs NONE:\n")
for(i in 2:ncol(coeffs)) {  # Skip intercept
  var_name <- colnames(coeffs)[i]
  coef_val <- coeffs[1, i]
  p_val <- p_values[1, i]
  significance <- ifelse(p_val < 0.001, "***", ifelse(p_val < 0.01, "**", ifelse(p_val < 0.05, "*", "")))
  cat(sprintf("%s: β = %.3f, p = %.4f %s\n", var_name, coef_val, p_val, significance))
}

cat("\nEXPLOIT vs NONE:\n")
for(i in 2:ncol(coeffs)) {  # Skip intercept
  var_name <- colnames(coeffs)[i]
  coef_val <- coeffs[2, i]
  p_val <- p_values[2, i]
  significance <- ifelse(p_val < 0.001, "***", ifelse(p_val < 0.01, "**", ifelse(p_val < 0.05, "*", "")))
  cat(sprintf("%s: γ = %.3f, p = %.4f %s\n", var_name, coef_val, p_val, significance))
}

# Cross-validation
cat("\n=== CROSS-VALIDATION ===\n")
test_predictions <- predict(best_model, newdata = test_data, type = "probs")
test_pred_classes <- predict(best_model, newdata = test_data)

accuracy <- mean(test_pred_classes == test_data$outcome_factor)
baseline_accuracy <- max(table(test_data$outcome_factor)) / nrow(test_data)

cat("Test accuracy:", round(accuracy * 100, 2), "%\n")
cat("Baseline accuracy:", round(baseline_accuracy * 100, 2), "%\n")
cat("Improvement:", round((accuracy - baseline_accuracy) * 100, 2), "percentage points\n")

# Observed vs predicted
observed_rates <- table(test_data$outcome_factor) / nrow(test_data) * 100
predicted_rates <- colMeans(test_predictions) * 100

cat("\nObserved vs Predicted (Test Set):\n")
for(outcome in names(observed_rates)) {
  obs_rate <- observed_rates[outcome]
  pred_rate <- predicted_rates[outcome]
  diff <- abs(obs_rate - pred_rate)
  cat(sprintf("%s: %.1f%% obs vs %.1f%% pred (diff: %.1f%%)\n", 
              outcome, obs_rate, pred_rate, diff))
}

# Effect size analysis
cat("\n=== EFFECT SIZE ANALYSIS ===\n")

# Social complexity effects
cat("Y10 - CONDITION EFFECTS:\n")
for(condition in levels(model_data$y10_condition)) {
  subset_data <- model_data[model_data$y10_condition == condition, ]
  rates <- table(subset_data$outcome_clean) / nrow(subset_data) * 100
  cat(sprintf("%s: Explore=%.1f%%, Exploit=%.1f%%, None=%.1f%%\n", 
              condition, rates["explore"], rates["exploit"], rates["none"]))
}

# Partner effects
cat("\nY02 - PARTNER EFFECTS:\n")
for(partner in levels(model_data$y02_partner_group)) {
  subset_data <- model_data[model_data$y02_partner_group == partner, ]
  rates <- table(subset_data$outcome_clean) / nrow(subset_data) * 100
  cat(sprintf("%s: Explore=%.1f%%, Exploit=%.1f%%, None=%.1f%%\n", 
              partner, rates["explore"], rates["exploit"], rates["none"]))
}

# Relative rank effects
cat("\nY03 - RELATIVE RANK EFFECTS:\n")
for(rank in sort(unique(model_data$y03_rel_rank))) {
  subset_data <- model_data[model_data$y03_rel_rank == rank, ]
  rates <- table(subset_data$outcome_clean) / nrow(subset_data) * 100
  cat(sprintf("Rank %d: Explore=%.1f%%, Exploit=%.1f%%, None=%.1f%%\n", 
              rank, rates["explore"], rates["exploit"], rates["none"]))
}

# Value correlations
cat("\nVALUE VARIABLE CORRELATIONS:\n")
cat("Y04 (Chosen Value) × Y05 (Exploit Value):", round(cor(model_data$y04_chosen_value, model_data$y05_exploit_value), 3), "\n")
cat("Y04 (Chosen Value) × Y06 (Explore Expectation):", round(cor(model_data$y04_chosen_value, model_data$y06_explore_expect), 3), "\n")
cat("Y05 (Exploit Value) × Y06 (Explore Expectation):", round(cor(model_data$y05_exploit_value, model_data$y06_explore_expect), 3), "\n")

# Save comprehensive results
cat("\n=== SAVING COMPREHENSIVE RESULTS ===\n")

# Model comparison
model_comparison <- data.frame(
  Model = names(aic_values),
  AIC = aic_values,
  Delta_AIC = aic_values - min(aic_values),
  Best = aic_values == min(aic_values)
)

# Coefficients
coeff_results <- data.frame(
  Variable = rep(colnames(coeffs)[-1], 2),
  Coefficient = c(coeffs[1, -1], coeffs[2, -1]),
  P_value = c(p_values[1, -1], p_values[2, -1]),
  Significant = c(p_values[1, -1] < 0.05, p_values[2, -1] < 0.05),
  Comparison = rep(c("Explore_vs_None", "Exploit_vs_None"), each = ncol(coeffs) - 1)
)

# Cross-validation results
cv_results <- data.frame(
  Outcome = names(observed_rates),
  Observed_Percent = as.numeric(observed_rates),
  Predicted_Percent = predicted_rates[names(observed_rates)],
  Difference = abs(as.numeric(observed_rates) - predicted_rates[names(observed_rates)])
)

# Variable effects summary
variable_effects <- data.frame(
  Variable = c("y10_solo", "y10_duo", "y10_trio", "y02_none", "y02_partnered", 
               "y03_rank_1", "y03_rank_2", "y03_rank_3"),
  Explore_Rate = c(
    mean(model_data$outcome_clean[model_data$y10_condition == "solo"] == "explore") * 100,
    mean(model_data$outcome_clean[model_data$y10_condition == "duo"] == "explore") * 100,
    mean(model_data$outcome_clean[model_data$y10_condition == "trio"] == "explore") * 100,
    mean(model_data$outcome_clean[model_data$y02_partner_group == "NONE"] == "explore") * 100,
    mean(model_data$outcome_clean[model_data$y02_partner_group == "PARTNERED"] == "explore") * 100,
    mean(model_data$outcome_clean[model_data$y03_rel_rank == 1] == "explore") * 100,
    mean(model_data$outcome_clean[model_data$y03_rel_rank == 2] == "explore") * 100,
    mean(model_data$outcome_clean[model_data$y03_rel_rank == 3] == "explore") * 100
  )
)

# Save all results
write.csv(model_comparison, "final_model_comparison.csv", row.names = FALSE)
write.csv(coeff_results, "final_coefficients.csv", row.names = FALSE)
write.csv(cv_results, "final_validation.csv", row.names = FALSE)
write.csv(variable_effects, "final_variable_effects.csv", row.names = FALSE)

cat("Files saved:\n")
cat("- final_model_comparison.csv\n")
cat("- final_coefficients.csv\n")
cat("- final_validation.csv\n")
cat("- final_variable_effects.csv\n")

cat("\n=== SUMMARY ===\n")
cat("Successfully analyzed trinomial model with all specified variables:\n")
cat("✓ y10 - Social condition (solo/duo/trio)\n")
cat("✓ y02 - Partner presence\n") 
cat("✓ y03 - Relative rank (1-3)\n")
cat("✓ y04 - Subjective chosen value\n")
cat("✓ y05 - Subjective exploit value\n")
cat("✓ y06 - Expected explore value\n")
cat("✓ Grouping factors (sex, hierarchy, individual monkeys)\n")
cat("✓ Proper cross-validation\n")
cat("✓ Statistical significance tests\n")
cat("Analysis complete!\n") 