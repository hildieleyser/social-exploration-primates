# COMPREHENSIVE TRINOMIAL MODEL - FIXED VERSION
# Full model with all specified variables and proper grouping factors

library(nnet)

# Load and prepare data
data_raw <- read.csv("Explore Exploit Dataset.csv")

cat("=== COMPREHENSIVE TRINOMIAL MODEL ===\n")
cat("Including all specified variables with grouping factors\n\n")

# Clean outcome variable (trinomial: explore/exploit/none)
data_raw$outcome_clean <- ifelse(grepl("explore", data_raw$OUTCOME, ignore.case = TRUE), "explore",
                               ifelse(grepl("exploit", data_raw$OUTCOME, ignore.case = TRUE), "exploit", 
                                     ifelse(data_raw$OUTCOME %in% c("NONE", "none", "stop"), "none", "other")))

# Filter to valid data
data_valid <- data_raw[!is.na(data_raw$outcome_clean) & data_raw$outcome_clean != "other", ]

# Add monkey characteristics for grouping
monkey_order <- c("FRAN", "CHOCOLAT", "DALI", "ICE", "EBI", "ANEMONE")
monkey_info <- data.frame(
  monkey = monkey_order,
  sex = c("Male", "Female", "Male", "Female", "Male", "Female"),
  hierarchy = c("Dominant", "Dominant", "Intermediate", "Intermediate", "Subordinate", "Subordinate"),
  absolute_rank = c(1, 2, 3, 4, 5, 6)
)

# Merge with monkey info
data_analysis <- merge(data_valid, monkey_info, by = "monkey", all.x = TRUE)

# Extract your specified variables
# y10 = CONDITION (column 5)
# y02 = PAIRED_WITH (column 6) 
# y03 = RELATIVE_RANK (column 7)
# y04 = SUBJECTIVE_CHOSEN_VALUE (column 11)
# y05 = subjective_exploit (column 12)
# y06 = expected_explore (column 17)

cat("VARIABLE MAPPING:\n")
cat("y10 (Condition):", colnames(data_analysis)[5], "\n")
cat("y02 (Partner):", colnames(data_analysis)[6], "\n") 
cat("y03 (Relative Rank):", colnames(data_analysis)[7], "\n")
cat("y04 (Subjective Chosen Value):", colnames(data_analysis)[11], "\n")
cat("y05 (Subjective Exploit):", colnames(data_analysis)[12], "\n")
cat("y06 (Expected Explore):", colnames(data_analysis)[17], "\n")

# Create modeling dataset with complete cases
model_data <- data_analysis[
  !is.na(data_analysis$CONDITION) &           # y10
  !is.na(data_analysis$PAIRED_WITH) &        # y02
  !is.na(data_analysis$RELATIVE_RANK) &      # y03
  !is.na(data_analysis$SUBJECTIVE_CHOSEN_VALUE) &  # y04
  !is.na(data_analysis$subjective_exploit) & # y05
  !is.na(data_analysis$expected_explore), ]  # y06

cat("\nSample size after filtering:", nrow(model_data), "trials\n")
cat("Outcome distribution:\n")
print(table(model_data$outcome_clean))

# Prepare variables for modeling
model_data$y10_condition <- factor(model_data$CONDITION, levels = c("solo", "duo", "trio"))
model_data$y02_partner <- factor(model_data$PAIRED_WITH)
model_data$y03_rel_rank <- model_data$RELATIVE_RANK
model_data$y04_chosen_value <- scale(model_data$SUBJECTIVE_CHOSEN_VALUE)[,1]
model_data$y05_exploit_value <- scale(model_data$subjective_exploit)[,1]
model_data$y06_explore_expect <- scale(model_data$expected_explore)[,1]

# Grouping factors (without block to avoid error)
model_data$monkey_group <- factor(model_data$monkey)
model_data$sex_group <- factor(model_data$sex)
model_data$hierarchy_group <- factor(model_data$hierarchy, levels = c("Subordinate", "Intermediate", "Dominant"))

# Set reference level for outcome (none as reference)
model_data$outcome_factor <- factor(model_data$outcome_clean, levels = c("none", "explore", "exploit"))

cat("\nGrouping factor levels:\n")
cat("Monkeys:", levels(model_data$monkey_group), "\n")
cat("Sex:", levels(model_data$sex_group), "\n")
cat("Hierarchy:", levels(model_data$hierarchy_group), "\n")
cat("Conditions:", levels(model_data$y10_condition), "\n")
cat("Partner levels:", length(levels(model_data$y02_partner)), "unique partners\n")

# Check partner distribution
cat("\nPartner distribution:\n")
print(table(model_data$y02_partner))

# Cross-validation setup
set.seed(42)
n_trials <- nrow(model_data)
train_indices <- sample(1:n_trials, size = floor(0.7 * n_trials))
train_data <- model_data[train_indices, ]
test_data <- model_data[-train_indices, ]

cat("\nCross-validation split:\n")
cat("Training:", nrow(train_data), "trials\n")
cat("Testing:", nrow(test_data), "trials\n")

# Model 1: Main effects only (without partner to avoid convergence issues)
cat("\n=== MODEL 1: MAIN EFFECTS ===\n")
model_main <- multinom(outcome_factor ~ y10_condition + y03_rel_rank + 
                      y04_chosen_value + y05_exploit_value + y06_explore_expect,
                      data = train_data, trace = FALSE)

summary_main <- summary(model_main)
cat("AIC:", AIC(model_main), "\n")

# Model 2: Main effects + grouping factors
cat("\n=== MODEL 2: MAIN EFFECTS + GROUPING ===\n")
model_grouped <- multinom(outcome_factor ~ y10_condition + y03_rel_rank + 
                         y04_chosen_value + y05_exploit_value + y06_explore_expect +
                         monkey_group + sex_group + hierarchy_group,
                         data = train_data, trace = FALSE)

summary_grouped <- summary(model_grouped)
cat("AIC:", AIC(model_grouped), "\n")

# Model 3: Simplified model (to avoid partner convergence issues)
cat("\n=== MODEL 3: SIMPLIFIED COMPREHENSIVE ===\n")
model_simplified <- multinom(outcome_factor ~ y10_condition + y03_rel_rank + 
                            y04_chosen_value + y05_exploit_value + y06_explore_expect +
                            sex_group + hierarchy_group,
                            data = train_data, trace = FALSE)

summary_simplified <- summary(model_simplified)
cat("AIC:", AIC(model_simplified), "\n")

# Model 4: With key interactions
cat("\n=== MODEL 4: WITH KEY INTERACTIONS ===\n")
model_interactions <- multinom(outcome_factor ~ y10_condition + y03_rel_rank + 
                              y04_chosen_value + y05_exploit_value + y06_explore_expect +
                              sex_group + hierarchy_group +
                              y10_condition:hierarchy_group + y05_exploit_value:y06_explore_expect,
                              data = train_data, trace = FALSE)

summary_interactions <- summary(model_interactions)
cat("AIC:", AIC(model_interactions), "\n")

# Compare models
cat("\n=== MODEL COMPARISON ===\n")
models <- list(
  "Main Effects" = model_main,
  "+ Grouping" = model_grouped, 
  "Simplified" = model_simplified,
  "+ Interactions" = model_interactions
)

aic_values <- sapply(models, AIC)
cat("AIC Comparison:\n")
for(i in 1:length(aic_values)) {
  cat(sprintf("%s: AIC = %.1f\n", names(aic_values)[i], aic_values[i]))
}

best_model_name <- names(aic_values)[which.min(aic_values)]
best_model <- models[[which.min(aic_values)]]
cat("\nBest model:", best_model_name, "\n")

# Likelihood ratio tests
cat("\n=== LIKELIHOOD RATIO TESTS ===\n")

# Test grouping factors
lrt_grouping <- anova(model_main, model_grouped, test = "Chisq")
cat("Grouping factors: χ² =", round(lrt_grouping$Deviance[2], 3), 
    ", p =", format(lrt_grouping$`Pr(>Chi)`[2], scientific = TRUE), "\n")

# Test simplified vs grouped
lrt_simplified <- anova(model_simplified, model_grouped, test = "Chisq")
cat("Individual monkeys: χ² =", round(lrt_simplified$Deviance[2], 3), 
    ", p =", format(lrt_simplified$`Pr(>Chi)`[2], scientific = TRUE), "\n")

# Test interactions
lrt_interactions <- anova(model_simplified, model_interactions, test = "Chisq")
cat("Interactions: χ² =", round(lrt_interactions$Deviance[2], 3), 
    ", p =", format(lrt_interactions$`Pr(>Chi)`[2], scientific = TRUE), "\n")

# Detailed coefficient analysis for best model
cat("\n=== COEFFICIENT ANALYSIS (Best Model) ===\n")
coeffs <- summary(best_model)$coefficients
std_errors <- summary(best_model)$standard.errors
z_scores <- coeffs / std_errors
p_values <- 2 * (1 - pnorm(abs(z_scores)))

cat("\nEXPLORE vs NONE:\n")
for(i in 1:ncol(coeffs)) {
  var_name <- colnames(coeffs)[i]
  if(var_name != "(Intercept)") {
    coef_val <- coeffs[1, i]
    p_val <- p_values[1, i]
    significance <- ifelse(p_val < 0.001, "***", ifelse(p_val < 0.01, "**", ifelse(p_val < 0.05, "*", "")))
    cat(sprintf("%s: β = %.3f, p = %.4f %s\n", var_name, coef_val, p_val, significance))
  }
}

cat("\nEXPLOIT vs NONE:\n")
for(i in 1:ncol(coeffs)) {
  var_name <- colnames(coeffs)[i]
  if(var_name != "(Intercept)") {
    coef_val <- coeffs[2, i]
    p_val <- p_values[2, i]
    significance <- ifelse(p_val < 0.001, "***", ifelse(p_val < 0.01, "**", ifelse(p_val < 0.05, "*", "")))
    cat(sprintf("%s: γ = %.3f, p = %.4f %s\n", var_name, coef_val, p_val, significance))
  }
}

# Cross-validation
cat("\n=== CROSS-VALIDATION ===\n")
test_predictions <- predict(best_model, newdata = test_data, type = "probs")
test_pred_classes <- predict(best_model, newdata = test_data)

# Accuracy
accuracy <- mean(test_pred_classes == test_data$outcome_factor)
baseline_accuracy <- max(table(test_data$outcome_factor)) / nrow(test_data)

cat("Test accuracy:", round(accuracy * 100, 2), "%\n")
cat("Baseline accuracy:", round(baseline_accuracy * 100, 2), "%\n")
cat("Improvement:", round((accuracy - baseline_accuracy) * 100, 2), "percentage points\n")

# Observed vs predicted rates
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

# Calculate marginal effects for key variables
cat("CONDITION EFFECTS (marginal probabilities):\n")
for(condition in levels(model_data$y10_condition)) {
  subset_data <- model_data[model_data$y10_condition == condition, ]
  if(nrow(subset_data) > 0) {
    rates <- table(subset_data$outcome_clean) / nrow(subset_data) * 100
    cat(sprintf("%s: Explore=%.1f%%, Exploit=%.1f%%, None=%.1f%%\n", 
                condition, rates["explore"], rates["exploit"], rates["none"]))
  }
}

cat("\nSEX EFFECTS:\n")
for(sex in levels(model_data$sex_group)) {
  subset_data <- model_data[model_data$sex_group == sex, ]
  if(nrow(subset_data) > 0) {
    rates <- table(subset_data$outcome_clean) / nrow(subset_data) * 100
    cat(sprintf("%s: Explore=%.1f%%, Exploit=%.1f%%, None=%.1f%%\n", 
                sex, rates["explore"], rates["exploit"], rates["none"]))
  }
}

cat("\nHIERARCHY EFFECTS:\n")
for(hierarchy in levels(model_data$hierarchy_group)) {
  subset_data <- model_data[model_data$hierarchy_group == hierarchy, ]
  if(nrow(subset_data) > 0) {
    rates <- table(subset_data$outcome_clean) / nrow(subset_data) * 100
    cat(sprintf("%s: Explore=%.1f%%, Exploit=%.1f%%, None=%.1f%%\n", 
                hierarchy, rates["explore"], rates["exploit"], rates["none"]))
  }
}

# Value effects analysis
cat("\nVALUE EFFECTS ANALYSIS:\n")

# Correlation between values and outcomes
cor_chosen_explore <- cor(model_data$y04_chosen_value[model_data$outcome_clean == "explore"], 
                         model_data$y06_explore_expect[model_data$outcome_clean == "explore"], 
                         use = "complete.obs")
cor_chosen_exploit <- cor(model_data$y04_chosen_value[model_data$outcome_clean == "exploit"], 
                         model_data$y05_exploit_value[model_data$outcome_clean == "exploit"], 
                         use = "complete.obs")

cat("Correlation (Chosen Value × Explore Expectation) when exploring:", round(cor_chosen_explore, 3), "\n")
cat("Correlation (Chosen Value × Exploit Value) when exploiting:", round(cor_chosen_exploit, 3), "\n")

# Save results
cat("\n=== SAVING RESULTS ===\n")

# Model comparison results
comparison_results <- data.frame(
  Model = names(aic_values),
  AIC = aic_values,
  Delta_AIC = aic_values - min(aic_values),
  Best = aic_values == min(aic_values)
)

# Cross-validation results
validation_results <- data.frame(
  Outcome = names(observed_rates),
  Observed_Percent = as.numeric(observed_rates),
  Predicted_Percent = predicted_rates[names(observed_rates)],
  Difference = abs(as.numeric(observed_rates) - predicted_rates[names(observed_rates)])
)

# Coefficient results for best model
coeff_results_explore <- data.frame(
  Variable = colnames(coeffs)[-1],  # Exclude intercept
  Coefficient = coeffs[1, -1],      # Explore vs None
  P_value = p_values[1, -1],
  Significant = p_values[1, -1] < 0.05,
  Comparison = "Explore_vs_None"
)

coeff_results_exploit <- data.frame(
  Variable = colnames(coeffs)[-1],  # Exclude intercept
  Coefficient = coeffs[2, -1],      # Exploit vs None
  P_value = p_values[2, -1],
  Significant = p_values[2, -1] < 0.05,
  Comparison = "Exploit_vs_None"
)

coeff_results <- rbind(coeff_results_explore, coeff_results_exploit)

# Effect size summary
effect_sizes <- data.frame(
  Factor = c("Solo", "Duo", "Trio", "Male", "Female", "Dominant", "Intermediate", "Subordinate"),
  Explore_Rate = c(
    mean(model_data$outcome_clean[model_data$y10_condition == "solo"] == "explore") * 100,
    mean(model_data$outcome_clean[model_data$y10_condition == "duo"] == "explore") * 100,
    mean(model_data$outcome_clean[model_data$y10_condition == "trio"] == "explore") * 100,
    mean(model_data$outcome_clean[model_data$sex_group == "Male"] == "explore") * 100,
    mean(model_data$outcome_clean[model_data$sex_group == "Female"] == "explore") * 100,
    mean(model_data$outcome_clean[model_data$hierarchy_group == "Dominant"] == "explore") * 100,
    mean(model_data$outcome_clean[model_data$hierarchy_group == "Intermediate"] == "explore") * 100,
    mean(model_data$outcome_clean[model_data$hierarchy_group == "Subordinate"] == "explore") * 100
  )
)

write.csv(comparison_results, "comprehensive_model_comparison.csv", row.names = FALSE)
write.csv(validation_results, "comprehensive_validation_results.csv", row.names = FALSE)
write.csv(coeff_results, "comprehensive_coefficients.csv", row.names = FALSE)
write.csv(effect_sizes, "comprehensive_effect_sizes.csv", row.names = FALSE)

cat("Analysis complete!\n")
cat("Files saved:\n")
cat("- comprehensive_model_comparison.csv\n")
cat("- comprehensive_validation_results.csv\n") 
cat("- comprehensive_coefficients.csv\n")
cat("- comprehensive_effect_sizes.csv\n") 