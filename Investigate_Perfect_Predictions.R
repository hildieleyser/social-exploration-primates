# =============================================================================
# INVESTIGATING PERFECT PREDICTIONS - This Shouldn't Be Possible!
# =============================================================================

library(ggplot2)
library(dplyr)
library(nnet)

cat("=== INVESTIGATING PERFECT PREDICTIONS ===\n")
cat("This shouldn't be possible - let's find the real issue!\n\n")

# Load data
raw_data <- read.csv("Explore Exploit Dataset.csv", stringsAsFactors = FALSE)

data_clean <- raw_data %>%
  filter(TRIAL_TYPE == "OIT_RE") %>%
  mutate(
    outcome = case_when(
      grepl("explore", tolower(OUTCOME)) ~ "Explore",
      grepl("exploit", tolower(OUTCOME)) ~ "Exploit", 
      grepl("none|stop|NONE", tolower(OUTCOME)) | OUTCOME == "" ~ "None",
      TRUE ~ "None"
    ),
    social_context = factor(CONDITION, levels = c("solo", "duo", "trio")),
    monkey_id = factor(monkey),
    social_complexity = as.numeric(social_context),
    rank_z = as.numeric(scale(ABSOLUTE_RANK)),
    expected_explore_z = as.numeric(scale(expected_explore)),
    subjective_exploit_z = as.numeric(scale(subjective_exploit))
  ) %>%
  filter(!is.na(outcome), !is.na(social_context), !is.na(monkey_id)) %>%
  filter(complete.cases(.[c("expected_explore_z", "subjective_exploit_z")]))

cat(sprintf("Dataset: %d trials\n", nrow(data_clean)))

# =============================================================================
# CHECK WHAT'S REALLY HAPPENING
# =============================================================================

cat("\n1. CHECKING ACTUAL DATA DISTRIBUTION:\n")
cat("=====================================\n")

# Overall proportions
actual_props <- prop.table(table(data_clean$outcome))
cat("Actual proportions:\n")
print(round(actual_props, 4))

# By social context
context_props <- data_clean %>%
  group_by(social_complexity, outcome) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(social_complexity) %>%
  mutate(prop = n / sum(n))

cat("\nBy social context:\n")
print(context_props)

# =============================================================================
# FIT MODEL AND CHECK PREDICTIONS STEP BY STEP
# =============================================================================

cat("\n2. FITTING MODEL STEP BY STEP:\n")
cat("==============================\n")

# Fit model
fit_hier <- multinom(outcome ~ social_complexity + expected_explore_z + 
                    subjective_exploit_z + rank_z + monkey_id, 
                    data = data_clean, trace = FALSE)

# Get predictions for each individual case
pred_individual <- predict(fit_hier, type = "probs")

cat("Individual prediction summary:\n")
cat("Min Exploit:", min(pred_individual[, "Exploit"]), "\n")
cat("Max Exploit:", max(pred_individual[, "Exploit"]), "\n")
cat("Min Explore:", min(pred_individual[, "Explore"]), "\n")
cat("Max Explore:", max(pred_individual[, "Explore"]), "\n")
cat("Min None:", min(pred_individual[, "None"]), "\n")
cat("Max None:", max(pred_individual[, "None"]), "\n")

# Check if predictions are identical for all cases
cat("\nChecking for identical predictions:\n")
cat("Unique Exploit predictions:", length(unique(pred_individual[, "Exploit"])), "\n")
cat("Unique Explore predictions:", length(unique(pred_individual[, "Explore"])), "\n")
cat("Unique None predictions:", length(unique(pred_individual[, "None"])), "\n")

# =============================================================================
# CHECK IF MODEL IS OVERFITTING
# =============================================================================

cat("\n3. CHECKING FOR OVERFITTING:\n")
cat("============================\n")

# Calculate mean predictions
pred_means <- colMeans(pred_individual)
cat("Mean predictions:\n")
print(round(pred_means, 4))

# Compare with actual
cat("\nComparison:\n")
comparison <- data.frame(
  Outcome = names(actual_props),
  Actual = as.numeric(actual_props),
  Predicted = pred_means[names(actual_props)],
  Difference = as.numeric(actual_props) - pred_means[names(actual_props)]
)
print(round(comparison, 4))

# =============================================================================
# CHECK MODEL COEFFICIENTS
# =============================================================================

cat("\n4. MODEL COEFFICIENTS:\n")
cat("=====================\n")

coef_matrix <- summary(fit_hier)$coefficients
se_matrix <- summary(fit_hier)$standard.errors

cat("Coefficient matrix:\n")
print(round(coef_matrix, 4))

cat("\nStandard errors:\n")
print(round(se_matrix, 4))

# Check for extreme coefficients
extreme_coefs <- abs(coef_matrix) > 10
if(any(extreme_coefs)) {
  cat("\nWARNING: Extreme coefficients detected!\n")
  print(coef_matrix[extreme_coefs])
} else {
  cat("\n✓ All coefficients are reasonable\n")
}

# =============================================================================
# CHECK MODEL CONVERGENCE
# =============================================================================

cat("\n5. MODEL CONVERGENCE:\n")
cat("====================\n")

# Check if model converged
if(is.null(fit_hier$convergence)) {
  cat("Model converged: TRUE\n")
} else {
  cat("Model converged:", fit_hier$convergence == 0, "\n")
}

# Check iterations
cat("Iterations:", fit_hier$iter, "\n")

# =============================================================================
# CROSS-VALIDATION TO CHECK REAL PERFORMANCE
# =============================================================================

cat("\n6. CROSS-VALIDATION:\n")
cat("===================\n")

# Simple 5-fold CV
set.seed(123)
cv_errors <- numeric(5)
for(i in 1:5) {
  # Split data
  test_idx <- sample(1:nrow(data_clean), size = round(nrow(data_clean) * 0.2))
  train_data <- data_clean[-test_idx, ]
  test_data <- data_clean[test_idx, ]
  
  # Fit model on training data
  cv_fit <- multinom(outcome ~ social_complexity + expected_explore_z + 
                     subjective_exploit_z + rank_z + monkey_id, 
                     data = train_data, trace = FALSE)
  
  # Predict on test data
  cv_pred <- predict(cv_fit, test_data, type = "probs")
  
  # Calculate error
  actual_test <- prop.table(table(test_data$outcome))
  pred_test <- colMeans(cv_pred)
  cv_errors[i] <- mean(abs(actual_test - pred_test[names(actual_test)]))
}

cat("Cross-validation errors:\n")
print(round(cv_errors, 4))
cat("Mean CV error:", round(mean(cv_errors), 4), "\n")

# =============================================================================
# CHECK IF PREDICTIONS ARE REALISTIC
# =============================================================================

cat("\n7. REALISTIC PREDICTION CHECK:\n")
cat("==============================\n")

# Create prediction grid for different scenarios
pred_grid <- expand.grid(
  social_complexity = 1:3,
  expected_explore_z = c(-1, 0, 1),
  subjective_exploit_z = c(-1, 0, 1),
  rank_z = c(-1, 0, 1),
  monkey_id = levels(data_clean$monkey_id)[1]  # Use first monkey
)

pred_grid_results <- predict(fit_hier, pred_grid, type = "probs")

cat("Prediction range check:\n")
cat("Exploit range:", round(range(pred_grid_results[, "Exploit"]), 4), "\n")
cat("Explore range:", round(range(pred_grid_results[, "Explore"]), 4), "\n")
cat("None range:", round(range(pred_grid_results[, "None"]), 4), "\n")

# Check for unrealistic predictions (0 or 1)
unrealistic <- pred_grid_results < 0.001 | pred_grid_results > 0.999
if(any(unrealistic)) {
  cat("\nWARNING: Unrealistic predictions detected!\n")
  cat("Number of near-zero predictions:", sum(pred_grid_results < 0.001), "\n")
  cat("Number of near-one predictions:", sum(pred_grid_results > 0.999), "\n")
} else {
  cat("\n✓ All predictions are realistic\n")
}

# =============================================================================
# SAVE INVESTIGATION RESULTS
# =============================================================================

cat("\n8. SAVING INVESTIGATION RESULTS:\n")
cat("================================\n")

# Save detailed results
write.csv(comparison, "Prediction_Investigation_Results.csv", row.names = FALSE)
write.csv(data.frame(CV_Errors = cv_errors), "Cross_Validation_Results.csv", row.names = FALSE)
write.csv(coef_matrix, "Model_Coefficients_Investigation.csv", row.names = FALSE)

cat("Files saved:\n")
cat("- Prediction_Investigation_Results.csv\n")
cat("- Cross_Validation_Results.csv\n")
cat("- Model_Coefficients_Investigation.csv\n")

cat("\n=== INVESTIGATION COMPLETE ===\n")
cat("Check the results to see what's really happening!\n") 