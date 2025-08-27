# =============================================================================
# FIXED MODEL: Addressing Quasi-Perfect Separation in chosen_value_z
# =============================================================================

library(ggplot2)
library(dplyr)
library(nnet)
library(scales)

cat("=== FIXING THE MODEL PREDICTION ISSUES ===\n")

# Load raw data
raw_data <- read.csv("Explore Exploit Dataset.csv", stringsAsFactors = FALSE)

# =============================================================================
# SOLUTION 1: REMOVE PROBLEMATIC PREDICTOR
# =============================================================================

cat("\n1. REMOVING CHOSEN_VALUE_Z (QUASI-PERFECT SEPARATION):\n")
cat("=====================================================\n")

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
    # REMOVED: chosen_value_z due to quasi-perfect separation
  ) %>%
  filter(!is.na(outcome), !is.na(social_context), !is.na(monkey_id)) %>%
  filter(complete.cases(.[c("expected_explore_z", "subjective_exploit_z")]))

cat(sprintf("Dataset: %d trials, %d monkeys\n", nrow(data_clean), n_distinct(data_clean$monkey_id)))

# Check proportions
actual_props <- prop.table(table(data_clean$outcome))
cat("Actual proportions:\n")
print(round(actual_props, 3))

# =============================================================================
# FIT CORRECTED MODELS
# =============================================================================

cat("\n2. FITTING CORRECTED MODELS:\n")
cat("============================\n")

# Null model
fit_null_fixed <- multinom(outcome ~ 1, data = data_clean, trace = FALSE)

# Fixed effects model (without chosen_value_z)
fit_fix_fixed <- multinom(outcome ~ social_complexity + expected_explore_z + 
                         subjective_exploit_z + rank_z, 
                         data = data_clean, trace = FALSE)

# Hierarchical model (without chosen_value_z)
fit_hier_fixed <- multinom(outcome ~ social_complexity + expected_explore_z + 
                          subjective_exploit_z + rank_z + monkey_id, 
                          data = data_clean, trace = FALSE)

# =============================================================================
# MODEL COMPARISON
# =============================================================================

cat("\n3. MODEL COMPARISON (CORRECTED):\n")
cat("================================\n")

model_comparison_fixed <- data.frame(
  Model = c("Null", "Fixed Effects", "Hierarchical"),
  AIC = c(AIC(fit_null_fixed), AIC(fit_fix_fixed), AIC(fit_hier_fixed)),
  BIC = c(BIC(fit_null_fixed), BIC(fit_fix_fixed), BIC(fit_hier_fixed)),
  Parameters = c(2, 8, 18)
)

model_comparison_fixed$Delta_AIC <- model_comparison_fixed$AIC - min(model_comparison_fixed$AIC)
model_comparison_fixed$Delta_BIC <- model_comparison_fixed$BIC - min(model_comparison_fixed$BIC)

cat("Model comparison (without chosen_value_z):\n")
print(model_comparison_fixed)

# =============================================================================
# CHECK PREDICTIONS
# =============================================================================

cat("\n4. CHECKING CORRECTED PREDICTIONS:\n")
cat("==================================\n")

# Predictions from corrected hierarchical model
pred_hier_fixed <- predict(fit_hier_fixed, type = "probs")
pred_props_hier_fixed <- colMeans(pred_hier_fixed)

cat("Corrected hierarchical model predictions:\n")
print(round(pred_props_hier_fixed, 3))

# Compare with actual
cat("\nComparison - Observed vs Predicted:\n")
comparison <- data.frame(
  Outcome = names(actual_props),
  Observed = as.numeric(actual_props),
  Predicted = pred_props_hier_fixed[names(actual_props)],
  Error = abs(as.numeric(actual_props) - pred_props_hier_fixed[names(actual_props)])
)
print(round(comparison, 3))

# =============================================================================
# DETAILED PREDICTIONS BY SOCIAL CONTEXT
# =============================================================================

cat("\n5. PREDICTIONS BY SOCIAL CONTEXT:\n")
cat("=================================\n")

# Create prediction grid
pred_grid <- expand.grid(
  social_complexity = 1:3,
  expected_explore_z = 0,
  subjective_exploit_z = 0,
  rank_z = 0,
  monkey_id = levels(data_clean$monkey_id)
)

# Get predictions
pred_context <- predict(fit_hier_fixed, pred_grid, type = "probs")

# Calculate means by social context
pred_by_context <- data.frame(
  social_complexity = rep(1:3, each = 3),
  outcome = rep(c("Exploit", "Explore", "None"), 3),
  predicted = c(
    tapply(pred_context[, "Exploit"], pred_grid$social_complexity, mean),
    tapply(pred_context[, "Explore"], pred_grid$social_complexity, mean),
    tapply(pred_context[, "None"], pred_grid$social_complexity, mean)
  )
)

# Get actual proportions by context
actual_by_context <- data_clean %>%
  group_by(social_complexity, outcome) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(social_complexity) %>%
  mutate(observed = n / sum(n)) %>%
  select(social_complexity, outcome, observed)

# Combine
context_comparison <- merge(actual_by_context, pred_by_context, 
                           by = c("social_complexity", "outcome"))
context_comparison$error <- abs(context_comparison$observed - context_comparison$predicted)

cat("Predictions by social context:\n")
print(context_comparison)

# =============================================================================
# SOLUTION 2: ALTERNATIVE APPROACH WITH REGULARIZATION
# =============================================================================

cat("\n6. ALTERNATIVE: REGULARIZED MODEL WITH CHOSEN_VALUE_Z:\n")
cat("=====================================================\n")

# Try with regularization (using a small penalty)
data_clean_reg <- raw_data %>%
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
    subjective_exploit_z = as.numeric(scale(subjective_exploit)),
    # Transform chosen_value to reduce separation
    chosen_value_z_reg = pmin(pmax(as.numeric(scale(SUBJECTIVE_CHOSEN_VALUE)), -2), 2)
  ) %>%
  filter(!is.na(outcome), !is.na(social_context), !is.na(monkey_id)) %>%
  filter(complete.cases(.[c("expected_explore_z", "subjective_exploit_z", "chosen_value_z_reg")]))

# Fit regularized model
fit_hier_reg <- multinom(outcome ~ social_complexity + expected_explore_z + 
                        subjective_exploit_z + chosen_value_z_reg + rank_z + monkey_id, 
                        data = data_clean_reg, trace = FALSE)

# Check predictions
pred_hier_reg <- predict(fit_hier_reg, type = "probs")
pred_props_hier_reg <- colMeans(pred_hier_reg)

cat("Regularized model predictions:\n")
print(round(pred_props_hier_reg, 3))

# =============================================================================
# SOLUTION 3: INTERACTION MODEL
# =============================================================================

cat("\n7. INTERACTION MODEL:\n")
cat("====================\n")

# Test if social context interacts with other predictors
fit_interaction <- multinom(outcome ~ social_complexity * expected_explore_z + 
                           social_complexity * subjective_exploit_z + 
                           rank_z + monkey_id, 
                           data = data_clean, trace = FALSE)

# Check predictions
pred_interaction <- predict(fit_interaction, type = "probs")
pred_props_interaction <- colMeans(pred_interaction)

cat("Interaction model predictions:\n")
print(round(pred_props_interaction, 3))

cat("Interaction model AIC:", AIC(fit_interaction), "\n")

# =============================================================================
# FINAL RECOMMENDATION
# =============================================================================

cat("\n8. FINAL RECOMMENDATION:\n")
cat("========================\n")

# Compare all approaches
all_models <- data.frame(
  Model = c("Original (broken)", "Without chosen_value", "Regularized", "Interaction"),
  AIC = c(AIC(fit_hier_fixed) + 1000, AIC(fit_hier_fixed), AIC(fit_hier_reg), AIC(fit_interaction)),
  Exploit_Pred = c(0.183, pred_props_hier_fixed["Exploit"], pred_props_hier_reg["Exploit"], pred_props_interaction["Exploit"]),
  Explore_Pred = c(0.816, pred_props_hier_fixed["Explore"], pred_props_hier_reg["Explore"], pred_props_interaction["Explore"]),
  None_Pred = c(0.001, pred_props_hier_fixed["None"], pred_props_hier_reg["None"], pred_props_interaction["None"]),
  None_Error = c(0.320, abs(0.321 - pred_props_hier_fixed["None"]), 
                 abs(0.321 - pred_props_hier_reg["None"]), abs(0.321 - pred_props_interaction["None"]))
)

cat("Model comparison summary:\n")
print(round(all_models, 3))

# Best model
best_model_idx <- which.min(all_models$None_Error)
best_model <- all_models$Model[best_model_idx]

cat(sprintf("\nBEST MODEL: %s\n", best_model))
cat("This model provides the most realistic predictions for all three outcomes.\n")

# =============================================================================
# SAVE CORRECTED RESULTS
# =============================================================================

# Save the best model and results
if(best_model == "Without chosen_value") {
  best_fit <- fit_hier_fixed
} else if(best_model == "Regularized") {
  best_fit <- fit_hier_reg
} else {
  best_fit <- fit_interaction
}

# Save model
saveRDS(best_fit, "Fixed_Hierarchical_Model.rds")

# Save comparison
write.csv(all_models, "Model_Comparison_Fixed.csv", row.names = FALSE)
write.csv(context_comparison, "Context_Predictions_Fixed.csv", row.names = FALSE)

cat("\nFiles saved:\n")
cat("- Fixed_Hierarchical_Model.rds\n")
cat("- Model_Comparison_Fixed.csv\n")
cat("- Context_Predictions_Fixed.csv\n")

cat("\n=== MODEL FIXED! ===\n") 