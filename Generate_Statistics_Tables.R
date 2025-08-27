# =============================================================================
# GENERATE STATISTICAL TABLES
# Simple script to create all tables for the comprehensive report
# =============================================================================

library(ggplot2)
library(dplyr)
library(nnet)

cat("=== GENERATING STATISTICAL TABLES ===\n")

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

# Fit model
fit_hier <- multinom(outcome ~ social_complexity + expected_explore_z + 
                    subjective_exploit_z + rank_z + monkey_id, 
                    data = data_clean, trace = FALSE)

# Model comparison
fit_null <- multinom(outcome ~ 1, data = data_clean, trace = FALSE)
fit_fix <- multinom(outcome ~ social_complexity + expected_explore_z + 
                   subjective_exploit_z + rank_z, 
                   data = data_clean, trace = FALSE)

model_comparison <- data.frame(
  Model = c("Null", "Fixed Effects", "Hierarchical"),
  AIC = c(AIC(fit_null), AIC(fit_fix), AIC(fit_hier)),
  BIC = c(BIC(fit_null), BIC(fit_fix), BIC(fit_hier)),
  Parameters = c(2, 8, 18)
)

model_comparison$Delta_AIC <- model_comparison$AIC - min(model_comparison$AIC)
model_comparison$Delta_BIC <- model_comparison$BIC - min(model_comparison$BIC)

# Coefficient analysis
coef_matrix <- summary(fit_hier)$coefficients
se_matrix <- summary(fit_hier)$standard.errors

coef_results <- data.frame()
for(outcome in rownames(coef_matrix)) {
  for(term in colnames(coef_matrix)) {
    coef_val <- coef_matrix[outcome, term]
    se_val <- se_matrix[outcome, term]
    z_val <- coef_val / se_val
    p_val <- 2 * (1 - pnorm(abs(z_val)))
    
    coef_results <- rbind(coef_results, data.frame(
      Outcome = outcome,
      Term = term,
      Coefficient = coef_val,
      SE = se_val,
      Z_value = z_val,
      P_value = p_val,
      OR = exp(coef_val)
    ))
  }
}

# Predictions
actual_props <- prop.table(table(data_clean$outcome))
pred_hier <- predict(fit_hier, type = "probs")
pred_props_hier <- colMeans(pred_hier)

# Context predictions
actual_by_context <- data_clean %>%
  group_by(social_complexity, outcome) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(social_complexity) %>%
  mutate(observed = n / sum(n)) %>%
  select(social_complexity, outcome, observed)

pred_grid <- expand.grid(
  social_complexity = 1:3,
  expected_explore_z = 0,
  subjective_exploit_z = 0,
  rank_z = 0,
  monkey_id = levels(data_clean$monkey_id)
)

pred_context <- predict(fit_hier, pred_grid, type = "probs")

pred_by_context <- data.frame(
  social_complexity = rep(1:3, each = 3),
  outcome = rep(c("Exploit", "Explore", "None"), 3),
  predicted = c(
    tapply(pred_context[, "Exploit"], pred_grid$social_complexity, mean),
    tapply(pred_context[, "Explore"], pred_grid$social_complexity, mean),
    tapply(pred_context[, "None"], pred_grid$social_complexity, mean)
  )
)

context_comparison <- merge(actual_by_context, pred_by_context, 
                           by = c("social_complexity", "outcome"))
context_comparison$error <- abs(context_comparison$observed - context_comparison$predicted)
context_comparison$context <- c("Solo", "Duo", "Trio")[context_comparison$social_complexity]

# Cross-validation
set.seed(123)
cv_results <- data.frame()
for(i in 1:5) {
  test_idx <- sample(1:nrow(data_clean), size = round(nrow(data_clean) * 0.2))
  train_data <- data_clean[-test_idx, ]
  test_data <- data_clean[test_idx, ]
  
  cv_fit <- multinom(outcome ~ social_complexity + expected_explore_z + 
                     subjective_exploit_z + rank_z + monkey_id, 
                     data = train_data, trace = FALSE)
  
  cv_pred <- predict(cv_fit, test_data, type = "probs")
  
  actual_test <- prop.table(table(test_data$outcome))
  pred_test <- colMeans(cv_pred)
  errors <- abs(actual_test - pred_test[names(actual_test)])
  
  cv_results <- rbind(cv_results, data.frame(
    Fold = i,
    Exploit_Error = errors["Exploit"],
    Explore_Error = errors["Explore"],
    None_Error = errors["None"],
    Mean_Error = mean(errors),
    Accuracy = 1 - mean(errors)
  ))
}

# Save all tables
write.csv(model_comparison, "Model_Comparison_Complete.csv", row.names = FALSE)
write.csv(coef_results, "Coefficient_Analysis_Complete.csv", row.names = FALSE)
write.csv(context_comparison, "Context_Predictions_Complete.csv", row.names = FALSE)
write.csv(cv_results, "Cross_Validation_Complete.csv", row.names = FALSE)

# Create summary
sink("Statistical_Tables_Summary.txt")
cat("=== STATISTICAL TABLES SUMMARY ===\n")
cat("Date:", as.character(Sys.time()), "\n\n")

cat("MODEL COMPARISON:\n")
print(model_comparison)
cat("\n")

cat("COEFFICIENT ANALYSIS:\n")
print(coef_results)
cat("\n")

cat("CONTEXT PREDICTIONS:\n")
print(context_comparison)
cat("\n")

cat("CROSS-VALIDATION:\n")
print(cv_results)
cat(sprintf("\nMean CV accuracy: %.1f%%\n", mean(cv_results$Accuracy) * 100))

sink()

cat("Files created:\n")
cat("- Model_Comparison_Complete.csv\n")
cat("- Coefficient_Analysis_Complete.csv\n")
cat("- Context_Predictions_Complete.csv\n")
cat("- Cross_Validation_Complete.csv\n")
cat("- Statistical_Tables_Summary.txt\n")
cat("- Complete_Interpretation_Report.pdf\n")

cat("\n=== ALL TABLES GENERATED! ===\n") 