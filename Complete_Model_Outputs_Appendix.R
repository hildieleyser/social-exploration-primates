# =============================================================================
# COMPLETE MODEL OUTPUTS AND DIAGNOSTICS FOR LATEX APPENDIX
# =============================================================================

library(ggplot2)
library(dplyr)
library(nnet)
library(scales)
library(patchwork)
library(cowplot)

cat("=== EXTRACTING COMPLETE MODEL OUTPUTS FOR LATEX APPENDIX ===\n")

# =============================================================================
# DATA PREPARATION
# =============================================================================

# Load and prepare data
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
    
    # Standardized predictors
    social_complexity = as.numeric(social_context),
    rank_z = as.numeric(scale(ABSOLUTE_RANK)),
    expected_explore_z = as.numeric(scale(expected_explore)),
    subjective_exploit_z = as.numeric(scale(subjective_exploit)),
    chosen_value_z = as.numeric(scale(SUBJECTIVE_CHOSEN_VALUE))
  ) %>%
  filter(!is.na(outcome), !is.na(social_context), !is.na(monkey_id)) %>%
  filter(complete.cases(.[c("expected_explore_z", "subjective_exploit_z", "chosen_value_z")]))

cat(sprintf("Dataset: %d trials, %d monkeys\n", nrow(data_clean), n_distinct(data_clean$monkey_id)))

# =============================================================================
# MODEL FITTING - ALL THREE MODELS
# =============================================================================

cat("Fitting all models...\n")

# Null model
fit_null <- multinom(outcome ~ 1, data = data_clean, trace = FALSE)

# Fixed effects model
fit_fix <- multinom(outcome ~ social_complexity + expected_explore_z + 
                   subjective_exploit_z + chosen_value_z + rank_z, 
                   data = data_clean, trace = FALSE)

# Hierarchical model
fit_hier <- multinom(outcome ~ social_complexity + expected_explore_z + 
                    subjective_exploit_z + chosen_value_z + rank_z + monkey_id, 
                    data = data_clean, trace = FALSE)

# =============================================================================
# COMPLETE MODEL SUMMARIES
# =============================================================================

cat("\n=== COMPLETE MODEL OUTPUTS ===\n")

# Print all model summaries
cat("\n1. NULL MODEL SUMMARY:\n")
cat("======================\n")
print(summary(fit_null))
cat("AIC:", AIC(fit_null), "\n")
cat("BIC:", BIC(fit_null), "\n")

cat("\n2. FIXED EFFECTS MODEL SUMMARY:\n")
cat("===============================\n")
print(summary(fit_fix))
cat("AIC:", AIC(fit_fix), "\n")
cat("BIC:", BIC(fit_fix), "\n")

cat("\n3. HIERARCHICAL MODEL SUMMARY:\n")
cat("==============================\n")
print(summary(fit_hier))
cat("AIC:", AIC(fit_hier), "\n")
cat("BIC:", BIC(fit_hier), "\n")

# Model comparison table
model_comparison <- data.frame(
  Model = c("Null", "Fixed Effects", "Hierarchical"),
  AIC = c(AIC(fit_null), AIC(fit_fix), AIC(fit_hier)),
  BIC = c(BIC(fit_null), BIC(fit_fix), BIC(fit_hier)),
  Parameters = c(2, 12, 22)
)

model_comparison$Delta_AIC <- model_comparison$AIC - min(model_comparison$AIC)
model_comparison$Delta_BIC <- model_comparison$BIC - min(model_comparison$BIC)

cat("\n4. MODEL COMPARISON TABLE:\n")
cat("==========================\n")
print(model_comparison)

# =============================================================================
# DETAILED COEFFICIENT EXTRACTION
# =============================================================================

cat("\n=== DETAILED COEFFICIENT ANALYSIS ===\n")

# Function to extract complete coefficient information
extract_complete_coefficients <- function(model, model_name) {
  coef_matrix <- summary(model)$coefficients
  se_matrix <- summary(model)$standard.errors
  
  cat(sprintf("\n%s COEFFICIENTS:\n", toupper(model_name)))
  cat(paste(rep("=", nchar(model_name) + 14), collapse = ""), "\n")
  
  coef_df <- data.frame()
  
  for(outcome in rownames(coef_matrix)) {
    cat(sprintf("\n%s vs Exploit:\n", outcome))
    cat(paste(rep("-", nchar(outcome) + 12), collapse = ""), "\n")
    
    for(term in colnames(coef_matrix)) {
      coef_val <- coef_matrix[outcome, term]
      se_val <- se_matrix[outcome, term]
      z_val <- coef_val / se_val
      p_val <- 2 * (1 - pnorm(abs(z_val)))
      ci_lower <- coef_val - 1.96 * se_val
      ci_upper <- coef_val + 1.96 * se_val
      or_val <- exp(coef_val)
      
      cat(sprintf("%-20s: Coef = %7.3f, SE = %6.3f, z = %6.3f, p = %6.3f, OR = %6.3f, 95%% CI = [%6.3f, %6.3f]\n",
                  term, coef_val, se_val, z_val, p_val, or_val, ci_lower, ci_upper))
      
      coef_df <- rbind(coef_df, data.frame(
        Model = model_name,
        Outcome = outcome,
        Term = term,
        Coefficient = coef_val,
        SE = se_val,
        Z_value = z_val,
        P_value = p_val,
        OR = or_val,
        CI_lower = ci_lower,
        CI_upper = ci_upper
      ))
    }
  }
  
  return(coef_df)
}

# Extract coefficients for all models
coef_null <- extract_complete_coefficients(fit_null, "Null Model")
coef_fix <- extract_complete_coefficients(fit_fix, "Fixed Effects Model")
coef_hier <- extract_complete_coefficients(fit_hier, "Hierarchical Model")

# Combine all coefficients
all_coefficients <- rbind(coef_null, coef_fix, coef_hier)

# =============================================================================
# RANDOM EFFECTS ANALYSIS
# =============================================================================

cat("\n=== RANDOM EFFECTS ANALYSIS ===\n")

# Extract random effects (individual deviations from population mean)
hier_coefs <- summary(fit_hier)$coefficients
hier_se <- summary(fit_hier)$standard.errors

# Get individual effects
individual_effects <- data.frame()

for(outcome in rownames(hier_coefs)) {
  # Find monkey columns
  monkey_cols <- grep("monkey_id", colnames(hier_coefs))
  
  for(i in monkey_cols) {
    monkey_name <- gsub("monkey_id", "", colnames(hier_coefs)[i])
    effect <- hier_coefs[outcome, i]
    se <- hier_se[outcome, i]
    
    individual_effects <- rbind(individual_effects, data.frame(
      Outcome = outcome,
      Monkey = monkey_name,
      Effect = effect,
      SE = se,
      CI_lower = effect - 1.96 * se,
      CI_upper = effect + 1.96 * se
    ))
  }
  
  # Add reference monkey (ANEMONE) with effect = 0
  individual_effects <- rbind(individual_effects, data.frame(
    Outcome = outcome,
    Monkey = "ANEMONE",
    Effect = 0,
    SE = 0,
    CI_lower = 0,
    CI_upper = 0
  ))
}

cat("INDIVIDUAL RANDOM EFFECTS:\n")
print(individual_effects)

# =============================================================================
# POSTERIOR PREDICTIVE CHECKS
# =============================================================================

cat("\n=== POSTERIOR PREDICTIVE CHECKS ===\n")

# Generate predictions
pred_data <- expand.grid(
  social_complexity = 1:3,
  expected_explore_z = 0,
  subjective_exploit_z = 0,
  chosen_value_z = 0,
  rank_z = 0,
  monkey_id = levels(data_clean$monkey_id)
)

# Get predicted probabilities
predictions <- predict(fit_hier, pred_data, type = "probs")

# Calculate observed proportions
observed_props <- data_clean %>%
  group_by(social_complexity) %>%
  summarise(
    Exploit = mean(outcome == "Exploit"),
    Explore = mean(outcome == "Explore"),
    None = mean(outcome == "None"),
    .groups = "drop"
  )

# Calculate predicted proportions (averaged across individuals)
pred_props <- data.frame(
  social_complexity = 1:3,
  Exploit = tapply(predictions[, "Exploit"], pred_data$social_complexity, mean),
  Explore = tapply(predictions[, "Explore"], pred_data$social_complexity, mean),
  None = tapply(predictions[, "None"], pred_data$social_complexity, mean)
)

cat("OBSERVED PROPORTIONS:\n")
print(observed_props)

cat("\nPREDICTED PROPORTIONS:\n")
print(pred_props)

# Calculate prediction errors
pred_errors <- data.frame(
  social_complexity = 1:3,
  Exploit_error = abs(observed_props$Exploit - pred_props$Exploit),
  Explore_error = abs(observed_props$Explore - pred_props$Explore),
  None_error = abs(observed_props$None - pred_props$None)
)

cat("\nPREDICTION ERRORS (|Observed - Predicted|):\n")
print(pred_errors)

# =============================================================================
# RESIDUAL ANALYSIS
# =============================================================================

cat("\n=== RESIDUAL ANALYSIS ===\n")

# Calculate residuals
fitted_probs <- fitted(fit_hier)
residuals_pearson <- residuals(fit_hier, type = "pearson")
residuals_deviance <- residuals(fit_hier, type = "deviance")

cat("RESIDUAL SUMMARY:\n")
cat("Pearson Residuals:\n")
print(summary(residuals_pearson))

cat("\nDeviance Residuals:\n")
print(summary(residuals_deviance))

# Calculate influential observations
leverage <- hatvalues(fit_hier)
cooks_distance <- cooks.distance(fit_hier)

cat("\nINFLUENTIAL OBSERVATIONS:\n")
cat("Max Leverage:", max(leverage), "\n")
cat("Max Cook's Distance:", max(cooks_distance), "\n")
cat("Observations with Cook's D > 0.5:", sum(cooks_distance > 0.5), "\n")

# =============================================================================
# GENERATE DIAGNOSTIC PLOTS
# =============================================================================

cat("\n=== GENERATING DIAGNOSTIC PLOTS ===\n")

# Create diagnostic plots
pdf("Model_Diagnostics_Complete.pdf", width = 12, height = 16)

# Layout for multiple plots
par(mfrow = c(4, 3), mar = c(4, 4, 3, 2))

# 1. Residual plots
plot(fitted_probs[, "Exploit"], residuals_pearson, 
     main = "Pearson Residuals vs Fitted (Exploit)",
     xlab = "Fitted Probabilities", ylab = "Pearson Residuals",
     pch = 16, col = alpha("blue", 0.6))
abline(h = 0, col = "red", lty = 2)

plot(fitted_probs[, "Explore"], residuals_pearson, 
     main = "Pearson Residuals vs Fitted (Explore)",
     xlab = "Fitted Probabilities", ylab = "Pearson Residuals",
     pch = 16, col = alpha("red", 0.6))
abline(h = 0, col = "red", lty = 2)

plot(fitted_probs[, "None"], residuals_pearson, 
     main = "Pearson Residuals vs Fitted (None)",
     xlab = "Fitted Probabilities", ylab = "Pearson Residuals",
     pch = 16, col = alpha("green", 0.6))
abline(h = 0, col = "red", lty = 2)

# 2. Q-Q plots
qqnorm(residuals_deviance, main = "Q-Q Plot: Deviance Residuals")
qqline(residuals_deviance, col = "red")

qqnorm(residuals_pearson, main = "Q-Q Plot: Pearson Residuals")
qqline(residuals_pearson, col = "red")

# 3. Leverage and Cook's distance
plot(leverage, main = "Leverage Values", ylab = "Leverage", pch = 16)
abline(h = 2 * mean(leverage), col = "red", lty = 2)

plot(cooks_distance, main = "Cook's Distance", ylab = "Cook's D", pch = 16)
abline(h = 0.5, col = "red", lty = 2)

# 4. Residuals vs predictors
plot(data_clean$social_complexity, residuals_pearson, 
     main = "Residuals vs Social Complexity", 
     xlab = "Social Complexity", ylab = "Pearson Residuals", pch = 16)
abline(h = 0, col = "red", lty = 2)

plot(data_clean$expected_explore_z, residuals_pearson, 
     main = "Residuals vs Expected Explore", 
     xlab = "Expected Explore (z)", ylab = "Pearson Residuals", pch = 16)
abline(h = 0, col = "red", lty = 2)

plot(data_clean$subjective_exploit_z, residuals_pearson, 
     main = "Residuals vs Subjective Exploit", 
     xlab = "Subjective Exploit (z)", ylab = "Pearson Residuals", pch = 16)
abline(h = 0, col = "red", lty = 2)

# 5. Individual effects plots
individual_explore <- individual_effects[individual_effects$Outcome == "Explore", ]
individual_none <- individual_effects[individual_effects$Outcome == "None", ]

plot(1:nrow(individual_explore), individual_explore$Effect,
     main = "Individual Random Effects (Explore)",
     xlab = "Individual", ylab = "Random Effect",
     pch = 16, col = "red", cex = 1.5,
     ylim = range(c(individual_explore$CI_lower, individual_explore$CI_upper)))
arrows(1:nrow(individual_explore), individual_explore$CI_lower,
       1:nrow(individual_explore), individual_explore$CI_upper,
       angle = 90, code = 3, length = 0.1)
abline(h = 0, col = "black", lty = 2)
text(1:nrow(individual_explore), individual_explore$Effect + 0.1,
     individual_explore$Monkey, cex = 0.8)

plot(1:nrow(individual_none), individual_none$Effect,
     main = "Individual Random Effects (None)",
     xlab = "Individual", ylab = "Random Effect",
     pch = 16, col = "green", cex = 1.5,
     ylim = range(c(individual_none$CI_lower, individual_none$CI_upper)))
arrows(1:nrow(individual_none), individual_none$CI_lower,
       1:nrow(individual_none), individual_none$CI_upper,
       angle = 90, code = 3, length = 0.1)
abline(h = 0, col = "black", lty = 2)
text(1:nrow(individual_none), individual_none$Effect + 0.1,
     individual_none$Monkey, cex = 0.8)

dev.off()

# =============================================================================
# SAVE ALL RESULTS
# =============================================================================

cat("\n=== SAVING ALL RESULTS ===\n")

# Save coefficient tables
write.csv(all_coefficients, "Complete_Model_Coefficients.csv", row.names = FALSE)
write.csv(individual_effects, "Individual_Random_Effects.csv", row.names = FALSE)
write.csv(model_comparison, "Model_Comparison_Table.csv", row.names = FALSE)
write.csv(observed_props, "Observed_Proportions.csv", row.names = FALSE)
write.csv(pred_props, "Predicted_Proportions.csv", row.names = FALSE)
write.csv(pred_errors, "Prediction_Errors.csv", row.names = FALSE)

# Save model objects
saveRDS(fit_null, "Model_Null.rds")
saveRDS(fit_fix, "Model_Fixed.rds")
saveRDS(fit_hier, "Model_Hierarchical.rds")

# Create summary report
sink("Complete_Model_Summary.txt")
cat("=== COMPLETE MODEL ANALYSIS SUMMARY ===\n")
cat("Date:", as.character(Sys.time()), "\n")
cat("Dataset:", nrow(data_clean), "trials,", n_distinct(data_clean$monkey_id), "monkeys\n\n")

cat("MODEL COMPARISON:\n")
print(model_comparison)

cat("\nHIERARCHICAL MODEL COEFFICIENTS (EXPLORE VS EXPLOIT):\n")
hier_explore <- coef_hier[coef_hier$Outcome == "Explore", ]
print(hier_explore[, c("Term", "Coefficient", "SE", "P_value", "OR", "CI_lower", "CI_upper")])

cat("\nHIERARCHICAL MODEL COEFFICIENTS (NONE VS EXPLOIT):\n")
hier_none <- coef_hier[coef_hier$Outcome == "None", ]
print(hier_none[, c("Term", "Coefficient", "SE", "P_value", "OR", "CI_lower", "CI_upper")])

cat("\nINDIVIDUAL RANDOM EFFECTS:\n")
print(individual_effects)

cat("\nPOSTERIOR PREDICTIVE CHECK:\n")
cat("Observed vs Predicted Proportions:\n")
print(data.frame(
  Context = c("Solo", "Duo", "Trio"),
  Obs_Exploit = observed_props$Exploit,
  Pred_Exploit = pred_props$Exploit,
  Obs_Explore = observed_props$Explore,
  Pred_Explore = pred_props$Explore,
  Obs_None = observed_props$None,
  Pred_None = pred_props$None
))

sink()

cat("\nFiles created:\n")
cat("- Complete_Model_Coefficients.csv\n")
cat("- Individual_Random_Effects.csv\n")
cat("- Model_Comparison_Table.csv\n")
cat("- Model_Diagnostics_Complete.pdf\n")
cat("- Complete_Model_Summary.txt\n")
cat("- Model objects: Model_Null.rds, Model_Fixed.rds, Model_Hierarchical.rds\n")

cat("\n=== COMPLETE MODEL ANALYSIS FINISHED ===\n") 