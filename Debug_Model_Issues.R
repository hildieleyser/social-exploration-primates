# =============================================================================
# DEBUGGING MODEL ISSUES: Why "None" Predictions Are Catastrophically Wrong
# =============================================================================

library(ggplot2)
library(dplyr)
library(nnet)

cat("=== DIAGNOSING MODEL PREDICTION FAILURES ===\n")

# Load raw data
raw_data <- read.csv("Explore Exploit Dataset.csv", stringsAsFactors = FALSE)

# =============================================================================
# ISSUE 1: INVESTIGATE OUTCOME CODING
# =============================================================================

cat("\n1. INVESTIGATING OUTCOME CODING:\n")
cat("================================\n")

# Check original OUTCOME values
outcome_table <- table(raw_data$OUTCOME, useNA = "always")
cat("Original OUTCOME values:\n")
print(outcome_table)

# Check our current coding
current_coding <- raw_data %>%
  filter(TRIAL_TYPE == "OIT_RE") %>%
  mutate(
    outcome = case_when(
      grepl("explore", tolower(OUTCOME)) ~ "Explore",
      grepl("exploit", tolower(OUTCOME)) ~ "Exploit", 
      grepl("none|stop|NONE", tolower(OUTCOME)) | OUTCOME == "" ~ "None",
      TRUE ~ "None"
    )
  ) %>%
  count(OUTCOME, outcome) %>%
  arrange(desc(n))

cat("\nCurrent coding mapping:\n")
print(current_coding)

# =============================================================================
# ISSUE 2: CHECK PREDICTOR DISTRIBUTIONS AND EXTREME VALUES
# =============================================================================

cat("\n2. CHECKING PREDICTOR DISTRIBUTIONS:\n")
cat("====================================\n")

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
    subjective_exploit_z = as.numeric(scale(subjective_exploit)),
    chosen_value_z = as.numeric(scale(SUBJECTIVE_CHOSEN_VALUE))
  ) %>%
  filter(!is.na(outcome), !is.na(social_context), !is.na(monkey_id)) %>%
  filter(complete.cases(.[c("expected_explore_z", "subjective_exploit_z", "chosen_value_z")]))

# Check predictor distributions
cat("Predictor summary statistics:\n")
predictors <- data_clean %>% 
  select(social_complexity, rank_z, expected_explore_z, subjective_exploit_z, chosen_value_z)

print(summary(predictors))

# Check for extreme values
cat("\nExtreme values (>3 SD):\n")
for(col in names(predictors)) {
  extreme <- abs(predictors[[col]]) > 3
  if(any(extreme, na.rm = TRUE)) {
    cat(sprintf("%s: %d extreme values (max = %.2f)\n", col, sum(extreme, na.rm = TRUE), max(abs(predictors[[col]]), na.rm = TRUE)))
  }
}

# =============================================================================
# ISSUE 3: ANALYZE ACTUAL OUTCOME PROPORTIONS BY PREDICTORS
# =============================================================================

cat("\n3. ACTUAL OUTCOME PROPORTIONS:\n")
cat("==============================\n")

# Overall proportions
overall_props <- prop.table(table(data_clean$outcome))
cat("Overall proportions:\n")
print(round(overall_props, 3))

# By social context
context_props <- data_clean %>%
  group_by(social_context, outcome) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(social_context) %>%
  mutate(prop = n / sum(n))

cat("\nBy social context:\n")
print(context_props)

# Check if "None" responses are associated with specific predictor values
cat("\n4. NONE RESPONSES BY PREDICTOR QUARTILES:\n")
cat("=========================================\n")

# Analyze chosen_value for None responses
none_data <- data_clean %>%
  filter(outcome == "None") %>%
  summarise(
    n = n(),
    mean_chosen_value = mean(chosen_value_z, na.rm = TRUE),
    sd_chosen_value = sd(chosen_value_z, na.rm = TRUE),
    min_chosen_value = min(chosen_value_z, na.rm = TRUE),
    max_chosen_value = max(chosen_value_z, na.rm = TRUE)
  )

cat("None responses - chosen_value_z statistics:\n")
print(none_data)

# Compare with other outcomes
value_by_outcome <- data_clean %>%
  group_by(outcome) %>%
  summarise(
    n = n(),
    mean_chosen_value = mean(chosen_value_z, na.rm = TRUE),
    sd_chosen_value = sd(chosen_value_z, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nChosen value by outcome:\n")
print(value_by_outcome)

# =============================================================================
# ISSUE 4: FIT MODELS WITH DIFFERENT REFERENCE CATEGORIES
# =============================================================================

cat("\n5. TESTING DIFFERENT REFERENCE CATEGORIES:\n")
cat("==========================================\n")

# Current model (Exploit as reference)
cat("Current model (Exploit as reference):\n")
fit_exploit_ref <- multinom(outcome ~ social_complexity + expected_explore_z + 
                           subjective_exploit_z + chosen_value_z + rank_z + monkey_id, 
                           data = data_clean, trace = FALSE)

# Get predictions
pred_exploit_ref <- predict(fit_exploit_ref, type = "probs")
pred_props_exploit_ref <- colMeans(pred_exploit_ref)
cat("Predicted proportions (Exploit ref):\n")
print(round(pred_props_exploit_ref, 3))

# Try with None as reference
cat("\nTrying None as reference:\n")
data_clean_none_ref <- data_clean %>%
  mutate(outcome = factor(outcome, levels = c("None", "Explore", "Exploit")))

fit_none_ref <- multinom(outcome ~ social_complexity + expected_explore_z + 
                        subjective_exploit_z + chosen_value_z + rank_z + monkey_id, 
                        data = data_clean_none_ref, trace = FALSE)

pred_none_ref <- predict(fit_none_ref, type = "probs")
pred_props_none_ref <- colMeans(pred_none_ref)
cat("Predicted proportions (None ref):\n")
print(round(pred_props_none_ref, 3))

# =============================================================================
# ISSUE 5: CHECK FOR PERFECT SEPARATION
# =============================================================================

cat("\n6. CHECKING FOR PERFECT SEPARATION:\n")
cat("===================================\n")

# Check if any predictor perfectly separates None from other outcomes
separation_check <- data_clean %>%
  group_by(outcome) %>%
  summarise(
    n = n(),
    min_chosen_value = min(chosen_value_z, na.rm = TRUE),
    max_chosen_value = max(chosen_value_z, na.rm = TRUE),
    min_expected_explore = min(expected_explore_z, na.rm = TRUE),
    max_expected_explore = max(expected_explore_z, na.rm = TRUE),
    min_subjective_exploit = min(subjective_exploit_z, na.rm = TRUE),
    max_subjective_exploit = max(subjective_exploit_z, na.rm = TRUE),
    .groups = "drop"
  )

cat("Range of predictors by outcome:\n")
print(separation_check)

# Check for non-overlapping ranges
cat("\nChecking for perfect separation:\n")
none_ranges <- separation_check %>% filter(outcome == "None")
other_ranges <- separation_check %>% filter(outcome != "None")

for(var in c("chosen_value", "expected_explore", "subjective_exploit")) {
  none_min <- none_ranges[[paste0("min_", var)]]
  none_max <- none_ranges[[paste0("max_", var)]]
  other_min <- min(other_ranges[[paste0("min_", var)]])
  other_max <- max(other_ranges[[paste0("max_", var)]])
  
  if(none_max < other_min || none_min > other_max) {
    cat(sprintf("PERFECT SEPARATION DETECTED in %s!\n", var))
    cat(sprintf("  None range: [%.3f, %.3f]\n", none_min, none_max))
    cat(sprintf("  Others range: [%.3f, %.3f]\n", other_min, other_max))
  } else {
    cat(sprintf("%s: overlapping ranges (OK)\n", var))
  }
}

# =============================================================================
# ISSUE 6: SIMPLIFIED MODEL TO ISOLATE PROBLEM
# =============================================================================

cat("\n7. SIMPLIFIED MODEL TO ISOLATE PROBLEM:\n")
cat("======================================\n")

# Fit very simple model
cat("Fitting intercept-only model:\n")
fit_simple <- multinom(outcome ~ 1, data = data_clean, trace = FALSE)
pred_simple <- predict(fit_simple, type = "probs")
pred_props_simple <- colMeans(pred_simple)
cat("Predicted proportions (intercept only):\n")
print(round(pred_props_simple, 3))

# Add just social complexity
cat("\nAdding social complexity:\n")
fit_social <- multinom(outcome ~ social_complexity, data = data_clean, trace = FALSE)
pred_social <- predict(fit_social, type = "probs")
pred_props_social <- colMeans(pred_social)
cat("Predicted proportions (social complexity only):\n")
print(round(pred_props_social, 3))

# =============================================================================
# ISSUE 7: EXAMINE COEFFICIENTS FOR EXTREME VALUES
# =============================================================================

cat("\n8. EXAMINING COEFFICIENTS FOR EXTREME VALUES:\n")
cat("=============================================\n")

# Get coefficients from full model
coef_matrix <- summary(fit_exploit_ref)$coefficients
se_matrix <- summary(fit_exploit_ref)$standard.errors

cat("Coefficients for None vs Exploit:\n")
none_coefs <- coef_matrix["None", ]
none_ses <- se_matrix["None", ]

coef_table <- data.frame(
  Coefficient = none_coefs,
  SE = none_ses,
  Z_value = none_coefs / none_ses
)

print(coef_table)

# Check for extremely large coefficients
extreme_coefs <- abs(coef_table$Coefficient) > 10
if(any(extreme_coefs)) {
  cat("\nEXTREME COEFFICIENTS DETECTED:\n")
  print(coef_table[extreme_coefs, ])
}

# =============================================================================
# PROPOSED SOLUTIONS
# =============================================================================

cat("\n9. PROPOSED SOLUTIONS:\n")
cat("======================\n")

cat("Based on diagnostics, potential issues and solutions:\n")
cat("1. Perfect separation: Check if chosen_value perfectly separates None responses\n")
cat("2. Extreme coefficients: Regularization or predictor transformation needed\n")
cat("3. Reference category: Try different reference categories\n")
cat("4. Interaction effects: Social context might interact with other predictors\n")
cat("5. Data quality: Check for coding errors in None responses\n")

# =============================================================================
# SAVE DIAGNOSTIC RESULTS
# =============================================================================

# Save detailed results
write.csv(current_coding, "Diagnostic_Outcome_Coding.csv", row.names = FALSE)
write.csv(context_props, "Diagnostic_Context_Proportions.csv", row.names = FALSE)
write.csv(separation_check, "Diagnostic_Separation_Check.csv", row.names = FALSE)
write.csv(coef_table, "Diagnostic_Coefficients.csv", row.names = FALSE)

cat("\n=== DIAGNOSTIC COMPLETE ===\n")
cat("Files saved:\n")
cat("- Diagnostic_Outcome_Coding.csv\n")
cat("- Diagnostic_Context_Proportions.csv\n")
cat("- Diagnostic_Separation_Check.csv\n")
cat("- Diagnostic_Coefficients.csv\n") 