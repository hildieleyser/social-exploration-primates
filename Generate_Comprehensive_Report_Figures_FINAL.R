# =============================================================================
# COMPREHENSIVE REPORT FIGURES - FINAL FIXED VERSION
# Publication-quality figures for the hierarchical multinomial Bayesian report
# =============================================================================

library(ggplot2)
library(dplyr)
library(nnet)
library(scales)
library(patchwork)

cat("=== GENERATING COMPREHENSIVE REPORT FIGURES (FINAL) ===\n")
cat("=======================================================\n\n")

# =============================================================================
# DATA PREPARATION
# =============================================================================

# Load data
raw_data <- read.csv("data/Explore Exploit Dataset.csv", stringsAsFactors = FALSE)

# Clean and prepare data
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

# Convert outcome to factor with proper ordering
data_clean$outcome <- factor(data_clean$outcome, levels = c("Exploit", "Explore", "None"))

cat("Dataset prepared: N =", nrow(data_clean), "trials\n")
cat("Monkeys: N =", length(unique(data_clean$monkey_id)), "\n")
cat("Contexts:", paste(levels(data_clean$social_context), collapse = ", "), "\n")
cat("Outcomes:", paste(levels(data_clean$outcome), collapse = ", "), "\n")
cat("Monkeys in data:", paste(sort(unique(as.character(data_clean$monkey_id))), collapse = ", "), "\n\n")

# =============================================================================
# FIT MODELS
# =============================================================================

cat("Fitting hierarchical multinomial models...\n")

# Fit models
fit_null <- multinom(outcome ~ 1, data = data_clean, trace = FALSE)
fit_fix <- multinom(outcome ~ social_complexity + expected_explore_z + 
                   subjective_exploit_z + rank_z, 
                   data = data_clean, trace = FALSE)
fit_hier <- multinom(outcome ~ social_complexity + expected_explore_z + 
                    subjective_exploit_z + rank_z + monkey_id, 
                    data = data_clean, trace = FALSE)

# Model comparison
model_comparison <- data.frame(
  Model = c("Null", "Fixed Effects", "Hierarchical"),
  AIC = c(AIC(fit_null), AIC(fit_fix), AIC(fit_hier)),
  BIC = c(BIC(fit_null), BIC(fit_fix), BIC(fit_hier)),
  Parameters = c(2, 8, 18)
) %>%
  mutate(
    Delta_AIC = AIC - min(AIC),
    Delta_BIC = BIC - min(BIC),
    Evidence = case_when(
      Delta_AIC == 0 ~ "Best",
      Delta_AIC < 2 ~ "Strong",
      Delta_AIC < 7 ~ "Moderate", 
      Delta_AIC < 10 ~ "Weak",
      TRUE ~ "None"
    )
  )

cat("Model comparison completed\n")
cat("Reference level for monkey_id:", levels(data_clean$monkey_id)[1], "\n\n")

# =============================================================================
# FIGURE 1: MODEL COMPARISON
# =============================================================================

cat("Creating Figure 1: Model Comparison...\n")

# Panel A: AIC comparison
p1a <- ggplot(model_comparison, aes(x = factor(Model, levels = rev(Model)), y = AIC)) +
  geom_col(fill = "steelblue", alpha = 0.8, width = 0.6) +
  geom_text(aes(label = round(AIC, 1)), hjust = -0.1, size = 3.5) +
  coord_flip() +
  labs(title = "A. Model Comparison (AIC)",
       x = "Model", y = "AIC") +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11)
  )

# Panel B: Delta AIC
p1b <- ggplot(model_comparison, aes(x = factor(Model, levels = rev(Model)), y = Delta_AIC)) +
  geom_col(fill = "darkorange", alpha = 0.8, width = 0.6) +
  geom_text(aes(label = round(Delta_AIC, 1)), hjust = -0.1, size = 3.5) +
  coord_flip() +
  labs(title = "B. Evidence Strength (ΔAIC)",
       x = "Model", y = "ΔAIC") +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11)
  )

# Combine panels
figure1 <- p1a + p1b + plot_layout(ncol = 2)

ggsave("Report_Figure1_Model_Comparison.pdf", figure1, width = 10, height = 5, dpi = 300)
ggsave("Report_Figure1_Model_Comparison.png", figure1, width = 10, height = 5, dpi = 300)

# =============================================================================
# FIGURE 2: FIXED EFFECTS COEFFICIENTS
# =============================================================================

cat("Creating Figure 2: Fixed Effects Coefficients...\n")

# Extract coefficients
extract_coef <- function(model) {
  coef_summary <- summary(model)
  coef_matrix <- coef_summary$coefficients
  se_matrix <- coef_summary$standard.errors
  
  results <- data.frame()
  for(outcome in rownames(coef_matrix)) {
    for(term in colnames(coef_matrix)) {
      if(term != "(Intercept)") {  # Skip intercepts for clarity
        results <- rbind(results, data.frame(
          outcome = outcome,
          term = term,
          estimate = coef_matrix[outcome, term],
          se = se_matrix[outcome, term],
          ci_lower = coef_matrix[outcome, term] - 1.96 * se_matrix[outcome, term],
          ci_upper = coef_matrix[outcome, term] + 1.96 * se_matrix[outcome, term]
        ))
      }
    }
  }
  return(results)
}

coef_data <- extract_coef(fit_hier)

# Clean term names
coef_data$term_clean <- case_when(
  coef_data$term == "social_complexity" ~ "Social Complexity",
  coef_data$term == "expected_explore_z" ~ "Expected Explore Value",
  coef_data$term == "subjective_exploit_z" ~ "Subjective Exploit Value",
  coef_data$term == "rank_z" ~ "Dominance Rank",
  grepl("monkey_id", coef_data$term) ~ gsub("monkey_id", "", coef_data$term),
  TRUE ~ coef_data$term
)

# Filter to main effects only (not individual monkey effects)
main_effects <- coef_data %>%
  filter(!grepl("CHOCOLAT|DALI|EBI|FRAN|ICE|ANEMONE", term)) %>%
  mutate(
    outcome = factor(outcome, levels = c("Explore", "None")),
    term_clean = factor(term_clean, levels = rev(c("Social Complexity", "Expected Explore Value", 
                                                  "Subjective Exploit Value", "Dominance Rank")))
  )

# Create coefficient plot
p2 <- ggplot(main_effects, aes(x = estimate, y = term_clean, color = outcome)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), 
                 height = 0.3, position = position_dodge(0.5)) +
  geom_point(size = 3, position = position_dodge(0.5)) +
  scale_color_manual(values = c("Explore" = "#E31A1C", "None" = "#1F78B4"),
                     name = "Outcome\n(vs. Exploit)") +
  labs(title = "Fixed Effects Coefficients (95% CI)",
       x = "Log-Odds Coefficient", y = "Predictor") +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    legend.position = "bottom"
  )

ggsave("Report_Figure2_Fixed_Effects.pdf", p2, width = 8, height = 6, dpi = 300)
ggsave("Report_Figure2_Fixed_Effects.png", p2, width = 8, height = 6, dpi = 300)

# =============================================================================
# FIGURE 3: PREDICTED PROBABILITIES BY CONTEXT - FIXED
# =============================================================================

cat("Creating Figure 3: Predicted Probabilities (FIXED)...\n")

# Generate predictions for average individual
prediction_data <- data.frame(
  social_complexity = 1:3,
  expected_explore_z = 0,  # Average values
  subjective_exploit_z = 0,
  rank_z = 0,
  monkey_id = levels(data_clean$monkey_id)[1]  # Use reference level
)

# Get predictions
pred_probs <- predict(fit_hier, newdata = prediction_data, type = "probs")

# Prepare prediction data - fixed to avoid duplication
pred_df <- data.frame(
  social_context = factor(c("Solo", "Duo", "Trio"), levels = c("Solo", "Duo", "Trio")),
  Exploit = pred_probs[,1],
  Explore = pred_probs[,2], 
  None = pred_probs[,3]
)

# Reshape manually
pred_long <- data.frame(
  social_context = rep(pred_df$social_context, 3),
  outcome = factor(rep(c("Exploit", "Explore", "None"), each = 3), 
                  levels = c("Exploit", "Explore", "None")),
  probability = c(pred_df$Exploit, pred_df$Explore, pred_df$None),
  type = "Predicted"
)

# Calculate observed proportions
obs_props <- data_clean %>%
  group_by(social_context, outcome) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(social_context) %>%
  mutate(
    total = sum(n),
    probability = n / total
  ) %>%
  ungroup() %>%
  mutate(type = "Observed") %>%
  select(social_context, outcome, probability, type)

# Combine data
combined_data <- rbind(
  pred_long[, c("social_context", "outcome", "probability", "type")],
  obs_props
)
combined_data$type <- factor(combined_data$type, levels = c("Observed", "Predicted"))

# Create plot - FIXED
p3 <- ggplot(combined_data, aes(x = social_context, y = probability, fill = outcome)) +
  geom_col(position = "stack", alpha = 0.8) +
  scale_fill_manual(values = c("Exploit" = "#33A02C", "Explore" = "#E31A1C", "None" = "#1F78B4"),
                    name = "Outcome") +
  facet_wrap(~type, ncol = 2) +
  labs(title = "Predicted vs. Observed Outcome Probabilities",
       x = "Social Context", y = "Probability") +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    strip.text = element_text(face = "bold", size = 11),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    legend.position = "bottom"
  )

ggsave("Report_Figure3_Predicted_Probabilities.pdf", p3, width = 10, height = 6, dpi = 300)
ggsave("Report_Figure3_Predicted_Probabilities.png", p3, width = 10, height = 6, dpi = 300)

# =============================================================================
# FIGURE 4: INDIVIDUAL DIFFERENCES - COMPLETELY FIXED
# =============================================================================

cat("Creating Figure 4: Individual Differences (FIXED)...\n")

# Extract ALL individual effects (including reference level as 0)
individual_effects <- coef_data %>%
  filter(grepl("monkey_id", term)) %>%
  mutate(
    monkey = gsub("monkey_id", "", term)
  )

# Add reference level (first level = 0 effect)
ref_level <- levels(data_clean$monkey_id)[1]
cat("Reference level (set to 0):", ref_level, "\n")

# Create reference rows for the reference monkey
ref_rows <- data.frame(
  outcome = c("Explore", "None"),
  term = paste0("monkey_id", ref_level),
  estimate = 0,
  se = 0,
  ci_lower = 0,
  ci_upper = 0,
  term_clean = ref_level,
  monkey = ref_level
)

# Combine with existing effects
individual_effects <- rbind(individual_effects, ref_rows)

# Create monkey info with initials and sex
monkey_info <- data.frame(
  monkey = c("ANEMONE", "CHOCOLAT", "DALI", "EBI", "FRAN", "ICE"),
  initial = c("A", "C", "D", "E", "F", "I"),
  sex = c("Female", "Female", "Male", "Male", "Male", "Female"),
  stringsAsFactors = FALSE
) %>%
  mutate(
    sex_initial = paste0(sex, "\n(", initial, ")"),
    monkey = factor(monkey, levels = c("ANEMONE", "CHOCOLAT", "DALI", "EBI", "FRAN", "ICE"))
  )

# Join with individual effects
individual_effects <- individual_effects %>%
  left_join(monkey_info, by = "monkey") %>%
  mutate(
    outcome = factor(outcome, levels = c("Explore", "None")),
    sex = factor(sex, levels = c("Male", "Female"))
  )

# Create individual differences plot with initials and sex grouping
p4 <- ggplot(individual_effects, aes(x = reorder(initial, as.numeric(sex)), y = estimate, fill = sex)) +
  geom_col(alpha = 0.8, width = 0.7) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_fill_manual(values = c("Male" = "#FF7F00", "Female" = "#6A3D9A"), name = "Sex") +
  facet_grid(outcome ~ sex, scales = "free", space = "free_x") +
  labs(title = "Individual Random Effects by Sex (Deviations from Population Mean)",
       subtitle = "Letters = Individual initials; Reference: A = 0",
       x = "Individual", y = "Random Effect Coefficient") +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    strip.text = element_text(face = "bold", size = 11),
    legend.position = "none"  # Remove legend since sex is in facets
  )

ggsave("Report_Figure4_Individual_Differences.pdf", p4, width = 10, height = 6, dpi = 300)
ggsave("Report_Figure4_Individual_Differences.png", p4, width = 10, height = 6, dpi = 300)

# =============================================================================
# SUMMARY STATISTICS TABLE
# =============================================================================

cat("Generating summary statistics...\n")

# Dataset summary
dataset_summary <- data_clean %>%
  group_by(social_context, outcome) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(social_context) %>%
  mutate(
    total = sum(n),
    proportion = n / total
  ) %>%
  ungroup()

# Individual summary
individual_summary <- data_clean %>%
  group_by(monkey_id, outcome) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(monkey_id) %>%
  mutate(
    total = sum(n),
    proportion = n / total
  ) %>%
  ungroup()

# Model performance
model_performance <- data.frame(
  Metric = c("AIC", "BIC", "Log-Likelihood", "Parameters"),
  Null = c(AIC(fit_null), BIC(fit_null), logLik(fit_null), 2),
  Fixed = c(AIC(fit_fix), BIC(fit_fix), logLik(fit_fix), 8),
  Hierarchical = c(AIC(fit_hier), BIC(fit_hier), logLik(fit_hier), 18)
)

# Save summaries
write.csv(dataset_summary, "Report_Dataset_Summary.csv", row.names = FALSE)
write.csv(individual_summary, "Report_Individual_Summary.csv", row.names = FALSE)
write.csv(model_performance, "Report_Model_Performance.csv", row.names = FALSE)
write.csv(model_comparison, "Report_Model_Comparison.csv", row.names = FALSE)

# Save individual effects for review
write.csv(individual_effects, "Report_Individual_Effects_Complete.csv", row.names = FALSE)

cat("\n=== COMPREHENSIVE REPORT FIGURES COMPLETED (FINAL) ===\n")
cat("======================================================\n")
cat("Generated files:\n")
cat("- Report_Figure1_Model_Comparison.pdf/.png\n")
cat("- Report_Figure2_Fixed_Effects.pdf/.png\n") 
cat("- Report_Figure3_Predicted_Probabilities.pdf/.png (FIXED - no duplication)\n")
cat("- Report_Figure4_Individual_Differences.pdf/.png (FIXED - includes ANEMONE, initials, sex groups)\n")
cat("- Report_Dataset_Summary.csv\n")
cat("- Report_Individual_Summary.csv\n")
cat("- Report_Model_Performance.csv\n")
cat("- Report_Model_Comparison.csv\n")
cat("- Report_Individual_Effects_Complete.csv (NEW - shows all individuals including reference)\n")
cat("\nAll figures fixed and ready for publication!\n") 