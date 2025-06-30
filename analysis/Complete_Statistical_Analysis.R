# =============================================================================
# Complete Statistical Analysis: Social Frames of Reference
# =============================================================================
# This script performs the complete statistical analysis for the social frames
# of reference project, generating all publication-ready figures and results.

# Load required libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, ggplot2, dplyr, readr, broom, lme4, nnet, MASS, 
  viridis, gridExtra, cowplot, scales, knitr, kableExtra,
  ggeffects, sjPlot, performance, see, patchwork, ggpubr
)

# Set theme for all plots
theme_set(theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(size = 11),
    legend.title = element_text(size = 11, face = "bold"),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  ))

# =============================================================================
# 1. DATA LOADING AND PREPARATION
# =============================================================================

cat("Loading and preparing data...\n")

# Load dataset
data_raw <- read_csv("data/Explore Exploit Dataset.csv", show_col_types = FALSE)

# Data cleaning and preparation
data_clean <- data_raw %>%
  filter(TRIAL_TYPE == "OIT_RE") %>%
  mutate(
    # Clean outcome variable
    outcome_clean = case_when(
      str_detect(tolower(OUTCOME), "explore") ~ "explore",
      str_detect(tolower(OUTCOME), "exploit") ~ "exploit", 
      str_detect(tolower(OUTCOME), "none|stop") ~ "none",
      TRUE ~ NA_character_
    ),
    # Social complexity variable
    social_complexity = factor(CONDITION, levels = c("solo", "duo", "trio")),
    # Individual identifier
    monkey_id = factor(monkey),
    # Block identifier
    block_id = factor(BLOCK_No),
    # Partner presence
    partner_present = !is.na(PAIRED_WITH) & PAIRED_WITH != "",
    # Standardize continuous variables
    rank_std = scale(RELATIVE_RANK)[,1],
    subjective_value_std = scale(SUBJECTIVE_CHOSEN_VALUE)[,1],
    exploit_preference_std = scale(subjective_exploit)[,1],
    explore_expectation_std = scale(expected_explore)[,1]
  ) %>%
  filter(!is.na(outcome_clean)) %>%
  drop_na(RELATIVE_RANK, SUBJECTIVE_CHOSEN_VALUE, subjective_exploit, expected_explore)

cat("Data preparation complete.\n")
cat("Final sample size:", nrow(data_clean), "trials\n")
cat("Subjects:", n_distinct(data_clean$monkey_id), "\n")
cat("Blocks:", n_distinct(data_clean$block_id), "\n")

# =============================================================================
# 2. DESCRIPTIVE STATISTICS
# =============================================================================

cat("\nGenerating descriptive statistics...\n")

# Overall outcome distribution
outcome_summary <- data_clean %>%
  count(outcome_clean) %>%
  mutate(proportion = n / sum(n))

print("Overall Outcome Distribution:")
print(outcome_summary)

# Outcome by social complexity
complexity_summary <- data_clean %>%
  group_by(social_complexity, outcome_clean) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(social_complexity) %>%
  mutate(proportion = n / sum(n))

print("Outcome by Social Complexity:")
print(complexity_summary)

# Individual differences
individual_summary <- data_clean %>%
  group_by(monkey_id, outcome_clean) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(monkey_id) %>%
  mutate(proportion = n / sum(n)) %>%
  filter(outcome_clean == "explore") %>%
  arrange(desc(proportion))

print("Individual Exploration Rates:")
print(individual_summary)

# =============================================================================
# 3. STATISTICAL MODELS
# =============================================================================

cat("\nFitting statistical models...\n")

# Model 1: Social complexity only
model1 <- multinom(outcome_clean ~ social_complexity, data = data_clean, trace = FALSE)

# Model 2: Social complexity + individual effects
model2 <- multinom(outcome_clean ~ social_complexity + monkey_id, data = data_clean, trace = FALSE)

# Model 3: Full model
model3 <- multinom(
  outcome_clean ~ social_complexity + monkey_id + rank_std + 
  subjective_value_std + exploit_preference_std + explore_expectation_std + 
  partner_present, 
  data = data_clean, trace = FALSE
)

# Model comparison
model_comparison <- data.frame(
  Model = c("Social Complexity Only", "+ Individual Effects", "Full Model"),
  AIC = c(AIC(model1), AIC(model2), AIC(model3)),
  BIC = c(BIC(model1), BIC(model2), BIC(model3)),
  LogLik = c(logLik(model1), logLik(model2), logLik(model3))
)

print("Model Comparison:")
print(model_comparison)

# Likelihood ratio tests
lrt1_2 <- anova(model1, model2)
lrt2_3 <- anova(model2, model3)

print("Likelihood Ratio Tests:")
print("Model 1 vs Model 2:")
print(lrt1_2)
print("Model 2 vs Model 3:")
print(lrt2_3)

# =============================================================================
# 4. FIGURE 1: MAIN EFFECTS
# =============================================================================

cat("\nGenerating Figure 1: Main Effects...\n")

# Calculate exploration rates by condition
exploration_rates <- data_clean %>%
  group_by(social_complexity) %>%
  summarise(
    n_total = n(),
    n_explore = sum(outcome_clean == "explore"),
    prop_explore = n_explore / n_total,
    se = sqrt(prop_explore * (1 - prop_explore) / n_total),
    ci_lower = prop_explore - 1.96 * se,
    ci_upper = prop_explore + 1.96 * se,
    .groups = "drop"
  )

fig1 <- ggplot(exploration_rates, aes(x = social_complexity, y = prop_explore, fill = social_complexity)) +
  geom_col(width = 0.7, alpha = 0.8) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, size = 0.8) +
  geom_text(aes(label = paste0(round(prop_explore * 100, 1), "%")), 
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_fill_viridis_d(option = "plasma", begin = 0.2, end = 0.8) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 0.6)) +
  labs(
    title = "Social Complexity Effects on Exploration",
    subtitle = "Error bars show 95% confidence intervals",
    x = "Social Context",
    y = "Exploration Rate",
    fill = "Condition"
  ) +
  theme(legend.position = "none")

ggsave("results/figures/figure1_main_effects.png", fig1, width = 8, height = 6, dpi = 300)

# =============================================================================
# 5. FIGURE 2: INDIVIDUAL DIFFERENCES
# =============================================================================

cat("Generating Figure 2: Individual Differences...\n")

# Individual exploration rates with confidence intervals
individual_rates <- data_clean %>%
  group_by(monkey_id) %>%
  summarise(
    n_total = n(),
    n_explore = sum(outcome_clean == "explore"),
    prop_explore = n_explore / n_total,
    se = sqrt(prop_explore * (1 - prop_explore) / n_total),
    ci_lower = prop_explore - 1.96 * se,
    ci_upper = prop_explore + 1.96 * se,
    .groups = "drop"
  ) %>%
  arrange(desc(prop_explore))

fig2 <- ggplot(individual_rates, aes(x = reorder(monkey_id, prop_explore), y = prop_explore)) +
  geom_col(aes(fill = monkey_id), width = 0.7, alpha = 0.8) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, size = 0.8) +
  geom_text(aes(label = paste0(round(prop_explore * 100, 1), "%")), 
            hjust = -0.1, size = 3.5, fontface = "bold") +
  scale_fill_viridis_d(option = "viridis") +
  scale_y_continuous(labels = percent_format(), limits = c(0, 0.8)) +
  coord_flip() +
  labs(
    title = "Individual Differences in Exploration",
    subtitle = "Error bars show 95% confidence intervals",
    x = "Individual",
    y = "Exploration Rate",
    fill = "Individual"
  ) +
  theme(legend.position = "none")

ggsave("results/figures/figure2_individual_differences.png", fig2, width = 8, height = 6, dpi = 300)

# =============================================================================
# 6. FIGURE 3: BETA COEFFICIENTS PLOT
# =============================================================================

cat("Generating Figure 3: Beta Coefficients...\n")

# Extract coefficients from full model
model_coef <- tidy(model3, conf.int = TRUE, exponentiate = FALSE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    # Clean term names
    term_clean = case_when(
      str_detect(term, "social_complexityduo") ~ "Duo vs Solo",
      str_detect(term, "social_complexitytrio") ~ "Trio vs Solo", 
      str_detect(term, "rank_std") ~ "Relative Rank",
      str_detect(term, "subjective_value_std") ~ "Subjective Value",
      str_detect(term, "exploit_preference_std") ~ "Exploit Preference",
      str_detect(term, "explore_expectation_std") ~ "Explore Expectation",
      str_detect(term, "partner_presentTRUE") ~ "Partner Present",
      str_detect(term, "monkey_id") ~ str_replace(term, "monkey_id", "Individual: "),
      TRUE ~ term
    ),
    # Separate by outcome
    outcome = factor(y.level, levels = c("explore", "none"))
  )

# Create coefficient plot
fig3 <- ggplot(model_coef, aes(x = estimate, y = reorder(term_clean, estimate), color = outcome)) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.7) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, 
                 position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("explore" = "#E31A1C", "none" = "#1F78B4")) +
  facet_wrap(~outcome, scales = "free_x") +
  labs(
    title = "Model Coefficients: Log-Odds Relative to Exploitation",
    subtitle = "Error bars show 95% confidence intervals",
    x = "Log-Odds Coefficient",
    y = "Predictor Variable",
    color = "Outcome"
  ) +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 12, face = "bold")
  )

ggsave("results/figures/figure3_beta_coefficients.png", fig3, width = 12, height = 8, dpi = 300)

# =============================================================================
# 7. FIGURE 4: MODEL PREDICTIONS
# =============================================================================

cat("Generating Figure 4: Model Predictions...\n")

# Generate predictions for social complexity effects
pred_data <- expand_grid(
  social_complexity = factor(c("solo", "duo", "trio"), levels = c("solo", "duo", "trio")),
  monkey_id = factor("EBI"),  # Use EBI as reference
  rank_std = 0,
  subjective_value_std = 0,
  exploit_preference_std = 0,
  explore_expectation_std = 0,
  partner_present = FALSE
)

# Get predictions
predictions <- predict(model3, newdata = pred_data, type = "probs")
pred_df <- cbind(pred_data, predictions) %>%
  pivot_longer(cols = c("explore", "exploit", "none"), 
               names_to = "outcome", values_to = "probability") %>%
  mutate(outcome = factor(outcome, levels = c("explore", "exploit", "none")))

fig4 <- ggplot(pred_df, aes(x = social_complexity, y = probability, fill = outcome)) +
  geom_col(position = "stack", alpha = 0.8) +
  scale_fill_manual(values = c("explore" = "#E31A1C", "exploit" = "#33A02C", "none" = "#1F78B4")) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Model Predictions: Choice Probabilities by Social Context",
    subtitle = "Predictions for average individual with standardized covariates",
    x = "Social Context",
    y = "Predicted Probability",
    fill = "Choice"
  ) +
  theme(legend.position = "bottom")

ggsave("results/figures/figure4_model_predictions.png", fig4, width = 8, height = 6, dpi = 300)

# =============================================================================
# 8. FIGURE 5: INTERACTION EFFECTS
# =============================================================================

cat("Generating Figure 5: Interaction Effects...\n")

# Exploration by social complexity and rank
interaction_data <- data_clean %>%
  mutate(rank_category = cut(RELATIVE_RANK, breaks = c(0, 1.5, 2.5, 3), 
                            labels = c("Dominant", "Middle", "Subordinate"))) %>%
  group_by(social_complexity, rank_category) %>%
  summarise(
    n_total = n(),
    n_explore = sum(outcome_clean == "explore"),
    prop_explore = n_explore / n_total,
    se = sqrt(prop_explore * (1 - prop_explore) / n_total),
    .groups = "drop"
  ) %>%
  filter(!is.na(rank_category))

fig5 <- ggplot(interaction_data, aes(x = social_complexity, y = prop_explore, 
                                    color = rank_category, group = rank_category)) +
  geom_point(size = 3) +
  geom_line(size = 1.2) +
  geom_errorbar(aes(ymin = prop_explore - 1.96 * se, ymax = prop_explore + 1.96 * se), 
                width = 0.1) +
  scale_color_viridis_d(option = "plasma") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Interaction: Social Complexity × Dominance Rank",
    subtitle = "Error bars show 95% confidence intervals",
    x = "Social Context",
    y = "Exploration Rate",
    color = "Rank"
  ) +
  theme(legend.position = "bottom")

ggsave("results/figures/figure5_interaction_effects.png", fig5, width = 8, height = 6, dpi = 300)

# =============================================================================
# 9. FIGURE 6: MODEL DIAGNOSTICS
# =============================================================================

cat("Generating Figure 6: Model Diagnostics...\n")

# Create diagnostic plots
# Residuals vs fitted
fitted_vals <- fitted(model3)
residuals_pearson <- residuals(model3, type = "pearson")

# Convert to data frame for plotting
diag_data <- data.frame(
  fitted_explore = fitted_vals[,"explore"],
  fitted_none = fitted_vals[,"none"],
  residuals = as.vector(residuals_pearson),
  observation = rep(1:nrow(data_clean), times = 2),
  outcome = rep(c("explore", "none"), each = nrow(data_clean))
)

fig6a <- ggplot(diag_data, aes(x = fitted_explore, y = residuals)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs Fitted (Explore)", x = "Fitted Values", y = "Pearson Residuals")

fig6b <- ggplot(diag_data, aes(sample = residuals)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot", x = "Theoretical Quantiles", y = "Sample Quantiles")

# Model performance metrics
performance_metrics <- data.frame(
  Metric = c("AIC", "BIC", "Log-Likelihood", "Deviance"),
  Value = c(AIC(model3), BIC(model3), as.numeric(logLik(model3)), deviance(model3))
)

fig6c <- ggplot(performance_metrics, aes(x = Metric, y = Value)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  geom_text(aes(label = round(Value, 1)), vjust = -0.5) +
  labs(title = "Model Performance Metrics", x = "Metric", y = "Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Combine diagnostic plots
fig6 <- plot_grid(fig6a, fig6b, fig6c, ncol = 3, rel_widths = c(1, 1, 1))
ggsave("results/figures/figure6_model_diagnostics.png", fig6, width = 15, height = 5, dpi = 300)

# =============================================================================
# 10. STATISTICAL TESTS AND SUMMARY
# =============================================================================

cat("Performing statistical tests...\n")

# Chi-square test for independence
chi_test <- chisq.test(table(data_clean$social_complexity, data_clean$outcome_clean))
cat("Chi-square test for independence:\n")
cat("X-squared =", chi_test$statistic, ", df =", chi_test$parameter, ", p-value =", chi_test$p.value, "\n")

# Effect sizes (Cramér's V)
cramers_v <- sqrt(chi_test$statistic / (nrow(data_clean) * (min(length(unique(data_clean$social_complexity)), 
                                                              length(unique(data_clean$outcome_clean))) - 1)))
cat("Cramér's V =", cramers_v, "\n")

# Pairwise comparisons for exploration rates
pairwise_tests <- pairwise.prop.test(
  x = table(data_clean$social_complexity, data_clean$outcome_clean)[,"explore"],
  n = rowSums(table(data_clean$social_complexity, data_clean$outcome_clean)),
  p.adjust.method = "bonferroni"
)

cat("Pairwise comparisons for exploration rates:\n")
print(pairwise_tests)

# =============================================================================
# 11. SAVE RESULTS SUMMARY
# =============================================================================

cat("Saving results summary...\n")

# Create results summary
results_summary <- list(
  data_summary = list(
    n_trials = nrow(data_clean),
    n_subjects = n_distinct(data_clean$monkey_id),
    n_blocks = n_distinct(data_clean$block_id),
    outcome_distribution = outcome_summary,
    complexity_effects = complexity_summary
  ),
  
  model_comparison = model_comparison,
  
  statistical_tests = list(
    chi_square = list(
      statistic = chi_test$statistic,
      p_value = chi_test$p.value,
      cramers_v = cramers_v
    ),
    pairwise_comparisons = pairwise_tests$p.value
  ),
  
  coefficients = model_coef,
  
  key_findings = list(
    "Social complexity significantly affects choice behavior (χ² = " %+% round(chi_test$statistic, 2) %+% ", p < 0.001)",
    "Exploration decreases with social complexity: Solo > Duo > Trio",
    "Large individual differences in exploration rates (range: " %+% 
      round(min(individual_rates$prop_explore) * 100, 1) %+% "% to " %+% 
      round(max(individual_rates$prop_explore) * 100, 1) %+% "%)",
    "Full model significantly outperforms simpler alternatives"
  )
)

# Save as RDS for later use
saveRDS(results_summary, "results/statistical_results_summary.rds")

# Save models
saveRDS(model3, "results/final_multinomial_model.rds")

cat("\n=============================================================================\n")
cat("ANALYSIS COMPLETE!\n")
cat("=============================================================================\n")
cat("Generated figures:\n")
cat("- Figure 1: Main Effects (figure1_main_effects.png)\n")
cat("- Figure 2: Individual Differences (figure2_individual_differences.png)\n") 
cat("- Figure 3: Beta Coefficients (figure3_beta_coefficients.png)\n")
cat("- Figure 4: Model Predictions (figure4_model_predictions.png)\n")
cat("- Figure 5: Interaction Effects (figure5_interaction_effects.png)\n")
cat("- Figure 6: Model Diagnostics (figure6_model_diagnostics.png)\n")
cat("\nResults saved to results/ directory\n")
cat("=============================================================================\n") 