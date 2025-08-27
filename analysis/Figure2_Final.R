# Figure 2: Hierarchical Multinomial Logistic Regression - Final version
# Simple approach handling missing data correctly

library(dplyr)
library(ggplot2) 
library(gridExtra)
library(grid)
library(nnet)

# Load and clean data
cat("Loading data...\n")
data_raw <- read.csv("data/Explore Exploit Dataset.csv")

data_clean <- data_raw %>%
  mutate(
    monkey_id = as.factor(monkey),
    social_complexity = factor(CONDITION, levels = c("solo", "duo", "trio")),
    outcome_clean = case_when(
      grepl("explore", tolower(OUTCOME)) ~ "explore",
      grepl("exploit", tolower(OUTCOME)) ~ "exploit", 
      tolower(OUTCOME) %in% c("none", "nonne", "non", "stop") | OUTCOME == "" | is.na(OUTCOME) ~ "none",
      TRUE ~ "none"
    ),
    rank_std = scale(ABSOLUTE_RANK)[,1],
    subjective_value_std = scale(SUBJECTIVE_CHOSEN_VALUE)[,1],
    exploit_preference_std = scale(subjective_exploit)[,1],
    explore_expectation_std = scale(expected_explore)[,1]
  ) %>%
  filter(!is.na(outcome_clean) & outcome_clean != "" & 
         !is.na(social_complexity) & 
         !is.na(monkey) & monkey != "")

cat(paste("Initial sample size: N =", nrow(data_clean), "\n"))

# Create complete case dataset
complete_predictors <- c("outcome_clean", "social_complexity", "rank_std", 
                        "subjective_value_std", "exploit_preference_std", 
                        "explore_expectation_std", "monkey_id")

data_complete <- data_clean[complete.cases(data_clean[, complete_predictors]), ]
cat(paste("Complete case sample size: N =", nrow(data_complete), "\n"))

# Fit models on complete cases
cat("Fitting models...\n")

fit_null <- multinom(outcome_clean ~ 1, data = data_complete, trace = FALSE)
fit_fix <- multinom(outcome_clean ~ social_complexity + rank_std + 
                   subjective_value_std + exploit_preference_std + 
                   explore_expectation_std, data = data_complete, trace = FALSE)
fit_hier <- multinom(outcome_clean ~ social_complexity + rank_std + 
                    subjective_value_std + exploit_preference_std + 
                    explore_expectation_std + monkey_id, 
                    data = data_complete, trace = FALSE)

cat("Models fitted successfully\n")

# Model comparison
aic_values <- c(AIC(fit_null), AIC(fit_fix), AIC(fit_hier))
names(aic_values) <- c("Null", "Fixed", "Hierarchical")
cat("AIC values:", aic_values, "\n")

# =============================================================================
# PANEL A: Fixed Effects Forest Plot
# =============================================================================

coef_summary <- summary(fit_hier)
coef_matrix <- coef_summary$coefficients
se_matrix <- coef_summary$standard.errors

# Focus on fixed effects (social complexity and continuous predictors)
fixed_terms <- c("social_complexityduo", "social_complexitytrio", 
                "rank_std", "subjective_value_std", "exploit_preference_std", 
                "explore_expectation_std")

forest_data <- data.frame()
for(outcome in rownames(coef_matrix)) {
  for(term in fixed_terms) {
    if(term %in% colnames(coef_matrix)) {
      coef_val <- coef_matrix[outcome, term]
      se_val <- se_matrix[outcome, term]
      forest_data <- rbind(forest_data, data.frame(
        outcome = outcome,
        term = term,
        coefficient = coef_val,
        se = se_val,
        ci_lower = coef_val - 1.96 * se_val,
        ci_upper = coef_val + 1.96 * se_val
      ))
    }
  }
}

# Clean predictor names
forest_data$predictor <- case_when(
  forest_data$term == "social_complexityduo" ~ "Social: Duo vs Solo",
  forest_data$term == "social_complexitytrio" ~ "Social: Trio vs Solo", 
  forest_data$term == "rank_std" ~ "Dominance Rank",
  forest_data$term == "subjective_value_std" ~ "Subjective Value",
  forest_data$term == "exploit_preference_std" ~ "Exploit Preference",
  forest_data$term == "explore_expectation_std" ~ "Explore Expectation",
  TRUE ~ forest_data$term
)

panel_a <- ggplot(forest_data, aes(x = coefficient, y = predictor)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_pointrange(aes(xmin = ci_lower, xmax = ci_upper, color = outcome), 
                  position = position_dodge(width = 0.5), size = 0.7) +
  scale_color_manual(values = c("explore" = "#E31A1C", "none" = "#1F78B4"), 
                     labels = c("Explore vs Exploit", "None vs Exploit")) +
  labs(title = "A. Fixed Effects", 
       x = "Log-odds coefficient", 
       y = "Predictor") +
  annotate("text", x = Inf, y = Inf, label = paste("n =", nrow(data_complete)), 
           hjust = 1.1, vjust = 1.5, size = 3, fontface = "bold") +
  theme_classic(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    legend.position = "bottom",
    legend.title = element_blank()
  )

# =============================================================================
# PANEL B: Individual Variation (Summary stats)
# =============================================================================

# Calculate individual exploration rates for illustration
indiv_rates <- data_complete %>%
  group_by(monkey_id) %>%
  summarise(
    n_trials = n(),
    explore_rate = mean(outcome_clean == "explore"),
    exploit_rate = mean(outcome_clean == "exploit"),
    none_rate = mean(outcome_clean == "none"),
    .groups = "drop"
  ) %>%
  arrange(desc(explore_rate))

panel_b <- ggplot(indiv_rates, aes(x = reorder(monkey_id, explore_rate), y = explore_rate)) +
  geom_col(fill = "#E31A1C", color = "black", width = 0.7) +
  geom_text(aes(label = paste0(round(explore_rate*100,1), "%")), 
            hjust = -0.1, size = 3) +
  coord_flip() +
  labs(title = "B. Individual Variation", 
       x = "Individual", 
       y = "Exploration rate") +
  theme_classic(base_size = 10) +
  theme(plot.title = element_text(face = "bold", size = 12)) +
  ylim(0, max(indiv_rates$explore_rate) * 1.2)

# =============================================================================
# PANEL C: Social Context Effects
# =============================================================================

# Calculate choice proportions by social context
context_summary <- data_complete %>%
  group_by(social_complexity, outcome_clean) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(social_complexity) %>%
  mutate(proportion = count / sum(count))

panel_c <- ggplot(context_summary, aes(x = social_complexity, y = proportion, fill = outcome_clean)) +
  geom_col(position = "stack", color = "black", size = 0.3) +
  scale_fill_manual(values = c("explore" = "#E31A1C", "exploit" = "#CD5C5C", "none" = "#1F78B4"),
                    labels = c("Explore", "Exploit", "None")) +
  labs(title = "C. Social Context Effects", 
       x = "Social complexity", 
       y = "Proportion of choices",
       fill = "Choice type") +
  theme_classic(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  geom_text(data = context_summary %>% filter(outcome_clean == "explore"),
            aes(label = paste0(round(proportion*100,1), "%")), 
            position = position_stack(vjust = 0.5), size = 3, color = "white", fontface = "bold")

# =============================================================================
# PANEL D: Model Comparison
# =============================================================================

model_comparison <- data.frame(
  model = c("Null", "Fixed", "Hierarchical"),
  aic = aic_values,
  delta_aic = aic_values - min(aic_values)
)

model_comparison$weight <- exp(-0.5 * model_comparison$delta_aic)
model_comparison$weight <- model_comparison$weight / sum(model_comparison$weight)

panel_d <- ggplot(model_comparison, aes(x = model, y = delta_aic, fill = model)) +
  geom_col(color = "black", width = 0.7) +
  geom_text(aes(label = round(delta_aic, 1)), vjust = -0.5, size = 3, fontface = "bold") +
  scale_fill_manual(values = c("Null" = "#E74C3C", "Fixed" = "#F39C12", 
                              "Hierarchical" = "#2ECC71")) +
  labs(title = "D. Model Comparison", 
       x = "Model", 
       y = "ΔAIC") +
  annotate("text", x = 3, y = max(model_comparison$delta_aic) * 0.8, 
           label = paste("Weight =", round(model_comparison$weight[3], 3)), 
           size = 3, fontface = "bold") +
  theme_classic(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    legend.position = "none"
  )

# =============================================================================
# ASSEMBLY AND SAVE
# =============================================================================

cat("Assembling Figure 2...\n")

# Arrange in 2x2 grid
figure2 <- arrangeGrob(
  arrangeGrob(panel_a, panel_b, nrow = 1),
  arrangeGrob(panel_c, panel_d, nrow = 1),
  nrow = 2
)

# Add main title
figure2_final <- arrangeGrob(
  textGrob("Figure 2. Hierarchical Multinomial Logistic Regression Analysis", 
           gp = gpar(fontsize = 14, fontface = "bold")),
  figure2,
  nrow = 2, heights = c(0.06, 0.94)
)

# Save files
ggsave("results/figures/Figure2REDONE.pdf", figure2_final, 
       width = 210, height = 297, units = "mm", device = "pdf", dpi = 300)

ggsave("results/figures/Figure2REDONE.png", figure2_final, 
       width = 210, height = 297, units = "mm", dpi = 300)

cat("Figure 2 completed successfully!\n")
cat("Files saved:\n")
cat("- results/figures/Figure2REDONE.pdf\n")
cat("- results/figures/Figure2REDONE.png\n")

# Print summary
cat("\n=============================================================================\n")
cat("MODEL RESULTS SUMMARY:\n")
cat("=============================================================================\n")
cat("Sample size (complete cases):", nrow(data_complete), "\n")
cat("AIC values:\n")
print(aic_values)
cat("AIC weights:\n")
print(round(model_comparison$weight, 3))
cat("\nBest model: Hierarchical (weight =", round(model_comparison$weight[3], 3), ")\n")

# Social complexity effects
explore_rates <- context_summary %>% 
  filter(outcome_clean == "explore") %>% 
  arrange(social_complexity)
cat("\nExploration rates by social complexity:\n")
for(i in 1:nrow(explore_rates)) {
  cat(paste(explore_rates$social_complexity[i], ":", 
            round(explore_rates$proportion[i]*100, 1), "%\n"))
}

sessionInfo()
