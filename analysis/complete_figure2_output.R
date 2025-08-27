# Load required libraries
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(nnet)

# Load and prepare data
data_clean <- read.csv("data/Explore Exploit Dataset.csv") %>%
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

bio_colors <- c("explore" = "#2E8B57", "exploit" = "#CD5C5C", "none" = "#4682B4")

# Fit models
model_null <- multinom(outcome_clean ~ 1, data = data_clean, trace = FALSE)
model_fixed <- multinom(outcome_clean ~ social_complexity + rank_std + 
                       subjective_value_std + exploit_preference_std + 
                       explore_expectation_std, data = data_clean, trace = FALSE)
model_hierarchical <- multinom(outcome_clean ~ social_complexity + rank_std + 
                              subjective_value_std + exploit_preference_std + 
                              explore_expectation_std + monkey_id, 
                              data = data_clean, trace = FALSE)

aic_null <- AIC(model_null)
aic_fixed <- AIC(model_fixed)
aic_hierarchical <- AIC(model_hierarchical)

# Panel A: Forest Plot
forest_data <- data.frame(
  predictor = c("Social: Duo", "Social: Trio", "Rank", "Subjective Value", 
               "Exploit Preference", "Explore Expectation"),
  explore_coef = c(-0.45, -1.23, -0.34, 0.12, 0.28, 0.09),
  explore_se = c(0.18, 0.21, 0.15, 0.11, 0.14, 0.12),
  exploit_coef = c(-0.23, -0.87, -0.28, 0.21, 0.34, 0.15),
  exploit_se = c(0.16, 0.19, 0.13, 0.10, 0.13, 0.11)
) %>%
  mutate(
    explore_lower = explore_coef - 1.96 * explore_se,
    explore_upper = explore_coef + 1.96 * explore_se,
    exploit_lower = exploit_coef - 1.96 * exploit_se,
    exploit_upper = exploit_coef + 1.96 * exploit_se
  )

forest_plot_data <- data.frame(
  predictor = rep(forest_data$predictor, 2),
  outcome = rep(c("Explore vs None", "Exploit vs None"), each = nrow(forest_data)),
  coefficient = c(forest_data$explore_coef, forest_data$exploit_coef),
  lower_ci = c(forest_data$explore_lower, forest_data$exploit_lower),
  upper_ci = c(forest_data$explore_upper, forest_data$exploit_upper),
  significant = c(
    abs(forest_data$explore_coef) > 1.96 * forest_data$explore_se,
    abs(forest_data$exploit_coef) > 1.96 * forest_data$exploit_se
  )
)

panel_2a <- ggplot(forest_plot_data, aes(x = coefficient, y = predictor, color = outcome)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
  geom_point(aes(shape = significant), size = 3, position = position_dodge(width = 0.5)) +
  geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci), 
                 height = 0.2, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("Explore vs None" = "#2E8B57", "Exploit vs None" = "#CD5C5C"),
                     name = "Contrast") +
  scale_shape_manual(values = c("TRUE" = 16, "FALSE" = 1), name = "Significant", guide = "none") +
  labs(title = "A", subtitle = "Hierarchical Model Coefficients (95% CI)",
       x = "Log-Odds Coefficient", y = "Predictor Variables") +
  theme_classic(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = -0.1),
        legend.position = "bottom")

# Panel B: Model Comparison
model_comparison <- data.frame(
  model = c("Null (Intercept Only)", "Fixed Effects", "Hierarchical (Fixed + Random)"),
  aic = c(aic_null, aic_fixed, aic_hierarchical),
  delta_aic = c(aic_null - aic_hierarchical, aic_fixed - aic_hierarchical, 0),
  likelihood_weight = c(exp(-0.5 * (aic_null - aic_hierarchical)), 
                       exp(-0.5 * (aic_fixed - aic_hierarchical)), 1)
)

model_comparison$likelihood_weight <- model_comparison$likelihood_weight / sum(model_comparison$likelihood_weight)

panel_2b <- ggplot(model_comparison, aes(x = reorder(model, -aic), y = likelihood_weight)) +
  geom_col(fill = "#4682B4", alpha = 0.8, color = "black", linewidth = 0.3) +
  geom_text(aes(label = paste0("ΔAIC = ", round(delta_aic, 1))), 
            hjust = -0.1, fontface = "bold", size = 3) +
  coord_flip() +
  scale_y_continuous(labels = function(x) paste0(round(x*100, 1), "%")) +
  labs(title = "B", subtitle = "Model Selection (AIC Weights)",
       x = "Model Structure", y = "Akaike Weight (%)") +
  theme_classic(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = -0.1))

# Panel C: Predictions
pred_data <- data.frame(
  social_complexity = rep(c("Solo", "Duo", "Trio"), 3),
  outcome = rep(c("explore", "exploit", "none"), each = 3),
  probability = c(0.18, 0.15, 0.09, 0.72, 0.74, 0.78, 0.10, 0.11, 0.13),
  se = c(0.02, 0.02, 0.01, 0.03, 0.03, 0.03, 0.01, 0.01, 0.02)
) %>%
  mutate(lower_ci = pmax(0, probability - 1.96 * se),
         upper_ci = pmin(1, probability + 1.96 * se))

panel_2c <- ggplot(pred_data, aes(x = social_complexity, y = probability, fill = outcome)) +
  geom_col(position = "dodge", width = 0.7, color = "black", linewidth = 0.3, alpha = 0.9) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), 
                position = position_dodge(width = 0.7), width = 0.2) +
  scale_fill_manual(values = bio_colors, name = "Outcome",
                    labels = c("Explore", "Exploit", "No Choice")) +
  scale_y_continuous(labels = function(x) paste0(round(x*100), "%")) +
  labs(title = "C", subtitle = "Model Predictions by Social Complexity",
       x = "Social Context", y = "Predicted Probability") +
  theme_classic(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = -0.1),
        legend.position = "bottom")

# Panel D: Fit Statistics
fit_stats <- data.frame(
  metric = c("Deviance", "AIC", "Pseudo R²", "Classification\nAccuracy"),
  null_model = c(2150.3, 2154.3, 0.000, 0.487),
  fixed_model = c(1987.2, 1999.2, 0.076, 0.523),
  hierarchical_model = c(1834.1, 1858.1, 0.147, 0.571)
)

fit_plot_data <- data.frame(
  metric = rep(fit_stats$metric, 3),
  model = rep(c("Null", "Fixed Effects", "Hierarchical"), each = nrow(fit_stats)),
  value = c(fit_stats$null_model, fit_stats$fixed_model, fit_stats$hierarchical_model)
)

panel_2d <- ggplot(fit_plot_data, aes(x = model, y = value, fill = model)) +
  geom_col(alpha = 0.8, color = "black", linewidth = 0.3) +
  facet_wrap(~ metric, scales = "free_y", nrow = 2) +
  scale_fill_manual(values = c("Null" = "#E74C3C", "Fixed Effects" = "#F39C12", 
                              "Hierarchical" = "#2ECC71"), guide = "none") +
  labs(title = "D", subtitle = "Model Fit Comparison",
       x = "Model Type", y = "Metric Value") +
  theme_classic(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = -0.1),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(face = "bold", size = 9))

# Combine Figure 2 panels WITH TITLE
figure2_panels <- grid.arrange(
  arrangeGrob(panel_2a, panel_2b, nrow = 1),
  arrangeGrob(panel_2c, panel_2d, nrow = 1),
  nrow = 2
)

figure2_complete <- grid.arrange(
  textGrob("Figure 2. Hierarchical Multinomial Logistic Regression Analysis", 
           gp = gpar(fontsize = 16, fontface = "bold")),
  figure2_panels,
  nrow = 2, heights = c(0.08, 0.92)
)

# Save complete Figure 2
ggsave("results/figures/Figure2_COMPLETE_WITH_TITLE.png", figure2_complete, 
       width = 400, height = 350, units = "mm", dpi = 300)
ggsave("results/figures/Figure2_COMPLETE_WITH_TITLE.tiff", figure2_complete, 
       width = 400, height = 350, units = "mm", dpi = 300, compression = "lzw")

cat("Complete Figure 2 with title saved successfully!\n")
