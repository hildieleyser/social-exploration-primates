# Complete Statistical Analysis: Social Context Effects on Exploration-Exploitation Decisions
# Author: Hilde Leyser
# Date: 2024
# Description: Comprehensive analysis pipeline for social context effects on decision-making

# Load required libraries
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(brms)
library(tidybayes)
library(emmeans)
library(viridis)
library(scales)
library(gridExtra)
library(patchwork)

# Set random seed for reproducibility
set.seed(123)

# Set theme for consistent plotting
theme_set(theme_minimal(base_size = 12) + 
          theme(panel.background = element_rect(fill = "white"),
                plot.background = element_rect(fill = "white")))

# Color palette
COL_CONTEXT <- c("solo" = "#8E9BFF", "duo" = "#FF8C42", "trio" = "#EB4559")
COL_MONKEY <- c("A" = "#1f77b4", "C" = "#ff7f0e", "D" = "#2ca02c", 
                "E" = "#d62728", "F" = "#9467bd", "I" = "#8c564b")
COL_EFFECT <- c("pos" = "#1f77b4", "neg" = "#d62728")

# Data preprocessing
cat("Loading and preprocessing data...\n")

# Load data
df <- read.csv("data/Explore Exploit Dataset.csv", stringsAsFactors = FALSE) %>%
  filter(TRIAL_TYPE == "OIT_RE") %>%
  mutate(
    context = factor(CONDITION, levels = c("solo", "duo", "trio")),
    exp_reward = expected_explore,
    explore = as.numeric(grepl("explore", tolower(OUTCOME))),
    explore_rate = explore,
    partner_count = as.numeric(context) - 1,
    rank = RELATIVE_RANK,
    monkey_id = factor(monkey)
  ) %>%
  filter(!is.na(exp_reward), !is.na(explore), !is.na(rank), !is.na(context))

# Data summary
cat("Data summary:\n")
cat("Total trials:", nrow(df), "\n")
cat("Subjects:", length(unique(df$monkey_id)), "\n")
cat("Exploration rate:", mean(df$explore), "\n")

# Create running means for uncertainty analysis
df <- df %>%
  group_by(monkey_id) %>%
  mutate(
    run_mean_white = cummean(exp_reward),
    run_explore = cummean(explore)
  ) %>%
  ungroup()

# Statistical modeling
cat("Fitting Bayesian hierarchical model...\n")

# Fit main model
fit <- brm(
  explore ~ partner_count + rank + context + (1|monkey_id),
  data = df,
  family = bernoulli(),
  chains = 4, 
  iter = 2000, 
  seed = 123,
  silent = 2
)

# Model summary
cat("Model summary:\n")
print(summary(fit))

# Model diagnostics
cat("Model diagnostics:\n")
print(loo(fit))

# Marginal effects
cat("Calculating marginal effects...\n")
marginal_effects <- emmeans(fit, ~ partner_count)
print(marginal_effects)

# Create visualizations
cat("Creating visualizations...\n")

# 1. Uncertainty vs Exploration
R1_data <- df %>%
  group_by(context) %>%
  summarise(
    run_mean_white = mean(run_mean_white, na.rm = TRUE),
    run_explore = mean(run_explore, na.rm = TRUE),
    .groups = "drop"
  )

R1_plot <- ggplot(df, aes(run_mean_white, run_explore, colour = context)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.2) +
  scale_colour_manual(values = COL_CONTEXT, name = "Context") +
  labs(
    title = "Uncertainty drives exploration, but social context suppresses it",
    x = "Running mean expected reward",
    y = "Running mean exploration rate"
  ) +
  theme(legend.position = "top")

# 2. Individual trajectories
spaghetti_data <- df %>% 
  group_by(monkey_id, partner_count) %>% 
  summarise(explore_rate = mean(explore), .groups = "drop") %>% 
  mutate(initial = substr(as.character(monkey_id), 1, 1))

R2_plot <- ggplot(spaghetti_data, aes(x = factor(partner_count), y = explore_rate, 
                                      group = monkey_id, color = initial)) +
  geom_line(size = 1.2, alpha = 0.7) + 
  geom_point(size = 2) +
  stat_summary(fun = mean, geom = "line", aes(group = 1), 
               color = "black", size = 2, linetype = "dashed") +
  scale_color_manual(values = COL_MONKEY, name = "Monkey") +
  scale_x_discrete(labels = c("0" = "Solo", "1" = "Duo", "2" = "Trio")) +
  labs(
    title = "Individual monkey trajectories and group mean",
    x = "Number of partners",
    y = "Exploration rate"
  ) +
  theme(legend.position = "top")

# 3. Observed vs Predicted heatmaps
pred_grid <- expand.grid(
  partner_count = 0:2,
  rank = 1:3,
  context = c("solo", "duo", "trio")
) %>%
  mutate(epred = fitted(fit, newdata = ., scale = "response")[,1])

obs_rates <- df %>%
  group_by(partner_count, rank, context) %>%
  summarise(observed = mean(explore), .groups = "drop")

heat_pred <- pred_grid %>% mutate(type = "Predicted", value = epred)
heat_obs <- obs_rates %>% mutate(type = "Observed", value = observed)
heatmap_data <- bind_rows(
  heat_pred %>% select(partner_count, rank, context, type, value),
  heat_obs %>% select(partner_count, rank, context, type, value)
)

R3_plot <- ggplot(heatmap_data, aes(x = factor(partner_count), y = factor(rank), fill = value)) +
  geom_tile(color = "white", size = 0.5) +
  geom_text(aes(label = scales::percent(value, 1)), size = 5, color = "black") +
  scale_fill_viridis_c(option = "plasma", name = "Exploration") +
  facet_wrap(~type) +
  labs(
    title = "Observed vs Predicted Exploration Rates",
    x = "Number of partners",
    y = "Social rank"
  ) +
  theme(legend.position = "right")

# 4. Model effects
R4_draws <- fit %>% 
  spread_draws(b_partner_count, b_rank, b_contextduo, b_contexttrio) %>%
  pivot_longer(cols = starts_with("b_"), names_to = "term", values_to = ".value")

R4_summary <- R4_draws %>% 
  group_by(term) %>% 
  median_qi(.value) %>% 
  mutate(sign = ifelse(.value >= 0, "pos", "neg"))

R4_plot <- ggplot(R4_summary, aes(.value, reorder(term, .value), fill = sign)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_bar(stat = "identity", width = 0.6) +
  geom_errorbarh(aes(xmin = .lower, xmax = .upper), height = 0.2, color = "black") +
  scale_fill_manual(values = COL_EFFECT, guide = "none") +
  labs(
    title = "Model Effects (log-odds)",
    x = "Effect on exploration",
    y = "Predictor"
  )

# 5. Calibration plot
df$pred <- as.numeric(fitted(fit, scale = "response")[,1])
df$bin <- cut(df$pred, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE)

calib_data <- df %>%
  group_by(bin) %>%
  summarise(
    pred_mean = mean(pred), 
    obs_mean = mean(explore), 
    n = n(), 
    .groups = "drop"
  )

R5_plot <- ggplot(calib_data, aes(x = pred_mean, y = obs_mean)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40") +
  geom_point(size = 3, color = COL_CONTEXT[2]) +
  geom_line(size = 1, color = COL_CONTEXT[2]) +
  labs(
    title = "Calibration: Predicted vs Observed",
    x = "Predicted exploration probability",
    y = "Observed exploration probability"
  )

# 6. Posterior predictive check
ppc_data <- df %>%
  mutate(
    pred = fitted(fit, scale = "response"),
    context = factor(context, levels = c("solo", "duo", "trio"))
  )

obs_counts <- ppc_data %>%
  group_by(context) %>%
  summarise(observed = sum(explore), n = n(), .groups = "drop")

sim_counts <- as.data.frame(posterior_predict(fit))
sim_counts_long <- sim_counts %>%
  mutate(draw = row_number()) %>%
  pivot_longer(-draw, names_to = "trial", values_to = "sim_explore") %>%
  mutate(trial = as.integer(trial))

real_trials <- df %>% 
  mutate(trial = as.integer(row_number()), context = context)

sim_counts_long <- sim_counts_long %>%
  left_join(real_trials %>% select(trial, context), by = "trial")

ppc_plot_data <- sim_counts_long %>%
  group_by(context, draw) %>%
  summarise(sim = sum(sim_explore), .groups = "drop")

R6_plot <- ggplot() +
  geom_violin(data = ppc_plot_data, aes(x = context, y = sim, fill = context), alpha = 0.5) +
  geom_point(data = obs_counts, aes(x = context, y = observed), color = "black", size = 3) +
  scale_fill_manual(values = COL_CONTEXT) +
  labs(
    title = "Posterior Predictive Check",
    x = "Context",
    y = "Number of exploration choices"
  ) +
  theme(legend.position = "none")

# 7. Individual monkey barplots
R7_data <- df %>%
  group_by(monkey_id, context) %>%
  summarise(explore_rate = mean(explore), n = n(), .groups = "drop") %>%
  mutate(initial = substr(as.character(monkey_id), 1, 1))

R7_plot <- ggplot(R7_data, aes(x = initial, y = explore_rate, fill = context)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = COL_CONTEXT, name = "Context") +
  labs(
    title = "Exploration Rate by Monkey and Context",
    x = "Monkey",
    y = "Exploration rate"
  ) +
  theme(legend.position = "top")

# Save plots
cat("Saving plots...\n")
ggsave("results/figures/R1_uncertainty_vs_exploration.png", R1_plot, 
       width = 7, height = 5, dpi = 400)
ggsave("results/figures/R2_individual_trajectories.png", R2_plot, 
       width = 7, height = 5, dpi = 400)
ggsave("results/figures/R3_observed_vs_predicted.png", R3_plot, 
       width = 10, height = 5, dpi = 400)
ggsave("results/figures/R4_model_effects.png", R4_plot, 
       width = 7, height = 5, dpi = 400)
ggsave("results/figures/R5_calibration.png", R5_plot, 
       width = 7, height = 5, dpi = 400)
ggsave("results/figures/R6_posterior_predictive.png", R6_plot, 
       width = 7, height = 5, dpi = 400)
ggsave("results/figures/R7_individual_barplots.png", R7_plot, 
       width = 7, height = 5, dpi = 400)

# Save model
cat("Saving model...\n")
saveRDS(fit, "results/models/main_model.rds")

# Generate summary statistics
cat("Generating summary statistics...\n")
summary_stats <- df %>%
  group_by(context) %>%
  summarise(
    n_trials = n(),
    exploration_rate = mean(explore),
    exploration_se = sd(explore) / sqrt(n()),
    .groups = "drop"
  )

write.csv(summary_stats, "results/tables/summary_statistics.csv", row.names = FALSE)

# Print results
cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Plots saved to: results/figures/\n")
cat("Model saved to: results/models/main_model.rds\n")
cat("Summary statistics saved to: results/tables/summary_statistics.csv\n")

print(summary_stats) 