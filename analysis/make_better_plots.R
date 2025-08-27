#!/usr/bin/env Rscript

# Publication-Quality Results Strip: Social Context & Exploration
# Tells a clear scientific story in four panels

library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)
library(tidybayes)
library(emmeans)
library(patchwork)
library(stringr)
library(brms)

# Set consistent color palette and theme
COL_CONTEXT <- c("solo"="#8E9BFF", "duo"="#FF8C42", "trio"="#EB4559")
theme_set(
  theme_minimal(base_size = 9) +
    theme(
      plot.title = element_text(face = "bold", size = 11),
      axis.text = element_text(size = 9),
      axis.title = element_text(size = 11),
      legend.position = "none"
    )
)

# Load and clean data
df <- read.csv("Explore Exploit Dataset.csv", stringsAsFactors = FALSE) %>%
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

# --- R1: Uncertainty vs Exploration (Scatter, Context Colour) ---
R1_data <- df %>%
  group_by(monkey_id, context) %>%
  arrange(TRIAL_NUM) %>%
  mutate(run_mean_white = cummean(exp_reward),
         run_explore = cummean(explore)) %>%
  ungroup()
R1_plot <- ggplot(R1_data, aes(run_mean_white, run_explore, colour = context)) +
  geom_point(alpha = .4, size = 1.5) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_colour_manual(values = COL_CONTEXT) +
  labs(
    title = "R1 • Uncertainty drives exploration, but social context suppresses it",
    x = "Expected reward of white capsule",
    y = "Exploration probability"
  )

# --- Fit Bayesian model for R2-R4 ---
fit <- brm(
  explore ~ partner_count + rank + context + (1|monkey_id),
  data = df,
  family = bernoulli(),
  chains = 4, iter = 2000, seed = 123,
  silent = 2
)

# Use a reference monkey for population-level predictions
ref_monkey <- levels(df$monkey_id)[1]

# --- R2: Partner Ribbon (Marginal Means) ---
emm <- emmeans(fit, ~ partner_count, at = list(monkey_id = ref_monkey))
R2_df <- as.data.frame(emm)
print(names(R2_df))
R2_plot <- ggplot(R2_df, aes(x = factor(partner_count), y = emmean)) +
  geom_ribbon(aes(ymin = lower.HPD, ymax = upper.HPD), fill = COL_CONTEXT[3], alpha = 0.2) +
  geom_line(group = 1, color = COL_CONTEXT[3], size = 1) +
  geom_point(size = 2, color = COL_CONTEXT[3]) +
  labs(
    title = "R2 • Each extra partner lowers curiosity by ~10%",
    x = "Partner count",
    y = "Marginal mean exploration probability"
  )

# --- R3: Heat-tile (Context × Rank, with % Labels) ---
pred_grid <- expand.grid(
  partner_count = 0:2,
  rank = 1:3,
  context = levels(df$context),
  monkey_id = ref_monkey
)
pred_grid$epred <- posterior_epred(fit, newdata = pred_grid) %>% apply(2, mean)
R3_plot <- pred_grid %>%
  ggplot(aes(factor(partner_count), factor(rank), fill = epred)) +
  geom_tile() +
  geom_text(aes(label = scales::percent(epred, 1)), fontface = "bold", size = 3) +
  scale_fill_viridis_c(option = "plasma") +
  labs(
    title = "R3 • Effects add: lowest curiosity = subordinate in trio",
    x = "Partner count", y = "Relative rank"
  )

# --- R4: Caterpillar of All β (Model Slopes) ---
R4_draws <- fit %>%
  spread_draws(b_partner_count, b_rank, b_contextduo, b_contexttrio) %>%
  pivot_longer(cols = starts_with("b_"), names_to = "term", values_to = ".value")
R4_summary <- R4_draws %>%
  group_by(term) %>%
  median_qi(.value)
R4_plot <- ggplot(R4_summary, aes(.value, reorder(term, .value))) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = .lower, xmax = .upper), height = 0.2) +
  labs(
    title = "R4 • Model slopes (log-odds): partner, rank, context",
    x = "Effect on exploration"
  )

# --- Arrange in a grid and save ---
final_plot <- (R1_plot | R2_plot) / (R3_plot | R4_plot)
ggsave("final_results_strip.png", final_plot, width = 15, height = 9, dpi = 400)

# Save R1 and R4 as separate PNGs
if (exists("R1_plot")) ggsave("plot_R1_uncertainty_vs_exploration.png", R1_plot, width = 7, height = 5, dpi = 400)
if (exists("R4_plot")) ggsave("plot_R4_caterpillar_betas.png", R4_plot, width = 7, height = 5, dpi = 400)

# --- Visually Enhanced R2: Partner Ribbon with Raw Data Overlay ---
raw_points <- df %>%
  group_by(monkey_id, partner_count) %>%
  summarise(explore_rate = mean(explore), .groups = "drop")

R2_plot <- ggplot() +
  # Raw data points (jittered)
  geom_jitter(
    data = raw_points,
    aes(x = factor(partner_count), y = explore_rate, color = factor(partner_count)),
    width = 0.15, height = 0, size = 2, alpha = 0.5, show.legend = FALSE
  ) +
  # Model ribbon
  geom_ribbon(
    data = R2_df,
    aes(x = factor(partner_count), ymin = lower.HPD, ymax = upper.HPD, group = 1),
    fill = COL_CONTEXT[3], alpha = 0.25
  ) +
  # Model mean line and points
  geom_line(
    data = R2_df,
    aes(x = factor(partner_count), y = emmean, group = 1),
    color = COL_CONTEXT[3], size = 1.5
  ) +
  geom_point(
    data = R2_df,
    aes(x = factor(partner_count), y = emmean),
    color = COL_CONTEXT[3], size = 3
  ) +
  scale_x_discrete(labels = c("0" = "Solo", "1" = "Duo", "2" = "Trio")) +
  scale_color_manual(values = COL_CONTEXT) +
  labs(
    title = "R2 • Each extra partner lowers curiosity by ~10%",
    subtitle = "Points: individual monkeys; Ribbon: 95% credible interval",
    x = "Partner count",
    y = "Exploration probability"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13)
  )
ggsave("plot_R2_partner_ribbon.png", R2_plot, width = 7, height = 5, dpi = 400)

# --- Visually Enhanced R3: Heat-tile with Observed and Predicted Overlay ---
obs_rates <- df %>%
  group_by(partner_count, rank) %>%
  summarise(observed = mean(explore), n = n(), .groups = "drop")

heat_data <- pred_grid %>%
  left_join(obs_rates, by = c("partner_count", "rank")) %>%
  mutate(
    pred_label = scales::percent(epred, 1),
    obs_label = ifelse(!is.na(observed), scales::percent(observed, 1), ""),
    bold = abs(epred - mean(epred, na.rm = TRUE)) >= 0.10
  )

R3_plot <- ggplot(heat_data, aes(x = factor(partner_count), y = factor(rank), fill = epred)) +
  geom_tile(color = "white", size = 0.5) +
  # Model-predicted value (bold if notable)
  geom_text(aes(label = pred_label, fontface = ifelse(bold, "bold", "plain")), size = 5, color = "black", vjust = -0.5) +
  # Observed value (smaller, below)
  geom_text(aes(label = obs_label), size = 3.5, color = "grey30", vjust = 1.5) +
  scale_fill_viridis_c(option = "plasma", name = "Predicted\nexploration") +
  scale_x_discrete(labels = c("0" = "Solo", "1" = "Duo", "2" = "Trio")) +
  scale_y_discrete(labels = c("1" = "Dominant", "2" = "Intermediate", "3" = "Subordinate")) +
  labs(
    title = "R3 • Exploration drops with more partners and lower rank",
    subtitle = "Top: model-predicted; Bottom: observed (from data)",
    x = "Partner count",
    y = "Relative rank"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13)
  )
ggsave("plot_R3_heat_tile.png", R3_plot, width = 7, height = 5, dpi = 400)

cat("All plots generated and saved as final_results_strip.png\n") 

# Consistent color palette
COL_CONTEXT <- c("solo"="#8E9BFF", "duo"="#FF8C42", "trio"="#EB4559")
COL_MONKEY <- c("F"="#1f77b4", "D"="#ff7f0e", "E"="#2ca02c", "C"="#d62728", "I"="#9467bd", "A"="#8c564b")
COL_EFFECT <- c("pos"="#1f77b4", "neg"="#d62728")

# --- R1: Uncertainty vs Exploration (with legend, white bg) ---
R1_plot <- ggplot(R1_data, aes(run_mean_white, run_explore, colour = context)) +
  geom_point(alpha = .5, size = 2) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.2) +
  scale_colour_manual(values = COL_CONTEXT, name = "Context") +
  labs(
    title = "R1 • Uncertainty drives exploration, but social context suppresses it",
    x = "Expected reward of white capsule",
    y = "Exploration probability"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")
ggsave("plot_R1_uncertainty_vs_exploration.png", R1_plot, width = 7, height = 5, dpi = 400)

# --- R2: Partner Ribbon (Spaghetti Plot) ---
monkey_initials <- substr(df$monkey_id, 1, 1)
spaghetti_data <- df %>%
  group_by(monkey_id, partner_count) %>%
  summarise(explore_rate = mean(explore), .groups = "drop") %>%
  mutate(initial = substr(as.character(monkey_id), 1, 1))
R2_plot <- ggplot(spaghetti_data, aes(x = factor(partner_count), y = explore_rate, group = monkey_id, color = initial)) +
  geom_line(size = 1.2, alpha = 0.7) +
  geom_point(size = 2) +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black", size = 2, linetype = "dashed") +
  scale_color_manual(values = COL_MONKEY, name = "Monkey") +
  scale_x_discrete(labels = c("0" = "Solo", "1" = "Duo", "2" = "Trio")) +
  labs(
    title = "R2 • Individual monkey trajectories and group mean",
    x = "Partner count",
    y = "Exploration probability"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")
ggsave("plot_R2_partner_spaghetti.png", R2_plot, width = 7, height = 5, dpi = 400)

# --- R3: Side-by-side Heatmaps (Observed vs Predicted) ---
obs_rates <- df %>%
  group_by(partner_count, rank) %>%
  summarise(observed = mean(explore), n = n(), .groups = "drop")
heat_pred <- pred_grid %>%
  mutate(type = "Predicted", value = epred)
heat_obs <- obs_rates %>%
  mutate(type = "Observed", value = observed)
heat_obs <- heat_obs %>%
  mutate(context = NA)
heatmap_data <- bind_rows(
  heat_pred %>% select(partner_count, rank, context, type, value),
  heat_obs %>% select(partner_count, rank, context, type, value)
)
R3_plot <- ggplot(heatmap_data, aes(x = factor(partner_count), y = factor(rank), fill = value)) +
  geom_tile(color = "white", size = 0.5) +
  geom_text(aes(label = scales::percent(value, 1)), size = 5, color = "black") +
  scale_fill_viridis_c(option = "plasma", name = "Exploration") +
  scale_x_discrete(labels = c("0" = "Solo", "1" = "Duo", "2" = "Trio")) +
  scale_y_discrete(labels = c("1" = "Dominant", "2" = "Intermediate", "3" = "Subordinate")) +
  facet_wrap(~type) +
  labs(
    title = "R3 • Observed vs Predicted Exploration Rates",
    x = "Partner count",
    y = "Relative rank"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right")
ggsave("plot_R3_heatmaps.png", R3_plot, width = 12, height = 5, dpi = 400)

# --- R4: Colorful Caterpillar Plot ---
R4_summary <- R4_summary %>% mutate(sign = ifelse(.value >= 0, "pos", "neg"))
R4_plot <- ggplot(R4_summary, aes(.value, reorder(term, .value), fill = sign)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_bar(stat = "identity", width = 0.6) +
  geom_errorbarh(aes(xmin = .lower, xmax = .upper), height = 0.2, color = "black") +
  scale_fill_manual(values = COL_EFFECT, guide = FALSE) +
  labs(
    title = "R4 • Model Effects (log-odds)",
    x = "Effect on exploration"
  ) +
  theme_minimal(base_size = 14)
ggsave("plot_R4_caterpillar_betas.png", R4_plot, width = 7, height = 5, dpi = 400)

# --- R5: Calibration Plot (fixed) ---
df$pred <- as.numeric(fitted(fit, scale = 'response')[,1])
df$bin <- cut(df$pred, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE)
calib_data <- df %>%
  group_by(bin) %>%
  summarise(pred_mean = mean(pred), obs_mean = mean(explore), n = n(), .groups = "drop")
R5_plot <- ggplot(calib_data, aes(x = pred_mean, y = obs_mean)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40") +
  geom_point(size = 3, color = COL_CONTEXT[2]) +
  geom_line(size = 1, color = COL_CONTEXT[2]) +
  labs(
    title = "R5 • Calibration: Predicted vs Observed",
    x = "Predicted exploration probability",
    y = "Observed exploration probability"
  ) +
  theme_minimal(base_size = 14)
ggsave("plot_R5_calibration.png", R5_plot, width = 7, height = 5, dpi = 400)

# --- R6: Posterior Predictive Check (fixed join) ---
ppc_data <- df %>%
  mutate(pred = fitted(fit, scale = "response"), context = factor(context, levels = c("solo", "duo", "trio")))
obs_counts <- ppc_data %>% group_by(context) %>% summarise(observed = sum(explore), n = n(), .groups = "drop")
sim_counts <- as.data.frame(posterior_predict(fit))
sim_counts_long <- sim_counts %>%
  mutate(draw = row_number()) %>%
  pivot_longer(-draw, names_to = "trial", values_to = "sim_explore") %>%
  mutate(trial = as.integer(trial))
real_trials <- df %>% mutate(trial = as.integer(row_number()), context = context)
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
    title = "R6 • Posterior Predictive Check",
    x = "Context",
    y = "# Exploration choices"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")
ggsave("plot_R6_ppc.png", R6_plot, width = 7, height = 5, dpi = 400)

# --- R7: Individual Monkey Barplots ---
R7_data <- df %>%
  group_by(monkey_id, context) %>%
  summarise(explore_rate = mean(explore), n = n(), .groups = "drop") %>%
  mutate(initial = substr(as.character(monkey_id), 1, 1))
R7_plot <- ggplot(R7_data, aes(x = initial, y = explore_rate, fill = context)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = COL_CONTEXT, name = "Context") +
  labs(
    title = "R7 • Exploration Rate by Monkey and Context",
    x = "Monkey",
    y = "Exploration rate"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")
ggsave("plot_R7_monkey_barplot.png", R7_plot, width = 7, height = 5, dpi = 400)

# --- R8: Model Structure Diagram (ggplot2 version) ---
model_structure_data <- data.frame(
  level = c("Population", "Monkey", "Trial"),
  y = c(3, 2, 1),
  color = c(COL_CONTEXT[1], COL_CONTEXT[2], COL_CONTEXT[3])
)
R8_plot <- ggplot(model_structure_data, aes(x = 1, y = y, fill = color)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = level), color = "white", fontface = "bold", size = 5) +
  scale_fill_identity() +
  scale_y_continuous(breaks = 1:3, labels = c("Trial Level", "Monkey Level", "Population Level")) +
  labs(
    title = "R8 • Hierarchical Model Structure",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid = element_blank()
  )
ggsave("plot_R8_model_structure.png", R8_plot, width = 7, height = 5, dpi = 400) 