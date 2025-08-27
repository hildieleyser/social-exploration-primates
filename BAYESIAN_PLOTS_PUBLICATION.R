#!/usr/bin/env Rscript

# Publication-quality plots for Bayesian hierarchical multinomial model
library(brms)
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)
library(patchwork)
library(bayesplot)

cat("=== GENERATING PUBLICATION-QUALITY PLOTS ===\n")

# Load model and data
bayesian_model <- readRDS("bayesian_hierarchical_multinomial_model.rds")
df <- read.csv("Explore Exploit Dataset.csv")

# Data prep
choices <- c("exploit", "explore", "none")
df <- df %>%
  filter(!is.na(OUTCOME)) %>%
  mutate(
    context = factor(CONDITION, levels = c("solo", "duo", "trio")),
    choice = case_when(
      OUTCOME == "explore" ~ "explore",
      OUTCOME %in% c("exploit_pink", "exploit_blue") ~ "exploit",
      TRUE ~ "none"
    ),
    choice = factor(choice, levels = choices)
  ) %>%
  filter(!is.na(choice))

# 1. Compute observed proportions
obs_props <- df %>%
  group_by(context, choice) %>%
  summarise(n = n(), .groups = 'drop') %>%
  group_by(context) %>%
  mutate(prop = n / sum(n))

# 2. Get predicted probabilities and 95% CI from model
newdata <- expand.grid(context = levels(df$context), monkey = unique(df$monkey))
preds <- posterior_epred(bayesian_model, newdata = newdata, allow_new_levels = FALSE)
# preds: iterations x rows(newdata) x choices

# For each context and choice, average over monkeys and get mean/CI
pred_summary <- lapply(1:length(choices), function(i) {
  sapply(levels(df$context), function(ctx) {
    idx <- which(newdata$context == ctx)
    draws <- preds[, idx, i]
    draws_mean <- if (is.matrix(draws)) rowMeans(draws) else draws
    c(
      pred_mean = mean(draws_mean),
      pred_lower = quantile(draws_mean, 0.025),
      pred_upper = quantile(draws_mean, 0.975)
    )
  })
})

# Build tidy data frame
pred_long <- do.call(rbind, lapply(1:length(choices), function(i) {
  df_tmp <- as.data.frame(t(pred_summary[[i]]))
  colnames(df_tmp) <- c("pred_mean", "pred_lower", "pred_upper")
  df_tmp$context <- rownames(df_tmp)
  df_tmp$choice <- choices[i]
  df_tmp
})) %>%
  select(context, choice, pred_mean, pred_lower, pred_upper)

# Merge observed and predicted
plot_df <- left_join(pred_long, obs_props, by = c("context", "choice"))

# 3. Publication-quality plot: predicted vs actual
p1 <- ggplot(plot_df, aes(x = context, group = choice)) +
  geom_col(aes(y = prop, fill = choice),
           position = position_dodge(width = 0.6), width = 0.5, alpha = 0.4) +
  geom_point(aes(y = pred_mean, color = choice),
             position = position_dodge(width = 0.6), size = 3) +
  geom_errorbar(aes(ymin = pred_lower, ymax = pred_upper, color = choice),
                position = position_dodge(width = 0.6), width = 0.2, size = 1.1) +
  scale_fill_viridis_d(option = "D", end = 0.8, name = "Choice (Observed)") +
  scale_color_viridis_d(option = "D", end = 0.8, name = "Choice (Predicted)") +
  labs(
    title = "Predicted vs. Observed Choice Probabilities by Social Context",
    subtitle = "Points and error bars: Model predicted mean ± 95% CI. Bars: Actual observed proportions.",
    x = "Social Context",
    y = "Probability"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
    plot.subtitle = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.major.y = element_line(color = "grey85"),
    panel.grid.minor = element_blank()
  )

ggsave("figure1_predicted_vs_actual.png", plot = p1, width = 10, height = 7, dpi = 400)
cat("Saved figure1_predicted_vs_actual.png\n")

# 4. Individual differences (random effects)
ranef_data <- ranef(bayesian_model)$monkey
monkey_labels <- levels(df$monkey)

ranef_explore <- as.data.frame(ranef_data[, , "muexplore_Intercept"]) %>%
  tibble::rownames_to_column("monkey")
ranef_none <- as.data.frame(ranef_data[, , "munone_Intercept"]) %>%
  tibble::rownames_to_column("monkey")

p2a <- ggplot(ranef_explore, aes(x = reorder(monkey, Estimate), y = Estimate)) +
  geom_pointrange(aes(ymin = Q2.5, ymax = Q97.5), color = viridis(1, end = 0.7), size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  coord_flip() +
  labs(title = "Individual Effects: Explore vs Exploit", x = "Monkey", y = "Log-Odds Deviation") +
  theme_classic(base_size = 15) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

p2b <- ggplot(ranef_none, aes(x = reorder(monkey, Estimate), y = Estimate)) +
  geom_pointrange(aes(ymin = Q2.5, ymax = Q97.5), color = viridis(1, begin = 0.5, end = 1), size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  coord_flip() +
  labs(title = "Individual Effects: None vs Exploit", x = "Monkey", y = "Log-Odds Deviation") +
  theme_classic(base_size = 15) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

p2 <- p2a + p2b + plot_layout(ncol = 2) +
  plot_annotation(
    title = "Individual Differences in Choice Behavior",
    caption = "Points: posterior means; lines: 95% credible intervals."
  )

ggsave("figure2_individual_differences_pub.png", plot = p2, width = 14, height = 7, dpi = 400)
cat("Saved figure2_individual_differences_pub.png\n")

# 5. Posterior predictive check: overlay observed and simulated
cat("Generating posterior predictive check...\n")
ppc <- pp_check(bayesian_model, type = 'bars_grouped', group = 'context', ndraws = 50) +
  labs(
    title = "Posterior Predictive Check by Social Context",
    subtitle = "Observed counts (y) vs. 50 posterior simulations (y_rep)",
    x = "Choice Outcome",
    y = "Count"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

ggsave("figure3_posterior_predictive_check_pub.png", plot = ppc, width = 10, height = 7, dpi = 400)
cat("Saved figure3_posterior_predictive_check_pub.png\n")

cat("\n=== PUBLICATION-QUALITY PLOTS COMPLETE ===\n")
cat("- figure1_predicted_vs_actual.png\n")
cat("- figure2_individual_differences_pub.png\n")
cat("- figure3_posterior_predictive_check_pub.png\n") 