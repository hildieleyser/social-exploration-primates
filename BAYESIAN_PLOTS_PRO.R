#!/usr/bin/env Rscript

# Professional publication-quality plots for Bayesian multinomial model
library(brms)
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)
library(bayesplot)

cat("=== GENERATING PROFESSIONAL PUBLICATION-QUALITY PLOTS ===\n")

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
    choice = factor(choice, levels = choices),
    sex = ifelse(monkey %in% c("DALI", "EBI", "FRAN"), "Male", "Female"),
    monkey = factor(monkey, levels = c("ANEMONE", "CHOCOLAT", "DALI", "EBI", "FRAN", "ICE"))
  ) %>%
  filter(!is.na(choice))

# 1. Observed proportions
obs_props <- df %>%
  group_by(context, choice) %>%
  summarise(n = n(), .groups = 'drop') %>%
  group_by(context) %>%
  mutate(prop = n / sum(n))

# 2. Predicted probabilities and 95% CI
newdata <- expand.grid(context = levels(df$context), monkey = unique(df$monkey))
preds <- posterior_epred(bayesian_model, newdata = newdata, allow_new_levels = FALSE)

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

# 3. Faceted predicted vs actual plot
p1 <- ggplot(plot_df, aes(x = context)) +
  geom_col(aes(y = prop), fill = "grey80", width = 0.5, alpha = 0.7) +
  geom_point(aes(y = pred_mean, color = context), size = 3, position = position_nudge(x = 0.15)) +
  geom_errorbar(aes(ymin = pred_lower, ymax = pred_upper, color = context), width = 0.15, size = 1.1, position = position_nudge(x = 0.15)) +
  facet_wrap(~choice, nrow = 1, labeller = labeller(choice = c(exploit = "Exploit", explore = "Explore", none = "None"))) +
  scale_color_viridis_d(option = "D", end = 0.8, name = "Context") +
  labs(
    title = "Predicted vs. Observed Probabilities by Social Context",
    subtitle = "Bars: Observed proportions. Points/lines: Model predictions ± 95% CI.",
    x = "Social Context",
    y = "Probability"
  ) +
  theme_classic(base_size = 18, base_family = "Arial") +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 22),
    plot.subtitle = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 16),
    strip.text = element_text(face = "bold", size = 18),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    panel.grid = element_blank()
  )

ggsave("figure1_predicted_vs_actual_PRO.png", plot = p1, width = 14, height = 6, dpi = 500)
cat("Saved figure1_predicted_vs_actual_PRO.png\n")

# 4. Individual differences (random effects, faceted by outcome, colored by sex)
ranef_data <- ranef(bayesian_model)$monkey
monkey_labels <- levels(df$monkey)
sex_map <- df %>% select(monkey, sex) %>% distinct()

ranef_explore <- as.data.frame(ranef_data[, , "muexplore_Intercept"]) %>%
  tibble::rownames_to_column("monkey") %>%
  left_join(sex_map, by = "monkey")
ranef_none <- as.data.frame(ranef_data[, , "munone_Intercept"]) %>%
  tibble::rownames_to_column("monkey") %>%
  left_join(sex_map, by = "monkey")

ranef_long <- bind_rows(
  ranef_explore %>% mutate(outcome = "Explore vs Exploit"),
  ranef_none %>% mutate(outcome = "None vs Exploit")
)

p2 <- ggplot(ranef_long, aes(x = reorder(monkey, Estimate), y = Estimate, color = sex, shape = sex)) +
  geom_pointrange(aes(ymin = Q2.5, ymax = Q97.5), size = 1.5, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  coord_flip() +
  facet_wrap(~outcome, nrow = 1) +
  scale_color_manual(values = c("Male" = "#0072B2", "Female" = "#D55E00")) +
  scale_shape_manual(values = c("Male" = 16, "Female" = 17)) +
  labs(
    title = "Individual Differences in Choice Behavior",
    subtitle = "Posterior means and 95% credible intervals, colored by sex",
    x = "Monkey",
    y = "Log-Odds Deviation"
  ) +
  theme_classic(base_size = 18, base_family = "Arial") +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 22),
    plot.subtitle = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 16),
    strip.text = element_text(face = "bold", size = 18),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    panel.grid = element_blank()
  )

ggsave("figure2_individual_differences_PRO.png", plot = p2, width = 14, height = 6, dpi = 500)
cat("Saved figure2_individual_differences_PRO.png\n")

# 5. Posterior predictive check: observed vs simulated, faceted by context
cat("Generating professional posterior predictive check...\n")
ppc <- pp_check(bayesian_model, type = 'bars_grouped', group = 'context', ndraws = 50) +
  labs(
    title = "Posterior Predictive Check by Social Context",
    subtitle = "Observed counts (black) vs. 50 posterior simulations (color)",
    x = "Choice Outcome",
    y = "Count"
  ) +
  theme_classic(base_size = 18, base_family = "Arial") +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 22),
    plot.subtitle = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 16),
    strip.text = element_text(face = "bold", size = 18),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    panel.grid = element_blank()
  )

ggsave("figure3_posterior_predictive_check_PRO.png", plot = ppc, width = 14, height = 6, dpi = 500)
cat("Saved figure3_posterior_predictive_check_PRO.png\n")

cat("\n=== PROFESSIONAL PUBLICATION-QUALITY PLOTS COMPLETE ===\n")
cat("- figure1_predicted_vs_actual_PRO.png\n")
cat("- figure2_individual_differences_PRO.png\n")
cat("- figure3_posterior_predictive_check_PRO.png\n") 