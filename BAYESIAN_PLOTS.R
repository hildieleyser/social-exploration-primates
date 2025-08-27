#!/usr/bin/env Rscript

# PLOTTING SCRIPT FOR THE BAYESIAN HIERARCHICAL MULTINOMIAL MODEL

# Load required libraries
library(brms)
library(ggplot2)
library(dplyr)
library(bayesplot)
library(patchwork) # For combining plots

cat("=== GENERATING PLOTS FOR BAYESIAN ANALYSIS ===\n")

# Load the saved model and data
cat("Loading saved model and data...\n")
bayesian_model <- readRDS("bayesian_hierarchical_multinomial_model.rds")
df <- read.csv("Explore Exploit Dataset.csv")

# Ensure data is processed consistently
df_processed <- df %>%
  filter(!is.na(OUTCOME)) %>%
  mutate(
    monkey_id = factor(monkey),
    sex = factor(ifelse(monkey %in% c("DALI", "EBI", "FRAN"), "Male", "Female")),
    context = factor(CONDITION, levels = c("solo", "duo", "trio"))
  )

# Define a professional and clean theme for all plots
theme_set(theme_bw(base_size = 14) + 
          theme(
            plot.title = element_text(hjust = 0.5, face = "bold"),
            legend.position = "bottom",
            panel.grid.major = element_line(color = "grey92"),
            panel.grid.minor = element_blank()
          ))
color_scheme_set("viridis")

# --- PLOT 1: PREDICTED PROBABILITIES OF CHOICE BY SOCIAL CONTEXT ---
cat("Generating Plot 1: Predicted Probabilities...\n")

# Get the conditional effects from the model
conditional_effects_plot <- conditional_effects(bayesian_model, "context", categorical = TRUE)

plot_predicted_probs <- plot(conditional_effects_plot, plot = FALSE)[[1]] +
  labs(
    title = "Effect of Social Context on Choice Probability",
    subtitle = "Predicted probabilities with 95% Credible Intervals",
    x = "Social Context",
    y = "Predicted Probability"
  ) +
  scale_y_continuous(labels = scales::percent_format())

ggsave("figure1_predicted_probabilities.png", plot = plot_predicted_probs, width = 10, height = 7, dpi = 300)
cat("Saved figure1_predicted_probabilities.png\n")


# --- PLOT 2: INDIVIDUAL MONKEY DIFFERENCES (RANDOM EFFECTS) ---
cat("Generating Plot 2: Individual Differences...\n")

# Extract random effects
ranef_data <- ranef(bayesian_model)$monkey

# Separate plots for explore vs none
plot_ranef_explore <- as.data.frame(ranef_data[, , "muexplore_Intercept"]) %>%
  tibble::rownames_to_column("monkey") %>%
  ggplot(aes(x = reorder(monkey, Estimate), y = Estimate, ymin = Q2.5, ymax = Q97.5)) +
  geom_pointrange(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(
    title = "Log-Odds of Explore vs. Exploit",
    subtitle = "Individual monkey effects (relative to average)",
    x = "Monkey",
    y = "Log-Odds Deviation"
  )

plot_ranef_none <- as.data.frame(ranef_data[, , "munone_Intercept"]) %>%
  tibble::rownames_to_column("monkey") %>%
  ggplot(aes(x = reorder(monkey, Estimate), y = Estimate, ymin = Q2.5, ymax = Q97.5)) +
  geom_pointrange(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(
    title = "Log-Odds of None vs. Exploit",
    subtitle = "Individual monkey effects (relative to average)",
    x = "Monkey",
    y = "Log-Odds Deviation"
  )

# Combine the two random effects plots
plot_individual_diffs <- plot_ranef_explore + plot_ranef_none +
  plot_annotation(
    title = "Individual Differences in Choice Behavior",
    caption = "Points are posterior means, lines represent 95% Credible Intervals."
  )

ggsave("figure2_individual_differences.png", plot = plot_individual_diffs, width = 14, height = 8, dpi = 300)
cat("Saved figure2_individual_differences.png\n")


# --- PLOT 3: POSTERIOR PREDICTIVE CHECK ---
cat("Generating Plot 3: Posterior Predictive Check...\n")

plot_pp_check <- pp_check(bayesian_model, type = 'bars_grouped', group = 'context', nsamples = 50) +
  labs(
    title = "Posterior Predictive Check by Social Context",
    subtitle = "Comparing observed choice counts (y) to 50 posterior simulations (y_rep)",
    x = "Choice Outcome",
    y = "Count"
  )

ggsave("figure3_posterior_predictive_check.png", plot = plot_pp_check, width = 10, height = 7, dpi = 300)
cat("Saved figure3_posterior_predictive_check.png\n")


# --- PLOT 4: MODEL PARAMETER TRACE PLOTS ---
cat("Generating Plot 4: MCMC Trace Plots...\n")

# Just plot the main fixed effects for clarity
plot_mcmc_traces <- plot(bayesian_model, N = 6, ask = FALSE)

png("figure4_mcmc_trace_plots.png", width = 12, height = 10, units = "in", res = 300)
print(plot_mcmc_traces)
dev.off()

cat("Saved figure4_mcmc_trace_plots.png\n")

cat("\n=== PLOT GENERATION COMPLETE ===\n")
cat("Generated files:\n")
cat("- figure1_predicted_probabilities.png\n")
cat("- figure2_individual_differences.png\n")
cat("- figure3_posterior_predictive_check.png\n")
cat("- figure4_mcmc_trace_plots.png\n") 