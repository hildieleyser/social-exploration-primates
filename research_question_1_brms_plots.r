# RESEARCH QUESTION 1: PROPER BRMS VISUALIZATIONS
# Using bayesplot, ggplot2, and brms plotting functions

library(brms)
library(bayesplot)
library(ggplot2)
library(dplyr)
library(posterior)
library(patchwork)

cat("=== LOADING BAYESIAN MODELS FOR VISUALIZATION ===\n")

# Load the fitted models (assuming they exist from previous run)
# If not, we'll fit them quickly

# Load and prepare data
data_raw <- read.csv("Explore Exploit Dataset.csv", stringsAsFactors = FALSE)
data_exp <- data_raw[data_raw$TRIAL_TYPE == "OIT_RE", ]
data_exp$outcome_clean <- ifelse(grepl("explore", data_exp$OUTCOME, ignore.case = TRUE), "explore",
                                ifelse(grepl("exploit", data_exp$OUTCOME, ignore.case = TRUE), "exploit", 
                                      ifelse(grepl("none|NONE", data_exp$OUTCOME, ignore.case = TRUE), "none", NA)))

data_clean <- data_exp[!is.na(data_exp$outcome_clean), ]
data_clean$outcome_clean <- factor(data_clean$outcome_clean, levels = c("exploit", "explore", "none"))

data_clean$monkey <- data_clean$monkey
data_clean$condition <- factor(data_clean$CONDITION, levels = c("solo", "duo", "trio"))
data_clean$sex <- ifelse(data_clean$monkey %in% c("FRAN", "DALI", "EBI"), "Male", "Female")
data_clean$sex <- factor(data_clean$sex, levels = c("Male", "Female"))
data_clean$relative_rank <- factor(data_clean$RELATIVE_RANK, levels = c(1, 2, 3))
data_clean$absolute_rank <- factor(data_clean$ABSOLUTE_RANK, levels = c(1, 2, 3))

model_data <- data_clean[complete.cases(data_clean[c("outcome_clean", "condition", "relative_rank", "absolute_rank", "sex", "monkey")]), ]

# Set up brms options
options(mc.cores = parallel::detectCores())

# Quick model fitting (reduced iterations for faster plotting)
priors_rel <- c(
  prior(normal(0, 1), class = Intercept, dpar = muexplore),
  prior(normal(0, 1), class = Intercept, dpar = munone),
  prior(normal(0, 0.5), class = b, dpar = muexplore),
  prior(normal(0, 0.5), class = b, dpar = munone),
  prior(exponential(1), class = sd, group = monkey, dpar = muexplore),
  prior(exponential(1), class = sd, group = monkey, dpar = munone)
)

cat("Fitting models for visualization...\n")

bayesian_relative <- brm(outcome_clean ~ condition + relative_rank + sex + (1|monkey),
                        data = model_data,
                        family = categorical(),
                        prior = priors_rel,
                        iter = 1000, warmup = 500, chains = 2,
                        silent = 2, refresh = 0)

bayesian_absolute <- brm(outcome_clean ~ condition + absolute_rank + sex + (1|monkey),
                        data = model_data,
                        family = categorical(),
                        prior = priors_rel,
                        iter = 1000, warmup = 500, chains = 2,
                        silent = 2, refresh = 0)

cat("Models fitted. Creating brms visualizations...\n")

# Set bayesplot theme
bayesplot_theme_set(bayesplot::theme_default())

# Create comprehensive brms visualization
pdf("BRMS_RESEARCH_QUESTION_1_PLOTS.pdf", width = 20, height = 16)

# 1. MCMC Diagnostics using bayesplot
cat("Creating MCMC diagnostics...\n")

# Trace plots
p1 <- mcmc_trace(bayesian_relative, pars = c("b_muexplore_relative_rank2", "b_muexplore_relative_rank3")) +
  ggtitle("A. MCMC Traces - Relative Rank Effects") +
  theme_minimal()

# Posterior distributions
p2 <- mcmc_areas(bayesian_relative, 
                pars = c("b_muexplore_relative_rank2", "b_muexplore_relative_rank3",
                        "b_muexplore_conditionduo", "b_muexplore_conditiontrio")) +
  ggtitle("B. Posterior Distributions - Relative Rank Model") +
  theme_minimal()

# Model comparison
p3 <- mcmc_areas(bayesian_absolute,
                pars = c("b_muexplore_absolute_rank2", "b_muexplore_absolute_rank3",
                        "b_muexplore_conditionduo", "b_muexplore_conditiontrio")) +
  ggtitle("C. Posterior Distributions - Absolute Rank Model") +
  theme_minimal()

# 2. Model-specific brms plots
cat("Creating brms model plots...\n")

# Conditional effects plots
p4 <- plot(conditional_effects(bayesian_relative, effects = "relative_rank", categorical = TRUE))[[1]] +
  ggtitle("D. Conditional Effects - Relative Rank") +
  theme_minimal() +
  scale_fill_viridis_d()

p5 <- plot(conditional_effects(bayesian_absolute, effects = "absolute_rank", categorical = TRUE))[[1]] +
  ggtitle("E. Conditional Effects - Absolute Rank") +
  theme_minimal() +
  scale_fill_viridis_d()

# Posterior predictive checks
p6 <- pp_check(bayesian_relative, ndraws = 50) +
  ggtitle("F. Posterior Predictive Check - Relative Rank") +
  theme_minimal()

p7 <- pp_check(bayesian_absolute, ndraws = 50) +
  ggtitle("G. Posterior Predictive Check - Absolute Rank") +
  theme_minimal()

# Random effects
p8 <- plot(bayesian_relative, ask = FALSE)[[1]] +
  ggtitle("H. Random Effects - Individual Monkeys") +
  theme_minimal()

# Combine plots using patchwork
combined_plot <- (p1 | p2) / (p3 | p4) / (p5 | p6) / (p7 | p8)

print(combined_plot)

dev.off()

# 3. Advanced bayesplot visualizations
pdf("ADVANCED_BAYESPLOT_VISUALIZATIONS.pdf", width = 16, height = 12)

par(mfrow = c(2, 3))

# Extract posterior samples
post_rel <- as.matrix(bayesian_relative)
post_abs <- as.matrix(bayesian_absolute)

# Rank plots
mcmc_rank_overlay(post_rel, pars = c("b_muexplore_relative_rank2", "b_muexplore_relative_rank3")) +
  ggtitle("A. Rank Plots - Relative Rank Effects")

# Autocorrelation
mcmc_acf(post_rel, pars = c("b_muexplore_relative_rank2", "b_muexplore_relative_rank3")) +
  ggtitle("B. Autocorrelation - Relative Rank")

# Effective sample size
mcmc_neff(neff_ratio(post_rel)) +
  ggtitle("C. Effective Sample Size Ratios")

# Rhat values
mcmc_rhat(rhat(post_rel)) +
  ggtitle("D. R-hat Diagnostics")

# Pairs plot for key parameters
mcmc_pairs(post_rel, 
          pars = c("b_muexplore_relative_rank2", "b_muexplore_relative_rank3",
                  "b_muexplore_conditionduo", "b_muexplore_conditiontrio"),
          off_diag_args = list(size = 0.75)) +
  ggtitle("E. Pairs Plot - Parameter Correlations")

# Intervals comparison
params_to_compare <- c("b_muexplore_relative_rank2", "b_muexplore_relative_rank3")
mcmc_intervals_data(post_rel, pars = params_to_compare) %>%
  ggplot(aes(x = parameter, y = m, ymin = ll, ymax = hh)) +
  geom_pointrange() +
  coord_flip() +
  ggtitle("F. Parameter Intervals") +
  theme_minimal()

dev.off()

# 4. Model comparison visualization using loo
cat("Creating LOO comparison plots...\n")

pdf("LOO_MODEL_COMPARISON.pdf", width = 12, height = 8)

# LOO comparison
loo_rel <- loo(bayesian_relative)
loo_abs <- loo(bayesian_absolute)

# Plot LOO comparison
loo_compare_plot <- loo_compare(loo_rel, loo_abs) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("model") %>%
  ggplot(aes(x = model, y = elpd_diff, ymin = elpd_diff - se_diff, ymax = elpd_diff + se_diff)) +
  geom_pointrange(size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(title = "LOO Model Comparison",
       subtitle = "Expected Log Predictive Density Difference",
       x = "Model", y = "ELPD Difference ± SE") +
  theme_minimal()

print(loo_compare_plot)

# PSIS diagnostic
plot(loo_rel) + ggtitle("PSIS Diagnostics - Relative Rank Model")

dev.off()

# 5. Effect size visualization
pdf("EFFECT_SIZES_BRMS.pdf", width = 14, height = 10)

# Extract and plot effect sizes
posterior_rel <- as_draws_df(bayesian_relative)

# Create effect size comparison
effect_data <- posterior_rel %>%
  select(contains("b_muexplore")) %>%
  select(-contains("Intercept")) %>%
  pivot_longer(everything(), names_to = "parameter", values_to = "estimate") %>%
  mutate(
    effect_type = case_when(
      grepl("rank", parameter) ~ "Rank Effects",
      grepl("condition", parameter) ~ "Social Context",
      grepl("sex", parameter) ~ "Sex Differences"
    ),
    parameter_clean = case_when(
      parameter == "b_muexplore_relative_rank2" ~ "Rank 2 vs 1",
      parameter == "b_muexplore_relative_rank3" ~ "Rank 3 vs 1", 
      parameter == "b_muexplore_conditionduo" ~ "Duo vs Solo",
      parameter == "b_muexplore_conditiontrio" ~ "Trio vs Solo",
      parameter == "b_muexplore_sexFemale" ~ "Female vs Male"
    )
  ) %>%
  filter(!is.na(parameter_clean))

# Effect size plot
effect_plot <- effect_data %>%
  ggplot(aes(x = estimate, y = parameter_clean, fill = effect_type)) +
  stat_halfeye(.width = c(0.66, 0.95), alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~effect_type, scales = "free_y") +
  labs(title = "Bayesian Effect Sizes with Uncertainty",
       subtitle = "66% and 95% Credible Intervals",
       x = "Log-Odds Effect Size", y = "Parameter") +
  scale_fill_viridis_d() +
  theme_minimal() +
  theme(legend.position = "none")

print(effect_plot)

dev.off()

cat("\n=== BRMS VISUALIZATION COMPLETE ===\n")
cat("Created files:\n")
cat("- BRMS_RESEARCH_QUESTION_1_PLOTS.pdf\n")
cat("- ADVANCED_BAYESPLOT_VISUALIZATIONS.pdf\n")
cat("- LOO_MODEL_COMPARISON.pdf\n")
cat("- EFFECT_SIZES_BRMS.pdf\n")

# Summary
cat("\n=== SUMMARY ===\n")
print(summary(bayesian_relative))
print(loo_compare(loo_rel, loo_abs)) 