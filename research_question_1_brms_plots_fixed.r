# RESEARCH QUESTION 1: PROPER BRMS VISUALIZATIONS (FIXED)
# Using bayesplot, ggplot2, and brms plotting functions

library(brms)
library(bayesplot)
library(ggplot2)
library(dplyr)
library(posterior)
library(tidyr)

cat("=== CREATING PROPER BRMS VISUALIZATIONS ===\n")

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

# Fit models with proper settings
priors_rel <- c(
  prior(normal(0, 1), class = Intercept, dpar = muexplore),
  prior(normal(0, 1), class = Intercept, dpar = munone),
  prior(normal(0, 0.5), class = b, dpar = muexplore),
  prior(normal(0, 0.5), class = b, dpar = munone),
  prior(exponential(1), class = sd, group = monkey, dpar = muexplore),
  prior(exponential(1), class = sd, group = monkey, dpar = munone)
)

cat("Fitting Bayesian models for visualization...\n")

bayesian_relative <- brm(outcome_clean ~ condition + relative_rank + sex + (1|monkey),
                        data = model_data,
                        family = categorical(),
                        prior = priors_rel,
                        iter = 1500, warmup = 750, chains = 3,
                        control = list(adapt_delta = 0.95),
                        silent = 2, refresh = 0)

bayesian_absolute <- brm(outcome_clean ~ condition + absolute_rank + sex + (1|monkey),
                        data = model_data,
                        family = categorical(),
                        prior = priors_rel,
                        iter = 1500, warmup = 750, chains = 3,
                        control = list(adapt_delta = 0.95),
                        silent = 2, refresh = 0)

cat("Models fitted successfully!\n")

# Set bayesplot theme
bayesplot_theme_set(bayesplot::theme_default())

# 1. COMPREHENSIVE BRMS VISUALIZATION
pdf("PROPER_BRMS_VISUALIZATIONS.pdf", width = 16, height = 20)

par(mfrow = c(4, 2), mar = c(4, 4, 3, 2))

# Plot 1: MCMC Trace plots
cat("Creating MCMC diagnostics...\n")
p1 <- mcmc_trace(bayesian_relative, pars = c("b_muexplore_relative_rank2", "b_muexplore_relative_rank3"))
print(p1 + ggtitle("A. MCMC Traces - Relative Rank Effects") + theme_minimal())

# Plot 2: Posterior distributions - Relative rank
p2 <- mcmc_areas(bayesian_relative, 
                pars = c("b_muexplore_relative_rank2", "b_muexplore_relative_rank3",
                        "b_muexplore_conditionduo", "b_muexplore_conditiontrio"))
print(p2 + ggtitle("B. Posterior Distributions - Relative Rank Model") + theme_minimal())

# Plot 3: Posterior distributions - Absolute rank
p3 <- mcmc_areas(bayesian_absolute,
                pars = c("b_muexplore_absolute_rank2", "b_muexplore_absolute_rank3",
                        "b_muexplore_conditionduo", "b_muexplore_conditiontrio"))
print(p3 + ggtitle("C. Posterior Distributions - Absolute Rank Model") + theme_minimal())

# Plot 4: Conditional effects - Relative rank
cat("Creating conditional effects plots...\n")
ce_rel <- conditional_effects(bayesian_relative, effects = "relative_rank", categorical = TRUE)
p4 <- plot(ce_rel)[[1]] + 
  ggtitle("D. Conditional Effects - Relative Rank") + 
  theme_minimal() +
  scale_fill_viridis_d(name = "Outcome")
print(p4)

# Plot 5: Conditional effects - Absolute rank
ce_abs <- conditional_effects(bayesian_absolute, effects = "absolute_rank", categorical = TRUE)
p5 <- plot(ce_abs)[[1]] + 
  ggtitle("E. Conditional Effects - Absolute Rank") + 
  theme_minimal() +
  scale_fill_viridis_d(name = "Outcome")
print(p5)

# Plot 6: Posterior predictive checks - Relative
cat("Creating posterior predictive checks...\n")
p6 <- pp_check(bayesian_relative, ndraws = 50, type = "bars") + 
  ggtitle("F. Posterior Predictive Check - Relative Rank") + 
  theme_minimal()
print(p6)

# Plot 7: Posterior predictive checks - Absolute
p7 <- pp_check(bayesian_absolute, ndraws = 50, type = "bars") + 
  ggtitle("G. Posterior Predictive Check - Absolute Rank") + 
  theme_minimal()
print(p7)

# Plot 8: Effect size comparison
posterior_rel <- as_draws_df(bayesian_relative)
posterior_abs <- as_draws_df(bayesian_absolute)

# Create effect comparison data
effect_comparison <- data.frame(
  Relative_Rank2 = posterior_rel$b_muexplore_relative_rank2,
  Relative_Rank3 = posterior_rel$b_muexplore_relative_rank3,
  Absolute_Rank2 = posterior_abs$b_muexplore_absolute_rank2,
  Absolute_Rank3 = posterior_abs$b_muexplore_absolute_rank3
) %>%
  pivot_longer(everything(), names_to = "Effect", values_to = "Estimate") %>%
  separate(Effect, into = c("Model", "Parameter"), sep = "_", extra = "merge")

p8 <- ggplot(effect_comparison, aes(x = Estimate, y = Parameter, fill = Model)) +
  stat_halfeye(alpha = 0.7, .width = c(0.66, 0.95)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~Model, scales = "free_x") +
  labs(title = "H. Model Comparison - Rank Effects",
       x = "Log-Odds Effect Size", y = "Parameter") +
  scale_fill_viridis_d() +
  theme_minimal() +
  theme(legend.position = "none")
print(p8)

dev.off()

# 2. ADVANCED BAYESPLOT DIAGNOSTICS
pdf("BAYESPLOT_DIAGNOSTICS.pdf", width = 16, height = 12)

# Extract posterior samples
post_rel <- as.matrix(bayesian_relative)
post_abs <- as.matrix(bayesian_absolute)

# Diagnostic plots
p_diag1 <- mcmc_rank_overlay(post_rel, pars = c("b_muexplore_relative_rank2", "b_muexplore_relative_rank3")) +
  ggtitle("A. Rank Plots - Relative Rank Effects")
print(p_diag1)

p_diag2 <- mcmc_acf(post_rel, pars = c("b_muexplore_relative_rank2", "b_muexplore_relative_rank3")) +
  ggtitle("B. Autocorrelation - Relative Rank")
print(p_diag2)

p_diag3 <- mcmc_neff(neff_ratio(post_rel)) +
  ggtitle("C. Effective Sample Size Ratios")
print(p_diag3)

p_diag4 <- mcmc_rhat(rhat(post_rel)) +
  ggtitle("D. R-hat Diagnostics")
print(p_diag4)

dev.off()

# 3. MODEL COMPARISON WITH LOO
pdf("LOO_COMPARISON_BRMS.pdf", width = 12, height = 8)

cat("Creating LOO comparison...\n")
loo_rel <- loo(bayesian_relative)
loo_abs <- loo(bayesian_absolute)

# LOO comparison table
loo_comp <- loo_compare(loo_rel, loo_abs)
print(loo_comp)

# Model weights
model_weights_vals <- model_weights(bayesian_relative, bayesian_absolute, weights = "loo")

# Create LOO comparison plot
loo_data <- data.frame(
  Model = c("Relative Rank", "Absolute Rank"),
  ELPD = c(loo_rel$estimates["elpd_loo", "Estimate"], 
           loo_abs$estimates["elpd_loo", "Estimate"]),
  SE = c(loo_rel$estimates["elpd_loo", "SE"], 
         loo_abs$estimates["elpd_loo", "SE"]),
  Weight = c(model_weights_vals[1], model_weights_vals[2])
)

p_loo <- ggplot(loo_data, aes(x = Model, y = ELPD, ymin = ELPD - SE, ymax = ELPD + SE)) +
  geom_pointrange(size = 1.2) +
  geom_text(aes(label = paste("Weight:", round(Weight, 3))), 
            vjust = -1.5, size = 4, fontface = "bold") +
  labs(title = "LOO Model Comparison",
       subtitle = "Expected Log Predictive Density with Standard Errors",
       y = "ELPD (higher = better)") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12))

print(p_loo)

dev.off()

# 4. EFFECT SIZES AND CREDIBLE INTERVALS
pdf("EFFECT_SIZES_CREDIBLE_INTERVALS.pdf", width = 14, height = 10)

# Extract all effects for exploration outcome
effects_data <- posterior_rel %>%
  select(starts_with("b_muexplore")) %>%
  select(-contains("Intercept")) %>%
  pivot_longer(everything(), names_to = "parameter", values_to = "estimate") %>%
  mutate(
    parameter_clean = case_when(
      parameter == "b_muexplore_relative_rank2" ~ "Rank 2 vs 1",
      parameter == "b_muexplore_relative_rank3" ~ "Rank 3 vs 1", 
      parameter == "b_muexplore_conditionduo" ~ "Duo vs Solo",
      parameter == "b_muexplore_conditiontrio" ~ "Trio vs Solo",
      parameter == "b_muexplore_sexFemale" ~ "Female vs Male",
      TRUE ~ parameter
    ),
    effect_category = case_when(
      grepl("rank", parameter) ~ "Social Rank",
      grepl("condition", parameter) ~ "Social Context",
      grepl("sex", parameter) ~ "Biological Sex",
      TRUE ~ "Other"
    )
  ) %>%
  filter(!is.na(parameter_clean))

# Create comprehensive effect plot
p_effects <- ggplot(effects_data, aes(x = estimate, y = parameter_clean, fill = effect_category)) +
  stat_halfeye(.width = c(0.66, 0.95), alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 1) +
  facet_wrap(~effect_category, scales = "free_y", ncol = 1) +
  labs(title = "Bayesian Effect Sizes with Credible Intervals",
       subtitle = "66% (thick) and 95% (thin) Credible Intervals",
       x = "Log-Odds Effect Size", 
       y = "Parameter",
       caption = "Red line = no effect") +
  scale_fill_viridis_d(name = "Effect Type") +
  theme_minimal() +
  theme(legend.position = "bottom",
        strip.text = element_text(size = 12, face = "bold"))

print(p_effects)

dev.off()

# Summary output
cat("\n=== BRMS VISUALIZATION SUMMARY ===\n")
cat("Created files:\n")
cat("- PROPER_BRMS_VISUALIZATIONS.pdf (Main results)\n")
cat("- BAYESPLOT_DIAGNOSTICS.pdf (MCMC diagnostics)\n")
cat("- LOO_COMPARISON_BRMS.pdf (Model comparison)\n")
cat("- EFFECT_SIZES_CREDIBLE_INTERVALS.pdf (Effect sizes)\n")

cat("\n=== MODEL COMPARISON RESULTS ===\n")
print(loo_comp)
cat("\nModel Weights:\n")
cat("Relative Rank:", round(model_weights_vals[1], 3), "\n")
cat("Absolute Rank:", round(model_weights_vals[2], 3), "\n")

cat("\n=== RELATIVE RANK MODEL SUMMARY ===\n")
print(summary(bayesian_relative))

cat("\nBRMS visualization complete!\n") 