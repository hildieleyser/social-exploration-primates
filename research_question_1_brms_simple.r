# RESEARCH QUESTION 1: SIMPLE BRMS VISUALIZATIONS
# Using only bayesplot and basic ggplot2 functions

library(brms)
library(bayesplot)
library(ggplot2)
library(dplyr)

cat("=== CREATING BRMS VISUALIZATIONS ===\n")

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

# Set up brms
options(mc.cores = parallel::detectCores())

priors_rel <- c(
  prior(normal(0, 1), class = Intercept, dpar = muexplore),
  prior(normal(0, 1), class = Intercept, dpar = munone),
  prior(normal(0, 0.5), class = b, dpar = muexplore),
  prior(normal(0, 0.5), class = b, dpar = munone),
  prior(exponential(1), class = sd, group = monkey, dpar = muexplore),
  prior(exponential(1), class = sd, group = monkey, dpar = munone)
)

cat("Fitting models...\n")

bayesian_relative <- brm(outcome_clean ~ condition + relative_rank + sex + (1|monkey),
                        data = model_data, family = categorical(), prior = priors_rel,
                        iter = 1200, warmup = 600, chains = 3,
                        silent = 2, refresh = 0)

bayesian_absolute <- brm(outcome_clean ~ condition + absolute_rank + sex + (1|monkey),
                        data = model_data, family = categorical(), prior = priors_rel,
                        iter = 1200, warmup = 600, chains = 3,
                        silent = 2, refresh = 0)

cat("Creating visualizations...\n")

# 1. MAIN BRMS PLOTS
pdf("BRMS_MAIN_PLOTS.pdf", width = 16, height = 12)

# MCMC diagnostics using bayesplot
p1 <- mcmc_trace(bayesian_relative, pars = c("b_muexplore_relative_rank2", "b_muexplore_relative_rank3"))
print(p1 + ggtitle("A. MCMC Traces - Relative Rank Effects"))

p2 <- mcmc_areas(bayesian_relative, 
                pars = c("b_muexplore_relative_rank2", "b_muexplore_relative_rank3",
                        "b_muexplore_conditionduo", "b_muexplore_conditiontrio"))
print(p2 + ggtitle("B. Posterior Distributions - Relative Rank Model"))

p3 <- mcmc_areas(bayesian_absolute,
                pars = c("b_muexplore_absolute_rank2", "b_muexplore_absolute_rank3",
                        "b_muexplore_conditionduo", "b_muexplore_conditiontrio"))
print(p3 + ggtitle("C. Posterior Distributions - Absolute Rank Model"))

# Conditional effects using brms
ce_rel <- conditional_effects(bayesian_relative, effects = "relative_rank", categorical = TRUE)
p4 <- plot(ce_rel)[[1]] + ggtitle("D. Conditional Effects - Relative Rank")
print(p4)

ce_abs <- conditional_effects(bayesian_absolute, effects = "absolute_rank", categorical = TRUE)
p5 <- plot(ce_abs)[[1]] + ggtitle("E. Conditional Effects - Absolute Rank")
print(p5)

# Posterior predictive checks
p6 <- pp_check(bayesian_relative, ndraws = 50) + ggtitle("F. Posterior Predictive Check - Relative")
print(p6)

dev.off()

# 2. DIAGNOSTICS
pdf("BRMS_DIAGNOSTICS.pdf", width = 12, height = 10)

post_rel <- as.matrix(bayesian_relative)

p_diag1 <- mcmc_rank_overlay(post_rel, pars = c("b_muexplore_relative_rank2", "b_muexplore_relative_rank3"))
print(p_diag1 + ggtitle("A. Rank Plots - Chain Mixing"))

p_diag2 <- mcmc_acf(post_rel, pars = c("b_muexplore_relative_rank2", "b_muexplore_relative_rank3"))
print(p_diag2 + ggtitle("B. Autocorrelation"))

p_diag3 <- mcmc_neff(neff_ratio(post_rel))
print(p_diag3 + ggtitle("C. Effective Sample Size"))

p_diag4 <- mcmc_rhat(rhat(post_rel))
print(p_diag4 + ggtitle("D. R-hat Values"))

dev.off()

# 3. MODEL COMPARISON
pdf("BRMS_MODEL_COMPARISON.pdf", width = 10, height = 8)

loo_rel <- loo(bayesian_relative)
loo_abs <- loo(bayesian_absolute)
loo_comp <- loo_compare(loo_rel, loo_abs)
model_weights_vals <- model_weights(bayesian_relative, bayesian_absolute, weights = "loo")

# Simple comparison plot
loo_data <- data.frame(
  Model = c("Relative Rank", "Absolute Rank"),
  ELPD = c(loo_rel$estimates["elpd_loo", "Estimate"], 
           loo_abs$estimates["elpd_loo", "Estimate"]),
  SE = c(loo_rel$estimates["elpd_loo", "SE"], 
         loo_abs$estimates["elpd_loo", "SE"]),
  Weight = c(model_weights_vals[1], model_weights_vals[2])
)

p_loo <- ggplot(loo_data, aes(x = Model, y = ELPD)) +
  geom_col(aes(fill = Weight), alpha = 0.7) +
  geom_errorbar(aes(ymin = ELPD - SE, ymax = ELPD + SE), width = 0.2) +
  geom_text(aes(label = paste("Weight:", round(Weight, 3))), 
            vjust = -0.5, fontface = "bold") +
  labs(title = "Bayesian Model Comparison",
       subtitle = "Expected Log Predictive Density ± SE",
       y = "ELPD (higher = better)") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  theme_minimal() +
  theme(legend.position = "none")

print(p_loo)

dev.off()

# 4. EFFECT SIZES WITH BASIC PLOTS
pdf("BRMS_EFFECT_SIZES.pdf", width = 12, height = 8)

# Extract posterior samples
posterior_rel <- as.data.frame(bayesian_relative)

# Create simple effect size plots
rank_effects <- posterior_rel[, c("b_muexplore_relative_rank2", "b_muexplore_relative_rank3")]
names(rank_effects) <- c("Rank 2 vs 1", "Rank 3 vs 1")

# Basic boxplot
boxplot(rank_effects, 
        main = "A. Relative Rank Effects on Exploration",
        ylab = "Log-Odds Effect Size",
        col = c("#E8F4FD", "#81D4FA"),
        border = "black")
abline(h = 0, lty = 2, col = "red", lwd = 2)

# Context effects
context_effects <- posterior_rel[, c("b_muexplore_conditionduo", "b_muexplore_conditiontrio")]
names(context_effects) <- c("Duo vs Solo", "Trio vs Solo")

boxplot(context_effects,
        main = "B. Social Context Effects on Exploration", 
        ylab = "Log-Odds Effect Size",
        col = c("#E8F5E8", "#C8E6C9"),
        border = "black")
abline(h = 0, lty = 2, col = "red", lwd = 2)

dev.off()

# Summary
cat("\n=== BRMS RESULTS SUMMARY ===\n")
cat("Files created:\n")
cat("- BRMS_MAIN_PLOTS.pdf\n")
cat("- BRMS_DIAGNOSTICS.pdf\n") 
cat("- BRMS_MODEL_COMPARISON.pdf\n")
cat("- BRMS_EFFECT_SIZES.pdf\n")

cat("\n=== LOO MODEL COMPARISON ===\n")
print(loo_comp)

cat("\nModel Weights:\n")
cat("Relative Rank:", round(model_weights_vals[1], 3), "\n")
cat("Absolute Rank:", round(model_weights_vals[2], 3), "\n")

cat("\n=== RELATIVE RANK MODEL SUMMARY ===\n")
print(summary(bayesian_relative))

cat("\nBRMS visualization complete!\n") 