#!/usr/bin/env Rscript

# FINAL BAYESIAN HIERARCHICAL MULTINOMIAL REGRESSION WITH BRMS
# As demanded: "YOU HAVE TO MAKE IT BAYESIAN WITH BRMS DO WHAT EVER YOU HAVE TO DO"

# Load required libraries
library(brms)
library(dplyr)
library(ggplot2)
library(bayesplot)
library(posterior)

# Set up parallel processing
options(mc.cores = parallel::detectCores())

cat("=== BAYESIAN HIERARCHICAL MULTINOMIAL REGRESSION WITH BRMS ===\n")
cat("Loading data...\n")

# Load the data
df <- read.csv("Social_Frames_Analysis_Dataset.csv")

# Data preprocessing
df <- df %>%
  filter(!is.na(CHOICE_monkey)) %>%
  mutate(
    monkey = factor(MONKEY, levels = c("ANEMONE", "CHOCOLAT", "DALI", "EBI", "FRAN", "ICE")),
    sex = factor(ifelse(MONKEY %in% c("DALI", "EBI", "FRAN"), "Male", "Female")),
    context = factor(SOCIAL_CONTEXT, levels = c("solo", "duo", "trio")),
    choice = factor(CHOICE_monkey, levels = c("exploit", "explore", "none"))
  ) %>%
  filter(choice %in% c("exploit", "explore", "none"))

cat("Data summary:\n")
print(table(df$choice, df$context))
cat("\nSample sizes by monkey:\n")
print(table(df$monkey))

cat("\n=== FITTING BAYESIAN HIERARCHICAL MULTINOMIAL MODEL ===\n")

# Set up weakly informative priors
priors <- c(
  # Priors for intercepts (baseline log-odds)
  prior(normal(0, 2.5), class = Intercept, dpar = muexplore),
  prior(normal(0, 2.5), class = Intercept, dpar = munone),
  
  # Priors for social context effects
  prior(normal(0, 1), class = b, dpar = muexplore),
  prior(normal(0, 1), class = b, dpar = munone),
  
  # Priors for random effects (individual differences)
  prior(exponential(1), class = sd, group = monkey, dpar = muexplore),
  prior(exponential(1), class = sd, group = monkey, dpar = munone),
  
  # Prior for correlation between random effects
  prior(lkj(2), class = cor, group = monkey)
)

cat("Fitting Bayesian hierarchical multinomial logistic regression...\n")
cat("This may take several minutes...\n")

# Fit the Bayesian hierarchical multinomial model
bayesian_model <- brm(
  choice ~ context + (context || monkey),
  data = df,
  family = categorical(link = "logit", refcat = "exploit"),
  prior = priors,
  chains = 4,
  iter = 4000,
  warmup = 2000,
  cores = 4,
  control = list(adapt_delta = 0.95, max_treedepth = 12),
  seed = 12345,
  backend = "rstan"
)

cat("\n=== MODEL SUMMARY ===\n")
print(summary(bayesian_model))

cat("\n=== POSTERIOR DIAGNOSTICS ===\n")

# Check convergence diagnostics
rhat_values <- rhat(bayesian_model)
ess_values <- neff_ratio(bayesian_model)

cat("R-hat values (should be < 1.01):\n")
print(summary(rhat_values))

cat("\nEffective sample size ratios (should be > 0.1):\n")
print(summary(ess_values))

# Check for divergent transitions
cat("\nDivergent transitions:\n")
print(nuts_params(bayesian_model))

cat("\n=== POSTERIOR INTERVALS ===\n")
posterior_intervals <- posterior_interval(bayesian_model, prob = 0.95)
print(posterior_intervals)

cat("\n=== BAYES FACTORS FOR SOCIAL CONTEXT EFFECTS ===\n")

# Extract posterior samples
posterior_samples <- as_draws_df(bayesian_model)

# Calculate probability that social context effects are meaningful
context_effects_explore <- posterior_samples[, grepl("b_muexplore_context", names(posterior_samples))]
context_effects_none <- posterior_samples[, grepl("b_munone_context", names(posterior_samples))]

if(ncol(context_effects_explore) > 0) {
  cat("Probability that duo context increases exploration vs exploit:\n")
  if("b_muexplore_contextduo" %in% names(posterior_samples)) {
    prob_duo_explore <- mean(posterior_samples$b_muexplore_contextduo > 0)
    cat(sprintf("P(β_duo_explore > 0) = %.3f\n", prob_duo_explore))
  }
  
  if("b_muexplore_contexttrio" %in% names(posterior_samples)) {
    prob_trio_explore <- mean(posterior_samples$b_muexplore_contexttrio > 0)
    cat(sprintf("P(β_trio_explore > 0) = %.3f\n", prob_trio_explore))
  }
}

cat("\n=== INDIVIDUAL DIFFERENCES (RANDOM EFFECTS) ===\n")

# Extract random effects
random_effects <- ranef(bayesian_model)
print(random_effects)

cat("\n=== PREDICTED PROBABILITIES ===\n")

# Generate predicted probabilities for each context
new_data <- expand.grid(
  context = c("solo", "duo", "trio"),
  monkey = unique(df$monkey)
)

# Posterior predictions
posterior_preds <- posterior_epred(bayesian_model, newdata = new_data, allow_new_levels = FALSE)

# Calculate mean predicted probabilities
pred_probs <- apply(posterior_preds, c(2, 3), mean)
dimnames(pred_probs) <- list(
  paste(new_data$context, new_data$monkey, sep = "_"),
  c("exploit", "explore", "none")
)

cat("Mean predicted probabilities:\n")
print(round(pred_probs, 3))

cat("\n=== SAVING RESULTS ===\n")

# Save the model
saveRDS(bayesian_model, "bayesian_hierarchical_multinomial_model.rds")

# Save posterior samples
write.csv(posterior_samples, "bayesian_posterior_samples.csv", row.names = FALSE)

# Save predicted probabilities
pred_summary <- data.frame(
  context = new_data$context,
  monkey = new_data$monkey,
  prob_exploit = pred_probs[, "exploit"],
  prob_explore = pred_probs[, "explore"],
  prob_none = pred_probs[, "none"]
)
write.csv(pred_summary, "bayesian_predicted_probabilities.csv", row.names = FALSE)

cat("\n=== MODEL COMPARISON ===\n")

# Fit null model for comparison
cat("Fitting null model for comparison...\n")
null_model <- brm(
  choice ~ 1 + (1 || monkey),
  data = df,
  family = categorical(link = "logit", refcat = "exploit"),
  chains = 4,
  iter = 2000,
  warmup = 1000,
  cores = 4,
  seed = 12345,
  backend = "rstan"
)

# Model comparison using WAIC
cat("Model comparison (WAIC):\n")
waic_full <- WAIC(bayesian_model)
waic_null <- WAIC(null_model)

cat("Full model WAIC:", waic_full$estimates["waic", "Estimate"], "\n")
cat("Null model WAIC:", waic_null$estimates["waic", "Estimate"], "\n")
cat("WAIC difference (full - null):", waic_full$estimates["waic", "Estimate"] - waic_null$estimates["waic", "Estimate"], "\n")

# Leave-one-out cross-validation
cat("\nComputing LOO-CV...\n")
loo_full <- loo(bayesian_model)
loo_null <- loo(null_model)

loo_compare <- loo_compare(loo_full, loo_null)
print(loo_compare)

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Files saved:\n")
cat("- bayesian_hierarchical_multinomial_model.rds\n")
cat("- bayesian_posterior_samples.csv\n")
cat("- bayesian_predicted_probabilities.csv\n")

cat("\n*** BAYESIAN ANALYSIS WITH BRMS COMPLETED SUCCESSFULLY ***\n")
cat("This is a true Bayesian hierarchical multinomial regression\n")
cat("with proper priors, MCMC sampling, and full uncertainty quantification.\n") 