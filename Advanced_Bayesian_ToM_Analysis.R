# =============================================================================
# ADVANCED BAYESIAN THEORY OF MIND ANALYSIS USING BRMS
# =============================================================================
# 
# This script implements a mathematically rigorous Bayesian hierarchical model
# to test the Theory of Mind hypothesis using brms (Bayesian Regression Models using Stan).
#
# KEY HYPOTHESIS: 
# - Subordinates will show different exploit rates based on partner rank
# - Dominants will show similar exploit rates regardless of partner rank
# - Intermediates will show context-dependent behavior
#
# BAYESIAN APPROACH:
# - Hierarchical logistic regression with random effects for individual monkeys
# - Proper prior specification and posterior inference
# - Credible intervals and probability of direction
# - Model comparison using WAIC and LOO-CV
#
# =============================================================================

# Load required libraries
library(tidyverse)
library(brms)
library(bayesplot)
library(tidybayes)
library(patchwork)
library(viridis)
library(gridExtra)

# Set seed for reproducibility
set.seed(42)

# =============================================================================
# DATA LOADING AND PREPROCESSING
# =============================================================================

# Load the dataset
data <- read.csv("Explore Exploit Dataset.csv", stringsAsFactors = FALSE)

# Clean and prepare data for Bayesian theory of mind analysis
tom_data <- data %>%
  # Convert date to proper format
  mutate(date = as.Date(date, format = "%d/%m/%Y")) %>%
  # Create key variables
  mutate(
    # Social condition (solo = 0, duo = 1, trio = 2)
    social_context = case_when(
      CONDITION == "solo" ~ 0,
      CONDITION == "duo" ~ 1,
      CONDITION == "trio" ~ 2
    ),
    
    # Rank position (1=dominant, 2=intermediate, 3=subordinate)
    rank_position = RELATIVE_RANK,
    
    # Binary exploit choice (key outcome variable)
    exploit_choice = ifelse(grepl("exploit", OUTCOME), 1, 0),
    
    # Partner information
    partner = PAIRED_WITH,
    
    # Expected value of explore option
    expected_explore = as.numeric(expected_explore),
    
    # Subjective chosen value
    subjective_chosen = as.numeric(SUBJECTIVE_CHOSEN_VALUE),
    
    # Create unique trial identifier
    trial_id = paste(BLOCK_No, TRIAL_NUM, monkey, sep = "_")
  ) %>%
  # Remove missing data
  filter(!is.na(exploit_choice), !is.na(rank_position)) %>%
  # Create individual monkey ID
  mutate(monkey_id = as.factor(monkey))

# Create theory of mind variables for social conditions only
tom_social_data <- tom_data %>%
  filter(social_context > 0) %>%  # Only social conditions (duo/trio)
  mutate(
    # Partner rank (simplified)
    partner_rank = case_when(
      grepl("FRAN", partner) ~ 1,
      grepl("DALI", partner) ~ 2,
      grepl("EBI", partner) ~ 3,
      TRUE ~ NA_real_
    ),
    
    # Rank difference (negative = subordinate to partner)
    rank_diff = rank_position - partner_rank,
    
    # Theory of mind context (KEY PREDICTOR)
    tom_context = case_when(
      rank_diff < 0 ~ "subordinate_to_partner",  # Key test condition
      rank_diff > 0 ~ "dominant_to_partner",     # Control condition
      rank_diff == 0 ~ "same_rank"               # Baseline
    ),
    
    # Is subordinate to partner (binary version)
    is_subordinate = rank_diff < 0,
    
    # Is dominant to partner (binary version)
    is_dominant = rank_diff > 0,
    
    # Rank category for analysis
    rank_category = case_when(
      rank_position == 1 ~ "dominant",
      rank_position == 2 ~ "intermediate", 
      rank_position == 3 ~ "subordinate"
    ),
    
    # Create interaction terms for Bayesian analysis
    rank_tom_interaction = paste(rank_category, tom_context, sep = "_"),
    
    # Partner-specific variables
    partner_dominance = case_when(
      partner_rank == 1 ~ "dominant_partner",
      partner_rank == 2 ~ "intermediate_partner",
      partner_rank == 3 ~ "subordinate_partner"
    ),
    
    # Create numeric variables for Bayesian modeling
    rank_numeric = as.numeric(factor(rank_category, levels = c("subordinate", "intermediate", "dominant"))),
    tom_context_numeric = as.numeric(factor(tom_context, levels = c("dominant_to_partner", "same_rank", "subordinate_to_partner"))),
    
    # Interaction term for Bayesian model
    rank_tom_interaction_numeric = rank_numeric * tom_context_numeric
  ) %>%
  # Remove rows with missing partner rank
  filter(!is.na(partner_rank))

# =============================================================================
# BAYESIAN MODEL SPECIFICATION
# =============================================================================

# Model 1: Hierarchical Logistic Regression with Random Effects
# This is the main model testing the Theory of Mind hypothesis

# Prior specification for Bayesian model
priors <- c(
  # Fixed effects priors (weakly informative)
  prior(normal(0, 2), class = "b"),  # Main effects
  
  # Random effects priors
  prior(student_t(3, 0, 2.5), class = "sd"),  # Standard deviation of random effects
  
  # Intercept prior
  prior(normal(0, 2), class = "Intercept")
)

# Fit the main Bayesian hierarchical model
cat("Fitting Bayesian hierarchical model...\n")

bayesian_model <- brm(
  # Model formula
  exploit_choice ~ rank_category * tom_context + 
                   (1 | monkey_id) +  # Random intercepts for individual monkeys
                   (1 | partner),     # Random intercepts for partners
  
  # Data and family
  data = tom_social_data,
  family = bernoulli(link = "logit"),
  
  # Priors
  prior = priors,
  
  # MCMC settings
  chains = 4,
  iter = 4000,
  warmup = 2000,
  cores = 4,
  
  # Control settings - increased adapt_delta to address divergent transitions
  control = list(adapt_delta = 0.99, max_treedepth = 12),
  
  # Save all parameters
  save_pars = save_pars(all = TRUE),
  
  # File for saving model
  file = "bayesian_tom_model"
)

# =============================================================================
# MODEL DIAGNOSTICS
# =============================================================================

cat("\n=== BAYESIAN MODEL DIAGNOSTICS ===\n")

# Check convergence
convergence_summary <- summary(bayesian_model)
print(convergence_summary)

# Extract R-hat and ESS from the summary (already printed above)
cat("\nR-hat values (should be < 1.1):\n")
rhat_values <- convergence_summary$fixed$Rhat
print(rhat_values)

cat("\nEffective sample sizes:\n")
ess_values <- convergence_summary$fixed$Bulk_ESS
print(ess_values)

# =============================================================================
# POSTERIOR ANALYSIS
# =============================================================================

# Extract posterior samples
posterior_samples <- posterior_samples(bayesian_model)

# Calculate probability of direction for key parameters
prob_direction <- function(x) {
  mean(x > 0)
}

# Key parameters for hypothesis testing
key_params <- c(
  "b_rank_categoryintermediate:tom_contextsubordinate_to_partner",
  "b_rank_categorydominant:tom_contextsubordinate_to_partner",
  "b_rank_categorysubordinate:tom_contextsubordinate_to_partner"
)

# Calculate probabilities of direction
prob_directions <- sapply(key_params, function(param) {
  if(param %in% colnames(posterior_samples)) {
    prob_direction(posterior_samples[[param]])
  } else {
    NA
  }
})

cat("\n=== PROBABILITY OF DIRECTION ===\n")
for(i in seq_along(key_params)) {
  if(!is.na(prob_directions[i])) {
    cat(names(prob_directions)[i], ":", round(prob_directions[i], 3), "\n")
  }
}

# =============================================================================
# HYPOTHESIS TESTING WITH CREDIBLE INTERVALS
# =============================================================================

# Get credible intervals for key parameters
credible_intervals <- posterior_summary(bayesian_model, prob = 0.95)

# Focus on interaction terms
interaction_terms <- credible_intervals[grep("rank_category.*tom_context", rownames(credible_intervals)), ]

cat("\n=== 95% CREDIBLE INTERVALS FOR INTERACTION TERMS ===\n")
print(interaction_terms)

# =============================================================================
# MODEL COMPARISON
# =============================================================================

# Fit a simpler model without interactions for comparison
simple_model <- brm(
  exploit_choice ~ rank_category + tom_context + 
                   (1 | monkey_id) + (1 | partner),
  data = tom_social_data,
  family = bernoulli(link = "logit"),
  prior = prior(normal(0, 2), class = "b"),
  chains = 4,
  iter = 2000,
  warmup = 1000,
  cores = 4,
  file = "bayesian_simple_model"
)

# Compute LOO for both models before comparison
cat("\nComputing LOO for model comparison...\n")
loo_bayesian <- loo(bayesian_model)
loo_simple <- loo(simple_model)

# Compare models using WAIC and LOO-CV
model_comparison <- loo_compare(loo_bayesian, loo_simple)
cat("\n=== MODEL COMPARISON TABLE (LOO-CV) ===\n")
print(model_comparison)

# =============================================================================
# PREDICTIVE ANALYSIS
# =============================================================================

# Generate predictions for different scenarios
prediction_data <- expand.grid(
  rank_category = c("subordinate", "intermediate", "dominant"),
  tom_context = c("dominant_to_partner", "subordinate_to_partner"),
  monkey_id = unique(tom_social_data$monkey_id)[1],  # Use first monkey as reference
  partner = unique(tom_social_data$partner)[1]        # Use first partner as reference
)

# Generate posterior predictions
predictions <- posterior_predict(bayesian_model, newdata = prediction_data, nsamples = 1000, allow_new_levels = TRUE)

# Calculate predicted probabilities
predicted_probs <- apply(predictions, 2, mean)
prediction_data$predicted_probability <- predicted_probs

# Calculate credible intervals for predictions
pred_ci_lower <- apply(predictions, 2, quantile, 0.025)
pred_ci_upper <- apply(predictions, 2, quantile, 0.975)
prediction_data$ci_lower <- pred_ci_lower
prediction_data$ci_upper <- pred_ci_upper

# =============================================================================
# VISUALIZATIONS
# =============================================================================

# Plot 1: Posterior distributions of key parameters
existing_pars <- intersect(
  c("b_rank_categoryintermediate:tom_contextsubordinate_to_partner",
    "b_rank_categorysubordinate:tom_contextsubordinate_to_partner"),
  names(fixef(bayesian_model))
)
if (length(existing_pars) > 0) {
  p1 <- mcmc_areas(bayesian_model, 
                   pars = existing_pars,
                   prob = 0.95) +
    labs(title = "Posterior Distributions of ToM Interaction Effects",
         subtitle = "95% credible intervals shown") +
    theme_minimal()
} else {
  cat("No ToM interaction parameters found for plotting posterior distributions.\n")
  p1 <- ggplot() + theme_void() + labs(title = "No ToM interaction parameters found")
}

# Plot 2: Predicted probabilities with credible intervals
p2 <- ggplot(prediction_data, 
             aes(x = rank_category, y = predicted_probability, 
                 color = tom_context, shape = tom_context)) +
  geom_point(size = 4, alpha = 0.8) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                width = 0.2, alpha = 0.6) +
  geom_line(aes(group = tom_context), alpha = 0.6) +
  scale_color_viridis_d(name = "ToM Context") +
  scale_shape_discrete(name = "ToM Context") +
  labs(title = "Bayesian Predicted Probabilities",
       subtitle = "Exploit probability with 95% credible intervals",
       x = "Rank Category",
       y = "Predicted Exploit Probability") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Plot 3: MCMC trace plots for key parameters
if (length(existing_pars) > 0) {
  p3 <- mcmc_trace(bayesian_model, 
                   pars = existing_pars) +
    labs(title = "MCMC Trace Plots",
         subtitle = "Convergence diagnostics for key parameters") +
    theme_minimal()
} else {
  cat("No ToM interaction parameters found for MCMC trace plots.\n")
  p3 <- ggplot() + theme_void() + labs(title = "No ToM interaction parameters found")
}

# Combine plots (only p1, p2, p3)
combined_plot <- (p1 + p2) / (p3) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

# Save plots
ggsave("advanced_bayesian_tom_analysis.png", combined_plot, width = 16, height = 12, dpi = 300)

# =============================================================================
# HYPOTHESIS TESTING RESULTS
# =============================================================================

cat("\n=== ADVANCED BAYESIAN THEORY OF MIND ANALYSIS RESULTS ===\n")

# Test the specific hypothesis
cat("\n1. HYPOTHESIS TESTING:\n")

# Check if subordinates show different behavior based on ToM context
subordinate_effect <- interaction_terms["b_rank_categorysubordinate:tom_contextsubordinate_to_partner", ]
if(!is.na(subordinate_effect[1])) {
  cat("Subordinate ToM Effect:\n")
  cat("  Estimate:", round(subordinate_effect[1], 3), "\n")
  cat("  95% CI:", round(subordinate_effect[3], 3), "to", round(subordinate_effect[4], 3), "\n")
  cat("  Probability > 0:", round(prob_direction(posterior_samples$`b_rank_categorysubordinate:tom_contextsubordinate_to_partner`), 3), "\n")
}

# Check if dominants show different behavior based on ToM context
if ("b_rank_categorydominant:tom_contextsubordinate_to_partner" %in% rownames(interaction_terms)) {
  dominant_effect <- interaction_terms["b_rank_categorydominant:tom_contextsubordinate_to_partner", ]
  cat("\nDominant ToM Effect:\n")
  cat("  Estimate:", round(dominant_effect[1], 3), "\n")
  cat("  95% CI:", round(dominant_effect[3], 3), "to", round(dominant_effect[4], 3), "\n")
  cat("  Probability > 0:", round(prob_direction(posterior_samples$`b_rank_categorydominant:tom_contextsubordinate_to_partner`), 3), "\n")
} else {
  cat("\nNo dominant ToM interaction parameter estimated (dominant is likely the reference level).\n")
}

# Check if intermediates show different behavior based on ToM context
if ("b_rank_categoryintermediate:tom_contextsubordinate_to_partner" %in% rownames(interaction_terms)) {
  intermediate_effect <- interaction_terms["b_rank_categoryintermediate:tom_contextsubordinate_to_partner", ]
  cat("\nIntermediate ToM Effect:\n")
  cat("  Estimate:", round(intermediate_effect[1], 3), "\n")
  cat("  95% CI:", round(intermediate_effect[3], 3), "to", round(intermediate_effect[4], 3), "\n")
  cat("  Probability > 0:", round(prob_direction(posterior_samples$`b_rank_categoryintermediate:tom_contextsubordinate_to_partner`), 3), "\n")
} else {
  cat("\nNo intermediate ToM interaction parameter estimated.\n")
}

cat("\n2. MODEL PERFORMANCE:\n")
cat("WAIC:", round(waic(bayesian_model)$estimates["waic", "Estimate"], 2), "\n")
cat("LOO-CV:", round(loo(bayesian_model)$estimates["looic", "Estimate"], 2), "\n")

cat("\n3. HYPOTHESIS EVALUATION:\n")

# Evaluate the hypothesis based on Bayesian evidence
subordinate_support <- ifelse(!is.na(subordinate_effect[1]), 
                             abs(subordinate_effect[1]) > 0.5 && 
                             prob_direction(posterior_samples$`b_rank_categorysubordinate:tom_contextsubordinate_to_partner`) > 0.8, 
                             FALSE)

dominant_support <- ifelse(!is.na(dominant_effect[1]), 
                          abs(dominant_effect[1]) < 0.3 && 
                          prob_direction(posterior_samples$`b_rank_categorydominant:tom_contextsubordinate_to_partner`) < 0.7, 
                          FALSE)

if(subordinate_support && dominant_support) {
  cat("STRONG SUPPORT: Subordinates show strong ToM effects, dominants show minimal effects\n")
} else if(subordinate_support) {
  cat("PARTIAL SUPPORT: Subordinates show ToM effects, but dominants also show some effects\n")
} else if(dominant_support) {
  cat("PARTIAL SUPPORT: Dominants show minimal ToM effects, but subordinates also show minimal effects\n")
} else {
  cat("NO SUPPORT: Neither subordinates nor dominants show clear ToM effects\n")
}

cat("\n=== BAYESIAN ANALYSIS COMPLETE ===\n")
cat("Check 'advanced_bayesian_tom_analysis.png' for comprehensive visualizations\n")
cat("Model saved as 'bayesian_tom_model.rds'\n") 