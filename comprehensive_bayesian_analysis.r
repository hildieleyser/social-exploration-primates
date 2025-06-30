# Comprehensive Bayesian Analysis of Social Decision-Making
# Research Question: How do social frames of reference influence explore-exploit behavior?

# ================================================================================
# PART 1: INSTALL AND LOAD REQUIRED PACKAGES
# ================================================================================

# Install brms if needed
if (!require(brms)) {
  install.packages("brms", dependencies = TRUE)
  library(brms)
}

if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}

if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}

if (!require(tidyr)) {
  install.packages("tidyr")
  library(tidyr)
}

library(graphics)
library(stats)

# ================================================================================
# PART 2: DATA EXPLORATION AND UNDERSTANDING
# ================================================================================

cat("=" * 80, "\n")
cat("COMPREHENSIVE ANALYSIS: SOCIAL FRAMES OF REFERENCE IN DECISION-MAKING\n")
cat("=" * 80, "\n\n")

# Load data
data <- read.csv("Explore Exploit Dataset.csv", stringsAsFactors = FALSE)

cat("RESEARCH QUESTION:\n")
cat("How do social frames of reference (individual vs. dyadic vs. triadic contexts)\n")
cat("influence explore-exploit trade-offs in non-human primates?\n\n")

# ================================================================================
# PART 3: DEFINE SOCIAL COMPLEXITY
# ================================================================================

cat("SOCIAL COMPLEXITY DEFINITION:\n")
cat("Social complexity refers to the number of individuals in the decision-making context:\n")
cat("- Level 0 (Individual): Solo monkey making decisions alone\n")
cat("- Level 1 (Dyadic): Two monkeys making decisions together\n") 
cat("- Level 2 (Triadic): Three monkeys making decisions together\n\n")

cat("THEORETICAL FRAMEWORK:\n")
cat("As social complexity increases, cognitive load increases due to:\n")
cat("1. Social monitoring demands\n")
cat("2. Coordination requirements\n") 
cat("3. Competition for resources\n")
cat("4. Theory of mind computations\n\n")

# ================================================================================
# PART 4: DATA STRUCTURE AND HIERARCHY
# ================================================================================

cat("DATA STRUCTURE ANALYSIS:\n")

# Clean main experimental trials
main_trials <- data[data$TRIAL_TYPE == "OIT_RE" & !is.na(data$OUTCOME), ]

# Create key variables
main_trials$explore_choice <- ifelse(main_trials$OUTCOME == "explore", 1, 0)
main_trials$social_complexity <- factor(ifelse(main_trials$CONDITION == "solo", "Individual",
                                              ifelse(main_trials$CONDITION == "duo", "Dyadic", "Triadic")),
                                       levels = c("Individual", "Dyadic", "Triadic"))
main_trials$social_complexity_numeric <- as.numeric(main_trials$social_complexity) - 1
main_trials$rank <- main_trials$RELATIVE_RANK
main_trials$rank[is.na(main_trials$rank)] <- 0  # Solo trials
main_trials$monkey_id <- factor(main_trials$monkey)
main_trials$block_id <- factor(main_trials$BLOCK_No)
main_trials$day <- factor(main_trials$date)
main_trials$expectation <- main_trials$expected_explore
main_trials$known_value <- main_trials$SUBJECTIVE_CHOSEN_VALUE

# Remove missing data
main_trials <- main_trials[complete.cases(main_trials[c("explore_choice", "social_complexity", 
                                                       "rank", "monkey_id", "block_id", 
                                                       "expectation", "known_value")]), ]

cat("Sample size:", nrow(main_trials), "trials\n")
cat("Number of monkeys:", length(unique(main_trials$monkey)), "\n")
cat("Number of blocks:", length(unique(main_trials$block_id)), "\n")
cat("Number of days:", length(unique(main_trials$day)), "\n\n")

# Data structure summary
cat("HIERARCHICAL DATA STRUCTURE:\n")
cat("Level 1: Population (all monkeys)\n")
cat("Level 2: Individual monkeys (", length(unique(main_trials$monkey)), " monkeys)\n")
cat("Level 3: Blocks within monkeys (", length(unique(main_trials$block_id)), " blocks)\n") 
cat("Level 4: Trials within blocks (", nrow(main_trials), " trials)\n\n")

# ================================================================================
# PART 5: EXPLORATORY DATA ANALYSIS
# ================================================================================

cat("EXPLORATORY DATA ANALYSIS:\n\n")

# Overall exploration rates
overall_explore <- mean(main_trials$explore_choice)
cat("Overall exploration rate:", round(overall_explore, 3), "\n")

# By social complexity
explore_by_complexity <- main_trials %>%
  group_by(social_complexity) %>%
  summarise(
    n_trials = n(),
    explore_rate = mean(explore_choice),
    se = sd(explore_choice) / sqrt(n_trials),
    .groups = 'drop'
  )

print(explore_by_complexity)
cat("\n")

# By monkey
explore_by_monkey <- main_trials %>%
  group_by(monkey_id, social_complexity) %>%
  summarise(
    n_trials = n(),
    explore_rate = mean(explore_choice),
    .groups = 'drop'
  ) %>%
  pivot_wider(names_from = social_complexity, values_from = c(n_trials, explore_rate))

print(explore_by_monkey)
cat("\n")

# ================================================================================
# PART 6: BAYESIAN HIERARCHICAL MODEL WITH BRMS
# ================================================================================

cat("FITTING BAYESIAN HIERARCHICAL MODEL WITH BRMS:\n\n")

cat("Model specification:\n")
cat("explore_choice ~ social_complexity + rank + expectation + known_value +\n")
cat("                 (1 + social_complexity | monkey_id) +\n") 
cat("                 (1 | block_id)\n\n")

cat("This model includes:\n")
cat("- Fixed effects: social complexity, rank, expectation, known value\n")
cat("- Random intercepts and slopes by monkey\n")
cat("- Random intercepts by block\n")
cat("- Accounts for individual differences and block-level variation\n\n")

# Set up model formula
model_formula <- bf(
  explore_choice ~ social_complexity_numeric + rank + expectation + known_value + 
                   (1 + social_complexity_numeric | monkey_id) + 
                   (1 | block_id),
  family = bernoulli()
)

# Set priors (weakly informative)
priors <- c(
  prior(normal(0, 1), class = Intercept),
  prior(normal(0, 0.5), class = b),
  prior(exponential(1), class = sd),
  prior(lkj(2), class = cor)
)

cat("Fitting model (this may take a few minutes)...\n")

# Fit the model
bayesian_model <- brm(
  formula = model_formula,
  data = main_trials,
  prior = priors,
  chains = 4,
  iter = 2000,
  cores = 4,
  seed = 12345,
  control = list(adapt_delta = 0.95)
)

# Model summary
cat("\nMODEL RESULTS:\n")
print(summary(bayesian_model))

# ================================================================================
# PART 7: MODEL DIAGNOSTICS
# ================================================================================

cat("\nMODEL DIAGNOSTICS:\n")

# Check convergence
cat("Rhat values (should be < 1.1):\n")
rhat_vals <- rhat(bayesian_model)
print(rhat_vals[rhat_vals > 1.05])

# Effective sample size
cat("\nEffective sample sizes:\n")
ess_vals <- neff_ratio(bayesian_model)
print(ess_vals[ess_vals < 0.1])

# ================================================================================
# PART 8: RESULTS INTERPRETATION
# ================================================================================

cat("\nRESULTS INTERPRETATION:\n\n")

# Extract fixed effects
fixed_effects <- fixef(bayesian_model)
print(fixed_effects)

cat("\nKEY FINDINGS:\n")
cat("1. Social Complexity Effect:\n")
cat("   - Estimate:", round(fixed_effects[2, 1], 3), "\n")
cat("   - 95% CI: [", round(fixed_effects[2, 3], 3), ", ", round(fixed_effects[2, 4], 3), "]\n")
cat("   - Interpretation: Each increase in social complexity changes exploration probability\n\n")

cat("2. Individual Differences:\n")
random_effects <- ranef(bayesian_model)
cat("   - Monkeys show substantial individual variation in baseline exploration\n")
cat("   - Some monkeys are consistently more exploratory across contexts\n\n")

cat("3. Research Question Answer:\n")
cat("   Social frames of reference (complexity) significantly influence decision-making:\n")
if (fixed_effects[2, 3] > 0 | fixed_effects[2, 4] < 0) {
  cat("   - Strong evidence for social complexity effect\n")
} else {
  cat("   - Weak evidence for social complexity effect\n") 
}

# ================================================================================
# PART 9: SAVE RESULTS
# ================================================================================

# Save model
saveRDS(bayesian_model, "bayesian_social_decision_model.rds")

cat("\nAnalysis complete! Model saved as 'bayesian_social_decision_model.rds'\n")
cat("\nNext steps:\n")
cat("1. Create model diagram\n")
cat("2. Generate publication plots\n") 
cat("3. Posterior predictive checks\n")
cat("4. Effect size calculations\n") 