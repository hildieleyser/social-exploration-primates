# =============================================================================
# QUICK Bayesian Analysis - Faster Version (~10-15 minutes total)
# =============================================================================

# Set CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org/"))

# Load required libraries
if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, brms, bayesplot, loo)

# Set up parallel processing
options(mc.cores = parallel::detectCores())

cat("Starting quick Bayesian analysis...\n")

# =============================================================================
# 1. LOAD AND CLEAN DATA
# =============================================================================

data <- read.csv("Explore Exploit Dataset.csv", stringsAsFactors = FALSE)

data_clean <- data %>%
  filter(!is.na(monkey) & !is.na(CONDITION) & !is.na(OUTCOME)) %>%
  mutate(
    # Create binary explore/exploit classification
    decision_type = case_when(
      str_detect(tolower(OUTCOME), "explore") ~ "explore",
      str_detect(tolower(OUTCOME), "exploit") ~ "exploit",
      TRUE ~ "other"
    ),
    
    # Clean social context
    social_context = case_when(
      CONDITION == "solo" ~ "solo",
      CONDITION == "duo" ~ "duo",
      CONDITION == "trio" ~ "trio",
      TRUE ~ "other"
    ),
    
    # Standardize predictors
    expected_explore_z = scale(expected_explore)[,1],
    subjective_exploit_z = scale(subjective_exploit)[,1],
    
    # Create factors
    monkey_id = factor(monkey),
    block_id = factor(BLOCK_No)
  ) %>%
  filter(decision_type %in% c("explore", "exploit")) %>%
  filter(!is.na(expected_explore) & social_context != "other") %>%
  group_by(monkey_id, social_context) %>%
  filter(n() >= 5) %>%  # Reduced threshold for quick analysis
  ungroup()

cat("Data cleaned. N =", nrow(data_clean), "\n")
cat("Monkeys:", length(unique(data_clean$monkey_id)), "\n")
cat("Social contexts:", table(data_clean$social_context), "\n")

# =============================================================================
# 2. QUICK DESCRIPTIVE STATS
# =============================================================================

summary_stats <- data_clean %>%
  group_by(social_context, decision_type) %>%
  summarise(
    count = n(),
    mean_expected_explore = mean(expected_explore, na.rm = TRUE),
    .groups = 'drop'
  )

print("Summary by Social Context:")
print(summary_stats)

# Quick plot
p1 <- data_clean %>%
  group_by(social_context) %>%
  summarise(explore_rate = mean(decision_type == "explore")) %>%
  ggplot(aes(x = social_context, y = explore_rate)) +
  geom_col(fill = "steelblue") +
  labs(title = "Exploration Rate by Social Context",
       x = "Social Context", y = "Proportion Explore") +
  theme_minimal()

print(p1)

# =============================================================================
# 3. QUICK BAYESIAN MODEL (Reduced iterations for speed)
# =============================================================================

cat("\nFitting quick Bayesian model...\n")

# Simple priors
priors <- c(
  prior(normal(0, 1), class = Intercept),
  prior(normal(0, 0.5), class = b),
  prior(exponential(1), class = sd)
)

# Single model with interaction (most important)
quick_model <- brm(
  decision_type ~ social_context * expected_explore_z + subjective_exploit_z + 
                  (1 | monkey_id) + (1 | block_id),
  data = data_clean,
  family = bernoulli(),
  prior = priors,
  chains = 2,           # Reduced from 4
  iter = 1000,          # Reduced from 2000
  warmup = 500,         # Reduced from 1000
  cores = 2,
  control = list(adapt_delta = 0.9),
  seed = 123
)

cat("Model fitted successfully!\n")

# =============================================================================
# 4. QUICK RESULTS
# =============================================================================

# Model summary
print(summary(quick_model))

# Extract key results
posterior_samples <- posterior_samples(quick_model)

# Key probabilities
prob_duo_greater <- mean(posterior_samples$b_social_contextduo > 0)
prob_trio_greater <- mean(posterior_samples$b_social_contexttrio > 0)
prob_expectation_positive <- mean(posterior_samples$b_expected_explore_z > 0)

cat("\n=============================================================================\n")
cat("QUICK RESULTS\n")
cat("=============================================================================\n")
cat("Probability that DUO context increases exploration vs SOLO:", 
    round(prob_duo_greater, 3), "\n")
cat("Probability that TRIO context increases exploration vs SOLO:", 
    round(prob_trio_greater, 3), "\n")
cat("Probability that higher EXPECTATION increases exploration:", 
    round(prob_expectation_positive, 3), "\n")

# Quick diagnostic plot
p_trace <- plot(quick_model, type = "trace")
print(p_trace)

# Quick effects plot
p_effects <- plot(quick_model, type = "fixed")
print(p_effects)

# Conditional effects
p_cond <- conditional_effects(quick_model, effects = "social_context")
print(p_cond$social_context)

# =============================================================================
# 5. SAVE QUICK RESULTS
# =============================================================================

save(quick_model, data_clean, summary_stats, 
     file = "quick_bayesian_results.RData")

cat("\nQuick analysis complete! Results saved to: quick_bayesian_results.RData\n")
cat("Runtime: Much faster than full analysis!\n") 