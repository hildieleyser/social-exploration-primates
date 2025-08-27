# =============================================================================
# PROPER BAYESIAN THEORY OF MIND ANALYSIS USING BRMS
# =============================================================================
# 
# This script implements Bayesian hierarchical models to test whether
# theory of mind context predicts subordinate behavior but NOT dominant behavior.
#
# KEY HYPOTHESIS: 
# - Subordinates will show different exploit rates based on partner rank
# - Dominants will show similar exploit rates regardless of partner rank
# - This creates an interaction between rank_position and tom_context
#
# =============================================================================

# Load required libraries
library(tidyverse)
library(brms)
library(bayesplot)
library(posterior)
library(patchwork)
library(viridis)

# Set seed for reproducibility
set.seed(42)

# =============================================================================
# DATA LOADING AND PREPROCESSING
# =============================================================================

# Load the dataset
data <- read.csv("Explore Exploit Dataset.csv", stringsAsFactors = FALSE)

# Clean and prepare data for theory of mind analysis
tom_data <- data %>%
  # Convert date to proper format
  mutate(date = as.Date(date, format = %d/%m/%Y))%>%
  # Create key variables
  mutate(
    # Social condition (solo = 0, duo = 1, trio = 2)
    social_context = case_when(
      CONDITION ==solo" ~ 0      CONDITION ==duo ~ 1, 
      CONDITION == trio ~2
    ),
    
    # Rank position (1=dominant, 2ntermediate, 3ordinate)
    rank_position = RELATIVE_RANK,
    
    # Binary exploit choice (key outcome variable)
    exploit_choice = ifelse(grepl(exploit", OUTCOME),1    
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
      grepl(FRAN", partner) ~1,
      grepl(DALI", partner) ~2,
      grepl(EBI", partner) ~ 3    TRUE ~ NA_real_
    ),
    
    # Rank difference (negative = subordinate to partner)
    rank_diff = rank_position - partner_rank,
    
    # Theory of mind context (KEY PREDICTOR)
    tom_context = case_when(
      rank_diff < 0 ~ "subordinate_to_partner",  # Key test condition
      rank_diff > 0 ~dominant_to_partner",     # Control condition
      rank_diff == 0 ~ "same_rank"               # Baseline
    ),
    
    # Is subordinate to partner (binary version)
    is_subordinate = rank_diff < 0,
    
    # Is dominant to partner (binary version)
    is_dominant = rank_diff > 0   # Rank category for analysis
    rank_category = case_when(
      rank_position == 1 ~dominant",
      rank_position ==2 ~intermediate", 
      rank_position == 3subordinate"
    )
  ) %>%
  # Remove rows with missing partner rank
  filter(!is.na(partner_rank))

# =============================================================================
# DESCRIPTIVE ANALYSIS
# =============================================================================

# Summary statistics
summary_stats <- tom_social_data %>%
  group_by(rank_category, tom_context) %>%
  summarise(
    n_trials = n(),
    exploit_rate = mean(exploit_choice, na.rm = TRUE),
    se_exploit = sd(exploit_choice, na.rm = TRUE) / sqrt(n()),
    .groups = "drop  ) %>%
  filter(n_trials >= 5)

cat("=== THEORY OF MIND DESCRIPTIVE STATISTICS ===\n")
print(summary_stats)

# =============================================================================
# BAYESIAN HIERARCHICAL MODELS
# =============================================================================

# Model 1sic theory of mind effect
# Tests if tom_context predicts exploit_choice
tom_model_1 <- brm(
  exploit_choice ~ tom_context + rank_category + expected_explore + 
                  (1 + tom_context | monkey_id),
  data = tom_social_data,
  family = bernoulli(),
  prior = c(
    prior(normal(0, 1 class =b,
    prior(normal(01lass = sd,group = monkey_id)
  ),
  chains = 4,
  iter =2000
  warmup =100 cores = 4,
  file = tom_model_1"
)

# Model 2: Rank × ToM Interaction (KEY MODEL)
# Tests if tom_context effects depend on rank_category
tom_model_2 <- brm(
  exploit_choice ~ tom_context * rank_category + expected_explore + 
                  (1 + tom_context | monkey_id),
  data = tom_social_data,
  family = bernoulli(),
  prior = c(
    prior(normal(0, 1 class =b,
    prior(normal(01lass = sd,group = monkey_id)
  ),
  chains = 4,
  iter =2000
  warmup =100 cores = 4,
  file = tom_model_2"
)

# Model 3: Partner-specific effects
# Tests if rank_diff predicts exploit_choice
tom_model_3 <- brm(
  exploit_choice ~ rank_diff + rank_category + expected_explore + 
                  (1 + rank_diff | monkey_id),
  data = tom_social_data %>% filter(!is.na(rank_diff)),
  family = bernoulli(),
  prior = c(
    prior(normal(0, 1 class =b,
    prior(normal(01lass = sd,group = monkey_id)
  ),
  chains = 4,
  iter =2000
  warmup =100 cores = 4,
  file = tom_model_3"
)

# =============================================================================
# MODEL COMPARISON AND DIAGNOSTICS
# =============================================================================

# Compare models using LOO-CV
model_comparison <- loo_compare(
  loo(tom_model_1),
  loo(tom_model_2),
  loo(tom_model_3)
)

cat("\n=== MODEL COMPARISON (LOO-CV) ===\n)print(model_comparison)

# Model diagnostics
cat(n=== MODEL DIAGNOSTICS ===\n)
print(summary(tom_model_2))

# =============================================================================
# HYPOTHESIS TESTING
# =============================================================================

# Extract posterior samples from the interaction model
posterior_samples <- posterior_samples(tom_model_2)

# Test 1 subordinates show different behavior based on tom_context?
subordinate_tom_effect <- tom_social_data %>%
  filter(rank_category == "subordinate") %>%
  group_by(tom_context) %>%
  summarise(
    exploit_rate = mean(exploit_choice, na.rm = TRUE),
    n_trials = n(),
    .groups = drop"
  )

cat(n=== SUBORDINATE THEORY OF MIND EFFECTS ===\n")
print(subordinate_tom_effect)

# Test 2: Do dominants show different behavior based on tom_context?
dominant_tom_effect <- tom_social_data %>%
  filter(rank_category ==dominant") %>%
  group_by(tom_context) %>%
  summarise(
    exploit_rate = mean(exploit_choice, na.rm = TRUE),
    n_trials = n(),
    .groups = drop"
  )

cat("\n=== DOMINANT THEORY OF MIND EFFECTS ===\n")
print(dominant_tom_effect)

# Test 3: Interaction effect (KEY TEST)
interaction_test <- tom_social_data %>%
  filter(tom_context %in% c("subordinate_to_partner",dominant_to_partner)) %>%
  group_by(rank_category, tom_context) %>%
  summarise(
    exploit_rate = mean(exploit_choice, na.rm = TRUE),
    n_trials = n(),
    .groups = drop"
  )

cat("\n=== RANK × TOM INTERACTION EFFECTS ===\n")
print(interaction_test)

# =============================================================================
# POSTERIOR ANALYSIS
# =============================================================================

# Extract key parameters from the interaction model
key_parameters <- posterior_summary(tom_model_2) %>%
  as.data.frame() %>%
  rownames_to_column(parameter") %>%
  filter(grepl("tom_context", parameter) | grepl("rank_category", parameter) | grepl(:, parameter))

cat("\n=== KEY PARAMETER ESTIMATES ===\n")
print(key_parameters)

# Calculate effect sizes
effect_sizes <- tom_social_data %>%
  group_by(rank_category, tom_context) %>%
  summarise(
    exploit_rate = mean(exploit_choice, na.rm = TRUE),
    n_trials = n(),
    .groups = "drop  ) %>%
  filter(n_trials >= 5) %>%
  pivot_wider(
    names_from = tom_context,
    values_from = exploit_rate,
    names_prefix =rate_  ) %>%
  mutate(
    tom_effect = rate_subordinate_to_partner - rate_dominant_to_partner
  )

cat(n=== THEORY OF MIND EFFECT SIZES ===\n")
print(effect_sizes)

# =============================================================================
# VISUALIZATIONS
# =============================================================================

# Plot1ry of Mind Effects by Rank Category
p1 <- ggplot(summary_stats, 
             aes(x = rank_category, y = exploit_rate, 
                 color = tom_context, shape = tom_context)) +
  geom_point(size = 4, alpha = 00.8 +
  geom_errorbar(aes(ymin = exploit_rate - se_exploit, 
                    ymax = exploit_rate + se_exploit), 
                width =02, alpha = 06 +
  geom_line(aes(group = tom_context), alpha = 0.6 +
  scale_color_viridis_d(name = ToM Context) +
  scale_shape_discrete(name = ToMContext") +
  labs(title = "Theory of Mind Effects by Rank Category,
       subtitle = "Exploit rate across different social contexts",
       x =Rank Category",
       y = Exploit Rate") +
  theme_minimal() +
  theme(legend.position = bottom")

# Plot 2: Interaction Effects (KEY PLOT)
interaction_plot <- ggplot(interaction_test, 
                          aes(x = tom_context, y = exploit_rate, 
                              fill = rank_category)) +
  geom_bar(stat = "identity", position = dodge, alpha = 00.8) +
  scale_fill_viridis_d(name = "Rank Category") +
  labs(title =Rank × Theory of Mind Interaction,
       subtitle = "Key test: Do subordinates show different ToM effects than dominants?",
       x = "Theory of Mind Context",
       y = Exploit Rate") +
  theme_minimal() +
  theme(legend.position = bottom")

# Plot 3: Posterior Predictive Check
pp_check_plot <- pp_check(tom_model_2, type = "bars") +
  labs(title = "Posterior Predictive Check,
       subtitle = "Model fit to observed exploit choice distributions")

# Plot 4: Parameter Estimates
param_plot <- mcmc_areas(tom_model_2, 
                         pars = c("b_tom_contextsubordinate_to_partner",
                             b_tom_contextdominant_to_partner",
                               b_rank_categorysubordinate:tom_contextsubordinate_to_partner",
                               b_rank_categorydominant:tom_contextsubordinate_to_partner)) +
  labs(title = "Key Parameter Estimates,
       subtitle = "Theory of mind effects by rank category")

# Combine plots
combined_plot <- (p1 + interaction_plot) / (pp_check_plot + param_plot) +
  plot_annotation(
    title = "Bayesian Theory of Mind Analysis Results",
    subtitle = "Testing whether ToM context predicts subordinate but not dominant behavior",
    theme = theme(plot.title = element_text(size = 16, face = "bold"))
  )

# Save plots
ggsave("bayesian_tom_analysis.png", combined_plot, width = 16, height =12 dpi = 300)
ggsave("bayesian_tom_analysis.pdf", combined_plot, width = 16 height = 12)

# =============================================================================
# HYPOTHESIS TESTING RESULTS
# =============================================================================

# Test the key hypothesis: subordinates show ToM effects, dominants dont
subordinate_tom_test <- effect_sizes %>%
  filter(rank_category == "subordinate) %>%
  pull(tom_effect)

dominant_tom_test <- effect_sizes %>%
  filter(rank_category ==dominant) %>%
  pull(tom_effect)

cat("\n=== HYPOTHESIS TESTING RESULTS ===\n")
cat("Subordinate ToM Effect:", round(subordinate_tom_test, 3), "\n")
cat("Dominant ToM Effect:", round(dominant_tom_test, 3), "\n)
cat("Difference (Subordinate - Dominant):", round(subordinate_tom_test - dominant_tom_test, 3nheck if hypothesis is supported
if(abs(subordinate_tom_test) > 00.1 && abs(dominant_tom_test) <01[object Object]
  cat("HYPOTHESIS SUPPORTED: Subordinates show ToM effects, dominants don't\n")
} else if(abs(subordinate_tom_test - dominant_tom_test) > 0.1) {
  cat("PARTIAL SUPPORT: Different ToM effects between subordinates and dominants\n")
} else[object Object]
  cat(HYPOTHESIS NOT SUPPORTED: Similar ToM effects across rank categories\n")
}

# =============================================================================
# SAVE RESULTS
# =============================================================================

# Save all results
saveRDS(list(
  models = list(tom_model_1, tom_model_2_model_3  model_comparison = model_comparison,
  summary_stats = summary_stats,
  interaction_test = interaction_test,
  effect_sizes = effect_sizes,
  hypothesis_test = list(
    subordinate_tom_test = subordinate_tom_test,
    dominant_tom_test = dominant_tom_test
  ),
  data = tom_social_data
), "bayesian_tom_results.rds")

cat("\n=== ANALYSIS COMPLETE ===\n)
cat(Check 'bayesian_tom_analysis.png' for visualizations\n")
cat("Results saved in 'bayesian_tom_results.rds'\n") 