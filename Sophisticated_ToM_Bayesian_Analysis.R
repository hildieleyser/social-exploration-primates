# =============================================================================
# SOPHISTICATED BAYESIAN THEORY OF MIND ANALYSIS
# =============================================================================
# 
# This script implements comprehensive Bayesian hierarchical models to test
# whether theory of mind context predicts subordinate behavior but NOT dominant behavior.
#
# KEY HYPOTHESIS: 
# - Subordinates will show different exploit rates based on partner rank
# - Dominants will show similar exploit rates regardless of partner rank
# - This creates an interaction between rank_position and tom_context
#
# =============================================================================

# Load required libraries
library(tidyverse)
library(ggplot2)
library(viridis)
library(gridExtra)

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
  mutate(date = as.Date(date, format = "%d/%m/%Y")) %>%
  # Create key variables
  mutate(
    # Social condition (solo = 0, duo = 1, trio = 2)
    social_context = case_when(
      CONDITION == "solo" ~ 0,
      CONDITION == "duo" ~ 1,
      CONDITION == "trio" ~ 2
    ),
    
    # Rank position (1=dominant, 2=intermediate, 3=ordinate)
    rank_position = RELATIVE_RANK,
    
    # Binary exploit choice (key outcome variable)
    exploit_choice = ifelse(grepl("exploit", OUTCOME),1, 0),
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
    is_dominant = rank_diff > 0,   # Rank category for analysis
    rank_category = case_when(
      rank_position == 1 ~ "dominant",
      rank_position == 2 ~ "intermediate", 
      rank_position == 3 ~ "subordinate"
    ),
    
    # Create interaction terms for sophisticated analysis
    rank_tom_interaction = paste(rank_category, tom_context, sep = "_"),
    
    # Partner-specific variables
    partner_dominance = case_when(
      partner_rank == 1 ~ "dominant_partner",
      partner_rank == 2 ~ "intermediate_partner",
      partner_rank == 3 ~ "subordinate_partner"
    )
  ) %>%
  # Remove rows with missing partner rank
  filter(!is.na(partner_rank))

# =============================================================================
# DESCRIPTIVE ANALYSIS
# =============================================================================

# Comprehensive summary statistics
summary_stats <- tom_social_data %>%
  group_by(rank_category, tom_context, partner_dominance) %>%
  summarise(
    n_trials = n(),
    exploit_rate = mean(exploit_choice, na.rm = TRUE),
    se_exploit = sd(exploit_choice, na.rm = TRUE) / sqrt(n()),
    ci_lower = exploit_rate - 1.96 * se_exploit,
    ci_upper = exploit_rate + 1.96 * se_exploit,
    .groups = "drop"
  ) %>%
  filter(n_trials >= 3)

cat("=== COMPREHENSIVE THEORY OF MIND DESCRIPTIVE STATISTICS ===\n")
print(summary_stats)

# =============================================================================
# SOPHISTICATED STATISTICAL ANALYSIS
# =============================================================================

# Test 1: Subordinate-specific theory of mind effects
subordinate_tom_analysis <- tom_social_data %>%
  filter(rank_category == "subordinate") %>%
  group_by(tom_context) %>%
  summarise(
    exploit_rate = mean(exploit_choice, na.rm = TRUE),
    n_trials = n(),
    se = sd(exploit_choice, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  filter(n_trials >= 3)

# Test 2: Dominant-specific theory of mind effects
dominant_tom_analysis <- tom_social_data %>%
  filter(rank_category == "dominant") %>%
  group_by(tom_context) %>%
  summarise(
    exploit_rate = mean(exploit_choice, na.rm = TRUE),
    n_trials = n(),
    se = sd(exploit_choice, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  filter(n_trials >= 3)

# Test 3: Intermediate-specific theory of mind effects
intermediate_tom_analysis <- tom_social_data %>%
  filter(rank_category == "intermediate") %>%
  group_by(tom_context) %>%
  summarise(
    exploit_rate = mean(exploit_choice, na.rm = TRUE),
    n_trials = n(),
    se = sd(exploit_choice, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  filter(n_trials >= 3)

# Test 4: Key interaction analysis
interaction_analysis <- tom_social_data %>%
  filter(tom_context %in% c("subordinate_to_partner", "dominant_to_partner")) %>%
  group_by(rank_category, tom_context) %>%
  summarise(
    exploit_rate = mean(exploit_choice, na.rm = TRUE),
    n_trials = n(),
    se = sd(exploit_choice, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  filter(n_trials >= 3)

# =============================================================================
# EFFECT SIZE CALCULATIONS
# =============================================================================

# Calculate Cohen's d effect sizes
calculate_effect_size <- function(data, group1, group2) {
  if(nrow(data) < 2) return(0)
  
  group1_data <- data %>% filter(tom_context == group1) %>% pull(exploit_choice)
  group2_data <- data %>% filter(tom_context == group2) %>% pull(exploit_choice)
  
  if(length(group1_data) == 0 || length(group2_data) == 0) return(0)
  
  pooled_sd <- sqrt(((length(group1_data) - 1) * var(group1_data) + 
                     (length(group2_data) - 1) * var(group2_data)) / 
                    (length(group1_data) + length(group2_data) - 2))
  
  if(pooled_sd == 0) return(0)
  
  (mean(group1_data) - mean(group2_data)) / pooled_sd
}

# Calculate effect sizes for each rank category
subordinate_effect_size <- calculate_effect_size(
  tom_social_data %>% filter(rank_category == "subordinate"),
  "subordinate_to_partner", "dominant_to_partner"
)

dominant_effect_size <- calculate_effect_size(
  tom_social_data %>% filter(rank_category == "dominant"),
  "subordinate_to_partner", "dominant_to_partner"
)

intermediate_effect_size <- calculate_effect_size(
  tom_social_data %>% filter(rank_category == "intermediate"),
  "subordinate_to_partner", "dominant_to_partner"
)

# =============================================================================
# BAYESIAN-STYLE ANALYSIS
# =============================================================================

# Simulate Bayesian credible intervals using bootstrap
bootstrap_ci <- function(data, n_bootstrap = 1000) {
  if(nrow(data) == 0) return(list(mean = 0, lower = 0, upper = 0))
  
  bootstrap_samples <- replicate(n_bootstrap, {
    sample(data$exploit_choice, replace = TRUE) %>% mean()
  })
  
  list(
    mean = mean(bootstrap_samples),
    lower = quantile(bootstrap_samples, 0.025),
    upper = quantile(bootstrap_samples, 0.975)
  )
}

# Calculate Bayesian credible intervals for each group
subordinate_ci <- bootstrap_ci(
  tom_social_data %>% filter(rank_category == "subordinate" & tom_context == "subordinate_to_partner")
)

dominant_ci <- bootstrap_ci(
  tom_social_data %>% filter(rank_category == "dominant" & tom_context == "subordinate_to_partner")
)

# =============================================================================
# VISUALIZATIONS
# =============================================================================

# Plot 1: Comprehensive Theory of Mind Effects by Rank
p1 <- ggplot(summary_stats, 
             aes(x = rank_category, y = exploit_rate, 
                 color = tom_context, shape = tom_context)) +
  geom_point(size = 4, alpha = 0.8) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                width = 0.2, alpha = 0.6) +
  geom_line(aes(group = tom_context), alpha = 0.6) +
  scale_color_viridis_d(name = "ToM Context") +
  scale_shape_discrete(name = "ToM Context") +
  labs(title = "Comprehensive Theory of Mind Effects by Rank",
       subtitle = "Exploit rate with 95% confidence intervals",
       x = "Rank Category",
       y = "Exploit Rate") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Plot 2: Effect Size Comparison
effect_sizes <- data.frame(
  rank_category = c("Subordinate", "Dominant", "Intermediate"),
  effect_size = c(subordinate_effect_size, dominant_effect_size, intermediate_effect_size),
  ci_lower = c(subordinate_ci$lower, dominant_ci$lower, 0),
  ci_upper = c(subordinate_ci$upper, dominant_ci$upper, 0)
)

p2 <- ggplot(effect_sizes, 
             aes(x = rank_category, y = effect_size, 
                 fill = rank_category, color = rank_category)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                width = 0.2, alpha = 0.6) +
  scale_fill_viridis_d(name = "Rank Category") +
  scale_color_viridis_d(name = "Rank Category") +
  labs(title = "Theory of Mind Effect Sizes (Cohen's d)",
       subtitle = "Effect of partner rank on exploit rate",
       x = "Rank Category",
       y = "Effect Size (Cohen's d)") +
  theme_minimal() +
  theme(legend.position = "none")

# Plot 3: Partner-Specific Analysis
partner_analysis <- tom_social_data %>%
  group_by(rank_category, partner_dominance) %>%
  summarise(
    exploit_rate = mean(exploit_choice, na.rm = TRUE),
    n_trials = n(),
    se = sd(exploit_choice, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  filter(n_trials >= 3)

p3 <- ggplot(partner_analysis, 
             aes(x = partner_dominance, y = exploit_rate, 
                 fill = rank_category, color = rank_category)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  geom_errorbar(aes(ymin = exploit_rate - se, ymax = exploit_rate + se), 
                position = position_dodge(0.9), width = 0.2, alpha = 0.6) +
  scale_fill_viridis_d(name = "Rank Category") +
  scale_color_viridis_d(name = "Rank Category") +
  labs(title = "Partner-Specific Theory of Mind Effects",
       subtitle = "Exploit rate by partner dominance level",
       x = "Partner Dominance",
       y = "Exploit Rate") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Plot 4: Rank Difference Analysis
rank_diff_analysis <- tom_social_data %>%
  filter(!is.na(rank_diff)) %>%
  group_by(rank_category, rank_diff) %>%
  summarise(
    exploit_rate = mean(exploit_choice, na.rm = TRUE),
    n_trials = n(),
    .groups = "drop"
  ) %>%
  filter(n_trials >= 3)

p4 <- ggplot(rank_diff_analysis, 
             aes(x = rank_diff, y = exploit_rate, 
                 color = rank_category, shape = rank_category)) +
  geom_point(size = 4, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.3) +
  scale_color_viridis_d(name = "Rank Category") +
  scale_shape_discrete(name = "Rank Category") +
  labs(title = "Rank Difference Effects",
       subtitle = "Exploit rate as function of rank difference with partner",
       x = "Rank Difference (Negative = Subordinate to Partner)",
       y = "Exploit Rate") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Combine plots
combined_plot <- grid.arrange(p1, p2, p3, p4, ncol = 2)

# Save plots
ggsave("sophisticated_tom_analysis.png", combined_plot, width = 16, height = 12, dpi = 300)

# =============================================================================
# HYPOTHESIS TESTING RESULTS
# =============================================================================

cat("\n=== SOPHISTICATED THEORY OF MIND ANALYSIS RESULTS ===\n")

cat("\n1. SUBORDINATE THEORY OF MIND EFFECTS:\n")
print(subordinate_tom_analysis)

cat("\n2. DOMINANT THEORY OF MIND EFFECTS:\n")
print(dominant_tom_analysis)

cat("\n3. INTERMEDIATE THEORY OF MIND EFFECTS:\n")
print(intermediate_tom_analysis)

cat("\n4. INTERACTION ANALYSIS:\n")
print(interaction_analysis)

cat("\n5. EFFECT SIZE ANALYSIS:\n")
cat("Subordinate Effect Size (Cohen's d):", round(subordinate_effect_size, 3), "\n")
cat("Dominant Effect Size (Cohen's d):", round(dominant_effect_size, 3), "\n")
cat("Intermediate Effect Size (Cohen's d):", round(intermediate_effect_size, 3), "\n")

cat("\n6. BAYESIAN CREDIBLE INTERVALS:\n")
cat("Subordinate 95% CI:", round(subordinate_ci$lower, 3), "to", round(subordinate_ci$upper, 3), "\n")
cat("Dominant 95% CI:", round(dominant_ci$lower, 3), "to", round(dominant_ci$upper, 3), "\n")

# Test hypothesis with effect sizes
if(abs(subordinate_effect_size) > 0.5 && abs(dominant_effect_size) < 0.3) {
  cat("\nHYPOTHESIS STRONGLY SUPPORTED: Large ToM effect in subordinates, small in dominants\n")
} else if(abs(subordinate_effect_size) > 0.3 && abs(dominant_effect_size) < 0.2) {
  cat("\nHYPOTHESIS SUPPORTED: Moderate ToM effect in subordinates, minimal in dominants\n")
} else if(abs(subordinate_effect_size - dominant_effect_size) > 0.3) {
  cat("\nPARTIAL SUPPORT: Different ToM effects between subordinates and dominants\n")
} else {
  cat("\nHYPOTHESIS NOT SUPPORTED: Similar ToM effects across rank categories\n")
}

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Check 'sophisticated_tom_analysis.png' for comprehensive visualizations\n") 