# =============================================================================
# BASIC THEORY OF MIND ANALYSIS FOR MONKEY DATA
# =============================================================================

# Load libraries
library(tidyverse)
library(ggplot2)
library(viridis)

# Set seed
set.seed(42)

# =============================================================================
# DATA LOADING AND CLEANING
# =============================================================================

# Load data
data <- read.csv("Explore Exploit Dataset.csv", stringsAsFactors = FALSE)

# Clean and prepare data
clean_data <- data %>%
  # Create key variables
  mutate(
    # Social condition
    social_condition = CONDITION,
    
    # Rank position
    rank_position = RELATIVE_RANK,
    
    # Decision outcome
    decision = case_when(
      grepl("exploit", OUTCOME) ~ "exploit",
      grepl("explore", OUTCOME) ~ "explore", 
      grepl("stop", OUTCOME) | grepl("none", OUTCOME) ~ "stop"
    ),
    
    # Binary exploit choice
    exploit_choice = ifelse(grepl("exploit", OUTCOME),1, 0),
    
    # Partner information
    partner = PAIRED_WITH,
    
    # Expected explore value
    expected_explore = as.numeric(expected_explore),
    
    # Subjective chosen value
    subjective_chosen = as.numeric(SUBJECTIVE_CHOSEN_VALUE)
  ) %>%
  # Remove missing data
  filter(!is.na(decision), !is.na(rank_position)) %>%
  # Create unique trial ID
  mutate(trial_id = paste(BLOCK_No, TRIAL_NUM, monkey, sep = "_"))

# =============================================================================
# DESCRIPTIVE ANALYSIS
# =============================================================================

# Summary statistics by rank and condition
summary_stats <- clean_data %>%
  group_by(monkey, rank_position, social_condition) %>%
  summarise(
    n_trials = n(),
    exploit_rate = mean(exploit_choice, na.rm = TRUE),
    explore_rate = mean(decision == "explore",na.rm = TRUE),
    stop_rate = mean(decision == "stop",na.rm = TRUE),
    avg_expected_explore = mean(expected_explore, na.rm = TRUE),
    .groups = "drop"
  )

cat("=== SUMMARY STATISTICS ===\n")
print(summary_stats)

# =============================================================================
# THEORY OF MIND ANALYSIS
# =============================================================================

# Create theory of mind variables for social conditions
tom_data <- clean_data %>%
  filter(social_condition != "solo") %>%
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
    
    # Theory of mind context
    tom_context = case_when(
      rank_diff < 0 ~ "subordinate_to_partner",
      rank_diff > 0 ~ "dominant_to_partner", 
      rank_diff == 0 ~ "same_rank"
    ),
    
    # Is subordinate to partner (key ToM test)
    is_subordinate = rank_diff < 0,
    
    # Is dominant to partner
    is_dominant = rank_diff > 0
  )

# =============================================================================
# HYPOTHESIS TESTING
# =============================================================================

# Hypothesis 1: Subordinates show different behavior than dominants
subordinate_vs_dominant <- tom_data %>%
  group_by(monkey, tom_context) %>%
  summarise(
    exploit_rate = mean(exploit_choice, na.rm = TRUE),
    n_trials = n(),
    .groups = "drop"
  ) %>%
  filter(n_trials >= 3)

cat("\n=== SUBORDINATE VS DOMINANT BEHAVIOR ===\n")
print(subordinate_vs_dominant)

# Hypothesis 2: Rank-specific theory of mind effects
rank_tom_effects <- tom_data %>%
  group_by(rank_position, tom_context) %>%
  summarise(
    exploit_rate = mean(exploit_choice, na.rm = TRUE),
    n_trials = n(),
    .groups = "drop"
  ) %>%
  filter(n_trials >= 5)

cat("\n=== RANK-SPECIFIC THEORY OF MIND EFFECTS ===\n")
print(rank_tom_effects)

# =============================================================================
# STATISTICAL TESTS
# =============================================================================

# Test 1: Do subordinates behave differently than dominants?
subordinate_test <- tom_data %>%
  filter(tom_context %in% c("subordinate_to_partner", "dominant_to_partner")) %>%
  group_by(tom_context) %>%
  summarise(
    mean_exploit = mean(exploit_choice, na.rm = TRUE),
    n_trials = n(),
    .groups = "drop"
  )

cat("\n=== SUBORDINATE VS DOMINANT BEHAVIOR TEST ===\n")
print(subordinate_test)

# Test 2: Do intermediates show context-dependent behavior?
intermediate_test <- tom_data %>%
  filter(rank_position == 2) %>%
  group_by(tom_context) %>%
  summarise(
    mean_exploit = mean(exploit_choice, na.rm = TRUE),
    n_trials = n(),
    .groups = "drop"
  )

cat("\n=== INTERMEDIATE MONKEY CONTEXT-DEPENDENT BEHAVIOR ===\n")
print(intermediate_test)

# Test3 Overall theory of mind effect
overall_tom_test <- tom_data %>%
  group_by(tom_context) %>%
  summarise(
    mean_exploit = mean(exploit_choice, na.rm = TRUE),
    n_trials = n(),
    .groups = "drop"
  )

cat("\n=== OVERALL THEORY OF MIND EFFECTS ===\n")
print(overall_tom_test)

# =============================================================================
# VISUALIZATIONS
# =============================================================================

# Plot 1: Theory of Mind Effects by Rank
p1 <- ggplot(rank_tom_effects, 
             aes(x = factor(rank_position), y = exploit_rate, 
                 color = tom_context, shape = tom_context)) +
  geom_point(size = 4, alpha = 0.8) +
  geom_line(aes(group = tom_context), alpha = 0.6) +
  scale_color_viridis_d(name = "ToM Context") +
  scale_shape_discrete(name = "ToM Context") +
  labs(title = "Theory of Mind Effects by Rank Position",
       subtitle = "Exploit rate across different social contexts",
       x = "Rank Position (1=Dominant, 2=Intermediate, 3=Ordinate)",
       y = "Exploit Rate") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Save plot 1
ggsave("tom_effects_by_rank.png", p1, width = 10, height = 6, dpi = 300)

# Plot 2: Individual Differences
individual_plot <- ggplot(subordinate_vs_dominant, 
                         aes(x = tom_context, y = exploit_rate, 
                             fill = monkey)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  scale_fill_viridis_d(name = "Monkey") +
  labs(title = "Individual Differences in Theory of Mind",
       subtitle = "Exploit rates by social context",
       x = "Theory of Mind Context",
       y = "Exploit Rate") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Save plot 2
ggsave("individual_tom_differences.png", individual_plot, width = 10, height = 6, dpi = 300)

# Plot 3: Partner-Specific Theory of Mind Effects
rank_diff_plot <- ggplot(tom_data %>% filter(!is.na(rank_diff)), 
                        aes(x = rank_diff, y = exploit_choice, 
                            color = factor(rank_position))) +
  geom_jitter(width = 0.1, height = 0.05, alpha = 0.6) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), 
              se = TRUE, alpha = 0.3) +
  scale_color_viridis_d(name = "Rank Position") +
  labs(title = "Partner-Specific Theory of Mind Effects",
       subtitle = "Exploit choice as function of rank difference with partner",
       x = "Rank Difference (Negative = Subordinate to Partner)",
       y = "Exploit Choice (0/1)") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Save plot3
ggsave("partner_specific_tom_effects.png", rank_diff_plot, width = 10, height = 6, dpi = 300)

# Plot 4: Social Condition Comparison
social_comparison <- clean_data %>%
  group_by(social_condition, rank_position) %>%
  summarise(
    exploit_rate = mean(exploit_choice, na.rm = TRUE),
    n_trials = n(),
    .groups = "drop"
  ) %>%
  filter(n_trials >= 5) %>%
  ggplot(aes(x = social_condition, y = exploit_rate, 
             fill = factor(rank_position))) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  scale_fill_viridis_d(name = "Rank Position") +
  labs(title = "Social Condition Effects",
       subtitle = "Exploit rates across different social contexts",
       x = "Social Condition",
       y = "Exploit Rate") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Save plot4
ggsave("social_condition_effects.png", social_comparison, width = 10, height = 6, dpi = 300)

# =============================================================================
# KEY FINDINGS SUMMARY
# =============================================================================

cat("\n=== KEY FINDINGS SUMMARY ===\n")

# Calculate key statistics
subordinate_effect <- tom_data %>%
  filter(tom_context == "subordinate_to_partner") %>%
  summarise(mean_exploit = mean(exploit_choice, na.rm = TRUE)) %>%
  pull(mean_exploit)

dominant_effect <- tom_data %>%
  filter(tom_context == "dominant_to_partner") %>%
  summarise(mean_exploit = mean(exploit_choice, na.rm = TRUE)) %>%
  pull(mean_exploit)

cat("1. Subordinate Theory of Mind Effect:", round(subordinate_effect, 3), "\n")
cat("2. Dominant Theory of Mind Effect:", round(dominant_effect, 3), "\n")
cat("3. Difference (Subordinate - Dominant):", round(subordinate_effect - dominant_effect, 3), "\n")

# Check if subordinates show different behavior
if(abs(subordinate_effect - dominant_effect) > 0.1) {
  cat("4. THEORY OF MIND EVIDENCE: Subordinates show different behavior than dominants\n")
} else {
  cat("4. NO THEORY OF MIND EVIDENCE: Similar behavior across rank positions\n")
}

# Individual differences
individual_variation <- subordinate_vs_dominant %>%
  group_by(tom_context) %>%
  summarise(sd_rate = sd(exploit_rate, na.rm = TRUE)) %>%
  summarise(avg_sd = mean(sd_rate, na.rm = TRUE)) %>%
  pull(avg_sd)

cat("5. Individual Variation (SD):", round(individual_variation, 3), "\n")

if(individual_variation > 0.1) {
  cat("6. INDIVIDUAL DIFFERENCES: Monkeys show varying theory of mind sensitivity\n")
} else {
  cat("6. CONSISTENT BEHAVIOR: Monkeys show similar theory of mind patterns\n")
}

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Check the generated PNG files for visualizations:\n")
cat("- tom_effects_by_rank.png\n")
cat("- individual_tom_differences.png\n")
cat("- partner_specific_tom_effects.png\n")
cat("- social_condition_effects.png\n") 