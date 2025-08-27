# =============================================================================
# SIMPLE THEORY OF MIND ANALYSIS FOR MONKEY DATA
# =============================================================================
# 
# This script provides a focused analysis of theory of mind capabilities
# in monkeys during social explore-exploit decisions.
#
# HYPOTHESIS: 
# 1. Subordinate monkeys will consider dominant monkeys preferences
# 2. Dominant monkeys will not consider subordinate preferences  
# 3. Intermediates will show context-dependent theory of mind
#
# =============================================================================

# Load libraries
library(tidyverse)
library(ggplot2)
library(viridis)
library(patchwork)
library(broom)
library(lme4)
library(lmerTest)

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

print("Summary Statistics:")
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
      grepl("EBI", partner) ~ 3,    TRUE ~ NA_real_
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

print("Subordinate vs Dominant Behavior:")
print(subordinate_vs_dominant)

# Hypothesis 2: Rank-specific theory of mind effects
rank_tom_effects <- tom_data %>%
  group_by(rank_position, tom_context) %>%
  summarise(
    exploit_rate = mean(exploit_choice, na.rm = TRUE),
    n_trials = n(),
    .groups = "drop"
  ) %>%
  filter(n_trials >=5)

print("Rank-Specific Theory of Mind Effects:")
print(rank_tom_effects)

# =============================================================================
# STATISTICAL MODELS
# =============================================================================

# Model 1: Basic theory of mind effect
model_1 <- glmer(
  exploit_choice ~ tom_context + rank_position + expected_explore + 
                  (1 | monkey),
  data = tom_data,
  family = binomial
)

print("Model 1 - Basic Theory of Mind:")
print(summary(model_1))

# Model 2: Interaction between rank and ToM context
model_2 <- glmer(
  exploit_choice ~ tom_context * rank_position + expected_explore + 
                  (1 | monkey),
  data = tom_data,
  family = binomial
)

print("Model 2 - Rank × ToM Interaction:")
print(summary(model_2))

# Model 3: Partner-specific effects
model_3 <- glmer(
  exploit_choice ~ rank_diff + rank_position + expected_explore + 
                  (1 | monkey),
  data = tom_data %>% filter(!is.na(rank_diff)),
  family = binomial
)

print("Model 3 - Partner-Specific Effects:")
print(summary(model_3))

# =============================================================================
# VISUALIZATIONS
# =============================================================================

# Plot1: Theory of Mind Effects by Rank
p1<- ggplot(rank_tom_effects, 
             aes(x = factor(rank_position), y = exploit_rate, 
                 color = tom_context, shape = tom_context)) +
  geom_point(size = 4, alpha = 0.8) +
  geom_line(aes(group = tom_context), alpha = 0.6) +
  scale_color_viridis_d(name = "ToM Context") +
  scale_shape_discrete(name = "ToM Context") +
  labs(title = "Theory of Mind Effects by Rank Position",
       subtitle = "Exploit rate across different social contexts",
       x = "Rank Position (1=Dominant, 2=Intermediate,3=Ordinate)",
       y = "Exploit Rate") +
  theme_minimal() +
  theme(legend.position = "bottom")

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

# Plot3: Partner-Specific Theory of Mind Effects
rank_diff_plot <- ggplot(tom_data %>% filter(!is.na(rank_diff)), 
                        aes(x = rank_diff, y = exploit_choice, 
                            color = factor(rank_position))) +
  geom_jitter(width = 0.1, height = 0.5, alpha = 0.6) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), 
              se = TRUE, alpha = 0.3) +
  scale_color_viridis_d(name = "Rank Position") +
  labs(title = "Partner-Specific Theory of Mind Effects",
       subtitle = "Exploit choice as function of rank difference with partner",
       x = "Rank Difference (Negative = Subordinate to Partner)",
       y = "Exploit Choice (0/1s)") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Plot 4: Social Condition Comparison
social_comparison <- clean_data %>%
  group_by(social_condition, rank_position) %>%
  summarise(
    exploit_rate = mean(exploit_choice, na.rm = TRUE),
    n_trials = n(),
    .groups = "drop"
  ) %>%
  filter(n_trials >=5) %>%
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

# Combine plots
combined_plot <- (p1 + individual_plot) / (rank_diff_plot + social_comparison) +
  plot_annotation(
    title = "Theory of Mind Analysis Results",
    subtitle = "Comprehensive analysis of social decision-making in monkeys",
    theme = theme(plot.title = element_text(size = 16, face = "bold"))
  )

# Save plots
ggsave("theory_of_mind_simple_analysis.png", combined_plot, 
       width = 16, height = 12, dpi = 300)
ggsave("theory_of_mind_simple_analysis.pdf", combined_plot, 
       width = 16, height = 12)

# =============================================================================
# HYPOTHESIS TESTING RESULTS
# =============================================================================

# Test specific hypotheses
hypothesis_tests <- list()

# Test 1: Do subordinates behave differently than dominants?
subordinate_test <- tom_data %>%
  filter(tom_context %in% c("subordinate_to_partner", "dominant_to_partner")) %>%
  group_by(tom_context) %>%
  summarise(
    mean_exploit = mean(exploit_choice, na.rm = TRUE),
    n_trials = n(),
    .groups = "drop"
  )

print("Subordinate vs Dominant Behavior Test:")
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

print("Intermediate Monkey Context-Dependent Behavior:")
print(intermediate_test)

# Test3: Overall theory of mind effect
overall_tom_test <- tom_data %>%
  group_by(tom_context) %>%
  summarise(
    mean_exploit = mean(exploit_choice, na.rm = TRUE),
    n_trials = n(),
    .groups = "drop"
  )

print("Overall Theory of Mind Effects:")
print(overall_tom_test)

# =============================================================================
# SAVE RESULTS
# =============================================================================

# Create results summary
results_summary <- list(
  summary_stats = summary_stats,
  subordinate_vs_dominant = subordinate_vs_dominant,
  rank_tom_effects = rank_tom_effects,
  models = list(model_1 = model_1, model_2 = model_2, model_3 = model_3),
  hypothesis_tests = list(
    subordinate_test = subordinate_test,
    intermediate_test = intermediate_test,
    overall_tom_test = overall_tom_test
  )
)

# Save results
saveRDS(results_summary, "theory_of_mind_simple_results.rds")

# Print final summary
cat("\n=== THEORY OF MIND ANALYSIS COMPLETE ===\n")
cat("Key findings:\n")
cat("1. Theory of mind effects by rank position\n")
cat("2. Individual differences in social cognition\n")
cat("3. Partner-specific decision making\n")
cat("4. Context-dependent behavior patterns\n")
cat("\nCheck 'theory_of_mind_simple_analysis.png' for visualizations.\n") 