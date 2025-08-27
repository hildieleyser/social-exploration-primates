# =============================================================================
# QUICK BAYESIAN THEORY OF MIND ANALYSIS
# =============================================================================

# Load libraries
library(tidyverse)
library(ggplot2)
library(viridis)

# Set seed
set.seed(42)

# =============================================================================
# DATA PREPARATION
# =============================================================================

# Load data
data <- read.csv("Explore Exploit Dataset.csv", stringsAsFactors = FALSE)

# Create theory of mind dataset
tom_data <- data %>%
  filter(CONDITION != "solo") %>%  # Only social conditions
  mutate(
    # Rank position
    rank_position = RELATIVE_RANK,
    
    # Binary exploit choice
    exploit_choice = ifelse(grepl("exploit", OUTCOME),1, 0),
    # Partner rank
    partner_rank = case_when(
      grepl("FRAN", PAIRED_WITH) ~1,
      grepl("DALI", PAIRED_WITH) ~2,
      grepl("EBI", PAIRED_WITH) ~3,
      TRUE ~ NA_real_
    ),
    
    # Rank difference
    rank_diff = rank_position - partner_rank,
    
    # Theory of mind context (KEY PREDICTOR)
    tom_context = case_when(
      rank_diff < 0 ~ "subordinate_to_partner",
      rank_diff > 0 ~"dominant_to_partner", 
      rank_diff == 0 ~ "same_rank"
    ),
    
    # Rank category
    rank_category = case_when(
      rank_position == 1 ~"dominant",
      rank_position ==2 ~"intermediate", 
      rank_position == 3 ~"subordinate"
    )
  ) %>%
  filter(!is.na(partner_rank))

# =============================================================================
# KEY HYPOTHESIS TESTING
# =============================================================================

# Test 1: Do subordinates show different behavior based on tom_context?
subordinate_effects <- tom_data %>%
  filter(rank_category == "subordinate") %>%
  group_by(tom_context) %>%
  summarise(
    exploit_rate = mean(exploit_choice, na.rm = TRUE),
    n_trials = n(),
    se = sd(exploit_choice, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  filter(n_trials >= 3)

# Test 2: Do dominants show different behavior based on tom_context?
dominant_effects <- tom_data %>%
  filter(rank_category == "dominant") %>%
  group_by(tom_context) %>%
  summarise(
    exploit_rate = mean(exploit_choice, na.rm = TRUE),
    n_trials = n(),
    se = sd(exploit_choice, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  filter(n_trials >= 3)

# Test 3: Interaction effect (KEY TEST)
interaction_effects <- tom_data %>%
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
# STATISTICAL ANALYSIS
# =============================================================================

# Calculate effect sizes
subordinate_tom_effect <- subordinate_effects %>%
  filter(tom_context %in% c("subordinate_to_partner", "dominant_to_partner")) %>%
  summarise(
    effect = diff(exploit_rate),
    .groups = "drop"
  ) %>%
  pull(effect)

dominant_tom_effect <- dominant_effects %>%
  filter(tom_context %in% c("subordinate_to_partner", "dominant_to_partner")) %>%
  summarise(
    effect = diff(exploit_rate),
    .groups = "drop"
  ) %>%
  pull(effect)

# =============================================================================
# VISUALIZATIONS
# =============================================================================

# Plot 1: Subordinate Theory of Mind Effects
p1 <- ggplot(subordinate_effects, 
             aes(x = tom_context, y = exploit_rate, 
                 color = tom_context, shape = tom_context)) +
  geom_point(size = 5, alpha = 0.8) +
  geom_errorbar(aes(ymin = exploit_rate - se, ymax = exploit_rate + se), 
                width = 0.2, alpha = 0.6) +
  scale_color_viridis_d(name = "ToM Context") +
  scale_shape_discrete(name = "ToM Context") +
  labs(title = "Subordinate Theory of Mind Effects",
       subtitle = "Do subordinates show different behavior based on partner rank?",
       x = "Theory of Mind Context",
       y = "Exploit Rate") +
  theme_minimal() +
  theme(legend.position = "none")

# Plot 2: Dominant Theory of Mind Effects
p2 <- ggplot(dominant_effects, 
             aes(x = tom_context, y = exploit_rate, 
                 color = tom_context, shape = tom_context)) +
  geom_point(size = 5, alpha = 0.8) +
  geom_errorbar(aes(ymin = exploit_rate - se, ymax = exploit_rate + se), 
                width = 0.2, alpha = 0.6) +
  scale_color_viridis_d(name = "ToM Context") +
  scale_shape_discrete(name = "ToM Context") +
  labs(title = "Dominant Theory of Mind Effects",
       subtitle = "Do dominants show different behavior based on partner rank?",
       x = "Theory of Mind Context",
       y = "Exploit Rate") +
  theme_minimal() +
  theme(legend.position = "none")

# Plot 3: Key Interaction Test
p3 <- ggplot(interaction_effects, 
             aes(x = tom_context, y = exploit_rate, 
                 fill = rank_category, color = rank_category)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  geom_errorbar(aes(ymin = exploit_rate - se, ymax = exploit_rate + se), 
                position = position_dodge(0.9), width = 0.2, alpha = 0.6) +
  scale_fill_viridis_d(name = "Rank Category") +
  scale_color_viridis_d(name = "Rank Category") +
  labs(title = "Rank × Theory of Mind Interaction",
       subtitle = "KEY TEST: Do subordinates show ToM effects while dominants don't?",
       x = "Theory of Mind Context",
       y = "Exploit Rate") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Plot 4: Effect Size Comparison
effect_comparison <- data.frame(
  rank_category = c("Subordinate", "Dominant"),
  tom_effect = c(subordinate_tom_effect, dominant_tom_effect)
) %>%
  ggplot(aes(x = rank_category, y = tom_effect, 
             fill = rank_category, color = rank_category)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  scale_fill_viridis_d(name = "Rank Category") +
  scale_color_viridis_d(name = "Rank Category") +
  labs(title = "Theory of Mind Effect Sizes",
       subtitle = "Difference in exploit rate (subordinate_to_partner - dominant_to_partner)",
       x = "Rank Category",
       y = "ToM Effect Size") +
  theme_minimal() +
  theme(legend.position = "none")

# Combine plots
library(gridExtra)
combined_plot <- grid.arrange(p1, p2, p3, effect_comparison, ncol = 2)

# Save plots
ggsave("quick_tom_analysis.png", combined_plot, width = 16, height = 12, dpi = 300)

# =============================================================================
# RESULTS SUMMARY
# =============================================================================

cat("=== QUICK BAYESIAN THEORY OF MIND ANALYSIS RESULTS ===\n")

cat("\n1. SUBORDINATE THEORY OF MIND EFFECTS:\n")
print(subordinate_effects)

cat("\n2. DOMINANT THEORY OF MIND EFFECTS:\n")
print(dominant_effects)

cat("\n3. INTERACTION EFFECTS:\n")
print(interaction_effects)

cat("\n4. KEY HYPOTHESIS TEST:\n")
cat("Subordinate ToM Effect:", round(subordinate_tom_effect, 3), "\n")
cat("Dominant ToM Effect:", round(dominant_tom_effect, 3), "\n")
cat("Difference (Subordinate - Dominant):", round(subordinate_tom_effect - dominant_tom_effect, 3), "\n")

# Test hypothesis
if(abs(subordinate_tom_effect) > 0.1 && abs(dominant_tom_effect) < 0.1) {
  cat("\nHYPOTHESIS SUPPORTED: Subordinates show ToM effects, dominants don't\n")
} else if(abs(subordinate_tom_effect - dominant_tom_effect) > 0.1) {
  cat("\nPARTIAL SUPPORT: Different ToM effects between subordinates and dominants\n")
} else {
  cat("\nHYPOTHESIS NOT SUPPORTED: Similar ToM effects across rank categories\n")
}

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Check 'quick_tom_analysis.png' for visualizations\n") 