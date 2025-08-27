# =============================================================================
# THEORY OF MIND BAYESIAN ANALYSIS FOR MONKEY EXPLORE-EXPLOIT DATA
# =============================================================================
# 
# This script implements a comprehensive Bayesian hierarchical model to test
# theory of mind capabilities in monkeys during social explore-exploit decisions.
#
# HYPOTHESIS: Subordinate monkeys will show theory of mind by considering
# dominant monkeys' preferences, while dominants will not consider subordinates.
# Intermediates will show this pattern depending on their relative position.
#
# Author: AI Assistant
# Date: 2024
# =============================================================================

# Load required libraries
library(tidyverse)
library(brms)
library(bayesplot)
library(posterior)
library(cmdstanr)
library(patchwork)
library(viridis)
library(ggdist)
library(performance)

# Set seed for reproducibility
set.seed(42)

# =============================================================================
# DATA LOADING AND PREPROCESSING
# =============================================================================

# Load the dataset
data <- read.csv("Explore Exploit Dataset.csv", stringsAsFactors = FALSE)

# Clean and prepare data
data_clean <- data %>%
  # Convert date to proper format
  mutate(date = as.Date(date, format = %d/%m/%Y)) %>%
  # Create unique trial identifier
  mutate(trial_id = paste(BLOCK_No, TRIAL_NUM, monkey, sep = "_)) %>%
  # Create social context variables
  mutate(
    # Social condition (solo = 0, duo = 1, trio = 2)
    social_context = case_when(
      CONDITION ==solo" ~ 0      CONDITION == "duo" ~ 1      CONDITION == trio ~ 2
    ),
    # Rank position (1=dominant, 2ntermediate, 3ordinate)
    rank_position = RELATIVE_RANK,
    # Absolute rank for reference
    absolute_rank = ABSOLUTE_RANK,
    # Create binary outcome: exploit (1 vs explore/stop (0)
    exploit_choice = ifelse(grepl(exploit", OUTCOME), 1, 0),
    # Create three-way outcome: exploit (1), explore (2), stop (3
    choice_type = case_when(
      grepl(exploit", OUTCOME) ~ 1,
      grepl(explore", OUTCOME) ~ 2,
      grepl(stop", OUTCOME) | grepl(none", OUTCOME) ~ 3
    ),
    # Partner rank (for social conditions)
    partner_rank = case_when(
      CONDITION ==solo" ~ NA_real_,
      CONDITION ==duo [object Object]       partner_ranks <- sapply(strsplit(PAIRED_WITH,), function(x) {
          if(length(x) == 1) [object Object]            case_when(x == FRAN~ 1, x == DALI" ~2, x == "EBI ~ 3)
          } else [object Object]            mean(case_when(x == FRAN~ 1, x == DALI" ~2 x == "EBI" ~ 3))
          }
        })
        unlist(partner_ranks)
      },
      TRUE ~ NA_real_
    ),
    # Rank difference (negative = subordinate to partner, positive = dominant to partner)
    rank_diff = rank_position - partner_rank,
    # Theory of mind context: when subordinate to dominant
    tom_context = case_when(
      CONDITION ==solo" ~ 0
      rank_diff < 0 ~1  # Subordinate to partner
      rank_diff > 02ominant to partner
      rank_diff == 0 ~ 3 Same rank
    ),
    # Expected value of explore option
    expected_explore = as.numeric(expected_explore),
    # Subjective chosen value
    subjective_chosen = as.numeric(SUBJECTIVE_CHOSEN_VALUE),
    # Exploit value (if available)
    exploit_value = as.numeric(exploit_B),
    # Block identifier
    block_id = BLOCK_No,
    # Trial number within block
    trial_num = TRIAL_NUM
  ) %>%
  # Remove rows with missing data
  filter(!is.na(choice_type), !is.na(rank_position)) %>%
  # Create individual monkey ID
  mutate(monkey_id = as.factor(monkey))

# Create summary statistics
summary_stats <- data_clean %>%
  group_by(monkey, CONDITION, rank_position) %>%
  summarise(
    n_trials = n(),
    exploit_rate = mean(exploit_choice, na.rm = TRUE),
    explore_rate = mean(choice_type == 2na.rm = TRUE),
    stop_rate = mean(choice_type == 3na.rm = TRUE),
    avg_expected_explore = mean(expected_explore, na.rm = TRUE),
    .groups =drop
  )

print("Data Summary:")
print(summary_stats)

# =============================================================================
# THEORY OF MIND BAYESIAN HIERARCHICAL MODEL
# =============================================================================

# Model 1sic Theory of Mind Model
# Tests if monkeys consider partner's rank when making decisions

tom_model_1brm(
  choice_type ~ 1+ social_context + rank_position + tom_context + 
                expected_explore + subjective_chosen + 
                (1+ social_context + tom_context | monkey_id),
  data = data_clean,
  family = categorical(),
  prior = c(
    prior(normal(0, 1 class =b,
    prior(normal(01lass = sd,group = monkey_id)
  ),
  chains = 4,
  iter =2000
  warmup =100 cores = 4,
  file = tom_model_1"
)

# Model 2: Interaction Model
# Tests if theory of mind effects depend on rank position

tom_model_2brm(
  choice_type ~ 1+ social_context * rank_position + tom_context * rank_position + 
                expected_explore + subjective_chosen + 
                (1+ social_context + tom_context | monkey_id),
  data = data_clean,
  family = categorical(),
  prior = c(
    prior(normal(0, 1 class =b,
    prior(normal(01lass = sd,group = monkey_id)
  ),
  chains = 4,
  iter =2000
  warmup =100 cores = 4,
  file = tom_model_2"
)

# Model 3: Partner-Specific Theory of Mind
# Tests if monkeys consider specific partner preferences

tom_model_3brm(
  choice_type ~ 1+ social_context + rank_position + tom_context + 
                rank_diff + expected_explore + subjective_chosen + 
                (1+ social_context + tom_context + rank_diff | monkey_id),
  data = data_clean %>% filter(!is.na(rank_diff)),
  family = categorical(),
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

# Compare models
model_comparison <- loo_compare(
  loo(tom_model_1),
  loo(tom_model_2),
  loo(tom_model_3("Model Comparison (LOO-CV):)print(model_comparison)

# Model diagnostics
plot(tom_model_1)
plot(tom_model_2)
plot(tom_model_3)

# =============================================================================
# POSTERIOR ANALYSIS AND VISUALIZATION
# =============================================================================

# Extract posterior samples from best model (assuming model_2)
posterior_samples <- posterior_samples(tom_model_2)

# Create theory of mind effect plots
tom_effects <- data_clean %>%
  group_by(monkey, rank_position, tom_context, CONDITION) %>%
  summarise(
    exploit_rate = mean(exploit_choice, na.rm = TRUE),
    n_trials = n(),
    .groups = 'drop  ) %>%
  filter(n_trials >= 5include conditions with sufficient data

# Plot1ry of Mind Effects by Rank
p1 <- ggplot(tom_effects, aes(x = factor(rank_position), y = exploit_rate, 
                              color = factor(tom_context), shape = CONDITION)) +
  geom_point(size = 3, alpha = 08 +
  geom_line(aes(group = interaction(monkey, tom_context)), alpha = 0.5 +
  scale_color_viridis_d(name = "ToM Context", 
                        labels = c("Solo", "Subordinate",Dominant",Same Rank")) +
  scale_shape_discrete(name =Social Condition") +
  labs(title = "Theory of Mind Effects by Rank Position,
       subtitle = "Exploit rate across different social contexts",
       x = Rank Position (1=Dominant, 2ntermediate,3ordinate)",
       y = Exploit Rate") +
  theme_minimal() +
  theme(legend.position = bottom")

# Plot 2: Partner-Specific Effects
partner_effects <- data_clean %>%
  filter(!is.na(rank_diff)) %>%
  group_by(monkey, rank_diff, CONDITION) %>%
  summarise(
    exploit_rate = mean(exploit_choice, na.rm = TRUE),
    n_trials = n(),
    .groups = 'drop  ) %>%
  filter(n_trials >=3 <- ggplot(partner_effects, aes(x = rank_diff, y = exploit_rate, 
                                  color = monkey, shape = CONDITION)) +
  geom_point(size = 3, alpha = 0.8 +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.3 +
  scale_color_viridis_d(name = Monkey") +
  labs(title =Partner-Specific Theory of Mind Effects,
       subtitle = "Exploit rate as function of rank difference with partner",
       x = Rank Difference (Negative = Subordinate to Partner)",
       y = Exploit Rate") +
  theme_minimal() +
  theme(legend.position = bottom")

# Plot 3: Posterior Predictive Checks
pp_check_plot <- pp_check(tom_model_2, type = "bars") +
  labs(title = "Posterior Predictive Check,
       subtitle = "Model fit to observed choice distributions")

# Plot 4: Individual Differences
individual_effects <- data_clean %>%
  group_by(monkey, rank_position, CONDITION) %>%
  summarise(
    exploit_rate = mean(exploit_choice, na.rm = TRUE),
    explore_rate = mean(choice_type == 2na.rm = TRUE),
    stop_rate = mean(choice_type == 3na.rm = TRUE),
    n_trials = n(),
    .groups = 'drop  ) %>%
  filter(n_trials >=5 ggplot(individual_effects, aes(x = factor(rank_position), y = exploit_rate, 
                                     fill = monkey)) +
  geom_bar(stat = "identity", position = dodge, alpha = 00.8) +
  facet_wrap(~CONDITION) +
  scale_fill_viridis_d(name = Monkey") +
  labs(title = "Individual Differences in Theory of Mind,
       subtitle =Exploit rates by rank position and social condition",
       x =Rank Position",
       y = Exploit Rate") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Combine plots
combined_plot <- (p1 + p2) / (pp_check_plot + p4) +
  plot_annotation(
    title = "Theory of Mind Bayesian Analysis Results",
    subtitle = "Comprehensive analysis of social decision-making in monkeys",
    theme = theme(plot.title = element_text(size = 16, face = "bold"))
  )

# Save plots
ggsave("theory_of_mind_analysis.png", combined_plot, width = 16, height =12 dpi = 30
ggsave("theory_of_mind_analysis.pdf", combined_plot, width = 16 height = 12)

# =============================================================================
# HYPOTHESIS TESTING
# =============================================================================

# Test specific hypotheses using posterior samples

# Hypothesis 1: Subordinates show stronger ToM effects than dominants
subordinate_tom <- data_clean %>%
  filter(rank_position == 3CONDITION != "solo)%>%
  group_by(monkey, tom_context) %>%
  summarise(exploit_rate = mean(exploit_choice, na.rm = TRUE), .groups =drop')

dominant_tom <- data_clean %>%
  filter(rank_position == 1CONDITION != "solo)%>%
  group_by(monkey, tom_context) %>%
  summarise(exploit_rate = mean(exploit_choice, na.rm = TRUE), .groups = 'drop)

# Hypothesis 2: Intermediates show context-dependent ToM
intermediate_tom <- data_clean %>%
  filter(rank_position == 2CONDITION != "solo)%>%
  group_by(monkey, tom_context) %>%
  summarise(exploit_rate = mean(exploit_choice, na.rm = TRUE), .groups =drop Create hypothesis test results
hypothesis_results <- list(
  subordinate_effects = subordinate_tom,
  dominant_effects = dominant_tom,
  intermediate_effects = intermediate_tom
)

# =============================================================================
# STATISTICAL SUMMARIES
# =============================================================================

# Model summaries
model_summaries <- list(
  model_1 = summary(tom_model_1),
  model_2 = summary(tom_model_2),
  model_3 = summary(tom_model_3)
)

# Effect sizes and credible intervals
effect_sizes <- posterior_summary(tom_model_2) %>%
  as.data.frame() %>%
  rownames_to_column(parameter") %>%
  filter(grepl("tom_context", parameter) | grepl("rank_position", parameter))

# =============================================================================
# SAVE RESULTS
# =============================================================================

# Save all results
saveRDS(list(
  models = list(tom_model_1, tom_model_2_model_3  model_comparison = model_comparison,
  hypothesis_results = hypothesis_results,
  model_summaries = model_summaries,
  effect_sizes = effect_sizes,
  data_clean = data_clean
), "theory_of_mind_results.rds")

# Create comprehensive report
cat(n=== THEORY OF MIND BAYESIAN ANALYSIS RESULTS ===\n)
cat("Best model (by LOO-CV):", rownames(model_comparison)[1], "\n")
cat("Model comparison:\n)print(model_comparison)

cat(nKey findings:\n")
cat(1. Theory of mind effects by rank position\n)
cat(. Partner-specific decision making\n")
cat("3. Individual differences in social cognition\n)
cat( Context-dependent theory of mind\n")

cat("\nAnalysis complete! Check 'theory_of_mind_analysis.png' for visualizations.\n") 