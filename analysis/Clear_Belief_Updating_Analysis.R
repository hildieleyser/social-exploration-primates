# =============================================================================
# CLEAR BELIEF UPDATING ANALYSIS: How monkeys learn about others' choices
# =============================================================================
# 
# Scientific Question: Do monkeys track and learn from other monkeys' decisions?
# 
# Approach: 
# 1. Model each monkey's belief about others' exploration probability
# 2. Test if monkeys adjust their own behavior based on observed social information
# 3. Compare learning rates and social sensitivity across individuals
#
# =============================================================================

library(dplyr)

cat("=============================================================================\n")
cat("BELIEF UPDATING ABOUT OTHER MONKEYS' DECISIONS\n")
cat("=============================================================================\n\n")

# Load and clean data
data_raw <- read.csv("data/Explore Exploit Dataset.csv", stringsAsFactors = FALSE)
data_clean <- data_raw[data_raw$TRIAL_TYPE == "OIT_RE", ]

# Clean outcomes
outcome_clean <- character(nrow(data_clean))
outcome_clean[grepl("explore", tolower(data_clean$OUTCOME))] <- "explore"
outcome_clean[grepl("exploit", tolower(data_clean$OUTCOME))] <- "exploit"  
outcome_clean[grepl("none|stop|NONE", tolower(data_clean$OUTCOME))] <- "none"

data_clean$outcome_clean <- outcome_clean
data_clean <- data_clean[data_clean$outcome_clean != "" & !is.na(data_clean$outcome_clean), ]

# Focus on social trials where belief updating can occur
social_data <- data_clean %>% 
  filter(CONDITION %in% c("duo", "trio")) %>%
  arrange(monkey, date, BLOCK_No, TRIAL_NUM)

cat("SCIENTIFIC APPROACH:\n")
cat("===================\n")
cat("1. Model: Each monkey maintains beliefs about others' exploration probability\n")
cat("2. Learning: Beliefs update based on observed social information\n")
cat("3. Behavior: Own choices influenced by beliefs about others\n\n")

# =============================================================================
# ANALYSIS 1: SOCIAL INFORMATION TRACKING
# =============================================================================

cat("ANALYSIS 1: SOCIAL INFORMATION TRACKING\n")
cat("=======================================\n\n")

# For each monkey in social contexts, model their belief about others
belief_analysis <- list()

for(monkey_name in unique(social_data$monkey)) {
  monkey_trials <- social_data %>% filter(monkey == monkey_name)
  
  if(nrow(monkey_trials) < 10) next  # Need sufficient data
  
  # Initialize belief about others' exploration rate
  initial_belief <- 0.3  # Prior belief that others explore 30% of time
  belief_trajectory <- numeric(nrow(monkey_trials))
  belief_trajectory[1] <- initial_belief
  
  # Learning parameters
  learning_rate <- 0.1  # How fast beliefs update
  
  # Track belief updating trial by trial
  for(i in 2:nrow(monkey_trials)) {
    current_trial <- monkey_trials[i, ]
    
    # In social contexts, monkey observes information about others
    # (This is a simplified model - in reality they might observe actual others' choices)
    
    # Social information: overall exploration rate in this session/block
    session_data <- social_data %>% 
      filter(date == current_trial$date, 
             BLOCK_No == current_trial$BLOCK_No,
             monkey != monkey_name)
    
    if(nrow(session_data) > 0) {
      # Observed exploration rate of others in this context
      observed_exploration <- mean(session_data$outcome_clean == "explore", na.rm = TRUE)
      
      # Update belief using weighted average (Bayesian-like updating)
      belief_trajectory[i] <- (1 - learning_rate) * belief_trajectory[i-1] + 
                             learning_rate * observed_exploration
    } else {
      # No social information, belief decays slightly toward prior
      belief_trajectory[i] <- 0.95 * belief_trajectory[i-1] + 0.05 * initial_belief
    }
  }
  
  monkey_trials$belief_about_others <- belief_trajectory
  monkey_trials$trial_number <- 1:nrow(monkey_trials)
  
  belief_analysis[[monkey_name]] <- monkey_trials
}

# Combine all belief data
all_beliefs <- do.call(rbind, belief_analysis)

cat("Belief updating model fitted for", length(belief_analysis), "monkeys\n")
cat("Average final belief about others' exploration:", 
    round(mean(sapply(belief_analysis, function(x) tail(x$belief_about_others, 1)), na.rm = TRUE) * 100, 1), "%\n\n")

# =============================================================================
# ANALYSIS 2: SOCIAL INFLUENCE ON BEHAVIOR
# =============================================================================

cat("ANALYSIS 2: SOCIAL INFLUENCE ON BEHAVIOR\n")
cat("========================================\n\n")

# Test if monkeys' own exploration is influenced by their beliefs about others
influence_results <- list()

for(monkey_name in names(belief_analysis)) {
  monkey_data <- belief_analysis[[monkey_name]]
  
  # Exclude 'none' responses for cleaner analysis
  choice_data <- monkey_data %>% filter(outcome_clean %in% c("explore", "exploit"))
  
  if(nrow(choice_data) < 20) next
  
  # Logistic regression: own exploration ~ belief about others + controls
  model <- tryCatch({
    glm(I(outcome_clean == "explore") ~ belief_about_others + 
        I(CONDITION == "trio") + trial_number,
        data = choice_data, 
        family = binomial)
  }, error = function(e) NULL)
  
  if(!is.null(model)) {
    # Extract key results
    belief_coef <- summary(model)$coefficients["belief_about_others", ]
    
    influence_results[[monkey_name]] <- data.frame(
      monkey = monkey_name,
      belief_coefficient = belief_coef[1],
      belief_se = belief_coef[2],
      belief_pvalue = belief_coef[4],
      social_influence = ifelse(belief_coef[4] < 0.05, "Significant", "Non-significant"),
      influence_direction = ifelse(belief_coef[1] > 0, "Positive", "Negative"),
      stringsAsFactors = FALSE
    )
  }
}

influence_summary <- do.call(rbind, influence_results)

cat("SOCIAL INFLUENCE RESULTS:\n")
print(influence_summary[, c("monkey", "belief_coefficient", "belief_pvalue", "social_influence")])

significant_monkeys <- sum(influence_summary$belief_pvalue < 0.05, na.rm = TRUE)
cat("\nMonkeys showing significant social influence:", significant_monkeys, "out of", nrow(influence_summary), "\n")

positive_influence <- sum(influence_summary$belief_coefficient > 0 & influence_summary$belief_pvalue < 0.05, na.rm = TRUE)
cat("Monkeys with positive social influence (copy others):", positive_influence, "\n")

negative_influence <- sum(influence_summary$belief_coefficient < 0 & influence_summary$belief_pvalue < 0.05, na.rm = TRUE)
cat("Monkeys with negative social influence (contrast others):", negative_influence, "\n\n")

# =============================================================================
# ANALYSIS 3: LEARNING DYNAMICS
# =============================================================================

cat("ANALYSIS 3: LEARNING DYNAMICS\n")
cat("=============================\n\n")

# Analyze how beliefs change over time and across contexts
learning_summary <- list()

for(monkey_name in names(belief_analysis)) {
  monkey_data <- belief_analysis[[monkey_name]]
  
  # Early vs late beliefs
  n_trials <- nrow(monkey_data)
  early_trials <- monkey_data[1:min(20, floor(n_trials/2)), ]
  late_trials <- monkey_data[max(21, ceiling(n_trials/2)):n_trials, ]
  
  early_belief <- mean(early_trials$belief_about_others, na.rm = TRUE)
  late_belief <- mean(late_trials$belief_about_others, na.rm = TRUE)
  belief_change <- late_belief - early_belief
  
  # Context differences
  duo_beliefs <- monkey_data %>% filter(CONDITION == "duo") %>% pull(belief_about_others)
  trio_beliefs <- monkey_data %>% filter(CONDITION == "trio") %>% pull(belief_about_others)
  
  learning_summary[[monkey_name]] <- data.frame(
    monkey = monkey_name,
    early_belief = early_belief,
    late_belief = late_belief,
    belief_change = belief_change,
    duo_belief = mean(duo_beliefs, na.rm = TRUE),
    trio_belief = mean(trio_beliefs, na.rm = TRUE),
    context_sensitivity = abs(mean(trio_beliefs, na.rm = TRUE) - mean(duo_beliefs, na.rm = TRUE)),
    stringsAsFactors = FALSE
  )
}

learning_df <- do.call(rbind, learning_summary)

cat("LEARNING DYNAMICS:\n")
print(learning_df[, c("monkey", "belief_change", "context_sensitivity")])

cat("\nAverage belief change over time:", round(mean(learning_df$belief_change, na.rm = TRUE) * 100, 1), "percentage points\n")
cat("Monkeys with increasing beliefs:", sum(learning_df$belief_change > 0.05, na.rm = TRUE), "\n")
cat("Monkeys with decreasing beliefs:", sum(learning_df$belief_change < -0.05, na.rm = TRUE), "\n")
cat("Average context sensitivity:", round(mean(learning_df$context_sensitivity, na.rm = TRUE) * 100, 1), "percentage points\n\n")

# =============================================================================
# SAVE RESULTS FOR VISUALIZATION
# =============================================================================

# Save key results
save(all_beliefs, influence_summary, learning_df, 
     file = "results/belief_updating_results.RData")

cat("SUMMARY:\n")
cat("========\n")
cat("- Belief updating analysis completed for", nrow(influence_summary), "monkeys\n")
cat("- Social influence detected in", significant_monkeys, "individuals\n")
cat("- Results saved for visualization\n")
cat("- Ready to create publication-quality figures\n\n")

cat("Next: Run visualization script to create clear, interpretable plots\n") 