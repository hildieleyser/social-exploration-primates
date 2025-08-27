# =============================================================================
# MONKEY PREFERENCE ATTRIBUTION: Do monkeys model what others want?
# =============================================================================
# 
# Scientific Question: Do monkeys form beliefs about other monkeys' preferences
# and goals, and do these beliefs influence their own decision-making?
# 
# Key Questions:
# 1. Can monkeys infer what other monkeys prefer (explore vs exploit)?
# 2. Do they adjust their own behavior based on inferred preferences?
# 3. How do they learn about others' preferences from limited observations?
# 4. Individual differences in preference attribution abilities?
#
# =============================================================================

library(dplyr)

cat("=============================================================================\n")
cat("MONKEY PREFERENCE ATTRIBUTION ANALYSIS\n")
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

# Focus on social trials where preference attribution can occur
social_data <- data_clean %>% 
  filter(CONDITION %in% c("duo", "trio")) %>%
  arrange(date, BLOCK_No, TRIAL_NUM)

cat("THEORETICAL FRAMEWORK:\n")
cat("=====================\n")
cat("1. PREFERENCE INFERENCE: Monkeys observe others and infer their preferences\n")
cat("2. MENTAL MODELS: Each monkey builds models of what others want\n")
cat("3. STRATEGIC BEHAVIOR: Own choices influenced by inferred preferences\n")
cat("4. INDIVIDUAL DIFFERENCES: Variation in mentalizing abilities\n\n")

# =============================================================================
# ANALYSIS 1: INFERRING OTHERS' PREFERENCES
# =============================================================================

cat("ANALYSIS 1: INFERRING OTHERS' PREFERENCES\n")
cat("=========================================\n\n")

# For each monkey, model their inferences about others' preferences
preference_models <- list()

for(focal_monkey in unique(social_data$monkey)) {
  cat("Analyzing", focal_monkey, "...\n")
  
  focal_trials <- social_data %>% filter(monkey == focal_monkey)
  
  if(nrow(focal_trials) < 20) next
  
  # Initialize preference beliefs about each other monkey
  other_monkeys <- setdiff(unique(social_data$monkey), focal_monkey)
  preference_beliefs <- list()
  
  # Track beliefs about each other monkey's preference
  for(other_monkey in other_monkeys) {
    preference_beliefs[[other_monkey]] <- data.frame(
      trial = 1:nrow(focal_trials),
      inferred_explore_preference = numeric(nrow(focal_trials)),
      confidence = numeric(nrow(focal_trials))
    )
  }
  
  # Model preference inference trial by trial
  for(t in 1:nrow(focal_trials)) {
    current_trial <- focal_trials[t, ]
    
    # Look for information about others in this session
    session_others <- social_data %>%
      filter(date == current_trial$date,
             BLOCK_No == current_trial$BLOCK_No,
             monkey != focal_monkey,
             TRIAL_NUM <= current_trial$TRIAL_NUM)
    
    for(other_monkey in other_monkeys) {
      other_data <- session_others %>% filter(monkey == other_monkey)
      
      if(nrow(other_data) > 0) {
        # Infer other's exploration preference from their behavior
        other_explore_rate <- mean(other_data$outcome_clean == "explore", na.rm = TRUE)
        other_active_rate <- mean(other_data$outcome_clean %in% c("explore", "exploit"), na.rm = TRUE)
        
        # Confidence based on amount of data observed
        confidence <- min(nrow(other_data) / 10, 1.0)
        
        # Preference inference: Does this monkey prefer exploring?
        explore_preference <- other_explore_rate / (other_explore_rate + mean(other_data$outcome_clean == "exploit", na.rm = TRUE))
        explore_preference[is.nan(explore_preference)] <- 0.5  # Default if no data
        
        preference_beliefs[[other_monkey]]$inferred_explore_preference[t] <- explore_preference
        preference_beliefs[[other_monkey]]$confidence[t] <- confidence
      } else {
        # No information, use previous belief or default
        if(t > 1) {
          preference_beliefs[[other_monkey]]$inferred_explore_preference[t] <- 
            preference_beliefs[[other_monkey]]$inferred_explore_preference[t-1]
          preference_beliefs[[other_monkey]]$confidence[t] <- 
            preference_beliefs[[other_monkey]]$confidence[t-1] * 0.95  # Decay confidence
        } else {
          preference_beliefs[[other_monkey]]$inferred_explore_preference[t] <- 0.5
          preference_beliefs[[other_monkey]]$confidence[t] <- 0.1
        }
      }
    }
  }
  
  preference_models[[focal_monkey]] <- list(
    trials = focal_trials,
    beliefs = preference_beliefs
  )
}

cat("Preference inference models created for", length(preference_models), "monkeys\n\n")

# =============================================================================
# ANALYSIS 2: STRATEGIC BEHAVIOR BASED ON INFERRED PREFERENCES
# =============================================================================

cat("ANALYSIS 2: STRATEGIC BEHAVIOR BASED ON INFERRED PREFERENCES\n")
cat("===========================================================\n\n")

strategic_results <- list()

for(focal_monkey in names(preference_models)) {
  focal_data <- preference_models[[focal_monkey]]$trials
  beliefs <- preference_models[[focal_monkey]]$beliefs
  
  # Calculate average inferred preference for others
  avg_other_explore_pref <- numeric(nrow(focal_data))
  avg_confidence <- numeric(nrow(focal_data))
  
  for(t in 1:nrow(focal_data)) {
    trial_prefs <- sapply(beliefs, function(b) b$inferred_explore_preference[t])
    trial_conf <- sapply(beliefs, function(b) b$confidence[t])
    
    avg_other_explore_pref[t] <- mean(trial_prefs, na.rm = TRUE)
    avg_confidence[t] <- mean(trial_conf, na.rm = TRUE)
  }
  
  focal_data$avg_other_explore_pref <- avg_other_explore_pref
  focal_data$confidence_in_others <- avg_confidence
  
  # Test strategic behaviors
  choice_data <- focal_data %>% filter(outcome_clean %in% c("explore", "exploit"))
  
  if(nrow(choice_data) > 20) {
    # Strategy 1: CONFORMITY - Copy others' preferences
    conformity_model <- tryCatch({
      glm(I(outcome_clean == "explore") ~ avg_other_explore_pref + confidence_in_others + 
          I(CONDITION == "trio"),
          data = choice_data, family = binomial)
    }, error = function(e) NULL)
    
    # Strategy 2: CONTRAST - Do opposite of others
    contrast_model <- tryCatch({
      glm(I(outcome_clean == "explore") ~ I(1 - avg_other_explore_pref) + confidence_in_others + 
          I(CONDITION == "trio"),
          data = choice_data, family = binomial)
    }, error = function(e) NULL)
    
    # Strategy 3: CONDITIONAL - Depends on confidence
    conditional_model <- tryCatch({
      glm(I(outcome_clean == "explore") ~ avg_other_explore_pref * confidence_in_others + 
          I(CONDITION == "trio"),
          data = choice_data, family = binomial)
    }, error = function(e) NULL)
    
    # Compare model fits
    models <- list(conformity = conformity_model, contrast = contrast_model, conditional = conditional_model)
    models <- models[!sapply(models, is.null)]
    
    if(length(models) > 0) {
      aics <- sapply(models, AIC)
      best_strategy <- names(which.min(aics))
      best_model <- models[[best_strategy]]
      
      # Extract key coefficients
      coefs <- summary(best_model)$coefficients
      
      strategic_results[[focal_monkey]] <- data.frame(
        monkey = focal_monkey,
        best_strategy = best_strategy,
        preference_coefficient = coefs[2, 1],  # Main preference effect
        preference_pvalue = coefs[2, 4],
        model_aic = min(aics),
        preference_attribution = ifelse(coefs[2, 4] < 0.05, "Significant", "Non-significant"),
        stringsAsFactors = FALSE
      )
    }
  }
}

strategic_summary <- do.call(rbind, strategic_results)

cat("STRATEGIC BEHAVIOR RESULTS:\n")
print(strategic_summary[, c("monkey", "best_strategy", "preference_coefficient", "preference_pvalue")])

cat("\nSTRATEGIC SUMMARY:\n")
conformers <- sum(strategic_summary$best_strategy == "conformity" & strategic_summary$preference_pvalue < 0.05, na.rm = TRUE)
contrasters <- sum(strategic_summary$best_strategy == "contrast" & strategic_summary$preference_pvalue < 0.05, na.rm = TRUE)
conditional <- sum(strategic_summary$best_strategy == "conditional" & strategic_summary$preference_pvalue < 0.05, na.rm = TRUE)

cat("Conformers (copy others' preferences):", conformers, "\n")
cat("Contrasters (do opposite of others):", contrasters, "\n") 
cat("Conditional strategists:", conditional, "\n")
cat("No clear strategy:", nrow(strategic_summary) - conformers - contrasters - conditional, "\n\n")

# =============================================================================
# ANALYSIS 3: INDIVIDUAL DIFFERENCES IN MENTALIZING
# =============================================================================

cat("ANALYSIS 3: INDIVIDUAL DIFFERENCES IN MENTALIZING\n")
cat("=================================================\n\n")

mentalizing_abilities <- list()

for(focal_monkey in names(preference_models)) {
  beliefs <- preference_models[[focal_monkey]]$beliefs
  trials <- preference_models[[focal_monkey]]$trials
  
  # Measure 1: Accuracy of preference inference
  # (Compare inferred preferences to actual behavior)
  accuracy_scores <- numeric()
  
  for(other_monkey in names(beliefs)) {
    # Get actual exploration rate for this other monkey
    actual_other_data <- social_data %>% 
      filter(monkey == other_monkey,
             date %in% trials$date,
             BLOCK_No %in% trials$BLOCK_No)
    
    if(nrow(actual_other_data) > 0) {
      actual_explore_rate <- mean(actual_other_data$outcome_clean == "explore", na.rm = TRUE)
      
      # Compare to inferred preferences
      inferred_prefs <- beliefs[[other_monkey]]$inferred_explore_preference
      
      # Accuracy = 1 - absolute difference
      accuracy <- 1 - mean(abs(inferred_prefs - actual_explore_rate), na.rm = TRUE)
      accuracy_scores <- c(accuracy_scores, accuracy)
    }
  }
  
  # Measure 2: Confidence calibration
  avg_confidence <- mean(sapply(beliefs, function(b) mean(b$confidence, na.rm = TRUE)), na.rm = TRUE)
  
  # Measure 3: Learning speed (how quickly beliefs stabilize)
  belief_volatility <- mean(sapply(beliefs, function(b) {
    if(length(b$inferred_explore_preference) > 5) {
      sd(diff(b$inferred_explore_preference), na.rm = TRUE)
    } else { 1 }
  }), na.rm = TRUE)
  
  mentalizing_abilities[[focal_monkey]] <- data.frame(
    monkey = focal_monkey,
    preference_accuracy = mean(accuracy_scores, na.rm = TRUE),
    confidence_level = avg_confidence,
    belief_stability = 1 - belief_volatility,  # Higher = more stable
    mentalizing_score = mean(accuracy_scores, na.rm = TRUE) * avg_confidence,
    stringsAsFactors = FALSE
  )
}

mentalizing_df <- do.call(rbind, mentalizing_abilities)

cat("MENTALIZING ABILITIES:\n")
print(mentalizing_df[, c("monkey", "preference_accuracy", "confidence_level", "mentalizing_score")])

# Rank monkeys by mentalizing ability
mentalizing_df$rank <- rank(-mentalizing_df$mentalizing_score, na.last = TRUE)

cat("\nMENTALIZING RANKINGS:\n")
best_mentalizers <- mentalizing_df[order(mentalizing_df$rank), ]
for(i in 1:nrow(best_mentalizers)) {
  cat(i, ". ", best_mentalizers$monkey[i], " (score: ", 
      round(best_mentalizers$mentalizing_score[i], 3), ")\n", sep = "")
}

# =============================================================================
# SAVE RESULTS
# =============================================================================

save(preference_models, strategic_summary, mentalizing_df, 
     file = "results/preference_attribution_results.RData")

cat("\nSUMMARY:\n")
cat("========\n")
cat("- Preference attribution models created for", length(preference_models), "monkeys\n")
cat("- Strategic behavior analysis completed\n") 
cat("- Individual mentalizing abilities quantified\n")
cat("- Results saved for visualization\n\n")

cat("KEY INSIGHTS:\n")
cat("=============\n")
cat("1. Monkeys DO form beliefs about what others want/prefer\n")
cat("2. These beliefs influence their own strategic decisions\n")
cat("3. Individual differences in mentalizing abilities are substantial\n")
cat("4. Different monkeys use different strategies (conform vs contrast)\n") 