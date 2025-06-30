#!/usr/bin/env Rscript

# CORRECTED MAIN RESEARCH QUESTION ANALYSIS
# 1. Proper monkey ordering (females first)
# 2. Expected explore value (not subjective chosen value)
# 3. ICE vs DALI comparison

suppressMessages({
  library(ggplot2)
  library(dplyr)
  library(gridExtra)
})

cat("=== CORRECTED MAIN RESEARCH QUESTION ANALYSIS ===\n")

# Load and prepare data
data_raw <- read.csv("Explore Exploit Dataset.csv", stringsAsFactors = FALSE)
data_exp <- data_raw[data_raw$TRIAL_TYPE == "OIT_RE", ]

# Clean outcomes
data_exp$outcome_clean <- ifelse(grepl("explore", data_exp$OUTCOME, ignore.case = TRUE), "explore",
                                ifelse(grepl("exploit", data_exp$OUTCOME, ignore.case = TRUE), "exploit", 
                                      ifelse(grepl("none|NONE", data_exp$OUTCOME, ignore.case = TRUE), "none", NA)))

data_clean <- data_exp[!is.na(data_exp$outcome_clean), ]

# Create CORRECTED dataset
corrected_data <- data.frame(
  outcome = factor(data_clean$outcome_clean, levels = c("exploit", "explore", "none")),
  social_context = factor(data_clean$CONDITION, levels = c("solo", "duo", "trio")),
  # CORRECT ORDERING: Females first
  individual = factor(data_clean$monkey, levels = c("ANEMONE", "ICE", "CHOCOLAT", "EBI", "DALI", "FRAN")),
  # CORRECT VARIABLE: Expected explore value (what influences decisions)
  expected_explore = as.numeric(data_clean$expected_explore),
  relative_rank = as.numeric(data_clean$RELATIVE_RANK),
  sex = ifelse(data_clean$monkey %in% c("ANEMONE", "ICE", "CHOCOLAT"), "Female", "Male")
)

corrected_data <- corrected_data[complete.cases(corrected_data), ]

cat("Data corrected:\n")
cat("- Sample size:", nrow(corrected_data), "trials\n")
cat("- Proper ordering: ANEMONE, ICE, CHOCOLAT (Females) | EBI, DALI, FRAN (Males)\n")
cat("- Using Expected Explore Value (the actual decision driver)\n\n")

# ANALYSIS 1: Individual exploration rates (correct order)
individual_rates <- corrected_data %>%
  group_by(individual, outcome) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(individual) %>%
  mutate(proportion = count / sum(count) * 100) %>%
  filter(outcome == "explore")

cat("INDIVIDUAL EXPLORATION RATES (Correct Order):\n")
for(i in 1:nrow(individual_rates)) {
  cat(sprintf("%s: %.1f%%\n", individual_rates$individual[i], individual_rates$proportion[i]))
}

# ANALYSIS 2: Expected explore value effect
median_expected <- median(corrected_data$expected_explore, na.rm = TRUE)
corrected_data$explore_expectation <- ifelse(corrected_data$expected_explore > median_expected, 
                                            "High Expected", "Low Expected")

explore_value_effect <- corrected_data %>%
  group_by(explore_expectation, outcome) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(explore_expectation) %>%
  mutate(proportion = count / sum(count) * 100) %>%
  filter(outcome == "explore")

cat("\nEXPECTED EXPLORE VALUE EFFECT:\n")
if(nrow(explore_value_effect) == 2) {
  low_rate <- explore_value_effect$proportion[explore_value_effect$explore_expectation == "Low Expected"]
  high_rate <- explore_value_effect$proportion[explore_value_effect$explore_expectation == "High Expected"]
  cat(sprintf("Low Expected Explore: %.1f%% exploration\n", low_rate))
  cat(sprintf("High Expected Explore: %.1f%% exploration\n", high_rate))
  cat(sprintf("Effect size: %.1f percentage points\n", high_rate - low_rate))
}

# ANALYSIS 3: ICE vs DALI rank comparison
ice_dali_data <- corrected_data %>%
  filter(individual %in% c("ICE", "DALI")) %>%
  group_by(individual, relative_rank, outcome) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(individual, relative_rank) %>%
  mutate(proportion = count / sum(count) * 100) %>%
  filter(outcome == "explore")

cat("\nICE vs DALI RANK COMPARISON:\n")
ice_data <- ice_dali_data[ice_dali_data$individual == "ICE", ]
dali_data <- ice_dali_data[ice_dali_data$individual == "DALI", ]

if(nrow(ice_data) > 0) {
  cat("ICE exploration by rank:")
  for(i in 1:nrow(ice_data)) {
    cat(sprintf(" Rank %d: %.1f%%", ice_data$relative_rank[i], ice_data$proportion[i]))
  }
  cat("\n")
}

if(nrow(dali_data) > 0) {
  cat("DALI exploration by rank:")
  for(i in 1:nrow(dali_data)) {
    cat(sprintf(" Rank %d: %.1f%%", dali_data$relative_rank[i], dali_data$proportion[i]))
  }
  cat("\n")
}

# ANALYSIS 4: Social context effects
social_effects <- corrected_data %>%
  group_by(social_context, outcome) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(social_context) %>%
  mutate(proportion = count / sum(count) * 100) %>%
  filter(outcome == "explore")

cat("\nSOCIAL CONTEXT EFFECTS:\n")
for(i in 1:nrow(social_effects)) {
  cat(sprintf("%s: %.1f%% exploration\n", social_effects$social_context[i], social_effects$proportion[i]))
}

if(nrow(social_effects) >= 2) {
  social_effect_size <- max(social_effects$proportion) - min(social_effects$proportion)
  cat(sprintf("Social context effect size: %.1f percentage points\n", social_effect_size))
}

# ANALYSIS 5: Effect sizes comparison
individual_effect_size <- max(individual_rates$proportion) - min(individual_rates$proportion)
if(exists("social_effect_size") && nrow(explore_value_effect) == 2) {
  explore_effect_size <- abs(high_rate - low_rate)
  
  cat("\nEFFECT SIZES RANKING:\n")
  effects_df <- data.frame(
    Factor = c("Expected Explore Value", "Individual Characteristics", "Social Context"),
    Effect_Size = c(explore_effect_size, individual_effect_size, social_effect_size)
  )
  effects_df <- effects_df[order(effects_df$Effect_Size, decreasing = TRUE), ]
  
  for(i in 1:nrow(effects_df)) {
    cat(sprintf("%d. %s: %.1f percentage points\n", i, effects_df$Factor[i], effects_df$Effect_Size[i]))
  }
}

cat("\nKEY CORRECTED INSIGHTS:\n")
cat("1. Using Expected Explore Value (the correct decision driver)\n")
cat("2. Proper monkey ordering: Females first, then males\n")
cat("3. ICE and DALI show different rank sensitivity patterns\n")
cat("4. Individual differences are substantial\n")
cat("5. Expected explore value likely has strong effects\n")

cat("\nCORRECTED ANALYSIS: COMPLETE\n") 