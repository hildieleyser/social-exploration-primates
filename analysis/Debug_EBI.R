# =============================================================================
# DEBUG EBI CALCULATIONS
# =============================================================================

library(dplyr)

cat("EBI DETAILED BREAKDOWN:\n")
cat("=======================\n")

# Load data
data_raw <- read.csv("data/Explore Exploit Dataset.csv", stringsAsFactors = FALSE)
data_clean <- data_raw[data_raw$TRIAL_TYPE == "OIT_RE", ]

# Clean outcomes
outcome_clean <- character(nrow(data_clean))
outcome_clean[grepl("explore", tolower(data_clean$OUTCOME))] <- "explore"
outcome_clean[grepl("exploit", tolower(data_clean$OUTCOME))] <- "exploit"  
outcome_clean[grepl("none|stop|NONE", tolower(data_clean$OUTCOME))] <- "none"

data_clean$outcome_clean <- outcome_clean
data_clean <- data_clean[data_clean$outcome_clean != "" & !is.na(data_clean$outcome_clean), ]

ebi_data <- data_clean %>% filter(monkey == "EBI")
cat("Total EBI trials:", nrow(ebi_data), "\n")

# Show detailed breakdown
cat("\nCondition breakdown:\n")
ebi_summary <- ebi_data %>%
  group_by(CONDITION) %>%
  summarise(
    total = n(),
    explore = sum(outcome_clean == "explore"),
    exploit = sum(outcome_clean == "exploit"),
    none = sum(outcome_clean == "none"),
    explore_rate = explore / total,
    .groups = "drop"
  )
print(ebi_summary)

# Solo vs Social
ebi_solo <- ebi_data %>% filter(CONDITION == "solo")
ebi_social <- ebi_data %>% filter(CONDITION %in% c("duo", "trio"))

cat("\nSOLO CONDITION:\n")
cat("Total solo trials:", nrow(ebi_solo), "\n")
cat("Explore:", sum(ebi_solo$outcome_clean == "explore"), "out of", nrow(ebi_solo), "\n")
cat("Exploit:", sum(ebi_solo$outcome_clean == "exploit"), "out of", nrow(ebi_solo), "\n")
cat("None:", sum(ebi_solo$outcome_clean == "none"), "out of", nrow(ebi_solo), "\n")

solo_explore_rate <- sum(ebi_solo$outcome_clean == "explore") / nrow(ebi_solo)
cat("Solo exploration rate:", solo_explore_rate, "(", round(solo_explore_rate * 100, 1), "%)\n")

cat("\nSOCIAL CONDITIONS (duo + trio):\n")
cat("Total social trials:", nrow(ebi_social), "\n")
cat("Explore:", sum(ebi_social$outcome_clean == "explore"), "out of", nrow(ebi_social), "\n")
cat("Exploit:", sum(ebi_social$outcome_clean == "exploit"), "out of", nrow(ebi_social), "\n")
cat("None:", sum(ebi_social$outcome_clean == "none"), "out of", nrow(ebi_social), "\n")

social_explore_rate <- sum(ebi_social$outcome_clean == "explore") / nrow(ebi_social)
cat("Social exploration rate:", social_explore_rate, "(", round(social_explore_rate * 100, 1), "%)\n")

# Calculate metrics
cat("\nMETRIC CALCULATIONS:\n")
cat("====================\n")

# Social influence
social_influence <- solo_explore_rate - social_explore_rate
cat("Social influence (solo - social):", social_influence, "\n")
cat("  This means EBI explores", round(solo_explore_rate * 100, 1), "% when alone vs", 
    round(social_explore_rate * 100, 1), "% in social contexts\n")
cat("  Difference:", round(social_influence * 100, 1), "percentage points\n")

# Activity rate
total_active <- sum(ebi_data$outcome_clean %in% c("explore", "exploit"))
total_trials <- nrow(ebi_data)
activity_rate <- total_active / total_trials
cat("Activity rate:", activity_rate, "(", total_active, "active out of", total_trials, "total)\n")

# Behavioral consistency
ebi_ordered <- ebi_data %>% arrange(BLOCK_No, TRIAL_NUM)
if(nrow(ebi_ordered) > 1) {
  switches <- sum(ebi_ordered$outcome_clean[-1] != ebi_ordered$outcome_clean[-nrow(ebi_ordered)], na.rm = TRUE)
  consistency <- 1 - (switches / (nrow(ebi_ordered) - 1))
  cat("Behavioral consistency:", consistency, "(", switches, "switches out of", nrow(ebi_ordered) - 1, "possible)\n")
} else {
  consistency <- 1
  cat("Behavioral consistency: 1 (only one trial)\n")
}

# Context sensitivity
context_rates <- ebi_data %>%
  group_by(CONDITION) %>%
  summarise(explore_rate = mean(outcome_clean == "explore"), .groups = "drop")

context_sensitivity <- sd(context_rates$explore_rate, na.rm = TRUE)
cat("Context sensitivity (SD of explore rates):", context_sensitivity, "\n")
cat("  Explore rates by condition:\n")
for(i in 1:nrow(context_rates)) {
  cat("   ", context_rates$CONDITION[i], ":", round(context_rates$explore_rate[i] * 100, 1), "%\n")
}

cat("\nDoes this match what you expected? What seems wrong?\n") 