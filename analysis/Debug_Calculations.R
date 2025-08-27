# =============================================================================
# DEBUG CALCULATIONS: Show exactly how metrics were calculated
# =============================================================================

library(dplyr)

cat("=============================================================================\n")
cat("DEBUGGING METRIC CALCULATIONS\n")
cat("=============================================================================\n\n")

# Load data
data_raw <- read.csv("data/Explore Exploit Dataset.csv", stringsAsFactors = FALSE)
data_clean <- data_raw[data_raw$TRIAL_TYPE == "OIT_RE", ]

cat("1. RAW DATA EXPLORATION:\n")
cat("Total raw trials:", nrow(data_raw), "\n")
cat("OIT_RE trials:", nrow(data_clean), "\n")

# Check outcomes
cat("\nFirst 20 unique outcomes in raw data:\n")
print(unique(data_clean$OUTCOME)[1:20])

# Clean outcomes
outcome_clean <- character(nrow(data_clean))
outcome_clean[grepl("explore", tolower(data_clean$OUTCOME))] <- "explore"
outcome_clean[grepl("exploit", tolower(data_clean$OUTCOME))] <- "exploit"  
outcome_clean[grepl("none|stop|NONE", tolower(data_clean$OUTCOME))] <- "none"

data_clean$outcome_clean <- outcome_clean
data_clean <- data_clean[data_clean$outcome_clean != "" & !is.na(data_clean$outcome_clean), ]

cat("\n2. AFTER CLEANING:\n")
cat("Cleaned trials:", nrow(data_clean), "\n")
cat("Outcome distribution:\n")
print(table(data_clean$outcome_clean))

# Check each monkey
cat("\n3. BREAKDOWN BY MONKEY:\n")
monkey_summary <- data_clean %>%
  group_by(monkey, CONDITION) %>%
  summarise(
    total_trials = n(),
    explore = sum(outcome_clean == "explore"),
    exploit = sum(outcome_clean == "exploit"), 
    none = sum(outcome_clean == "none"),
    explore_rate = explore / total_trials,
    .groups = "drop"
  )

print(monkey_summary)

# Detailed calculation for FRAN
cat("\n4. DETAILED FRAN CALCULATION:\n")
cat("=====================================\n")

fran_data <- data_clean %>% filter(monkey == "FRAN")
cat("Total FRAN trials:", nrow(fran_data), "\n")

# Solo vs Social breakdown
fran_solo <- fran_data %>% filter(CONDITION == "solo")
fran_social <- fran_data %>% filter(CONDITION %in% c("duo", "trio"))

cat("\nSOLO CONDITION:\n")
cat("Total solo trials:", nrow(fran_solo), "\n")
cat("Explore:", sum(fran_solo$outcome_clean == "explore"), "\n")
cat("Exploit:", sum(fran_solo$outcome_clean == "exploit"), "\n")
cat("None:", sum(fran_solo$outcome_clean == "none"), "\n")

solo_explore_rate <- sum(fran_solo$outcome_clean == "explore") / nrow(fran_solo)
cat("Solo exploration rate:", solo_explore_rate, "\n")

cat("\nSOCIAL CONDITIONS (duo + trio):\n")
cat("Total social trials:", nrow(fran_social), "\n")
cat("Explore:", sum(fran_social$outcome_clean == "explore"), "\n")
cat("Exploit:", sum(fran_social$outcome_clean == "exploit"), "\n")
cat("None:", sum(fran_social$outcome_clean == "none"), "\n")

social_explore_rate <- sum(fran_social$outcome_clean == "explore") / nrow(fran_social)
cat("Social exploration rate:", social_explore_rate, "\n")

# Calculate metrics
cat("\n5. METRIC CALCULATIONS FOR FRAN:\n")
cat("=================================\n")

# Social influence
social_influence <- solo_explore_rate - social_explore_rate
cat("Social influence (solo - social):", social_influence, "\n")

# Activity rate  
activity_rate <- sum(fran_data$outcome_clean %in% c("explore", "exploit")) / nrow(fran_data)
cat("Activity rate (explore + exploit / total):", activity_rate, "\n")

# Behavioral consistency
fran_ordered <- fran_data %>% arrange(BLOCK_No, TRIAL_NUM)
switches <- sum(fran_ordered$outcome_clean[-1] != fran_ordered$outcome_clean[-nrow(fran_ordered)], na.rm = TRUE)
consistency <- 1 - (switches / (nrow(fran_ordered) - 1))
cat("Behavioral consistency (1 - switch_rate):", consistency, "\n")

# Context sensitivity
context_rates <- fran_data %>%
  group_by(CONDITION) %>%
  summarise(explore_rate = mean(outcome_clean == "explore"), .groups = "drop")

context_sensitivity <- sd(context_rates$explore_rate, na.rm = TRUE)
cat("Context sensitivity (SD of explore rates across conditions):", context_sensitivity, "\n")

# Rank responsiveness
rank_data <- fran_data %>% filter(!is.na(RELATIVE_RANK) & outcome_clean != "none")
if(nrow(rank_data) > 10) {
  rank_correlation <- cor(rank_data$RELATIVE_RANK, 
                         as.numeric(rank_data$outcome_clean == "explore"), 
                         use = "complete.obs")
} else {
  rank_correlation <- 0
}
cat("Rank responsiveness (correlation with rank):", rank_correlation, "\n")

cat("\n6. DOES THIS MATCH THE HEATMAP VALUES?\n")
cat("======================================\n")
cat("Expected heatmap values for FRAN:\n")
cat("- Social influence:", social_influence, "\n") 
cat("- Activity rate:", activity_rate, "\n")
cat("- Behavioral consistency:", consistency, "\n")
cat("- Context sensitivity:", context_sensitivity, "\n")
cat("- Rank responsiveness:", rank_correlation, "\n")

cat("\nDo these numbers make sense given the data breakdown above?\n") 