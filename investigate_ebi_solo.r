# INVESTIGATE EBI'S SOLO PERFORMANCE
# Check why EBI might not have solo performance data

# Load data
data_raw <- read.csv("Explore Exploit Dataset.csv")

# Clean outcome variable
data_raw$outcome_clean <- ifelse(grepl("explore", data_raw$OUTCOME, ignore.case = TRUE), "explore",
                               ifelse(grepl("exploit", data_raw$OUTCOME, ignore.case = TRUE), "exploit", 
                                     ifelse(data_raw$OUTCOME %in% c("NONE", "none", "stop"), "none", "other")))

data_valid <- data_raw[!is.na(data_raw$outcome_clean) & data_raw$outcome_clean != "other", ]

cat("=== INVESTIGATING EBI'S SOLO PERFORMANCE ===\n")

# Check EBI's data specifically
ebi_data <- data_valid[data_valid$monkey == "EBI", ]
cat("Total EBI trials:", nrow(ebi_data), "\n")

# Check EBI's solo trials
ebi_solo <- ebi_data[ebi_data$CONDITION == "solo", ]
cat("EBI solo trials:", nrow(ebi_solo), "\n")

if(nrow(ebi_solo) > 0) {
  # Calculate exploration rate
  explore_count <- sum(ebi_solo$outcome_clean == "explore", na.rm = TRUE)
  total_count <- nrow(ebi_solo)
  exploration_rate <- (explore_count / total_count) * 100
  
  cat("EBI solo exploration rate:", round(exploration_rate, 1), "%\n")
  cat("EBI solo trials breakdown:\n")
  print(table(ebi_solo$outcome_clean))
} else {
  cat("WARNING: EBI has NO solo trials!\n")
}

# Check all conditions for EBI
cat("\nEBI trials by condition:\n")
ebi_conditions <- table(ebi_data$CONDITION)
print(ebi_conditions)

# Check EBI's PAIRED_WITH values
cat("\nEBI's PAIRED_WITH values:\n")
ebi_partners <- table(ebi_data$PAIRED_WITH)
print(ebi_partners)

# Check if there are any solo trials with empty PAIRED_WITH
ebi_empty_partner <- ebi_data[ebi_data$PAIRED_WITH == "" | is.na(ebi_data$PAIRED_WITH), ]
cat("\nEBI trials with empty PAIRED_WITH:", nrow(ebi_empty_partner), "\n")

if(nrow(ebi_empty_partner) > 0) {
  cat("Conditions for EBI with empty partner:\n")
  print(table(ebi_empty_partner$CONDITION))
  
  if(any(ebi_empty_partner$CONDITION == "solo")) {
    solo_empty <- ebi_empty_partner[ebi_empty_partner$CONDITION == "solo", ]
    explore_count <- sum(solo_empty$outcome_clean == "explore", na.rm = TRUE)
    exploration_rate <- (explore_count / nrow(solo_empty)) * 100
    cat("EBI solo (empty partner) exploration rate:", round(exploration_rate, 1), "%\n")
  }
}

# Compare with other monkeys for context
cat("\n=== COMPARISON WITH OTHER MONKEYS ===\n")
all_monkeys <- c("ANEMONE", "ICE", "CHOCOLAT", "EBI", "FRAN", "DALI")

for(monkey in all_monkeys) {
  monkey_data <- data_valid[data_valid$monkey == monkey, ]
  solo_data <- monkey_data[monkey_data$CONDITION == "solo", ]
  
  if(nrow(solo_data) > 0) {
    explore_count <- sum(solo_data$outcome_clean == "explore", na.rm = TRUE)
    exploration_rate <- (explore_count / nrow(solo_data)) * 100
    cat(monkey, "solo:", nrow(solo_data), "trials,", round(exploration_rate, 1), "% exploration\n")
  } else {
    cat(monkey, "solo: NO TRIALS\n")
  }
}

# Check the matrix calculation specifically
cat("\n=== MATRIX CALCULATION CHECK ===\n")

safe_rate <- function(subset_data, outcome) {
  if(nrow(subset_data) == 0) return(NA)
  sum(subset_data$outcome_clean == outcome, na.rm = TRUE) / nrow(subset_data) * 100
}

# Recreate the matrix calculation for EBI specifically
group2_monkeys <- c("EBI", "FRAN", "DALI")

cat("Group 2 matrix diagonal calculations:\n")
for(i in 1:3) {
  monkey <- group2_monkeys[i]
  solo_data <- data_valid[data_valid$monkey == monkey & data_valid$CONDITION == "solo", ]
  rate <- safe_rate(solo_data, "explore")
  cat(monkey, "diagonal (solo):", nrow(solo_data), "trials,", 
      ifelse(is.na(rate), "NA", paste0(round(rate, 1), "%")), "\n")
}

# Check if there might be a data filtering issue
cat("\n=== CHECKING DATA FILTERING ===\n")

# Check complete data requirements
model_data <- data_valid[
  !is.na(data_valid$CONDITION) &
  !is.na(data_valid$PAIRED_WITH) &
  !is.na(data_valid$RELATIVE_RANK) &
  !is.na(data_valid$SUBJECTIVE_CHOSEN_VALUE) &
  !is.na(data_valid$subjective_exploit) &
  !is.na(data_valid$expected_explore), ]

cat("Original data rows:", nrow(data_valid), "\n")
cat("After filtering for complete model data:", nrow(model_data), "\n")

# Check EBI specifically in filtered data
ebi_filtered <- model_data[model_data$monkey == "EBI", ]
ebi_solo_filtered <- ebi_filtered[ebi_filtered$CONDITION == "solo", ]

cat("EBI in filtered data:", nrow(ebi_filtered), "trials\n")
cat("EBI solo in filtered data:", nrow(ebi_solo_filtered), "trials\n")

if(nrow(ebi_solo_filtered) > 0) {
  explore_count <- sum(ebi_solo_filtered$outcome_clean == "explore", na.rm = TRUE)
  exploration_rate <- (explore_count / nrow(ebi_solo_filtered)) * 100
  cat("EBI solo exploration rate (filtered):", round(exploration_rate, 1), "%\n")
} 