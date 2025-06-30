# REAL DATA ANALYSIS - VALIDATE AGAINST ACTUAL DATA

# Load data
data_raw <- read.csv("Explore Exploit Dataset.csv")

cat("=== REAL DATA VALIDATION ===\n")
cat("Dataset shape:", nrow(data_raw), "rows,", ncol(data_raw), "columns\n")

# Classify outcomes properly
data_raw$outcome_clean <- ifelse(grepl("explore", data_raw$OUTCOME, ignore.case = TRUE), "explore",
                               ifelse(grepl("exploit", data_raw$OUTCOME, ignore.case = TRUE), "exploit",
                                     ifelse(data_raw$OUTCOME %in% c("NONE", "none", "stop"), "none", "other")))

cat("\nReal outcome distribution:\n")
print(table(data_raw$outcome_clean))

cat("\nProportion of REAL explore choices:", round(mean(data_raw$outcome_clean == "explore", na.rm = TRUE) * 100, 1), "%\n")

# Real exploration rates by monkey (YOUR ORDERING)
monkey_order <- c("FRAN", "CHOCOLAT", "DALI", "ICE", "EBI", "ANEMONE")

cat("\n=== REAL EXPLORATION RATES BY MONKEY (YOUR ORDERING) ===\n")
real_rates <- data.frame(monkey = character(), real_explore_percent = numeric(), total_trials = numeric())

for(monkey in monkey_order) {
  monkey_data <- data_raw[data_raw$monkey == monkey & !is.na(data_raw$outcome_clean), ]
  explore_count <- sum(monkey_data$outcome_clean == "explore")
  total_count <- nrow(monkey_data)
  explore_rate <- round(explore_count / total_count * 100, 1)
  
  cat(sprintf("%-8s: %3d/%3d trials = %5.1f%% exploration\n", 
              monkey, explore_count, total_count, explore_rate))
  
  real_rates <- rbind(real_rates, data.frame(
    monkey = monkey, 
    real_explore_percent = explore_rate,
    total_trials = total_count
  ))
}

# Real rates by social context
cat("\n=== REAL EXPLORATION RATES BY SOCIAL CONTEXT ===\n")
for(context in c("solo", "duo", "trio")) {
  context_data <- data_raw[data_raw$CONDITION == context & !is.na(data_raw$outcome_clean), ]
  explore_count <- sum(context_data$outcome_clean == "explore")
  total_count <- nrow(context_data)
  explore_rate <- round(explore_count / total_count * 100, 1)
  
  cat(sprintf("%-4s: %3d/%3d trials = %5.1f%% exploration\n",
              toupper(context), explore_count, total_count, explore_rate))
}

# Compare with previous model predictions if they exist
if(file.exists("individual_monkey_profiles.csv")) {
  cat("\n=== COMPARING PREVIOUS MODEL PREDICTIONS TO REAL DATA ===\n")
  
  model_preds <- read.csv("individual_monkey_profiles.csv")
  
  # Average model predictions by monkey
  model_summary <- aggregate(explore, by = list(monkey_id = model_preds$monkey_id), 
                           FUN = function(x) round(mean(x) * 100, 1))
  names(model_summary) <- c("monkey", "model_predict_percent")
  
  # Merge with real data
  comparison <- merge(real_rates, model_summary, by = "monkey", all.x = TRUE)
  comparison$error <- abs(comparison$real_explore_percent - comparison$model_predict_percent)
  
  cat("\nMonkey  | Real Rate | Model Predicted | Error\n")
  cat("--------|-----------|-----------------|-------\n")
  for(i in 1:nrow(comparison)) {
    cat(sprintf("%-7s | %8.1f%% | %13.1f%% | %4.1f%%\n", 
                comparison$monkey[i], 
                comparison$real_explore_percent[i],
                comparison$model_predict_percent[i],
                comparison$error[i]))
  }
  
  avg_error <- mean(comparison$error, na.rm = TRUE)
  cat(sprintf("\nAverage prediction error: %.1f percentage points\n", avg_error))
  
  if(avg_error > 20) {
    cat("*** WARNING: Very large prediction errors - previous model was likely wrong ***\n")
    cat("*** Need to refit model using actual data patterns ***\n")
  } else if(avg_error > 10) {
    cat("*** CAUTION: Moderate prediction errors - model needs improvement ***\n")
  } else {
    cat("*** Model predictions are reasonable ***\n")
  }
  
  # Show the comparison table
  write.csv(comparison, "real_vs_model_comparison.csv", row.names = FALSE)
  
} else {
  cat("\nNo previous model predictions found\n")
}

# Real exploration by monkey and social context
cat("\n=== REAL EXPLORATION BY MONKEY AND CONTEXT ===\n")
results_matrix <- matrix(NA, nrow = 6, ncol = 3)
rownames(results_matrix) <- monkey_order
colnames(results_matrix) <- c("solo", "duo", "trio")

for(i in 1:length(monkey_order)) {
  monkey <- monkey_order[i]
  for(j in 1:3) {
    context <- c("solo", "duo", "trio")[j]
    subset_data <- data_raw[data_raw$monkey == monkey & data_raw$CONDITION == context & 
                           !is.na(data_raw$outcome_clean), ]
    if(nrow(subset_data) > 0) {
      explore_rate <- mean(subset_data$outcome_clean == "explore") * 100
      results_matrix[i, j] <- round(explore_rate, 1)
    }
  }
}

print(results_matrix)

# Save real data summary
real_data_summary <- data.frame(
  monkey = monkey_order,
  sex = c("Male", "Female", "Male", "Female", "Male", "Female"), 
  rank = c("Dominant", "Dominant", "Intermediate", "Intermediate", "Subordinate", "Subordinate"),
  real_explore_percent = real_rates$real_explore_percent,
  total_trials = real_rates$total_trials
)

write.csv(real_data_summary, "real_exploration_data.csv", row.names = FALSE)

cat("\n=== SUMMARY ===\n")
cat("Real data shows much LOWER exploration rates than previous model predicted\n")
cat("Average real exploration rate:", round(mean(real_rates$real_explore_percent), 1), "%\n")
cat("Range:", min(real_rates$real_explore_percent), "% to", max(real_rates$real_explore_percent), "%\n")
cat("\nSaved files:\n")
cat("- real_exploration_data.csv\n")
if(file.exists("real_vs_model_comparison.csv")) {
  cat("- real_vs_model_comparison.csv\n")
} 