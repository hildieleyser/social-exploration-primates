# SIMPLE DATA VALIDATION TEST

# Load data
data_raw <- read.csv("Explore Exploit Dataset.csv")

cat("=== BASIC DATA VALIDATION ===\n")
cat("Dataset shape:", nrow(data_raw), "rows,", ncol(data_raw), "columns\n")
cat("Unique monkeys:", paste(sort(unique(data_raw$monkey)), collapse = ", "), "\n")

# Real exploration rates
explore_outcomes <- c("explore", "explore_bitter", "explore_sweet") 
exploit_outcomes <- c("exploit_pink", "exploit_blue", "exploit_green", "exploit_red", "exploit_yellow", "exploit")

data_raw$outcome_type <- ifelse(data_raw$OUTCOME %in% explore_outcomes, "explore",
                               ifelse(grepl("exploit", data_raw$OUTCOME), "exploit",
                                     ifelse(data_raw$OUTCOME %in% c("NONE", "none", "stop"), "none", "other")))

cat("\nReal outcome distribution:\n")
print(table(data_raw$outcome_type))

# Individual monkey exploration rates from REAL data
real_rates <- aggregate(outcome_type == "explore", 
                       by = list(monkey = data_raw$monkey), 
                       FUN = function(x) round(mean(x, na.rm = TRUE) * 100, 1))
names(real_rates) <- c("monkey", "real_explore_percent")

# Order as requested: FRAN-CHOCOLAT, DALI-ICE, EBI-ANEMONE
monkey_order <- c("FRAN", "CHOCOLAT", "DALI", "ICE", "EBI", "ANEMONE")
real_rates_ordered <- real_rates[match(monkey_order, real_rates$monkey), ]

cat("\nREAL exploration rates (YOUR ORDERING):\n")
for(i in 1:nrow(real_rates_ordered)) {
  cat(sprintf("%s: %.1f%%\n", real_rates_ordered$monkey[i], real_rates_ordered$real_explore_percent[i]))
}

# Check if previous model predictions were reasonable
if(file.exists("individual_monkey_profiles.csv")) {
  cat("\n=== COMPARING PREVIOUS MODEL TO REAL DATA ===\n")
  model_preds <- read.csv("individual_monkey_profiles.csv")
  
  model_summary <- aggregate(explore, by = list(monkey = model_preds$monkey_id), FUN = function(x) round(mean(x) * 100, 1))
  names(model_summary) <- c("monkey", "model_explore_percent")
  
  comparison <- merge(real_rates_ordered, model_summary, by = "monkey", all.x = TRUE)
  comparison$error <- abs(comparison$real_explore_percent - comparison$model_explore_percent)
  
  cat("Monkey | Real Rate | Model Rate | Error\n")
  cat("-------|-----------|------------|------\n")
  for(i in 1:nrow(comparison)) {
    cat(sprintf("%-6s | %8.1f%% | %9.1f%% | %4.1f\n", 
                comparison$monkey[i], 
                comparison$real_explore_percent[i],
                comparison$model_explore_percent[i],
                comparison$error[i]))
  }
  
  avg_error <- mean(comparison$error, na.rm = TRUE)
  cat(sprintf("\nAverage prediction error: %.1f percentage points\n", avg_error))
  
  if(avg_error > 15) {
    cat("*** WARNING: Large prediction errors - previous model may have been wrong ***\n")
  } else {
    cat("*** Model predictions appear reasonable ***\n")
  }
} else {
  cat("\nNo previous model predictions found to compare\n")
}

cat("\n=== DONE ===\n") 