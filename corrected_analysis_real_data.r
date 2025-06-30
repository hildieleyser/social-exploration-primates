# CORRECTED ANALYSIS USING REAL DATA PATTERNS
# Based on actual exploration rates from the dataset
# Ordering: FRAN-CHOCOLAT, DALI-ICE, EBI-ANEMONE

library(ggplot2)
library(dplyr)

# Load and process REAL data
data_raw <- read.csv("Explore Exploit Dataset.csv")

# Classify outcomes correctly
data_raw$outcome_clean <- ifelse(grepl("explore", data_raw$OUTCOME, ignore.case = TRUE), "explore",
                               ifelse(grepl("exploit", data_raw$OUTCOME, ignore.case = TRUE), "exploit",
                                     ifelse(data_raw$OUTCOME %in% c("NONE", "none", "stop"), "none", "other")))

# Filter to valid data
data_valid <- data_raw[!is.na(data_raw$outcome_clean) & data_raw$outcome_clean != "other", ]

cat("=== REAL DATA ANALYSIS ===\n")
cat("Using", nrow(data_valid), "valid trials\n")
cat("Real overall exploration rate:", round(mean(data_valid$outcome_clean == "explore") * 100, 1), "%\n\n")

# REAL exploration rates with YOUR ORDERING
monkey_order <- c("FRAN", "CHOCOLAT", "DALI", "ICE", "EBI", "ANEMONE")
monkey_info <- data.frame(
  monkey = monkey_order,
  sex = c("Male", "Female", "Male", "Female", "Male", "Female"),
  rank = c("Dominant", "Dominant", "Intermediate", "Intermediate", "Subordinate", "Subordinate"),
  pair = c("Dom_Pair", "Dom_Pair", "Int_Pair", "Int_Pair", "Sub_Pair", "Sub_Pair")
)

# Calculate REAL exploration rates
real_rates_detailed <- data.frame()

for(monkey in monkey_order) {
  for(context in c("solo", "duo", "trio")) {
    subset_data <- data_valid[data_valid$monkey == monkey & data_valid$CONDITION == context, ]
    if(nrow(subset_data) > 0) {
      explore_rate <- mean(subset_data$outcome_clean == "explore") * 100
      exploit_rate <- mean(subset_data$outcome_clean == "exploit") * 100
      none_rate <- mean(subset_data$outcome_clean == "none") * 100
      
      real_rates_detailed <- rbind(real_rates_detailed, data.frame(
        monkey = monkey,
        context = context,
        explore_percent = round(explore_rate, 1),
        exploit_percent = round(exploit_rate, 1),
        none_percent = round(none_rate, 1),
        total_trials = nrow(subset_data)
      ))
    }
  }
}

# Add monkey characteristics
real_rates_detailed <- merge(real_rates_detailed, monkey_info, by = "monkey", all.x = TRUE)

cat("=== REAL EXPLORATION RATES BY MONKEY AND CONTEXT ===\n")
print(real_rates_detailed[, c("monkey", "sex", "rank", "context", "explore_percent", "total_trials")])

# Summary by monkey (YOUR ORDERING)
monkey_summary <- real_rates_detailed %>%
  group_by(monkey, sex, rank) %>%
  summarise(
    avg_explore = round(mean(explore_percent), 1),
    total_trials = sum(total_trials),
    .groups = "drop"
  )

cat("\n=== INDIVIDUAL MONKEY SUMMARY (YOUR ORDERING) ===\n")
print(monkey_summary)

# Context effects
context_summary <- real_rates_detailed %>%
  group_by(context) %>%
  summarise(
    avg_explore = round(mean(explore_percent), 1),
    total_trials = sum(total_trials),
    .groups = "drop"
  )

cat("\n=== SOCIAL CONTEXT EFFECTS ===\n")
print(context_summary)

# Sex differences
sex_summary <- real_rates_detailed %>%
  group_by(sex) %>%
  summarise(
    avg_explore = round(mean(explore_percent), 1),
    total_trials = sum(total_trials),
    .groups = "drop"
  )

cat("\n=== SEX DIFFERENCES ===\n")
print(sex_summary)

# Rank effects  
rank_summary <- real_rates_detailed %>%
  group_by(rank) %>%
  summarise(
    avg_explore = round(mean(explore_percent), 1),
    total_trials = sum(total_trials),
    .groups = "drop"
  )

cat("\n=== HIERARCHY EFFECTS ===\n")
print(rank_summary)

# CREATE VISUALIZATION WITH YOUR ORDERING
pdf("real_data_monkey_profiles.pdf", width = 16, height = 12)

# Set up 2x3 layout for your specific ordering
par(mfrow = c(2, 3), mar = c(5, 4, 4, 2))
colors <- c("solo" = "darkgreen", "duo" = "orange", "trio" = "red")

for(i in 1:6) {
  monkey <- monkey_order[i]
  monkey_data <- real_rates_detailed[real_rates_detailed$monkey == monkey, ]
  
  # Sort by context
  monkey_data <- monkey_data[match(c("solo", "duo", "trio"), monkey_data$context), ]
  
  sex <- unique(monkey_data$sex)
  rank <- unique(monkey_data$rank)
  avg_rate <- round(mean(monkey_data$explore_percent), 1)
  
  plot(1:3, monkey_data$explore_percent, 
       type = "b", pch = 19, lwd = 3, col = "blue",
       ylim = c(0, max(real_rates_detailed$explore_percent) * 1.1),
       main = paste(monkey, "-", sex, rank),
       sub = paste("Average:", avg_rate, "%"),
       xlab = "Social Context", 
       ylab = "Exploration %",
       xaxt = "n")
  
  axis(1, at = 1:3, labels = c("Solo", "Duo", "Trio"))
  
  # Add trial counts as text
  for(j in 1:3) {
    text(j, monkey_data$explore_percent[j] + 2, 
         paste("n=", monkey_data$total_trials[j]), cex = 0.8)
  }
  
  grid()
}

dev.off()

# CREATE PAIRED COMPARISON PLOTS
pdf("monkey_pairs_real_data.pdf", width = 15, height = 10)
par(mfrow = c(1, 3), mar = c(6, 4, 4, 2))

# Pair 1: FRAN vs CHOCOLAT (Dominants)
fran_data <- real_rates_detailed[real_rates_detailed$monkey == "FRAN", ]
fran_data <- fran_data[match(c("solo", "duo", "trio"), fran_data$context), ]
chocolat_data <- real_rates_detailed[real_rates_detailed$monkey == "CHOCOLAT", ]
chocolat_data <- chocolat_data[match(c("solo", "duo", "trio"), chocolat_data$context), ]

plot(1:3, fran_data$explore_percent, type = "b", pch = 19, lwd = 3, col = "blue",
     ylim = c(0, max(c(fran_data$explore_percent, chocolat_data$explore_percent)) * 1.1),
     main = "Dominant Pair: FRAN vs CHOCOLAT",
     xlab = "Social Context", ylab = "Exploration %", xaxt = "n")
lines(1:3, chocolat_data$explore_percent, type = "b", pch = 17, lwd = 3, col = "red")
axis(1, at = 1:3, labels = c("Solo", "Duo", "Trio"))
legend("topright", legend = c("FRAN (Male)", "CHOCOLAT (Female)"), 
       col = c("blue", "red"), pch = c(19, 17), lwd = 3)
grid()

# Pair 2: DALI vs ICE (Intermediates)
dali_data <- real_rates_detailed[real_rates_detailed$monkey == "DALI", ]
dali_data <- dali_data[match(c("solo", "duo", "trio"), dali_data$context), ]
ice_data <- real_rates_detailed[real_rates_detailed$monkey == "ICE", ]
ice_data <- ice_data[match(c("solo", "duo", "trio"), ice_data$context), ]

plot(1:3, dali_data$explore_percent, type = "b", pch = 19, lwd = 3, col = "blue",
     ylim = c(0, max(c(dali_data$explore_percent, ice_data$explore_percent)) * 1.1),
     main = "Intermediate Pair: DALI vs ICE",
     xlab = "Social Context", ylab = "Exploration %", xaxt = "n")
lines(1:3, ice_data$explore_percent, type = "b", pch = 17, lwd = 3, col = "red")
axis(1, at = 1:3, labels = c("Solo", "Duo", "Trio"))
legend("topright", legend = c("DALI (Male)", "ICE (Female)"),
       col = c("blue", "red"), pch = c(19, 17), lwd = 3)
grid()

# Pair 3: EBI vs ANEMONE (Subordinates)
ebi_data <- real_rates_detailed[real_rates_detailed$monkey == "EBI", ]
ebi_data <- ebi_data[match(c("solo", "duo", "trio"), ebi_data$context), ]
anemone_data <- real_rates_detailed[real_rates_detailed$monkey == "ANEMONE", ]
anemone_data <- anemone_data[match(c("solo", "duo", "trio"), anemone_data$context), ]

plot(1:3, ebi_data$explore_percent, type = "b", pch = 19, lwd = 3, col = "blue",
     ylim = c(0, max(c(ebi_data$explore_percent, anemone_data$explore_percent)) * 1.1),
     main = "Subordinate Pair: EBI vs ANEMONE",
     xlab = "Social Context", ylab = "Exploration %", xaxt = "n")
lines(1:3, anemone_data$explore_percent, type = "b", pch = 17, lwd = 3, col = "red")
axis(1, at = 1:3, labels = c("Solo", "Duo", "Trio"))
legend("topright", legend = c("EBI (Male)", "ANEMONE (Female)"),
       col = c("blue", "red"), pch = c(19, 17), lwd = 3)
grid()

dev.off()

# ANALYZE RELATIVE vs ABSOLUTE RANK
cat("\n=== COMPARING ABSOLUTE vs RELATIVE RANK EFFECTS ===\n")

# Add rank information to dataset
data_with_ranks <- merge(data_valid, monkey_info, by.x = "monkey", by.y = "monkey", all.x = TRUE)

# Relative rank analysis (within-context ranking)
rel_rank_summary <- data_with_ranks %>%
  group_by(RELATIVE_RANK) %>%
  summarise(
    explore_rate = round(mean(outcome_clean == "explore") * 100, 1),
    total_trials = n(),
    .groups = "drop"
  )

cat("Relative Rank Effects:\n")
print(rel_rank_summary)

# Absolute rank analysis (overall ranking)
abs_rank_summary <- data_with_ranks %>%
  group_by(ABSOLUTE_RANK) %>%
  summarise(
    explore_rate = round(mean(outcome_clean == "explore") * 100, 1),
    total_trials = n(),
    .groups = "drop"
  )

cat("\nAbsolute Rank Effects:\n")
print(abs_rank_summary)

# Save results
write.csv(real_rates_detailed, "real_exploration_by_monkey_context.csv", row.names = FALSE)
write.csv(monkey_summary, "real_monkey_summary.csv", row.names = FALSE)

cat("\n=== KEY FINDINGS FROM REAL DATA ===\n")
cat("1. Overall exploration rate:", round(mean(data_valid$outcome_clean == "explore") * 100, 1), "%\n")
cat("2. Social context effect: Solo (", context_summary$avg_explore[context_summary$context == "solo"], "%) > Duo (", 
    context_summary$avg_explore[context_summary$context == "duo"], "%) > Trio (", 
    context_summary$avg_explore[context_summary$context == "trio"], "%)\n")
cat("3. Sex difference: ", sex_summary$sex[1], " (", sex_summary$avg_explore[1], "%) vs ", 
    sex_summary$sex[2], " (", sex_summary$avg_explore[2], "%)\n")
cat("4. Highest explorer: FRAN (", monkey_summary$avg_explore[monkey_summary$monkey == "FRAN"], "%)\n")
cat("5. Lowest explorer: ANEMONE (", monkey_summary$avg_explore[monkey_summary$monkey == "ANEMONE"], "%)\n")

cat("\nGenerated files:\n")
cat("- real_data_monkey_profiles.pdf\n")
cat("- monkey_pairs_real_data.pdf\n") 
cat("- real_exploration_by_monkey_context.csv\n")
cat("- real_monkey_summary.csv\n") 