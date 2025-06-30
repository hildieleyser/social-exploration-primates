# INTUITIVE DATA VISUALIZATIONS FOR DEEP INSIGHTS
# Creating working plots that reveal patterns in the data

library(ggplot2)
library(dplyr)
library(reshape2)

# Load and process data
data_raw <- read.csv("Explore Exploit Dataset.csv")

data_raw$outcome_clean <- ifelse(grepl("explore", data_raw$OUTCOME, ignore.case = TRUE), "explore",
                               ifelse(grepl("exploit", data_raw$OUTCOME, ignore.case = TRUE), "exploit", 
                                     ifelse(data_raw$OUTCOME %in% c("NONE", "none", "stop"), "none", "other")))

data_valid <- data_raw[!is.na(data_raw$outcome_clean) & data_raw$outcome_clean != "other", ]

# Add monkey info in your preferred order
monkey_order <- c("FRAN", "CHOCOLAT", "DALI", "ICE", "EBI", "ANEMONE")
monkey_info <- data.frame(
  monkey = monkey_order,
  sex = c("Male", "Female", "Male", "Female", "Male", "Female"),
  rank = c("Dominant", "Dominant", "Intermediate", "Intermediate", "Subordinate", "Subordinate")
)

data_analysis <- merge(data_valid, monkey_info, by = "monkey", all.x = TRUE)

cat("=== CREATING INTUITIVE VISUALIZATIONS ===\n")

pdf("intuitive_data_insights.pdf", width = 16, height = 20)
par(mfrow = c(4, 3), mar = c(5, 4, 4, 2))

# PLOT 1: Basic Heatmap - Exploration by Monkey and Context
cat("1. Creating exploration heatmap...\n")

heatmap_data <- data_analysis %>%
  group_by(monkey, CONDITION) %>%
  summarise(explore_rate = mean(outcome_clean == "explore") * 100, .groups = "drop")

heatmap_matrix <- dcast(heatmap_data, monkey ~ CONDITION, value.var = "explore_rate")
rownames(heatmap_matrix) <- heatmap_matrix$monkey
heatmap_matrix <- heatmap_matrix[monkey_order, -1]

image(1:3, 1:6, t(as.matrix(heatmap_matrix)), 
      col = heat.colors(20), main = "Exploration Rates: Monkey × Context",
      xlab = "Context", ylab = "Monkey", xaxt = "n", yaxt = "n")
axis(1, at = 1:3, labels = c("Solo", "Duo", "Trio"))
axis(2, at = 1:6, labels = monkey_order, las = 1)

for(i in 1:6) {
  for(j in 1:3) {
    text(j, i, paste0(round(heatmap_matrix[i,j], 1), "%"), cex = 0.7)
  }
}

# PLOT 2: Decision Space - Explore vs Exploit Trade-off
cat("2. Creating decision space plot...\n")

decision_data <- data_analysis %>%
  group_by(monkey, sex, rank) %>%
  summarise(
    explore_pct = mean(outcome_clean == "explore") * 100,
    exploit_pct = mean(outcome_clean == "exploit") * 100,
    none_pct = mean(outcome_clean == "none") * 100,
    .groups = "drop"
  )

colors_monkey <- rainbow(6)
plot(decision_data$explore_pct, decision_data$exploit_pct,
     pch = 19, cex = 2, col = colors_monkey,
     xlim = c(0, 80), ylim = c(0, 80),
     xlab = "Exploration %", ylab = "Exploitation %",
     main = "Decision Trade-offs")

text(decision_data$explore_pct, decision_data$exploit_pct,
     decision_data$monkey, pos = 3, cex = 0.8)

abline(a = 100, b = -1, col = "gray", lty = 2)

# PLOT 3: Sex Differences Across Contexts
cat("3. Creating sex differences plot...\n")

sex_context <- data_analysis %>%
  group_by(sex, CONDITION) %>%
  summarise(explore_rate = mean(outcome_clean == "explore") * 100, .groups = "drop")

barplot(matrix(sex_context$explore_rate, nrow = 2), 
        beside = TRUE, names.arg = c("Solo", "Duo", "Trio"),
        main = "Sex Differences Across Contexts",
        ylab = "Exploration %", xlab = "Social Context",
        col = c("pink", "lightblue"), 
        legend.text = c("Female", "Male"))

# PLOT 4: Hierarchy Effects
cat("4. Creating hierarchy effects plot...\n")

rank_context <- data_analysis %>%
  group_by(rank, CONDITION) %>%
  summarise(explore_rate = mean(outcome_clean == "explore") * 100, .groups = "drop")

barplot(matrix(rank_context$explore_rate, nrow = 3),
        beside = TRUE, names.arg = c("Solo", "Duo", "Trio"),
        main = "Hierarchy Effects Across Contexts", 
        ylab = "Exploration %", xlab = "Social Context",
        col = c("darkgreen", "orange", "red"),
        legend.text = c("Dominant", "Intermediate", "Subordinate"))

# PLOT 5: Individual Exploration Profiles
cat("5. Creating individual profiles...\n")

individual_data <- data_analysis %>%
  group_by(monkey, CONDITION) %>%
  summarise(explore_rate = mean(outcome_clean == "explore") * 100, .groups = "drop")

plot(1:3, rep(0, 3), type = "n", ylim = c(0, 80),
     main = "Individual Exploration Profiles",
     xlab = "Social Context", ylab = "Exploration %", xaxt = "n")
axis(1, at = 1:3, labels = c("Solo", "Duo", "Trio"))

colors_ind <- rainbow(6)
for(i in 1:6) {
  monkey <- monkey_order[i]
  monkey_data <- individual_data[individual_data$monkey == monkey, ]
  monkey_data <- monkey_data[match(c("solo", "duo", "trio"), monkey_data$CONDITION), ]
  lines(1:3, monkey_data$explore_rate, col = colors_ind[i], lwd = 2, type = "b", pch = 19)
}
legend("topright", legend = monkey_order, col = colors_ind, lwd = 2, cex = 0.6)

# PLOT 6: Value-Based Decisions
cat("6. Creating value-based analysis...\n")

value_data <- data_analysis %>%
  filter(!is.na(SUBJECTIVE_CHOSEN_VALUE) & !is.na(subjective_exploit)) %>%
  mutate(value_advantage = SUBJECTIVE_CHOSEN_VALUE - subjective_exploit) %>%
  filter(abs(value_advantage) <= 1)  # Remove extreme outliers

plot(value_data$value_advantage, as.numeric(value_data$outcome_clean == "explore"),
     pch = 19, cex = 0.5, col = "steelblue",
     xlab = "Value Advantage (Chosen - Exploit)", ylab = "Exploration (0/1)",
     main = "Value-Based Decision Making")

# Add trend line
if(nrow(value_data) > 10) {
  model_trend <- lm(as.numeric(outcome_clean == "explore") ~ value_advantage, data = value_data)
  abline(model_trend, col = "red", lwd = 2)
}

# PLOT 7: Expected Value Response
cat("7. Creating expected value response...\n")

expectation_data <- data_analysis %>%
  filter(!is.na(expected_explore)) %>%
  mutate(exp_bin = cut(expected_explore, breaks = 4)) %>%
  group_by(exp_bin) %>%
  summarise(
    explore_rate = mean(outcome_clean == "explore") * 100,
    n_trials = n(),
    .groups = "drop"
  ) %>%
  filter(n_trials >= 20)

if(nrow(expectation_data) > 0) {
  barplot(expectation_data$explore_rate,
          names.arg = 1:nrow(expectation_data),
          main = "Response to Expected Explore Value",
          ylab = "Exploration %", xlab = "Expected Value Level",
          col = "darkgreen")
}

# PLOT 8: Choice Consistency
cat("8. Creating choice consistency analysis...\n")

consistency_data <- data_analysis %>%
  group_by(monkey, CONDITION) %>%
  summarise(
    explore_rate = mean(outcome_clean == "explore"),
    n_trials = n(),
    .groups = "drop"
  ) %>%
  filter(n_trials >= 10) %>%
  mutate(
    consistency = 1 - 4 * explore_rate * (1 - explore_rate)  # 1 - variance for binary
  )

plot(consistency_data$explore_rate * 100, consistency_data$consistency,
     pch = 19, cex = 1.5, col = c("red", "blue", "green")[as.factor(consistency_data$CONDITION)],
     xlab = "Exploration Rate %", ylab = "Choice Consistency",
     main = "Exploration vs Consistency")

legend("bottomright", legend = c("Solo", "Duo", "Trio"), 
       col = c("red", "blue", "green"), pch = 19)

# PLOT 9: Temporal Learning Effects
cat("9. Creating temporal analysis...\n")

# Analyze by trial number within session
data_analysis <- data_analysis %>%
  arrange(monkey, date, as.numeric(BLOCK_No), TRIAL_NUM) %>%
  group_by(monkey, date, BLOCK_No) %>%
  mutate(trial_in_block = row_number()) %>%
  ungroup()

temporal_data <- data_analysis %>%
  filter(trial_in_block <= 10) %>%  # First 10 trials per block
  group_by(trial_in_block) %>%
  summarise(explore_rate = mean(outcome_clean == "explore") * 100, .groups = "drop")

plot(temporal_data$trial_in_block, temporal_data$explore_rate,
     type = "b", pch = 19, lwd = 2, col = "purple",
     main = "Learning Within Blocks",
     xlab = "Trial Number in Block", ylab = "Exploration %")

# PLOT 10: Relative Rank Effects
cat("10. Creating relative rank analysis...\n")

rank_effect <- data_analysis %>%
  group_by(RELATIVE_RANK) %>%
  summarise(
    explore_rate = mean(outcome_clean == "explore") * 100,
    n_trials = n(),
    .groups = "drop"
  )

barplot(rank_effect$explore_rate,
        names.arg = paste("Rank", rank_effect$RELATIVE_RANK),
        main = "Relative Rank Effects",
        ylab = "Exploration %", xlab = "Relative Rank Position",
        col = c("gold", "gray", "brown"))

# PLOT 11: Choice Transitions
cat("11. Creating choice transition analysis...\n")

# Create lagged data for transitions
data_transitions <- data_analysis %>%
  arrange(monkey, date, as.numeric(BLOCK_No), TRIAL_NUM) %>%
  group_by(monkey) %>%
  mutate(prev_choice = lag(outcome_clean)) %>%
  filter(!is.na(prev_choice)) %>%
  ungroup()

transition_table <- table(data_transitions$prev_choice, data_transitions$outcome_clean)
transition_props <- prop.table(transition_table, margin = 1) * 100

barplot(transition_props, beside = TRUE,
        main = "Choice Transitions",
        ylab = "Probability %", xlab = "Current Choice",
        legend.text = paste("After", rownames(transition_props)),
        col = c("lightgreen", "lightcoral", "lightblue"))

# PLOT 12: Summary Dashboard
cat("12. Creating summary dashboard...\n")

summary_data <- data_analysis %>%
  group_by(monkey, sex, rank) %>%
  summarise(
    explore = mean(outcome_clean == "explore") * 100,
    exploit = mean(outcome_clean == "exploit") * 100,
    none = mean(outcome_clean == "none") * 100,
    .groups = "drop"
  )

# Reorder by your preference
summary_data <- summary_data[match(monkey_order, summary_data$monkey), ]

barplot(t(as.matrix(summary_data[, c("explore", "exploit", "none")])),
        names.arg = summary_data$monkey,
        main = "Complete Choice Profiles",
        ylab = "Percentage", xlab = "Monkey",
        col = c("lightgreen", "lightcoral", "lightgray"),
        legend.text = c("Explore", "Exploit", "None"))

dev.off()

# DETAILED INSIGHTS
cat("\n=== KEY DATA INSIGHTS ===\n")

# Context sensitivity ranking
context_sensitivity <- data_analysis %>%
  group_by(monkey) %>%
  summarise(
    solo_rate = mean(outcome_clean[CONDITION == "solo"] == "explore", na.rm = TRUE) * 100,
    duo_rate = mean(outcome_clean[CONDITION == "duo"] == "explore", na.rm = TRUE) * 100,
    trio_rate = mean(outcome_clean[CONDITION == "trio"] == "explore", na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  mutate(
    context_range = pmax(solo_rate, duo_rate, trio_rate, na.rm = TRUE) - 
                   pmin(solo_rate, duo_rate, trio_rate, na.rm = TRUE)
  ) %>%
  arrange(desc(context_range))

cat("MOST CONTEXT-SENSITIVE MONKEYS:\n")
for(i in 1:3) {
  cat(sprintf("%s: %.1f%% range (%.1f%% solo, %.1f%% duo, %.1f%% trio)\n",
              context_sensitivity$monkey[i], context_sensitivity$context_range[i],
              context_sensitivity$solo_rate[i], context_sensitivity$duo_rate[i], 
              context_sensitivity$trio_rate[i]))
}

# Value sensitivity
if(sum(!is.na(data_analysis$SUBJECTIVE_CHOSEN_VALUE)) > 100) {
  value_sensitivity <- data_analysis %>%
    filter(!is.na(SUBJECTIVE_CHOSEN_VALUE)) %>%
    group_by(monkey) %>%
    summarise(
      value_corr = cor(SUBJECTIVE_CHOSEN_VALUE, as.numeric(outcome_clean == "explore"), 
                      use = "complete.obs"),
      .groups = "drop"
    ) %>%
    arrange(desc(abs(value_corr)))
  
  cat("\nMOST VALUE-SENSITIVE MONKEYS:\n")
  for(i in 1:min(3, nrow(value_sensitivity))) {
    cat(sprintf("%s: correlation = %.3f\n", 
                value_sensitivity$monkey[i], value_sensitivity$value_corr[i]))
  }
}

# Learning effects
learning_effects <- data_analysis %>%
  group_by(monkey) %>%
  mutate(trial_number = row_number()) %>%
  filter(trial_number <= 100) %>%  # First 100 trials
  summarise(
    early_explore = mean(outcome_clean[trial_number <= 50] == "explore", na.rm = TRUE) * 100,
    late_explore = mean(outcome_clean[trial_number > 50] == "explore", na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  mutate(learning_change = late_explore - early_explore) %>%
  arrange(desc(abs(learning_change)))

cat("\nSTRONGEST LEARNING EFFECTS:\n")
for(i in 1:3) {
  direction <- ifelse(learning_effects$learning_change[i] > 0, "INCREASED", "DECREASED")
  cat(sprintf("%s: %s exploration by %.1f%% (%.1f%% → %.1f%%)\n",
              learning_effects$monkey[i], direction, abs(learning_effects$learning_change[i]),
              learning_effects$early_explore[i], learning_effects$late_explore[i]))
}

# Save detailed results
write.csv(context_sensitivity, "context_sensitivity_detailed.csv", row.names = FALSE)
write.csv(summary_data, "complete_choice_profiles.csv", row.names = FALSE)

cat("\nGenerated comprehensive visualization: intuitive_data_insights.pdf\n")
cat("Contains 12 different analytical perspectives to answer your research questions!\n") 