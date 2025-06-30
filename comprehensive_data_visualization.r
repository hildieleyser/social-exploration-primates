# COMPREHENSIVE DATA VISUALIZATION AND INSIGHTS
# Creating intuitive plots to explore patterns and answer research questions

library(ggplot2)
library(dplyr)
library(reshape2)
library(RColorBrewer)

# Load the real data
data_raw <- read.csv("Explore Exploit Dataset.csv")

# Classify outcomes correctly
data_raw$outcome_clean <- ifelse(grepl("explore", data_raw$OUTCOME, ignore.case = TRUE), "explore",
                               ifelse(grepl("exploit", data_raw$OUTCOME, ignore.case = TRUE), "exploit",
                                     ifelse(data_raw$OUTCOME %in% c("NONE", "none", "stop"), "none", "other")))

# Filter to valid data
data_valid <- data_raw[!is.na(data_raw$outcome_clean) & data_raw$outcome_clean != "other", ]

# Add monkey characteristics
monkey_order <- c("FRAN", "CHOCOLAT", "DALI", "ICE", "EBI", "ANEMONE")
monkey_info <- data.frame(
  monkey = monkey_order,
  sex = c("Male", "Female", "Male", "Female", "Male", "Female"),
  rank = c("Dominant", "Dominant", "Intermediate", "Intermediate", "Subordinate", "Subordinate"),
  pair_type = c("Dom_Male", "Dom_Female", "Int_Male", "Int_Female", "Sub_Male", "Sub_Female")
)

data_analysis <- merge(data_valid, monkey_info, by = "monkey", all.x = TRUE)

cat("=== CREATING COMPREHENSIVE VISUALIZATIONS ===\n")

# PLOT 1: HEATMAP - Exploration by Monkey × Context
cat("Creating heatmap visualization...\n")

exploration_summary <- data_analysis %>%
  group_by(monkey, CONDITION) %>%
  summarise(explore_rate = mean(outcome_clean == "explore") * 100, .groups = "drop")

exploration_matrix <- dcast(exploration_summary, monkey ~ CONDITION, value.var = "explore_rate")
rownames(exploration_matrix) <- exploration_matrix$monkey
exploration_matrix <- exploration_matrix[, -1]

# Reorder rows by your preferred order
exploration_matrix <- exploration_matrix[monkey_order[monkey_order %in% rownames(exploration_matrix)], ]

pdf("comprehensive_visualizations.pdf", width = 20, height = 24)
layout(matrix(1:12, nrow = 4, ncol = 3, byrow = TRUE))

# Heatmap 1: Basic exploration rates
image(1:3, 1:6, t(as.matrix(exploration_matrix)), 
      col = colorRampPalette(c("white", "yellow", "orange", "red", "darkred"))(20),
      main = "Exploration Rates: Monkey × Social Context",
      xlab = "Social Context", ylab = "Monkey",
      xaxt = "n", yaxt = "n")
axis(1, at = 1:3, labels = colnames(exploration_matrix))
axis(2, at = 1:6, labels = rownames(exploration_matrix), las = 1)

# Add values to heatmap
for(i in 1:6) {
  for(j in 1:3) {
    text(j, i, paste0(round(exploration_matrix[i,j], 1), "%"), cex = 0.8)
  }
}

# PLOT 2: DECISION TRIANGLE - What choices do they make?
cat("Creating decision triangle...\n")

decision_summary <- data_analysis %>%
  group_by(monkey, sex, rank) %>%
  summarise(
    explore = mean(outcome_clean == "explore") * 100,
    exploit = mean(outcome_clean == "exploit") * 100,
    none = mean(outcome_clean == "none") * 100,
    .groups = "drop"
  )

plot(decision_summary$explore, decision_summary$exploit, 
     pch = 19, cex = 2, col = rainbow(6),
     xlim = c(0, 80), ylim = c(0, 80),
     xlab = "Exploration %", ylab = "Exploitation %",
     main = "Decision Space: Explore vs Exploit vs None")

# Add monkey labels
text(decision_summary$explore, decision_summary$exploit, 
     decision_summary$monkey, pos = 3, cex = 0.8)

# Add diagonal line (explore + exploit = 100%)
abline(a = 100, b = -1, col = "gray", lty = 2)
text(50, 45, "100% Active Choices", srt = -45, col = "gray")

# PLOT 3: TEMPORAL PATTERNS - Do they change over time?
cat("Creating temporal analysis...\n")

# Add trial order within each block/session
data_analysis <- data_analysis %>%
  arrange(monkey, date, BLOCK_No, TRIAL_NUM) %>%
  group_by(monkey) %>%
  mutate(trial_order = row_number()) %>%
  ungroup()

# Early vs late trials
data_analysis$trial_phase <- ifelse(data_analysis$trial_order <= median(data_analysis$trial_order), "Early", "Late")

temporal_summary <- data_analysis %>%
  group_by(monkey, trial_phase) %>%
  summarise(explore_rate = mean(outcome_clean == "explore") * 100, .groups = "drop")

temporal_wide <- dcast(temporal_summary, monkey ~ trial_phase, value.var = "explore_rate")

plot(temporal_wide$Early, temporal_wide$Late,
     pch = 19, cex = 2, col = rainbow(6),
     xlim = c(0, 80), ylim = c(0, 80),
     xlab = "Early Trials Exploration %", ylab = "Late Trials Exploration %",
     main = "Learning Effects: Early vs Late Exploration")

text(temporal_wide$Early, temporal_wide$Late, 
     temporal_wide$monkey, pos = 3, cex = 0.8)

abline(0, 1, col = "gray", lty = 2)
text(40, 35, "No Change", srt = 45, col = "gray")

# PLOT 4: VALUE-BASED DECISIONS - How do values influence choices?
cat("Creating value-based analysis...\n")

# Analyze choices based on subjective values
value_analysis <- data_analysis %>%
  filter(!is.na(SUBJECTIVE_CHOSEN_VALUE) & !is.na(subjective_exploit)) %>%
  mutate(
    value_diff = SUBJECTIVE_CHOSEN_VALUE - subjective_exploit,
    value_bin = cut(value_diff, breaks = 5, labels = c("Much_Lower", "Lower", "Similar", "Higher", "Much_Higher"))
  ) %>%
  group_by(value_bin) %>%
  summarise(
    explore_rate = mean(outcome_clean == "explore") * 100,
    n_trials = n(),
    .groups = "drop"
  ) %>%
  filter(n_trials >= 10)  # Only include bins with sufficient data

barplot(value_analysis$explore_rate, 
        names.arg = value_analysis$value_bin,
        main = "Exploration by Relative Value\n(Chosen - Exploit Value)",
        ylab = "Exploration %", xlab = "Value Difference",
        col = "steelblue")

# PLOT 5: RISK vs REWARD - Expected explore value effects
cat("Creating risk-reward analysis...\n")

risk_analysis <- data_analysis %>%
  filter(!is.na(expected_explore)) %>%
  mutate(expectation_bin = cut(expected_explore, breaks = 4, labels = c("Low", "Medium", "High", "Very_High"))) %>%
  group_by(monkey, expectation_bin) %>%
  summarise(explore_rate = mean(outcome_clean == "explore") * 100, .groups = "drop") %>%
  filter(!is.na(expectation_bin))

# Create matrix for plotting
risk_matrix_data <- dcast(risk_analysis, monkey ~ expectation_bin, value.var = "explore_rate")
if(nrow(risk_matrix_data) > 0) {
  rownames(risk_matrix_data) <- risk_matrix_data$monkey
  risk_matrix <- risk_matrix_data[, -1]
} else {
  risk_matrix <- data.frame()
}

# Reorder by monkey preference
if(nrow(risk_matrix) > 0) {
  risk_matrix <- risk_matrix[monkey_order[monkey_order %in% rownames(risk_matrix)], ]
  
  matplot(t(risk_matrix), type = "b", pch = 19, lwd = 2,
          main = "Response to Expected Explore Value",
          xlab = "Expected Value Level", ylab = "Exploration %",
          xaxt = "n", col = rainbow(nrow(risk_matrix)))
  
  axis(1, at = 1:ncol(risk_matrix), labels = colnames(risk_matrix))
  legend("topleft", legend = rownames(risk_matrix), col = rainbow(nrow(risk_matrix)), 
         lwd = 2, cex = 0.7)
}

# PLOT 6: SOCIAL INFLUENCE - Who influences whom?
cat("Creating social influence analysis...\n")

social_data <- data_analysis %>%
  filter(CONDITION %in% c("duo", "trio") & !is.na(PAIRED_WITH)) %>%
  group_by(monkey, PAIRED_WITH, CONDITION) %>%
  summarise(
    explore_rate = mean(outcome_clean == "explore") * 100,
    n_trials = n(),
    .groups = "drop"
  ) %>%
  filter(n_trials >= 5)

if(nrow(social_data) > 0) {
  # Create network-style plot
  plot(1:10, 1:10, type = "n", xlim = c(0, 10), ylim = c(0, 10),
       main = "Social Context Effects", xlab = "", ylab = "", axes = FALSE)
  
  # Add monkey positions in circle
  angles <- seq(0, 2*pi, length.out = 7)[-7]
  x_pos <- 5 + 3 * cos(angles)
  y_pos <- 5 + 3 * sin(angles)
  
  for(i in 1:6) {
    points(x_pos[i], y_pos[i], pch = 19, cex = 2, col = rainbow(6)[i])
    text(x_pos[i], y_pos[i], monkey_order[i], pos = 1, cex = 0.8)
  }
}

# PLOT 7: RANK DYNAMICS - How does relative position matter?
cat("Creating rank dynamics analysis...\n")

rank_dynamics <- data_analysis %>%
  group_by(monkey, RELATIVE_RANK, CONDITION) %>%
  summarise(explore_rate = mean(outcome_clean == "explore") * 100, .groups = "drop")

# Create rank transition plot
unique_monkeys <- unique(rank_dynamics$monkey)
colors_rank <- rainbow(length(unique_monkeys))

plot(1:3, rep(0, 3), type = "n", ylim = c(0, 80),
     main = "Exploration by Relative Rank Position",
     xlab = "Relative Rank (1=Dominant, 3=Subordinate)", 
     ylab = "Exploration %")

for(i in 1:length(unique_monkeys)) {
  monkey_data <- rank_dynamics[rank_dynamics$monkey == unique_monkeys[i], ]
  if(nrow(monkey_data) > 1) {
    lines(monkey_data$RELATIVE_RANK, monkey_data$explore_rate, 
          col = colors_rank[i], lwd = 2, type = "b", pch = 19)
  }
}

legend("topright", legend = unique_monkeys, col = colors_rank, lwd = 2, cex = 0.7)

# PLOT 8: CHOICE CONSISTENCY - How variable are individual choices?
cat("Creating choice consistency analysis...\n")

consistency_analysis <- data_analysis %>%
  group_by(monkey, CONDITION) %>%
  summarise(
    explore_rate = mean(outcome_clean == "explore"),
    explore_var = var(outcome_clean == "explore"),
    n_trials = n(),
    .groups = "drop"
  ) %>%
  mutate(consistency = 1 - explore_var)  # Higher = more consistent

plot(consistency_analysis$explore_rate * 100, consistency_analysis$consistency,
     pch = 19, cex = 2, col = rep(c("red", "blue", "green"), each = 6),
     xlim = c(0, 80), ylim = c(0, 1),
     xlab = "Exploration Rate %", ylab = "Choice Consistency",
     main = "Exploration Rate vs Choice Consistency")

# Add context legend
legend("topright", legend = c("Solo", "Duo", "Trio"), 
       col = c("red", "blue", "green"), pch = 19)

# PLOT 9: BLOCK EFFECTS - Learning within sessions
cat("Creating block effects analysis...\n")

block_analysis <- data_analysis %>%
  group_by(monkey, BLOCK_No) %>%
  summarise(
    explore_rate = mean(outcome_clean == "explore") * 100,
    block_size = n(),
    .groups = "drop"
  ) %>%
  filter(block_size >= 5)  # Only blocks with sufficient trials

# Plot learning curves
unique_monkeys_blocks <- unique(block_analysis$monkey)
plot(1:max(block_analysis$BLOCK_No), rep(0, max(block_analysis$BLOCK_No)), 
     type = "n", ylim = c(0, 100),
     main = "Learning Curves: Exploration Across Blocks",
     xlab = "Block Number", ylab = "Exploration %")

for(i in 1:length(unique_monkeys_blocks)) {
  monkey_data <- block_analysis[block_analysis$monkey == unique_monkeys_blocks[i], ]
  lines(monkey_data$BLOCK_No, monkey_data$explore_rate, 
        col = rainbow(length(unique_monkeys_blocks))[i], lwd = 2)
}

legend("topright", legend = unique_monkeys_blocks, 
       col = rainbow(length(unique_monkeys_blocks)), lwd = 2, cex = 0.7)

# PLOT 10: OUTCOME CONSEQUENCES - What happens after explore/exploit?
cat("Creating outcome consequences analysis...\n")

# Create lagged outcome analysis
data_lagged <- data_analysis %>%
  arrange(monkey, date, BLOCK_No, TRIAL_NUM) %>%
  group_by(monkey) %>%
  mutate(
    prev_outcome = lag(outcome_clean),
    next_outcome = lead(outcome_clean)
  ) %>%
  filter(!is.na(prev_outcome) & !is.na(next_outcome)) %>%
  ungroup()

transition_matrix <- table(data_lagged$prev_outcome, data_lagged$next_outcome)
transition_props <- prop.table(transition_matrix, margin = 1) * 100

# Plot transition probabilities
barplot(transition_props, beside = TRUE, 
        main = "Choice Transitions: What Follows What?",
        ylab = "Probability %", xlab = "Next Choice",
        legend.text = rownames(transition_props),
        col = c("lightgreen", "lightcoral", "lightblue"))

# PLOT 11: CONTEXTUAL FLEXIBILITY - How much do monkeys adapt?
cat("Creating contextual flexibility analysis...\n")

flexibility_analysis <- data_analysis %>%
  group_by(monkey) %>%
  summarise(
    solo_explore = mean(outcome_clean[CONDITION == "solo"] == "explore", na.rm = TRUE) * 100,
    duo_explore = mean(outcome_clean[CONDITION == "duo"] == "explore", na.rm = TRUE) * 100,
    trio_explore = mean(outcome_clean[CONDITION == "trio"] == "explore", na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  mutate(
    flexibility_score = pmax(solo_explore, duo_explore, trio_explore) - 
                       pmin(solo_explore, duo_explore, trio_explore, na.rm = TRUE)
  )

plot(flexibility_analysis$solo_explore, flexibility_analysis$flexibility_score,
     pch = 19, cex = 2, col = rainbow(nrow(flexibility_analysis)),
     xlab = "Solo Context Exploration %", ylab = "Contextual Flexibility Score",
     main = "Baseline Exploration vs Contextual Adaptation")

text(flexibility_analysis$solo_explore, flexibility_analysis$flexibility_score,
     flexibility_analysis$monkey, pos = 3, cex = 0.8)

# PLOT 12: SUMMARY DASHBOARD
cat("Creating summary dashboard...\n")

# Overall summary statistics
summary_stats <- data_analysis %>%
  group_by(monkey, sex, rank) %>%
  summarise(
    explore_rate = mean(outcome_clean == "explore") * 100,
    exploit_rate = mean(outcome_clean == "exploit") * 100,
    none_rate = mean(outcome_clean == "none") * 100,
    total_trials = n(),
    .groups = "drop"
  )

# Stacked bar chart
barplot(t(as.matrix(summary_stats[, c("explore_rate", "exploit_rate", "none_rate")])),
        names.arg = summary_stats$monkey,
        main = "Complete Choice Profile by Monkey",
        ylab = "Percentage", xlab = "Monkey",
        col = c("lightgreen", "lightcoral", "lightgray"),
        legend.text = c("Explore", "Exploit", "None"))

dev.off()

# CREATE INTERACTIVE-STYLE DATA SUMMARIES
cat("\n=== DETAILED DATA INSIGHTS ===\n")

# 1. Most extreme monkeys
cat("MOST EXTREME EXPLORATION PATTERNS:\n")
extreme_analysis <- summary_stats %>%
  arrange(desc(explore_rate))
cat("Highest explorer:", extreme_analysis$monkey[1], "(", round(extreme_analysis$explore_rate[1], 1), "%)\n")
cat("Lowest explorer:", extreme_analysis$monkey[nrow(extreme_analysis)], "(", round(extreme_analysis$explore_rate[nrow(extreme_analysis)], 1), "%)\n")

# 2. Context sensitivity
context_sensitivity <- data_analysis %>%
  group_by(monkey) %>%
  summarise(
    context_range = max(c(
      mean(outcome_clean[CONDITION == "solo"] == "explore", na.rm = TRUE),
      mean(outcome_clean[CONDITION == "duo"] == "explore", na.rm = TRUE), 
      mean(outcome_clean[CONDITION == "trio"] == "explore", na.rm = TRUE)
    ), na.rm = TRUE) - min(c(
      mean(outcome_clean[CONDITION == "solo"] == "explore", na.rm = TRUE),
      mean(outcome_clean[CONDITION == "duo"] == "explore", na.rm = TRUE),
      mean(outcome_clean[CONDITION == "trio"] == "explore", na.rm = TRUE)
    ), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(context_range))

cat("\nMOST CONTEXT-SENSITIVE MONKEYS:\n")
for(i in 1:3) {
  cat(context_sensitivity$monkey[i], ": ", round(context_sensitivity$context_range[i] * 100, 1), "% range\n")
}

# Save detailed results
write.csv(summary_stats, "detailed_monkey_profiles.csv", row.names = FALSE)
write.csv(context_sensitivity, "context_sensitivity_scores.csv", row.names = FALSE)

cat("\nGenerated comprehensive visualization file: comprehensive_visualizations.pdf\n")
cat("This includes 12 different analytical perspectives on your data!\n") 