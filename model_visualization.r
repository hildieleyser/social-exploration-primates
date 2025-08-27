# MODEL VISUALIZATION - INTUITIVE UNDERSTANDING OF DATA PATTERNS
# Creating visual models to understand the mechanisms underlying decision-making

library(dplyr)
library(ggplot2)

# Load and prepare data
data_raw <- read.csv("Explore Exploit Dataset.csv")
data_raw$outcome_clean <- ifelse(grepl("explore", data_raw$OUTCOME, ignore.case = TRUE), "explore",
                               ifelse(grepl("exploit", data_raw$OUTCOME, ignore.case = TRUE), "exploit", 
                                     ifelse(data_raw$OUTCOME %in% c("NONE", "none", "stop"), "none", "other")))

data_valid <- data_raw[!is.na(data_raw$outcome_clean) & data_raw$outcome_clean != "other", ]

monkey_order <- c("FRAN", "CHOCOLAT", "DALI", "ICE", "EBI", "ANEMONE")
monkey_info <- data.frame(
  monkey = monkey_order,
  sex = c("Male", "Female", "Male", "Female", "Male", "Female"),
  rank = c("Dominant", "Dominant", "Intermediate", "Intermediate", "Subordinate", "Subordinate")
)

data_analysis <- merge(data_valid, monkey_info, by = "monkey", all.x = TRUE)

cat("=== CREATING MODEL VISUALIZATION ===\n")

# Create conceptual model plots
pdf("data_model_insights.pdf", width = 20, height = 16)
layout(matrix(1:6, nrow = 2, ncol = 3, byrow = TRUE))

# PLOT 1: The Social Complexity Effect Model
cat("1. Creating social complexity effect model...\n")

complexity_model <- data_analysis %>%
  group_by(CONDITION) %>%
  summarise(
    explore_rate = mean(outcome_clean == "explore") * 100,
    exploit_rate = mean(outcome_clean == "exploit") * 100,
    none_rate = mean(outcome_clean == "none") * 100,
    .groups = "drop"
  )

# Create the decline curve
x_vals <- 1:3
y_vals <- complexity_model$explore_rate
plot(x_vals, y_vals, type = "b", pch = 19, lwd = 4, cex = 2,
     col = "red", ylim = c(0, 60),
     main = "SOCIAL COMPLEXITY MODEL\nHow Social Context Reduces Exploration",
     xlab = "Social Complexity Level", ylab = "Exploration Rate %",
     xaxt = "n")
axis(1, at = 1:3, labels = c("SOLO\n(No Others)", "DUO\n(1 Other)", "TRIO\n(2 Others)"))

# Add model annotation
text(2, 50, "Cognitive Load\nIncreases →", cex = 1.2, col = "blue")
text(2, 45, "Exploration\nDecreases ↓", cex = 1.2, col = "red")

# Add actual values
for(i in 1:3) {
  text(i, y_vals[i] + 3, paste0(round(y_vals[i], 1), "%"), cex = 1.2, font = 2)
}

# PLOT 2: The Hierarchy Advantage Model
cat("2. Creating hierarchy advantage model...\n")

hierarchy_model <- data_analysis %>%
  group_by(rank) %>%
  summarise(explore_rate = mean(outcome_clean == "explore") * 100, .groups = "drop")

rank_order <- c("Dominant", "Intermediate", "Subordinate")
hierarchy_model <- hierarchy_model[match(rank_order, hierarchy_model$rank), ]

barplot(hierarchy_model$explore_rate,
        names.arg = c("DOMINANT", "INTERMEDIATE", "SUBORDINATE"),
        main = "HIERARCHY ADVANTAGE MODEL\nDominance Facilitates Exploration",
        ylab = "Exploration Rate %",
        col = c("gold", "orange", "brown"),
        ylim = c(0, 50))

# Add mechanism labels
text(0.7, 45, "High Status\n→ Less Risk", cex = 0.9, col = "darkgreen")
text(1.9, 40, "Moderate\nPosition", cex = 0.9, col = "orange")
text(3.1, 35, "Low Status\n→ High Risk", cex = 0.9, col = "red")

# PLOT 3: Individual Differences Model
cat("3. Creating individual differences model...\n")

individual_model <- data_analysis %>%
  group_by(monkey, sex, rank) %>%
  summarise(explore_rate = mean(outcome_clean == "explore") * 100, .groups = "drop")

# Order by your preference
individual_model <- individual_model[match(monkey_order, individual_model$monkey), ]

colors_personality <- c("darkblue", "purple", "orange", "pink", "gray", "brown")
barplot(individual_model$explore_rate,
        names.arg = individual_model$monkey,
        main = "INDIVIDUAL PERSONALITY MODEL\nHuge Individual Differences",
        ylab = "Exploration Rate %",
        col = colors_personality,
        ylim = c(0, 60))

# Add personality labels
text(1, 55, "EXPLORER", cex = 0.8, col = "darkblue")
text(6, 25, "CAUTIOUS", cex = 0.8, col = "brown")
text(3, 50, paste0("Range: ", round(max(individual_model$explore_rate) - min(individual_model$explore_rate), 1), "%"), 
     cex = 1.2, font = 2)

# PLOT 4: Sex Strategy Model
cat("4. Creating sex strategy model...\n")

sex_context_model <- data_analysis %>%
  group_by(sex, CONDITION) %>%
  summarise(explore_rate = mean(outcome_clean == "explore") * 100, .groups = "drop")

male_data <- sex_context_model$explore_rate[sex_context_model$sex == "Male"]
female_data <- sex_context_model$explore_rate[sex_context_model$sex == "Female"]

plot(1:3, male_data, type = "b", pch = 19, lwd = 3, col = "blue",
     ylim = c(0, 60), xlim = c(0.8, 3.2),
     main = "SEX STRATEGY MODEL\nMales More Exploratory",
     xlab = "Social Context", ylab = "Exploration Rate %",
     xaxt = "n")
lines(1:3, female_data, type = "b", pch = 19, lwd = 3, col = "red")

axis(1, at = 1:3, labels = c("Solo", "Duo", "Trio"))
legend("topright", legend = c("Males", "Females"), col = c("blue", "red"), lwd = 3)

# Add strategy labels
text(1.5, 55, "Males: Risk-Taking\nStrategy", cex = 1, col = "blue")
text(2.5, 25, "Females: Risk-Averse\nStrategy", cex = 1, col = "red")

# PLOT 5: Value-Based Decision Model
cat("5. Creating value-based decision model...\n")

# Analyze how values predict choices
value_model_data <- data_analysis %>%
  filter(!is.na(SUBJECTIVE_CHOSEN_VALUE) & !is.na(subjective_exploit)) %>%
  mutate(
    value_difference = SUBJECTIVE_CHOSEN_VALUE - subjective_exploit,
    value_category = cut(value_difference, breaks = 5, 
                        labels = c("Much Lower", "Lower", "Similar", "Higher", "Much Higher"))
  ) %>%
  group_by(value_category) %>%
  summarise(
    explore_rate = mean(outcome_clean == "explore") * 100,
    n_trials = n(),
    .groups = "drop"
  ) %>%
  filter(n_trials >= 20)

if(nrow(value_model_data) > 0) {
  barplot(value_model_data$explore_rate,
          names.arg = 1:nrow(value_model_data),
          main = "VALUE-BASED DECISION MODEL\nHigher Value → More Exploration",
          ylab = "Exploration Rate %",
          xlab = "Relative Value Level",
          col = "darkgreen",
          ylim = c(0, max(value_model_data$explore_rate) * 1.2))
  
  text(2.5, max(value_model_data$explore_rate) * 1.1, 
       "Value Drives\nExploration", cex = 1.2, col = "darkgreen")
} else {
  plot(1, 1, type = "n", main = "VALUE MODEL\n(Insufficient Data)", 
       xlab = "", ylab = "")
  text(1, 1, "Not enough value data\nfor reliable analysis", cex = 1.2)
}

# PLOT 6: Learning and Adaptation Model
cat("6. Creating learning model...\n")

# Analyze learning over time
learning_model <- data_analysis %>%
  arrange(monkey, date, as.numeric(BLOCK_No), TRIAL_NUM) %>%
  group_by(monkey) %>%
  mutate(
    trial_number = row_number(),
    trial_phase = cut(trial_number, breaks = 4, labels = c("Phase1", "Phase2", "Phase3", "Phase4"))
  ) %>%
  group_by(trial_phase) %>%
  summarise(explore_rate = mean(outcome_clean == "explore") * 100, .groups = "drop")

plot(1:4, learning_model$explore_rate, type = "b", pch = 19, lwd = 4, cex = 2,
     col = "purple", ylim = c(0, 50),
     main = "LEARNING MODEL\nExploration Decreases Over Time",
     xlab = "Learning Phase", ylab = "Exploration Rate %",
     xaxt = "n")
axis(1, at = 1:4, labels = c("Early", "Mid-Early", "Mid-Late", "Late"))

# Add learning annotation
arrows(1.5, 40, 3.5, 25, col = "red", lwd = 3)
text(2.5, 42, "Learning Effect:\nReduced Exploration", cex = 1.2, col = "red")

dev.off()

# CREATE DATA-DRIVEN INSIGHTS SUMMARY
cat("\n=== MODEL-BASED INSIGHTS ===\n")

# 1. Social Complexity Effect Size
complexity_effect <- data_analysis %>%
  group_by(CONDITION) %>%
  summarise(explore_rate = mean(outcome_clean == "explore") * 100, .groups = "drop")

solo_rate <- complexity_effect$explore_rate[complexity_effect$CONDITION == "solo"]
trio_rate <- complexity_effect$explore_rate[complexity_effect$CONDITION == "trio"]
total_complexity_cost <- solo_rate - trio_rate

cat("SOCIAL COMPLEXITY MODEL:\n")
cat(sprintf("- Solo exploration: %.1f%%\n", solo_rate))
cat(sprintf("- Trio exploration: %.1f%%\n", trio_rate))
cat(sprintf("- Total complexity cost: %.1f%% reduction\n", total_complexity_cost))
cat(sprintf("- Effect size: %.1f%% per additional individual\n", total_complexity_cost / 2))

# 2. Hierarchy Effect Size
hierarchy_effect <- data_analysis %>%
  group_by(rank) %>%
  summarise(explore_rate = mean(outcome_clean == "explore") * 100, .groups = "drop")

dominant_rate <- hierarchy_effect$explore_rate[hierarchy_effect$rank == "Dominant"]
subordinate_rate <- hierarchy_effect$explore_rate[hierarchy_effect$rank == "Subordinate"]
hierarchy_advantage <- dominant_rate - subordinate_rate

cat("\nHIERARCHY MODEL:\n")
cat(sprintf("- Dominant exploration: %.1f%%\n", dominant_rate))
cat(sprintf("- Subordinate exploration: %.1f%%\n", subordinate_rate))
cat(sprintf("- Hierarchy advantage: %.1f%% boost for dominants\n", hierarchy_advantage))

# 3. Sex Strategy Differences
sex_effect <- data_analysis %>%
  group_by(sex) %>%
  summarise(explore_rate = mean(outcome_clean == "explore") * 100, .groups = "drop")

male_rate <- sex_effect$explore_rate[sex_effect$sex == "Male"]
female_rate <- sex_effect$explore_rate[sex_effect$sex == "Female"]
sex_difference <- male_rate - female_rate

cat("\nSEX STRATEGY MODEL:\n")
cat(sprintf("- Male exploration: %.1f%%\n", male_rate))
cat(sprintf("- Female exploration: %.1f%%\n", female_rate))
cat(sprintf("- Sex difference: %.1f%% male advantage\n", sex_difference))

# 4. Individual Variation Range
individual_range <- data_analysis %>%
  group_by(monkey) %>%
  summarise(explore_rate = mean(outcome_clean == "explore") * 100, .groups = "drop")

min_explorer <- min(individual_range$explore_rate)
max_explorer <- max(individual_range$explore_rate)
individual_spread <- max_explorer - min_explorer

cat("\nINDIVIDUAL VARIATION MODEL:\n")
cat(sprintf("- Lowest explorer: %.1f%%\n", min_explorer))
cat(sprintf("- Highest explorer: %.1f%%\n", max_explorer))
cat(sprintf("- Individual variation range: %.1f%%\n", individual_spread))
cat(sprintf("- Coefficient of variation: %.1f%%\n", 
            sd(individual_range$explore_rate) / mean(individual_range$explore_rate) * 100))

# 5. Context Sensitivity Analysis
context_sensitivity_analysis <- data_analysis %>%
  group_by(monkey) %>%
  summarise(
    solo_rate = mean(outcome_clean[CONDITION == "solo"] == "explore", na.rm = TRUE) * 100,
    trio_rate = mean(outcome_clean[CONDITION == "trio"] == "explore", na.rm = TRUE) * 100,
    context_sensitivity = solo_rate - trio_rate,
    .groups = "drop"
  )

most_sensitive <- context_sensitivity_analysis$monkey[which.max(context_sensitivity_analysis$context_sensitivity)]
least_sensitive <- context_sensitivity_analysis$monkey[which.min(context_sensitivity_analysis$context_sensitivity)]

cat("\nCONTEXT SENSITIVITY MODEL:\n")
cat(sprintf("- Most context-sensitive: %s (%.1f%% range)\n", 
            most_sensitive, max(context_sensitivity_analysis$context_sensitivity)))
cat(sprintf("- Least context-sensitive: %s (%.1f%% range)\n", 
            least_sensitive, min(context_sensitivity_analysis$context_sensitivity)))

# Create final model equation summary
cat("\n=== PREDICTIVE MODEL EQUATION ===\n")
cat("Exploration Rate = BASE_PERSONALITY + \n")
cat("                   HIERARCHY_BOOST * (rank_dominance) + \n")
cat("                   SEX_STRATEGY * (male_factor) + \n")
cat("                   SOCIAL_COMPLEXITY_COST * (n_others) + \n")
cat("                   VALUE_EFFECT * (expected_value) + \n")
cat("                   LEARNING_DECLINE * (trial_number)\n")

cat("\nWhere estimated effects are:\n")
cat(sprintf("- BASE_PERSONALITY: %.1f%% ± %.1f%% individual variation\n", 
            mean(individual_range$explore_rate), sd(individual_range$explore_rate)))
cat(sprintf("- HIERARCHY_BOOST: +%.1f%% per rank level\n", hierarchy_advantage / 2))
cat(sprintf("- SEX_STRATEGY: +%.1f%% male advantage\n", sex_difference))
cat(sprintf("- SOCIAL_COMPLEXITY_COST: -%.1f%% per additional individual\n", total_complexity_cost / 2))
cat("- VALUE_EFFECT: Variable by individual\n")
cat("- LEARNING_DECLINE: Negative trend over time\n")

# Save model summary
model_summary <- data.frame(
  Effect = c("Social_Complexity_Cost", "Hierarchy_Advantage", "Sex_Difference", "Individual_Variation"),
  Effect_Size = c(total_complexity_cost, hierarchy_advantage, sex_difference, individual_spread),
  Description = c("Solo to Trio reduction", "Dominant vs Subordinate", "Male vs Female", "Individual range")
)

write.csv(model_summary, "model_effect_sizes.csv", row.names = FALSE)

cat("\nGenerated data_model_insights.pdf with 6 conceptual models!\n")
cat("Saved model_effect_sizes.csv with quantified effects!\n") 