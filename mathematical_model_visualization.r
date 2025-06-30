# MATHEMATICAL MODEL VISUALIZATION AND IMPROVED GRAPHS
# Creating visual representation of the mathematical model plus enhanced specific plots

library(dplyr)
library(ggplot2)
library(nnet)

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
  rank = c("Dominant", "Dominant", "Intermediate", "Intermediate", "Subordinate", "Subordinate"),
  absolute_rank = c(1, 2, 3, 4, 5, 6)  # FRAN=1 (highest), ANEMONE=6 (lowest)
)

data_analysis <- merge(data_valid, monkey_info, by = "monkey", all.x = TRUE)

cat("=== MATHEMATICAL MODEL VISUALIZATION ===\n")

pdf("mathematical_model_and_improved_graphs.pdf", width = 20, height = 24)
layout(matrix(1:12, nrow = 4, ncol = 3, byrow = TRUE))

# PLOT 1: MATHEMATICAL MODEL VISUALIZATION
cat("1. Creating mathematical model visualization...\n")

# Fit actual multinomial model to get coefficients
data_model <- data_analysis %>%
  filter(!is.na(RELATIVE_RANK) & !is.na(expected_explore)) %>%
  mutate(
    social_complexity = as.numeric(factor(CONDITION, levels = c("solo", "duo", "trio"))) - 1,
    sex_numeric = as.numeric(sex == "Male"),
    rank_numeric = 4 - RELATIVE_RANK,  # Higher = more dominant
    expected_explore_scaled = scale(expected_explore)[,1]
  )

# Fit multinomial model
model_fit <- multinom(outcome_clean ~ social_complexity + sex_numeric + rank_numeric + expected_explore_scaled, 
                     data = data_model, trace = FALSE)

# Extract coefficients
coeffs <- summary(model_fit)$coefficients

# Create model equation visualization
plot(1:10, 1:10, type = "n", xlim = c(0, 10), ylim = c(0, 10),
     main = "MATHEMATICAL MODEL STRUCTURE\nTrinomial Decision Model", 
     xlab = "", ylab = "", axes = FALSE)

# Draw model structure
# Input variables
text(1, 8, "INPUTS:", cex = 1.2, font = 2)
text(1, 7.2, "• Social Complexity", cex = 1)
text(1, 6.6, "• Sex (Male/Female)", cex = 1)
text(1, 6.0, "• Hierarchy Rank", cex = 1)
text(1, 5.4, "• Expected Value", cex = 1)

# Model equation
text(5, 8, "MODEL:", cex = 1.2, font = 2)
rect(3, 4, 7, 7.5, border = "black", lwd = 2)
text(5, 6.8, "Multinomial Logit", cex = 1.1, font = 2)
text(5, 6.2, "log(P(explore)/P(none))", cex = 0.9)
text(5, 5.8, "= β₀ + β₁×Social + β₂×Sex", cex = 0.9)
text(5, 5.4, "  + β₃×Rank + β₄×Value", cex = 0.9)
text(5, 4.8, "log(P(exploit)/P(none))", cex = 0.9)
text(5, 4.4, "= γ₀ + γ₁×Social + γ₂×Sex", cex = 0.9)

# Outputs
text(9, 8, "OUTPUTS:", cex = 1.2, font = 2)
text(9, 7.2, "P(Explore)", cex = 1, col = "green")
text(9, 6.6, "P(Exploit)", cex = 1, col = "red")
text(9, 6.0, "P(None)", cex = 1, col = "blue")

# Add arrows
arrows(2.5, 6.5, 2.8, 6.5, lwd = 2)
arrows(7.2, 6.5, 8.5, 6.5, lwd = 2)

# PLOT 2: MODEL COEFFICIENTS VISUALIZATION
cat("2. Creating model coefficients plot...\n")

# Extract and plot coefficients
coeff_explore <- coeffs[1, ]  # explore vs none
coeff_exploit <- coeffs[2, ]  # exploit vs none

barplot(rbind(coeff_explore, coeff_exploit), beside = TRUE,
        names.arg = c("Intercept", "Social", "Sex", "Rank", "Value"),
        main = "MODEL COEFFICIENTS\nEffect Sizes on Decision Probabilities",
        ylab = "Log-Odds Coefficient",
        col = c("green", "red"),
        legend.text = c("Explore vs None", "Exploit vs None"))

# PLOT 3: IMPROVED - Exploration by Relative Rank Position
cat("3. Creating improved relative rank plot...\n")

relative_rank_data <- data_analysis %>%
  group_by(RELATIVE_RANK, monkey) %>%
  summarise(explore_rate = mean(outcome_clean == "explore") * 100, .groups = "drop")

# Create individual monkey lines
plot(1:3, rep(0, 3), type = "n", ylim = c(0, 80),
     main = "EXPLORATION BY RELATIVE RANK POSITION\n(Within-Context Dominance)",
     xlab = "Relative Rank (1=Dominant in context, 3=Subordinate)", 
     ylab = "Exploration Rate %",
     xaxt = "n")
axis(1, at = 1:3, labels = c("Dominant\n(in context)", "Middle\n(in context)", "Subordinate\n(in context)"))

colors_monkey <- rainbow(6)
for(i in 1:6) {
  monkey <- monkey_order[i]
  monkey_data <- relative_rank_data[relative_rank_data$monkey == monkey, ]
  if(nrow(monkey_data) > 1) {
    lines(monkey_data$RELATIVE_RANK, monkey_data$explore_rate, 
          col = colors_monkey[i], lwd = 3, type = "b", pch = 19)
  }
}
legend("topright", legend = monkey_order, col = colors_monkey, lwd = 2, cex = 0.7)

# Add average trend
avg_by_rank <- relative_rank_data %>%
  group_by(RELATIVE_RANK) %>%
  summarise(avg_explore = mean(explore_rate), .groups = "drop")
lines(avg_by_rank$RELATIVE_RANK, avg_by_rank$avg_explore, lwd = 5, col = "black", lty = 2)

# PLOT 4: NEW - Exploration by Absolute Rank Position  
cat("4. Creating absolute rank plot...\n")

absolute_rank_data <- data_analysis %>%
  group_by(monkey, absolute_rank) %>%
  summarise(explore_rate = mean(outcome_clean == "explore") * 100, .groups = "drop") %>%
  arrange(absolute_rank)

barplot(absolute_rank_data$explore_rate,
        names.arg = absolute_rank_data$monkey,
        main = "EXPLORATION BY ABSOLUTE RANK POSITION\n(Overall Social Status)",
        ylab = "Exploration Rate %",
        xlab = "Monkey (1=Highest Status → 6=Lowest Status)",
        col = c("gold", "orange", "yellow", "lightgreen", "lightblue", "gray"))

# Add rank labels
text(0.7, 55, "Rank 1\n(Alpha)", cex = 0.8)
text(6.1, 25, "Rank 6\n(Omega)", cex = 0.8)

# PLOT 5: IMPROVED - Response to Expected Explore Value (with sample sizes)
cat("5. Creating improved expected value response...\n")

expected_value_data <- data_analysis %>%
  filter(!is.na(expected_explore)) %>%
  mutate(value_quintile = cut(expected_explore, breaks = 5, labels = 1:5)) %>%
  group_by(value_quintile) %>%
  summarise(
    explore_rate = mean(outcome_clean == "explore") * 100,
    n_trials = n(),
    se = sd(outcome_clean == "explore") / sqrt(n()) * 100,
    .groups = "drop"
  ) %>%
  filter(n_trials >= 20)

barplot(expected_value_data$explore_rate,
        names.arg = paste("Q", expected_value_data$value_quintile, "\n(n=", expected_value_data$n_trials, ")", sep=""),
        main = "RESPONSE TO EXPECTED EXPLORE VALUE\nHigher Expectations → More Exploration",
        ylab = "Exploration Rate %",
        xlab = "Expected Value Quintile (with sample sizes)",
        col = "darkgreen",
        ylim = c(0, max(expected_value_data$explore_rate) * 1.2))

# Add error bars
for(i in 1:nrow(expected_value_data)) {
  arrows((i-1)*1.2 + 0.7, expected_value_data$explore_rate[i] - expected_value_data$se[i],
         (i-1)*1.2 + 0.7, expected_value_data$explore_rate[i] + expected_value_data$se[i],
         angle = 90, code = 3, length = 0.1)
}

# PLOT 6: IMPROVED - Learning Effects with Statistics
cat("6. Creating improved learning effects plot...\n")

learning_data <- data_analysis %>%
  arrange(monkey, date, as.numeric(BLOCK_No), TRIAL_NUM) %>%
  group_by(monkey) %>%
  mutate(trial_number = row_number()) %>%
  filter(trial_number <= 200) %>%  # First 200 trials
  mutate(learning_phase = ifelse(trial_number <= 100, "Early", "Late")) %>%
  group_by(monkey, learning_phase) %>%
  summarise(explore_rate = mean(outcome_clean == "explore") * 100, .groups = "drop")

learning_wide <- reshape2::dcast(learning_data, monkey ~ learning_phase, value.var = "explore_rate")

plot(learning_wide$Early, learning_wide$Late,
     pch = 19, cex = 2, col = rainbow(6),
     xlim = c(0, 80), ylim = c(0, 80),
     xlab = "Early Trials Exploration %", ylab = "Late Trials Exploration %",
     main = "LEARNING EFFECTS: EARLY vs LATE EXPLORATION\nPoints Below Line = Learning (Reduced Exploration)")

text(learning_wide$Early, learning_wide$Late, 
     learning_wide$monkey, pos = 3, cex = 0.8)

abline(0, 1, col = "gray", lty = 2, lwd = 2)
text(40, 35, "No Change Line", srt = 45, col = "gray")

# Add learning statistics
below_line <- sum(learning_wide$Late < learning_wide$Early, na.rm = TRUE)
total_monkeys <- nrow(learning_wide)
text(60, 20, paste("Learning Effect:", below_line, "of", total_monkeys, "monkeys\nreduced exploration"), 
     cex = 1.1, col = "red")

# PLOT 7: IMPROVED - Complete Choice Profiles with percentages
cat("7. Creating improved choice profiles...\n")

choice_profiles <- data_analysis %>%
  group_by(monkey) %>%
  summarise(
    explore = mean(outcome_clean == "explore") * 100,
    exploit = mean(outcome_clean == "exploit") * 100,
    none = mean(outcome_clean == "none") * 100,
    .groups = "drop"
  )

choice_profiles <- choice_profiles[match(monkey_order, choice_profiles$monkey), ]

choice_matrix <- t(as.matrix(choice_profiles[, c("explore", "exploit", "none")]))
barplot(choice_matrix,
        names.arg = choice_profiles$monkey,
        main = "COMPLETE CHOICE PROFILES\nExplore (Green) | Exploit (Red) | None (Gray)",
        ylab = "Percentage of Choices", xlab = "Monkey",
        col = c("lightgreen", "lightcoral", "lightgray"),
        legend.text = c("Explore", "Exploit", "None"))

# Add percentage labels
for(i in 1:6) {
  text(i*1.2 - 0.5, choice_profiles$explore[i]/2, 
       paste0(round(choice_profiles$explore[i], 1), "%"), cex = 0.7)
}

# PLOT 8: IMPROVED - Individual Exploration Profiles with context labels
cat("8. Creating improved individual profiles...\n")

individual_context_data <- data_analysis %>%
  group_by(monkey, CONDITION) %>%
  summarise(explore_rate = mean(outcome_clean == "explore") * 100, .groups = "drop")

plot(1:3, rep(0, 3), type = "n", ylim = c(0, 80),
     main = "INDIVIDUAL EXPLORATION PROFILES\nHow Each Monkey Responds to Social Context",
     xlab = "Social Context", ylab = "Exploration Rate %", xaxt = "n")
axis(1, at = 1:3, labels = c("SOLO\n(Alone)", "DUO\n(1 Partner)", "TRIO\n(2 Partners)"))

colors_ind <- rainbow(6)
for(i in 1:6) {
  monkey <- monkey_order[i]
  monkey_data <- individual_context_data[individual_context_data$monkey == monkey, ]
  monkey_data <- monkey_data[match(c("solo", "duo", "trio"), monkey_data$CONDITION), ]
  if(nrow(monkey_data) == 3) {
    lines(1:3, monkey_data$explore_rate, col = colors_ind[i], lwd = 3, type = "b", pch = 19)
    # Add monkey label at trio point
    text(3.1, monkey_data$explore_rate[3], monkey, cex = 0.8, col = colors_ind[i])
  }
}

# PLOT 9: IMPROVED - Decision Space with "None" Analysis
cat("9. Creating improved decision space analysis...\n")

decision_space_data <- data_analysis %>%
  group_by(monkey, sex, rank) %>%
  summarise(
    explore_pct = mean(outcome_clean == "explore") * 100,
    exploit_pct = mean(outcome_clean == "exploit") * 100,
    none_pct = mean(outcome_clean == "none") * 100,
    .groups = "drop"
  )

# Size of points represents "none" percentage
plot(decision_space_data$explore_pct, decision_space_data$exploit_pct,
     pch = 19, cex = decision_space_data$none_pct/10,  # Scale by none percentage
     col = rainbow(6),
     xlim = c(0, 60), ylim = c(0, 50),
     xlab = "Exploration %", ylab = "Exploitation %",
     main = 'DECISION SPACE: WHY DO THEY CHOOSE "NONE"?\nPoint Size = % of "None" Choices')

text(decision_space_data$explore_pct, decision_space_data$exploit_pct,
     decision_space_data$monkey, pos = 3, cex = 0.8)

# Add diagonal reference lines
abline(a = 100, b = -1, col = "gray", lty = 2)
text(30, 65, "100% Active\nChoices", srt = -45, col = "gray")

# Add none percentage legend
text(50, 10, '"NONE" CHOICE ANALYSIS:\nLarger points = More "none" choices\nSuggests decision fatigue\nor uncertainty', 
     cex = 1, col = "blue")

# PLOT 10: IMPROVED - Choice Consistency with Individual Labels
cat("10. Creating improved choice consistency plot...\n")

consistency_detailed <- data_analysis %>%
  group_by(monkey, CONDITION) %>%
  summarise(
    explore_rate = mean(outcome_clean == "explore"),
    n_trials = n(),
    .groups = "drop"
  ) %>%
  filter(n_trials >= 10) %>%
  mutate(
    consistency = 1 - 4 * explore_rate * (1 - explore_rate),  # 1 - variance
    context_label = paste(monkey, CONDITION, sep = "-")
  )

colors_context <- c("red", "blue", "green")
context_nums <- as.numeric(factor(consistency_detailed$CONDITION, levels = c("solo", "duo", "trio")))

plot(consistency_detailed$explore_rate * 100, consistency_detailed$consistency,
     pch = 19, cex = 1.5, col = colors_context[context_nums],
     xlab = "Exploration Rate %", ylab = "Choice Consistency (1 = Always Same Choice)",
     main = "EXPLORATION vs CHOICE CONSISTENCY\nEach Point = One Monkey in One Context")

# Add text labels for each point
text(consistency_detailed$explore_rate * 100, consistency_detailed$consistency,
     consistency_detailed$context_label, pos = 1, cex = 0.6)

legend("bottomright", legend = c("Solo", "Duo", "Trio"), 
       col = colors_context, pch = 19, title = "Context")

# PLOT 11: AIC Model Comparison
cat("11. Creating AIC model comparison...\n")

# Fit different models for comparison
data_for_models <- data_analysis %>%
  filter(!is.na(RELATIVE_RANK) & !is.na(expected_explore)) %>%
  mutate(
    social_complexity = as.numeric(factor(CONDITION, levels = c("solo", "duo", "trio"))) - 1,
    sex_numeric = as.numeric(sex == "Male"),
    rank_numeric = 4 - RELATIVE_RANK
  )

# Fit multiple models
model1 <- multinom(outcome_clean ~ 1, data = data_for_models, trace = FALSE)  # Null
model2 <- multinom(outcome_clean ~ social_complexity, data = data_for_models, trace = FALSE)  # Social only
model3 <- multinom(outcome_clean ~ social_complexity + sex_numeric, data = data_for_models, trace = FALSE)  # Social + Sex
model4 <- multinom(outcome_clean ~ social_complexity + sex_numeric + rank_numeric, data = data_for_models, trace = FALSE)  # Full without value
model5 <- multinom(outcome_clean ~ social_complexity + sex_numeric + rank_numeric + scale(expected_explore), data = data_for_models, trace = FALSE)  # Full model

# Calculate AIC values
aic_values <- c(AIC(model1), AIC(model2), AIC(model3), AIC(model4), AIC(model5))
model_names <- c("Null", "Social Only", "Social+Sex", "Social+Sex+Rank", "Full Model")

barplot(aic_values, names.arg = model_names,
        main = "MODEL COMPARISON: AIC VALUES\nLower AIC = Better Model Fit",
        ylab = "AIC Value", xlab = "Model",
        col = c("red", "orange", "yellow", "lightgreen", "darkgreen"),
        las = 2)

# Add AIC differences
best_aic <- min(aic_values)
aic_diff <- aic_values - best_aic
text(1:5, aic_values + 10, paste("Δ =", round(aic_diff, 1)), cex = 0.8)

# PLOT 12: Model Predictions vs Observed
cat("12. Creating model validation plot...\n")

# Get predictions from best model
predictions <- predict(model5, type = "probs")
observed_rates <- table(data_for_models$outcome_clean) / nrow(data_for_models) * 100
predicted_rates <- colMeans(predictions) * 100

comparison_data <- data.frame(
  Outcome = names(observed_rates),
  Observed = as.numeric(observed_rates),
  Predicted = predicted_rates[names(observed_rates)]
)

barplot(t(as.matrix(comparison_data[, c("Observed", "Predicted")])),
        beside = TRUE, names.arg = comparison_data$Outcome,
        main = "MODEL VALIDATION\nObserved vs Predicted Choice Rates",
        ylab = "Percentage", xlab = "Choice Type",
        col = c("blue", "red"),
        legend.text = c("Observed", "Predicted"))

dev.off()

# SAVE MODEL COMPARISON RESULTS
model_comparison <- data.frame(
  Model = model_names,
  AIC = aic_values,
  Delta_AIC = aic_values - min(aic_values),
  Weight = exp(-0.5 * (aic_values - min(aic_values))) / sum(exp(-0.5 * (aic_values - min(aic_values))))
)

write.csv(model_comparison, "model_aic_comparison.csv", row.names = FALSE)

cat("\nGenerated mathematical_model_and_improved_graphs.pdf with all requested visualizations!\n")
cat("Saved model_aic_comparison.csv with AIC comparison results!\n") 