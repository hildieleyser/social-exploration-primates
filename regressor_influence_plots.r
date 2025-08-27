# REGRESSOR INFLUENCE PLOTS - SHOWING WHICH VARIABLES DRIVE DECISIONS
# Analyzing how y03, y04, y05, y06 influence explore vs exploit choices

library(nnet)
library(dplyr)
library(ggplot2)

# Load the fitted model
model_trinomial <- readRDS("trinomial_model.rds")

cat("=== REGRESSOR INFLUENCE ANALYSIS ===\n")
cat("Examining how each predictor variable influences explore vs exploit decisions\n\n")

# Set baseline values (will vary one at a time)
baseline_values <- list(
  y10 = factor("solo", levels = c("solo", "duo", "trio")),
  y03 = 2,     # Middle rank
  y04 = 0.5,   # Middle subjective chosen value
  y05 = 0.5,   # Middle subjective exploit value
  y06 = 0.6,   # Middle expected explore value
  monkey_id = factor("FRAN", levels = c("ANEMONE", "CHOCOLAT", "DALI", "EBI", "FRAN", "ICE"))
)

# 1) RELATIVE RANK (y03) INFLUENCE
cat("=== 1) RELATIVE RANK (y03) INFLUENCE ===\n")
rank_values <- seq(1, 3, by = 0.5)  # 1=dominant, 2=intermediate, 3=subordinate

rank_pred_data <- data.frame(
  y10 = baseline_values$y10,
  y03 = rank_values,
  y04 = baseline_values$y04,
  y05 = baseline_values$y05,
  y06 = baseline_values$y06,
  monkey_id = baseline_values$monkey_id
)

rank_predictions <- predict(model_trinomial, newdata = rank_pred_data, type = "probs")

rank_results <- data.frame(
  y03_rank = rank_values,
  exploit = rank_predictions[, "exploit"],
  explore = rank_predictions[, "explore"],
  none = rank_predictions[, "none"]
)

cat("Rank Effects on Choice Probability:\n")
print(rank_results)

# 2) SUBJECTIVE CHOSEN VALUE (y04) INFLUENCE  
cat("\n=== 2) SUBJECTIVE CHOSEN VALUE (y04) INFLUENCE ===\n")
chosen_values <- seq(0, 1, by = 0.2)

chosen_pred_data <- data.frame(
  y10 = baseline_values$y10,
  y03 = baseline_values$y03,
  y04 = chosen_values,
  y05 = baseline_values$y05,
  y06 = baseline_values$y06,
  monkey_id = baseline_values$monkey_id
)

chosen_predictions <- predict(model_trinomial, newdata = chosen_pred_data, type = "probs")

chosen_results <- data.frame(
  y04_subjective_chosen = chosen_values,
  exploit = chosen_predictions[, "exploit"],
  explore = chosen_predictions[, "explore"],
  none = chosen_predictions[, "none"]
)

cat("Subjective Chosen Value Effects:\n")
print(chosen_results)

# 3) SUBJECTIVE EXPLOIT VALUE (y05) INFLUENCE
cat("\n=== 3) SUBJECTIVE EXPLOIT VALUE (y05) INFLUENCE ===\n")
exploit_values <- seq(0, 1, by = 0.2)

exploit_pred_data <- data.frame(
  y10 = baseline_values$y10,
  y03 = baseline_values$y03,
  y04 = baseline_values$y04,
  y05 = exploit_values,
  y06 = baseline_values$y06,
  monkey_id = baseline_values$monkey_id
)

exploit_predictions <- predict(model_trinomial, newdata = exploit_pred_data, type = "probs")

exploit_results <- data.frame(
  y05_subjective_exploit = exploit_values,
  exploit = exploit_predictions[, "exploit"],
  explore = exploit_predictions[, "explore"],
  none = exploit_predictions[, "none"]
)

cat("Subjective Exploit Value Effects:\n")
print(exploit_results)

# 4) EXPECTED EXPLORE VALUE (y06) INFLUENCE
cat("\n=== 4) EXPECTED EXPLORE VALUE (y06) INFLUENCE ===\n")
expected_values <- seq(0, 1, by = 0.2)

expected_pred_data <- data.frame(
  y10 = baseline_values$y10,
  y03 = baseline_values$y03,
  y04 = baseline_values$y04,
  y05 = baseline_values$y05,
  y06 = expected_values,
  monkey_id = baseline_values$monkey_id
)

expected_predictions <- predict(model_trinomial, newdata = expected_pred_data, type = "probs")

expected_results <- data.frame(
  y06_expected_explore = expected_values,
  exploit = expected_predictions[, "exploit"],
  explore = expected_predictions[, "explore"],
  none = expected_predictions[, "none"]
)

cat("Expected Explore Value Effects:\n")
print(expected_results)

# CREATE COMPREHENSIVE REGRESSOR INFLUENCE PLOTS
pdf("regressor_influence_comprehensive.pdf", width = 16, height = 12)
par(mfrow = c(2, 4), mar = c(5, 4, 4, 2))

# Plot 1: Rank influence on Exploit
plot(rank_results$y03_rank, rank_results$exploit, 
     type = "b", pch = 19, col = "red", lwd = 2,
     main = "Rank (y03) → Exploit Probability",
     xlab = "Relative Rank (1=Dom, 3=Sub)", 
     ylab = "Exploit Probability",
     ylim = c(0, max(rank_results$exploit) * 1.1))
grid()

# Plot 2: Rank influence on Explore
plot(rank_results$y03_rank, rank_results$explore,
     type = "b", pch = 19, col = "green", lwd = 2,
     main = "Rank (y03) → Explore Probability",
     xlab = "Relative Rank (1=Dom, 3=Sub)",
     ylab = "Explore Probability",
     ylim = c(0, max(rank_results$explore) * 1.1))
grid()

# Plot 3: Subjective chosen value influence on Exploit
plot(chosen_results$y04_subjective_chosen, chosen_results$exploit,
     type = "b", pch = 19, col = "red", lwd = 2,
     main = "Subjective Chosen (y04) → Exploit",
     xlab = "Subjective Chosen Value",
     ylab = "Exploit Probability",
     ylim = c(0, max(chosen_results$exploit) * 1.1))
grid()

# Plot 4: Subjective chosen value influence on Explore  
plot(chosen_results$y04_subjective_chosen, chosen_results$explore,
     type = "b", pch = 19, col = "green", lwd = 2,
     main = "Subjective Chosen (y04) → Explore",
     xlab = "Subjective Chosen Value",
     ylab = "Explore Probability",
     ylim = c(0, max(chosen_results$explore) * 1.1))
grid()

# Plot 5: Subjective exploit value influence on Exploit
plot(exploit_results$y05_subjective_exploit, exploit_results$exploit,
     type = "b", pch = 19, col = "red", lwd = 2,
     main = "Subjective Exploit (y05) → Exploit",
     xlab = "Subjective Exploit Value",
     ylab = "Exploit Probability",
     ylim = c(0, max(exploit_results$exploit) * 1.1))
grid()

# Plot 6: Subjective exploit value influence on Explore
plot(exploit_results$y05_subjective_exploit, exploit_results$explore,
     type = "b", pch = 19, col = "green", lwd = 2,
     main = "Subjective Exploit (y05) → Explore",
     xlab = "Subjective Exploit Value", 
     ylab = "Explore Probability",
     ylim = c(0, max(exploit_results$explore) * 1.1))
grid()

# Plot 7: Expected explore value influence on Exploit
plot(expected_results$y06_expected_explore, expected_results$exploit,
     type = "b", pch = 19, col = "red", lwd = 2,
     main = "Expected Explore (y06) → Exploit",
     xlab = "Expected Explore Value",
     ylab = "Exploit Probability",
     ylim = c(0, max(expected_results$exploit) * 1.1))
grid()

# Plot 8: Expected explore value influence on Explore
plot(expected_results$y06_expected_explore, expected_results$explore,
     type = "b", pch = 19, col = "green", lwd = 2,
     main = "Expected Explore (y06) → Explore",
     xlab = "Expected Explore Value",
     ylab = "Explore Probability",
     ylim = c(0, max(expected_results$explore) * 1.1))
grid()

dev.off()

# CREATE HIGH-QUALITY GGPLOT VERSIONS

# Combine all results for plotting
rank_long <- data.frame(
  variable = "y03_Relative_Rank",
  value = rank_results$y03_rank,
  exploit = rank_results$exploit,
  explore = rank_results$explore
)

chosen_long <- data.frame(
  variable = "y04_Subjective_Chosen",
  value = chosen_results$y04_subjective_chosen,
  exploit = chosen_results$exploit,
  explore = chosen_results$explore
)

exploit_long <- data.frame(
  variable = "y05_Subjective_Exploit",
  value = exploit_results$y05_subjective_exploit,
  exploit = exploit_results$exploit,
  explore = exploit_results$explore
)

expected_long <- data.frame(
  variable = "y06_Expected_Explore",
  value = expected_results$y06_expected_explore,
  exploit = expected_results$exploit,
  explore = expected_results$explore
)

# Combine all data
all_regressor_data <- rbind(rank_long, chosen_long, exploit_long, expected_long)

# Reshape for ggplot
exploit_data <- data.frame(
  variable = all_regressor_data$variable,
  value = all_regressor_data$value,
  outcome = "exploit",
  probability = all_regressor_data$exploit
)

explore_data <- data.frame(
  variable = all_regressor_data$variable,
  value = all_regressor_data$value,
  outcome = "explore", 
  probability = all_regressor_data$explore
)

plot_data <- rbind(exploit_data, explore_data)

# Create comprehensive ggplot
p1 <- ggplot(plot_data, aes(x = value, y = probability, color = outcome)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  facet_wrap(~variable, scales = "free_x", ncol = 2) +
  labs(title = "Regressor Influence on Decision-Making",
       subtitle = "How each predictor variable affects explore vs exploit choices",
       x = "Predictor Value", 
       y = "Predicted Probability",
       color = "Choice") +
  theme_minimal() +
  scale_color_manual(values = c("exploit" = "red", "explore" = "darkgreen")) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        strip.text = element_text(size = 12, face = "bold"))

ggsave("regressor_influence_ggplot.pdf", p1, width = 12, height = 10)

# Create individual plots for each regressor
p_rank <- ggplot(subset(plot_data, variable == "y03_Relative_Rank"), 
                 aes(x = value, y = probability, color = outcome)) +
  geom_line(size = 2) +
  geom_point(size = 4) +
  labs(title = "Relative Rank (y03) Influence",
       subtitle = "1=Dominant, 2=Intermediate, 3=Subordinate",
       x = "Relative Rank", y = "Predicted Probability") +
  theme_minimal() +
  scale_color_manual(values = c("exploit" = "red", "explore" = "darkgreen"))

p_chosen <- ggplot(subset(plot_data, variable == "y04_Subjective_Chosen"),
                   aes(x = value, y = probability, color = outcome)) +
  geom_line(size = 2) +
  geom_point(size = 4) +
  labs(title = "Subjective Chosen Value (y04) Influence",
       subtitle = "Value of the option that was actually chosen",
       x = "Subjective Chosen Value", y = "Predicted Probability") +
  theme_minimal() +
  scale_color_manual(values = c("exploit" = "red", "explore" = "darkgreen"))

p_exploit <- ggplot(subset(plot_data, variable == "y05_Subjective_Exploit"),
                    aes(x = value, y = probability, color = outcome)) +
  geom_line(size = 2) +
  geom_point(size = 4) +
  labs(title = "Subjective Exploit Value (y05) Influence", 
       subtitle = "Visible value of the exploit option",
       x = "Subjective Exploit Value", y = "Predicted Probability") +
  theme_minimal() +
  scale_color_manual(values = c("exploit" = "red", "explore" = "darkgreen"))

p_expected <- ggplot(subset(plot_data, variable == "y06_Expected_Explore"),
                     aes(x = value, y = probability, color = outcome)) +
  geom_line(size = 2) +
  geom_point(size = 4) +
  labs(title = "Expected Explore Value (y06) Influence",
       subtitle = "Running expectation for explore option value", 
       x = "Expected Explore Value", y = "Predicted Probability") +
  theme_minimal() +
  scale_color_manual(values = c("exploit" = "red", "explore" = "darkgreen"))

# Save individual plots
ggsave("rank_influence.pdf", p_rank, width = 10, height = 6)
ggsave("chosen_value_influence.pdf", p_chosen, width = 10, height = 6)
ggsave("exploit_value_influence.pdf", p_exploit, width = 10, height = 6)
ggsave("expected_explore_influence.pdf", p_expected, width = 10, height = 6)

# CALCULATE EFFECT SIZES
cat("\n=== EFFECT SIZES (Range of Influence) ===\n")

rank_effect <- max(rank_results$explore) - min(rank_results$explore)
cat(sprintf("Rank (y03) effect on exploration: %.1f percentage points\n", rank_effect * 100))

chosen_effect <- max(chosen_results$explore) - min(chosen_results$explore)
cat(sprintf("Subjective chosen (y04) effect on exploration: %.1f percentage points\n", chosen_effect * 100))

exploit_effect <- max(exploit_results$explore) - min(exploit_results$explore)
cat(sprintf("Subjective exploit (y05) effect on exploration: %.1f percentage points\n", exploit_effect * 100))

expected_effect <- max(expected_results$explore) - min(expected_results$explore)
cat(sprintf("Expected explore (y06) effect on exploration: %.1f percentage points\n", expected_effect * 100))

# DIRECTION OF EFFECTS
cat("\n=== DIRECTION OF EFFECTS ===\n")
cat("Rank (y03): Higher rank (more subordinate) →", 
    ifelse(rank_results$explore[length(rank_results$explore)] > rank_results$explore[1], 
           "MORE exploration", "LESS exploration"), "\n")

cat("Subjective chosen (y04): Higher chosen value →",
    ifelse(chosen_results$explore[length(chosen_results$explore)] > chosen_results$explore[1],
           "MORE exploration", "LESS exploration"), "\n")

cat("Subjective exploit (y05): Higher exploit value →",
    ifelse(exploit_results$explore[length(exploit_results$explore)] > exploit_results$explore[1],
           "MORE exploration", "LESS exploration"), "\n")

cat("Expected explore (y06): Higher expected explore →",
    ifelse(expected_results$explore[length(expected_results$explore)] > expected_results$explore[1],
           "MORE exploration", "LESS exploration"), "\n")

# Save all results
write.csv(rank_results, "rank_influence_predictions.csv", row.names = FALSE)
write.csv(chosen_results, "chosen_value_influence_predictions.csv", row.names = FALSE)
write.csv(exploit_results, "exploit_value_influence_predictions.csv", row.names = FALSE)
write.csv(expected_results, "expected_explore_influence_predictions.csv", row.names = FALSE)

cat("\nAnalysis complete! Generated files:\n")
cat("- regressor_influence_comprehensive.pdf\n")
cat("- regressor_influence_ggplot.pdf\n")
cat("- rank_influence.pdf\n")
cat("- chosen_value_influence.pdf\n")
cat("- exploit_value_influence.pdf\n")
cat("- expected_explore_influence.pdf\n")
cat("- Individual CSV files for each regressor\n") 