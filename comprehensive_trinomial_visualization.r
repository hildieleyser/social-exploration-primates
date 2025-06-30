# COMPREHENSIVE TRINOMIAL MODEL VISUALIZATION
# Complete visualization suite for all specified variables (y10, y02, y03, y04, y05, y06)

library(nnet)

# Load and prepare data (same as final model)
data_raw <- read.csv("Explore Exploit Dataset.csv")

# Clean outcome variable
data_raw$outcome_clean <- ifelse(grepl("explore", data_raw$OUTCOME, ignore.case = TRUE), "explore",
                               ifelse(grepl("exploit", data_raw$OUTCOME, ignore.case = TRUE), "exploit", 
                                     ifelse(data_raw$OUTCOME %in% c("NONE", "none", "stop"), "none", "other")))

data_valid <- data_raw[!is.na(data_raw$outcome_clean) & data_raw$outcome_clean != "other", ]

# Add monkey characteristics
monkey_info <- data.frame(
  monkey = c("FRAN", "CHOCOLAT", "DALI", "ICE", "EBI", "ANEMONE"),
  sex = c("Male", "Female", "Male", "Female", "Male", "Female"),
  hierarchy = c("Dominant", "Dominant", "Intermediate", "Intermediate", "Subordinate", "Subordinate")
)

data_analysis <- merge(data_valid, monkey_info, by = "monkey", all.x = TRUE)

# Create complete dataset
model_data <- data_analysis[
  !is.na(data_analysis$CONDITION) &
  !is.na(data_analysis$PAIRED_WITH) &
  !is.na(data_analysis$RELATIVE_RANK) &
  !is.na(data_analysis$SUBJECTIVE_CHOSEN_VALUE) &
  !is.na(data_analysis$subjective_exploit) &
  !is.na(data_analysis$expected_explore), ]

# Prepare variables
model_data$y10_condition <- factor(model_data$CONDITION, levels = c("solo", "duo", "trio"))
model_data$y02_partner_group <- ifelse(model_data$PAIRED_WITH == "", "NONE", "PARTNERED")
model_data$y02_partner_group <- factor(model_data$y02_partner_group)
model_data$y03_rel_rank <- model_data$RELATIVE_RANK
model_data$y04_chosen_value <- scale(model_data$SUBJECTIVE_CHOSEN_VALUE)[,1]
model_data$y05_exploit_value <- scale(model_data$subjective_exploit)[,1]
model_data$y06_explore_expect <- scale(model_data$expected_explore)[,1]
model_data$sex_group <- factor(model_data$sex)
model_data$hierarchy_group <- factor(model_data$hierarchy, levels = c("Subordinate", "Intermediate", "Dominant"))
model_data$monkey_group <- factor(model_data$monkey)
model_data$outcome_factor <- factor(model_data$outcome_clean, levels = c("none", "explore", "exploit"))

# Cross-validation
set.seed(42)
train_indices <- sample(1:nrow(model_data), size = floor(0.7 * nrow(model_data)))
train_data <- model_data[train_indices, ]
test_data <- model_data[-train_indices, ]

# Fit models for comparison
model_all_main <- multinom(outcome_factor ~ y10_condition + y02_partner_group + y03_rel_rank + 
                          y04_chosen_value + y05_exploit_value + y06_explore_expect,
                          data = train_data, trace = FALSE)

model_with_groups <- multinom(outcome_factor ~ y10_condition + y02_partner_group + y03_rel_rank + 
                             y04_chosen_value + y05_exploit_value + y06_explore_expect +
                             sex_group + hierarchy_group,
                             data = train_data, trace = FALSE)

model_full_individuals <- multinom(outcome_factor ~ y10_condition + y02_partner_group + y03_rel_rank + 
                                  y04_chosen_value + y05_exploit_value + y06_explore_expect +
                                  monkey_group,
                                  data = train_data, trace = FALSE)

# Model comparison
models <- list(
  "Main Effects" = model_all_main,
  "With Groups" = model_with_groups,
  "With Individuals" = model_full_individuals
)

aic_values <- sapply(models, AIC)
best_model <- models[[which.min(aic_values)]]

# Calculate AIC weights
delta_aic <- aic_values - min(aic_values)
aic_weights <- exp(-0.5 * delta_aic) / sum(exp(-0.5 * delta_aic))

# Cross-validation results
test_predictions <- predict(best_model, newdata = test_data, type = "probs")
test_pred_classes <- predict(best_model, newdata = test_data)
accuracy <- mean(test_pred_classes == test_data$outcome_factor)
baseline_accuracy <- max(table(test_data$outcome_factor)) / nrow(test_data)

# Calculate effect sizes for each variable
solo_rates <- table(model_data$outcome_clean[model_data$y10_condition == "solo"]) / 
              sum(model_data$y10_condition == "solo") * 100
duo_rates <- table(model_data$outcome_clean[model_data$y10_condition == "duo"]) / 
             sum(model_data$y10_condition == "duo") * 100
trio_rates <- table(model_data$outcome_clean[model_data$y10_condition == "trio"]) / 
              sum(model_data$y10_condition == "trio") * 100

none_partner_rates <- table(model_data$outcome_clean[model_data$y02_partner_group == "NONE"]) / 
                      sum(model_data$y02_partner_group == "NONE") * 100
partnered_rates <- table(model_data$outcome_clean[model_data$y02_partner_group == "PARTNERED"]) / 
                   sum(model_data$y02_partner_group == "PARTNERED") * 100

rank1_rates <- table(model_data$outcome_clean[model_data$y03_rel_rank == 1]) / 
               sum(model_data$y03_rel_rank == 1) * 100
rank2_rates <- table(model_data$outcome_clean[model_data$y03_rel_rank == 2]) / 
               sum(model_data$y03_rel_rank == 2) * 100
rank3_rates <- table(model_data$outcome_clean[model_data$y03_rel_rank == 3]) / 
               sum(model_data$y03_rel_rank == 3) * 100

male_rates <- table(model_data$outcome_clean[model_data$sex_group == "Male"]) / 
              sum(model_data$sex_group == "Male") * 100
female_rates <- table(model_data$outcome_clean[model_data$sex_group == "Female"]) / 
                sum(model_data$sex_group == "Female") * 100

# Individual monkey rates
monkey_rates <- data.frame()
for(monkey in levels(model_data$monkey_group)) {
  rates <- table(model_data$outcome_clean[model_data$monkey_group == monkey]) / 
           sum(model_data$monkey_group == monkey) * 100
  monkey_rates <- rbind(monkey_rates, data.frame(
    monkey = monkey,
    explore = rates["explore"],
    exploit = rates["exploit"], 
    none = rates["none"]
  ))
}

# Create comprehensive visualization
pdf("comprehensive_trinomial_results.pdf", width = 24, height = 18)
layout(matrix(1:15, nrow = 3, ncol = 5, byrow = TRUE))

# Plot 1: AIC Model Comparison
barplot(aic_values, main = "Model Comparison (AIC)", ylab = "AIC", 
        col = ifelse(aic_values == min(aic_values), "darkgreen", "lightblue"),
        las = 2, cex.names = 0.8)
text(1:length(aic_values), aic_values + 5, round(aic_values, 1), cex = 0.8)

# Plot 2: AIC Weights
barplot(aic_weights * 100, main = "Model Weights (%)", ylab = "Weight %",
        col = c("lightblue", "darkgreen", "orange"), las = 2, cex.names = 0.8)
text(1:length(aic_weights), aic_weights * 100 + 2, 
     paste0(round(aic_weights * 100, 1), "%"), cex = 0.8)

# Plot 3: Cross-validation Performance
barplot(c(baseline_accuracy, accuracy) * 100,
        names.arg = c("Baseline", "Model"),
        main = "Cross-Validation Performance", ylab = "Accuracy %",
        col = c("gray", "darkgreen"))
text(1:2, c(baseline_accuracy, accuracy) * 100 + 2, 
     paste0(round(c(baseline_accuracy, accuracy) * 100, 1), "%"), cex = 0.8)

# Plot 4: Y10 - Social Condition Effects
condition_matrix <- rbind(solo_rates, duo_rates, trio_rates)
barplot(condition_matrix, beside = TRUE,
        main = "Y10: Social Condition Effects", ylab = "Percentage",
        col = c("lightblue", "orange", "red"),
        legend.text = c("Solo", "Duo", "Trio"),
        names.arg = c("Explore", "Exploit", "None"))

# Plot 5: Y02 - Partner Effects  
partner_matrix <- rbind(none_partner_rates, partnered_rates)
barplot(partner_matrix, beside = TRUE,
        main = "Y02: Partner Effects", ylab = "Percentage", 
        col = c("lightgray", "darkblue"),
        legend.text = c("No Partner", "Partnered"),
        names.arg = c("Explore", "Exploit", "None"))

# Plot 6: Y03 - Relative Rank Effects
rank_matrix <- rbind(rank1_rates, rank2_rates, rank3_rates)
barplot(rank_matrix, beside = TRUE,
        main = "Y03: Relative Rank Effects", ylab = "Percentage",
        col = c("gold", "orange", "brown"),
        legend.text = c("Rank 1", "Rank 2", "Rank 3"),
        names.arg = c("Explore", "Exploit", "None"))

# Plot 7: Sex Effects
sex_matrix <- rbind(female_rates, male_rates)
barplot(sex_matrix, beside = TRUE,
        main = "Sex Effects", ylab = "Percentage",
        col = c("pink", "lightblue"),
        legend.text = c("Female", "Male"),
        names.arg = c("Explore", "Exploit", "None"))

# Plot 8: Individual Monkey Exploration Rates
barplot(monkey_rates$explore, names.arg = monkey_rates$monkey,
        main = "Individual Exploration Rates", ylab = "Exploration %",
        col = c("brown", "yellow", "gold", "orange", "red", "darkred"),
        las = 2)
text(1:6, monkey_rates$explore + 2, round(monkey_rates$explore, 1), cex = 0.8)

# Plot 9: Y04 - Subjective Chosen Value Distribution
hist(model_data$SUBJECTIVE_CHOSEN_VALUE, breaks = 30,
     main = "Y04: Subjective Chosen Value", xlab = "Value", 
     col = "lightgreen", border = "darkgreen")

# Plot 10: Y05 - Subjective Exploit Value Distribution  
hist(model_data$subjective_exploit, breaks = 30,
     main = "Y05: Subjective Exploit Value", xlab = "Value",
     col = "lightcoral", border = "darkred")

# Plot 11: Y06 - Expected Explore Value Distribution
hist(model_data$expected_explore, breaks = 30,
     main = "Y06: Expected Explore Value", xlab = "Value",
     col = "lightblue", border = "darkblue")

# Plot 12: Value Correlations
plot(model_data$y04_chosen_value, model_data$y05_exploit_value,
     main = "Y04 vs Y05 Correlation", xlab = "Chosen Value (scaled)", 
     ylab = "Exploit Value (scaled)", pch = 16, col = "blue")
abline(lm(model_data$y05_exploit_value ~ model_data$y04_chosen_value), col = "red", lwd = 2)
text(-2, 2, paste("r =", round(cor(model_data$y04_chosen_value, model_data$y05_exploit_value), 3)), 
     cex = 1.2, col = "red")

# Plot 13: Test Set Validation
observed_rates <- table(test_data$outcome_factor) / nrow(test_data) * 100
predicted_rates <- colMeans(test_predictions) * 100
validation_matrix <- rbind(observed_rates, predicted_rates)
barplot(validation_matrix, beside = TRUE,
        main = "Test Set Validation", ylab = "Percentage",
        col = c("blue", "red"),
        legend.text = c("Observed", "Predicted"),
        names.arg = c("None", "Explore", "Exploit"))

# Plot 14: Decision Triangle
monkey_triangles <- data.frame()
for(monkey in levels(model_data$monkey_group)) {
  subset_data <- model_data[model_data$monkey_group == monkey, ]
  total <- nrow(subset_data)
  explore_prop <- sum(subset_data$outcome_clean == "explore") / total
  exploit_prop <- sum(subset_data$outcome_clean == "exploit") / total
  none_prop <- sum(subset_data$outcome_clean == "none") / total
  
  monkey_triangles <- rbind(monkey_triangles, data.frame(
    monkey = monkey,
    explore = explore_prop,
    exploit = exploit_prop,
    none = none_prop
  ))
}

plot(monkey_triangles$explore, monkey_triangles$exploit,
     main = "Decision Space: Explore vs Exploit\n(Point size = None frequency)",
     xlab = "Exploration Rate", ylab = "Exploitation Rate",
     pch = 16, cex = monkey_triangles$none * 8 + 1,
     col = c("brown", "yellow", "gold", "orange", "red", "darkred"))
text(monkey_triangles$explore, monkey_triangles$exploit + 0.03, 
     monkey_triangles$monkey, cex = 0.8)

# Plot 15: Coefficient Significance
coeffs <- summary(best_model)$coefficients
std_errors <- summary(best_model)$standard.errors
z_scores <- coeffs / std_errors
p_values <- 2 * (1 - pnorm(abs(z_scores)))

sig_p_explore <- p_values[1, -1]  # Exclude intercept, explore vs none
sig_p_exploit <- p_values[2, -1]  # Exclude intercept, exploit vs none
var_names <- names(sig_p_explore)

barplot(c(-log10(sig_p_explore), -log10(sig_p_exploit)),
        main = "Coefficient Significance\n-log10(p-value)",
        ylab = "-log10(p-value)", las = 2, cex.names = 0.4,
        col = rep(c("darkgreen", "darkorange"), each = length(sig_p_explore)))
abline(h = -log10(0.05), col = "red", lty = 2)
legend("topright", c("Explore vs None", "Exploit vs None"), 
       fill = c("darkgreen", "darkorange"), cex = 0.8)

dev.off()

# Create detailed AIC comparison table
aic_comparison <- data.frame(
  Model = names(aic_values),
  AIC = aic_values,
  Delta_AIC = delta_aic,
  AIC_Weight = aic_weights,
  Evidence_Ratio = max(aic_weights) / aic_weights,
  Interpretation = ifelse(delta_aic == 0, "Best model",
                         ifelse(delta_aic < 2, "Substantial support",
                               ifelse(delta_aic < 4, "Considerably less support",
                                     ifelse(delta_aic < 7, "Much less support", "No support"))))
)

# Save detailed results
write.csv(aic_comparison, "comprehensive_aic_comparison.csv", row.names = FALSE)

# Create effect sizes summary
effect_sizes_summary <- data.frame(
  Variable = c("Y10_Solo", "Y10_Duo", "Y10_Trio", 
               "Y02_None", "Y02_Partnered",
               "Y03_Rank1", "Y03_Rank2", "Y03_Rank3",
               "Sex_Female", "Sex_Male"),
  Explore_Rate = c(solo_rates["explore"], duo_rates["explore"], trio_rates["explore"],
                   none_partner_rates["explore"], partnered_rates["explore"],
                   rank1_rates["explore"], rank2_rates["explore"], rank3_rates["explore"],
                   female_rates["explore"], male_rates["explore"]),
  Exploit_Rate = c(solo_rates["exploit"], duo_rates["exploit"], trio_rates["exploit"],
                   none_partner_rates["exploit"], partnered_rates["exploit"],
                   rank1_rates["exploit"], rank2_rates["exploit"], rank3_rates["exploit"],
                   female_rates["exploit"], male_rates["exploit"]),
  None_Rate = c(solo_rates["none"], duo_rates["none"], trio_rates["none"],
                none_partner_rates["none"], partnered_rates["none"],
                rank1_rates["none"], rank2_rates["none"], rank3_rates["none"],
                female_rates["none"], male_rates["none"])
)

write.csv(effect_sizes_summary, "comprehensive_effect_sizes.csv", row.names = FALSE)

# Create individual monkey summary
monkey_summary <- data.frame(
  Monkey = monkey_rates$monkey,
  Sex = c("Female", "Female", "Male", "Male", "Male", "Female")[match(monkey_rates$monkey, 
         c("ANEMONE", "CHOCOLAT", "DALI", "EBI", "FRAN", "ICE"))],
  Hierarchy = c("Subordinate", "Dominant", "Intermediate", "Subordinate", "Dominant", "Intermediate")[match(monkey_rates$monkey,
              c("ANEMONE", "CHOCOLAT", "DALI", "EBI", "FRAN", "ICE"))],
  Explore_Rate = monkey_rates$explore,
  Exploit_Rate = monkey_rates$exploit,
  None_Rate = monkey_rates$none,
  Total_Trials = as.numeric(table(model_data$monkey_group))
)

write.csv(monkey_summary, "comprehensive_monkey_summary.csv", row.names = FALSE)

cat("=== COMPREHENSIVE VISUALIZATION COMPLETE ===\n")
cat("Files generated:\n")
cat("- comprehensive_trinomial_results.pdf (15 visualizations)\n")
cat("- comprehensive_aic_comparison.csv\n")
cat("- comprehensive_effect_sizes.csv\n") 
cat("- comprehensive_monkey_summary.csv\n")
cat("\nVisualization includes:\n")
cat("✓ AIC model comparison with weights\n")
cat("✓ Cross-validation performance\n")
cat("✓ All variable effects (Y10, Y02, Y03)\n")
cat("✓ Value distributions (Y04, Y05, Y06)\n")
cat("✓ Individual monkey profiles\n")
cat("✓ Decision space triangular plot\n")
cat("✓ Statistical significance visualization\n")
cat("✓ Test set validation\n") 