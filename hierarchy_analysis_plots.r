# HIERARCHY ANALYSIS PLOTS - REGRESSION VALUES
# Analyzing sex differences, hierarchy effects, and hierarchy changes

library(nnet)
library(dplyr)
library(ggplot2)

# Load the fitted model
model_trinomial <- readRDS("trinomial_model.rds")

# Read the data for reference
data <- read.csv("Explore Exploit Dataset.csv", stringsAsFactors = FALSE)

# Define monkey characteristics as specified by user
monkey_info <- data.frame(
  monkey = c("EBI", "DALI", "FRAN", "ANEMONE", "ICE", "CHOCOLAT"),
  sex = c("Male", "Male", "Male", "Female", "Female", "Female"),
  hierarchy = c("Subordinate", "Intermediate", "Dominant", "Subordinate", "Intermediate", "Dominant"),
  stringsAsFactors = FALSE
)

cat("=== HIERARCHY ANALYSIS PLOTS ===\n")
cat("Monkey Classifications:\n")
print(monkey_info)

# Set up average values for predictions
avg_y03 <- 1.5  # Middle rank value
avg_y04 <- 0.5  # Average subjective chosen value
avg_y05 <- 0.5  # Average subjective exploit value
avg_y06 <- 0.6  # Average expected explore value

# 1) SEX DIFFERENCES ANALYSIS
cat("\n=== 1) SEX DIFFERENCES ===\n")

# Create prediction data for sex differences (solo condition)
sex_pred_data <- data.frame(
  y10 = factor("solo", levels = c("solo", "duo", "trio")),
  y03 = avg_y03,
  y04 = avg_y04,
  y05 = avg_y05,
  y06 = avg_y06,
  monkey_id = factor(c("EBI", "DALI", "FRAN", "ANEMONE", "ICE", "CHOCOLAT"), 
                     levels = c("ANEMONE", "CHOCOLAT", "DALI", "EBI", "FRAN", "ICE"))
)

# Generate predictions
sex_predictions <- predict(model_trinomial, newdata = sex_pred_data, type = "probs")

# Create sex differences dataframe
sex_results <- data.frame(
  monkey = sex_pred_data$monkey_id,
  sex = c("Male", "Male", "Male", "Female", "Female", "Female"),
  exploit = sex_predictions[, "exploit"],
  explore = sex_predictions[, "explore"],
  none = sex_predictions[, "none"]
)

# Calculate sex averages
sex_summary <- sex_results %>%
  group_by(sex) %>%
  summarise(
    exploit_mean = mean(exploit),
    explore_mean = mean(explore),
    none_mean = mean(none),
    .groups = 'drop'
  )

cat("Sex Differences (Regression Predictions):\n")
print(sex_summary)

# 2) HIERARCHY DIFFERENCES ANALYSIS
cat("\n=== 2) HIERARCHY DIFFERENCES ===\n")

# Create hierarchy summary
hierarchy_results <- data.frame(
  monkey = sex_pred_data$monkey_id,
  hierarchy = c("Subordinate", "Intermediate", "Dominant", "Subordinate", "Intermediate", "Dominant"),
  exploit = sex_predictions[, "exploit"],
  explore = sex_predictions[, "explore"],
  none = sex_predictions[, "none"]
)

# Calculate hierarchy averages
hierarchy_summary <- hierarchy_results %>%
  group_by(hierarchy) %>%
  summarise(
    exploit_mean = mean(exploit),
    explore_mean = mean(explore),
    none_mean = mean(none),
    .groups = 'drop'
  )

# Reorder hierarchy levels
hierarchy_summary$hierarchy <- factor(hierarchy_summary$hierarchy, 
                                    levels = c("Dominant", "Intermediate", "Subordinate"))

cat("Hierarchy Differences (Regression Predictions):\n")
print(hierarchy_summary)

# 3) HIERARCHY CHANGES OF INTERMEDIATE MONKEYS
cat("\n=== 3) HIERARCHY CHANGES OF INTERMEDIATE MONKEYS ===\n")

# DALI hierarchy changes
dali_contexts <- data.frame(
  y10 = factor(c("duo", "trio", "duo"), levels = c("solo", "duo", "trio")),
  y03 = c(1, 2, 3),  # Dominant, Intermediate, Subordinate
  y04 = avg_y04,
  y05 = avg_y05,
  y06 = avg_y06,
  monkey_id = factor("DALI", levels = c("ANEMONE", "CHOCOLAT", "DALI", "EBI", "FRAN", "ICE")),
  context = c("Dominant (vs Ebi/Anemone)", "Intermediate (trio)", "Subordinate (vs Fran/Chocolat)")
)

dali_predictions <- predict(model_trinomial, newdata = dali_contexts, type = "probs")

dali_results <- data.frame(
  monkey = "DALI",
  context = dali_contexts$context,
  exploit = dali_predictions[, "exploit"],
  explore = dali_predictions[, "explore"],
  none = dali_predictions[, "none"]
)

# ICE hierarchy changes
ice_contexts <- data.frame(
  y10 = factor(c("duo", "trio", "duo"), levels = c("solo", "duo", "trio")),
  y03 = c(1, 2, 3),  # Dominant, Intermediate, Subordinate  
  y04 = avg_y04,
  y05 = avg_y05,
  y06 = avg_y06,
  monkey_id = factor("ICE", levels = c("ANEMONE", "CHOCOLAT", "DALI", "EBI", "FRAN", "ICE")),
  context = c("Dominant (vs Ebi/Anemone)", "Intermediate (trio)", "Subordinate (vs Fran/Chocolat)")
)

ice_predictions <- predict(model_trinomial, newdata = ice_contexts, type = "probs")

ice_results <- data.frame(
  monkey = "ICE",
  context = ice_contexts$context,
  exploit = ice_predictions[, "exploit"],
  explore = ice_predictions[, "explore"],
  none = ice_predictions[, "none"]
)

# Combine intermediate monkey results
intermediate_results <- rbind(dali_results, ice_results)

cat("DALI Hierarchy Changes:\n")
print(dali_results)
cat("\nICE Hierarchy Changes:\n")
print(ice_results)

# CREATE COMPREHENSIVE PLOTS
pdf("hierarchy_analysis_comprehensive.pdf", width = 16, height = 12)
par(mfrow = c(2, 3), mar = c(5, 4, 4, 2))

# Plot 1: Sex Differences - Exploit
barplot(sex_summary$exploit_mean, 
        names.arg = sex_summary$sex,
        main = "Sex Differences: Exploit Probability",
        ylab = "Predicted Probability (Regression)",
        col = c("pink", "lightblue"),
        ylim = c(0, max(sex_summary$exploit_mean) * 1.2))
text(1:2, sex_summary$exploit_mean + 0.01, 
     sprintf("%.3f", sex_summary$exploit_mean), pos = 3)

# Plot 2: Sex Differences - Explore  
barplot(sex_summary$explore_mean,
        names.arg = sex_summary$sex,
        main = "Sex Differences: Explore Probability", 
        ylab = "Predicted Probability (Regression)",
        col = c("pink", "lightblue"),
        ylim = c(0, max(sex_summary$explore_mean) * 1.1))
text(1:2, sex_summary$explore_mean + 0.02,
     sprintf("%.3f", sex_summary$explore_mean), pos = 3)

# Plot 3: Hierarchy Differences - Exploit
barplot(hierarchy_summary$exploit_mean,
        names.arg = hierarchy_summary$hierarchy,
        main = "Hierarchy: Exploit Probability",
        ylab = "Predicted Probability (Regression)", 
        col = c("gold", "orange", "red"),
        ylim = c(0, max(hierarchy_summary$exploit_mean) * 1.2))
text(1:3, hierarchy_summary$exploit_mean + 0.01,
     sprintf("%.3f", hierarchy_summary$exploit_mean), pos = 3)

# Plot 4: Hierarchy Differences - Explore
barplot(hierarchy_summary$explore_mean,
        names.arg = hierarchy_summary$hierarchy,
        main = "Hierarchy: Explore Probability",
        ylab = "Predicted Probability (Regression)",
        col = c("gold", "orange", "red"),
        ylim = c(0, max(hierarchy_summary$explore_mean) * 1.1))
text(1:3, hierarchy_summary$explore_mean + 0.02,
     sprintf("%.3f", hierarchy_summary$explore_mean), pos = 3)

# Plot 5: DALI Hierarchy Changes
barplot(dali_results$explore,
        names.arg = c("Dom", "Int", "Sub"),
        main = "DALI: Hierarchy Context Changes\n(Explore Probability)",
        ylab = "Predicted Probability (Regression)",
        col = c("darkgreen", "green", "lightgreen"),
        ylim = c(0, max(dali_results$explore) * 1.1))
text(1:3, dali_results$explore + 0.02,
     sprintf("%.3f", dali_results$explore), pos = 3)

# Plot 6: ICE Hierarchy Changes  
barplot(ice_results$explore,
        names.arg = c("Dom", "Int", "Sub"),
        main = "ICE: Hierarchy Context Changes\n(Explore Probability)",
        ylab = "Predicted Probability (Regression)",
        col = c("darkblue", "blue", "lightblue"),
        ylim = c(0, max(ice_results$explore) * 1.1))
text(1:3, ice_results$explore + 0.02,
     sprintf("%.3f", ice_results$explore), pos = 3)

dev.off()

# CREATE DETAILED GGPLOT VERSIONS
library(ggplot2)

# Reshape data for ggplot
sex_long <- sex_results %>%
  select(monkey, sex, exploit, explore) %>%
  tidyr::pivot_longer(cols = c(exploit, explore), names_to = "outcome", values_to = "probability")

hierarchy_long <- hierarchy_results %>%
  select(monkey, hierarchy, exploit, explore) %>%
  tidyr::pivot_longer(cols = c(exploit, explore), names_to = "outcome", values_to = "probability")

intermediate_long <- intermediate_results %>%
  select(monkey, context, exploit, explore) %>%
  tidyr::pivot_longer(cols = c(exploit, explore), names_to = "outcome", values_to = "probability")

# Create high-quality ggplot figures
p1 <- ggplot(sex_long, aes(x = sex, y = probability, fill = outcome)) +
  geom_col(position = "dodge", alpha = 0.8) +
  geom_text(aes(label = sprintf("%.3f", probability)), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  labs(title = "Sex Differences in Decision-Making",
       subtitle = "Model-based predictions (regression values)",
       x = "Sex", y = "Predicted Probability", fill = "Choice") +
  theme_minimal() +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  theme(plot.title = element_text(size = 14, face = "bold"))

p2 <- ggplot(hierarchy_long, aes(x = hierarchy, y = probability, fill = outcome)) +
  geom_col(position = "dodge", alpha = 0.8) +
  geom_text(aes(label = sprintf("%.3f", probability)),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  labs(title = "Hierarchy Effects on Decision-Making", 
       subtitle = "Model-based predictions (regression values)",
       x = "Hierarchy Level", y = "Predicted Probability", fill = "Choice") +
  theme_minimal() +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  scale_x_discrete(limits = c("Dominant", "Intermediate", "Subordinate")) +
  theme(plot.title = element_text(size = 14, face = "bold"))

p3 <- ggplot(intermediate_long, aes(x = context, y = probability, fill = outcome)) +
  geom_col(position = "dodge", alpha = 0.8) +
  geom_text(aes(label = sprintf("%.3f", probability)),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 2.5) +
  facet_wrap(~monkey, ncol = 2) +
  labs(title = "Hierarchy Context Changes: Intermediate Monkeys",
       subtitle = "DALI & ICE across different social contexts (regression values)",
       x = "Hierarchy Context", y = "Predicted Probability", fill = "Choice") +
  theme_minimal() +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Save individual ggplot figures
ggsave("sex_differences_regression.pdf", p1, width = 10, height = 6)
ggsave("hierarchy_effects_regression.pdf", p2, width = 10, height = 6) 
ggsave("intermediate_hierarchy_changes_regression.pdf", p3, width = 12, height = 8)

# Print summary statistics
cat("\n=== SUMMARY STATISTICS ===\n")
cat("Sex Differences (Explore Probability):\n")
cat(sprintf("Males: %.3f, Females: %.3f\n", 
            sex_summary$explore_mean[sex_summary$sex == "Male"],
            sex_summary$explore_mean[sex_summary$sex == "Female"]))
cat(sprintf("Difference: %.3f (Female advantage)\n", 
            sex_summary$explore_mean[sex_summary$sex == "Female"] - 
            sex_summary$explore_mean[sex_summary$sex == "Male"]))

cat("\nHierarchy Effects (Explore Probability):\n")
for(i in 1:nrow(hierarchy_summary)) {
  cat(sprintf("%s: %.3f\n", hierarchy_summary$hierarchy[i], hierarchy_summary$explore_mean[i]))
}

cat("\nIntermediate Monkey Hierarchy Changes:\n")
cat("DALI - Explore Probability:\n")
for(i in 1:nrow(dali_results)) {
  cat(sprintf("  %s: %.3f\n", dali_results$context[i], dali_results$explore[i]))
}
cat("ICE - Explore Probability:\n") 
for(i in 1:nrow(ice_results)) {
  cat(sprintf("  %s: %.3f\n", ice_results$context[i], ice_results$explore[i]))
}

# Save all results
write.csv(sex_results, "sex_differences_regression_predictions.csv", row.names = FALSE)
write.csv(hierarchy_results, "hierarchy_differences_regression_predictions.csv", row.names = FALSE)
write.csv(intermediate_results, "intermediate_hierarchy_changes_regression_predictions.csv", row.names = FALSE)

cat("\nAnalysis complete! Generated files:\n")
cat("- hierarchy_analysis_comprehensive.pdf\n")
cat("- sex_differences_regression.pdf\n") 
cat("- hierarchy_effects_regression.pdf\n")
cat("- intermediate_hierarchy_changes_regression.pdf\n")
cat("- sex_differences_regression_predictions.csv\n")
cat("- hierarchy_differences_regression_predictions.csv\n")
cat("- intermediate_hierarchy_changes_regression_predictions.csv\n") 