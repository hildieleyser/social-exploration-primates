# COMPREHENSIVE SOCIAL HIERARCHY AND CONTEXT ANALYSIS
# Research Question: How does social hierarchy and social context influence 
# explore/exploit/none outcomes, and how do other variables modulate this
# across different monkeys, sexes, and hierarchical positions?

library(nnet)
library(dplyr)
library(ggplot2)
library(gridExtra)

# Load the fitted model
model_trinomial <- readRDS("trinomial_model.rds")

cat("=== COMPREHENSIVE SOCIAL HIERARCHY & CONTEXT ANALYSIS ===\n")
cat("Research Question: Social hierarchy × social context effects on trinomial outcomes\n")
cat("Including individual differences by sex and hierarchical position\n\n")

# Define monkey characteristics
monkey_info <- data.frame(
  monkey_id = c("ANEMONE", "CHOCOLAT", "DALI", "EBI", "FRAN", "ICE"),
  sex = c("Female", "Female", "Male", "Male", "Male", "Female"),
  overall_rank = c("Subordinate", "Dominant", "Intermediate", "Subordinate", "Dominant", "Intermediate"),
  stringsAsFactors = FALSE
)

cat("Monkey Characteristics:\n")
print(monkey_info)

# ANALYSIS 1: SOCIAL CONTEXT MAIN EFFECTS ACROSS HIERARCHY LEVELS
cat("\n=== 1) SOCIAL CONTEXT EFFECTS BY HIERARCHY LEVEL ===\n")

# Create prediction grid for social context × hierarchy
context_hierarchy_grid <- expand.grid(
  y10 = factor(c("solo", "duo", "trio"), levels = c("solo", "duo", "trio")),
  y03 = c(1, 2, 3),  # 1=dominant, 2=intermediate, 3=subordinate
  y04 = 0.5,  # baseline values
  y05 = 0.5,
  y06 = 0.6,
  monkey_id = factor("FRAN", levels = c("ANEMONE", "CHOCOLAT", "DALI", "EBI", "FRAN", "ICE"))
)

context_hierarchy_pred <- predict(model_trinomial, newdata = context_hierarchy_grid, type = "probs")

context_hierarchy_results <- data.frame(
  social_context = context_hierarchy_grid$y10,
  hierarchy_level = factor(context_hierarchy_grid$y03, 
                          levels = c(1, 2, 3), 
                          labels = c("Dominant", "Intermediate", "Subordinate")),
  exploit = context_hierarchy_pred[, "exploit"],
  explore = context_hierarchy_pred[, "explore"],
  none = context_hierarchy_pred[, "none"]
)

cat("Social Context × Hierarchy Effects:\n")
print(context_hierarchy_results)

# ANALYSIS 2: SEX DIFFERENCES IN SOCIAL HIERARCHY EFFECTS
cat("\n=== 2) SEX DIFFERENCES IN HIERARCHY EFFECTS ===\n")

# Males: EBI, DALI, FRAN
# Females: ANEMONE, ICE, CHOCOLAT

sex_hierarchy_results <- list()

for (sex in c("Male", "Female")) {
  if (sex == "Male") {
    monkey_sample <- "FRAN"  # Representative male
  } else {
    monkey_sample <- "CHOCOLAT"  # Representative female
  }
  
  sex_grid <- expand.grid(
    y10 = factor(c("solo", "duo", "trio"), levels = c("solo", "duo", "trio")),
    y03 = c(1, 2, 3),
    y04 = 0.5,
    y05 = 0.5, 
    y06 = 0.6,
    monkey_id = factor(monkey_sample, levels = c("ANEMONE", "CHOCOLAT", "DALI", "EBI", "FRAN", "ICE"))
  )
  
  sex_pred <- predict(model_trinomial, newdata = sex_grid, type = "probs")
  
  sex_hierarchy_results[[sex]] <- data.frame(
    sex = sex,
    social_context = sex_grid$y10,
    hierarchy_level = factor(sex_grid$y03, levels = c(1, 2, 3), 
                           labels = c("Dominant", "Intermediate", "Subordinate")),
    exploit = sex_pred[, "exploit"],
    explore = sex_pred[, "explore"],
    none = sex_pred[, "none"]
  )
}

sex_combined <- do.call(rbind, sex_hierarchy_results)
cat("Sex Differences in Hierarchy Effects:\n")
print(sex_combined)

# ANALYSIS 3: INDIVIDUAL MONKEY PROFILES ACROSS CONTEXTS
cat("\n=== 3) INDIVIDUAL MONKEY PROFILES ===\n")

individual_results <- list()

for (monkey in c("ANEMONE", "CHOCOLAT", "DALI", "EBI", "FRAN", "ICE")) {
  monkey_grid <- expand.grid(
    y10 = factor(c("solo", "duo", "trio"), levels = c("solo", "duo", "trio")),
    y03 = 2,  # baseline hierarchy
    y04 = 0.5,
    y05 = 0.5,
    y06 = 0.6,
    monkey_id = factor(monkey, levels = c("ANEMONE", "CHOCOLAT", "DALI", "EBI", "FRAN", "ICE"))
  )
  
  monkey_pred <- predict(model_trinomial, newdata = monkey_grid, type = "probs")
  
  individual_results[[monkey]] <- data.frame(
    monkey_id = monkey,
    sex = monkey_info$sex[monkey_info$monkey_id == monkey],
    overall_rank = monkey_info$overall_rank[monkey_info$monkey_id == monkey],
    social_context = monkey_grid$y10,
    exploit = monkey_pred[, "exploit"],
    explore = monkey_pred[, "explore"],
    none = monkey_pred[, "none"]
  )
}

individual_combined <- do.call(rbind, individual_results)
cat("Individual Monkey Profiles:\n")
print(individual_combined)

# ANALYSIS 4: OTHER VARIABLES' INFLUENCE WITHIN SOCIAL CONTEXTS
cat("\n=== 4) OTHER VARIABLES' MODULATION BY SOCIAL CONTEXT ===\n")

# Test how y04, y05, y06 effects change across social contexts
other_vars_results <- list()

for (context in c("solo", "duo", "trio")) {
  # Test y06 (expected explore) effect in this context
  y06_grid <- data.frame(
    y10 = factor(context, levels = c("solo", "duo", "trio")),
    y03 = 2,
    y04 = 0.5,
    y05 = 0.5,
    y06 = c(0.2, 0.4, 0.6, 0.8),
    monkey_id = factor("FRAN", levels = c("ANEMONE", "CHOCOLAT", "DALI", "EBI", "FRAN", "ICE"))
  )
  
  y06_pred <- predict(model_trinomial, newdata = y06_grid, type = "probs")
  
  other_vars_results[[context]] <- data.frame(
    social_context = context,
    y06_expected_explore = y06_grid$y06,
    exploit = y06_pred[, "exploit"],
    explore = y06_pred[, "explore"],
    none = y06_pred[, "none"]
  )
}

other_vars_combined <- do.call(rbind, other_vars_results)
cat("Expected Explore Value (y06) Effects by Social Context:\n")
print(other_vars_combined)

# CREATE COMPREHENSIVE VISUALIZATION
pdf("comprehensive_social_hierarchy_analysis.pdf", width = 20, height = 16)
par(mfrow = c(3, 4), mar = c(5, 4, 4, 2))

# Plot 1: Social Context × Hierarchy - Explore
explore_matrix <- matrix(context_hierarchy_results$explore, nrow = 3, ncol = 3)
rownames(explore_matrix) <- c("Solo", "Duo", "Trio")
colnames(explore_matrix) <- c("Dominant", "Intermediate", "Subordinate")

image(1:3, 1:3, explore_matrix, 
      main = "Exploration by Context × Hierarchy",
      xlab = "Hierarchy Level", ylab = "Social Context",
      xaxt = "n", yaxt = "n", col = heat.colors(20))
axis(1, at = 1:3, labels = colnames(explore_matrix))
axis(2, at = 1:3, labels = rownames(explore_matrix))
for(i in 1:3) for(j in 1:3) text(j, i, round(explore_matrix[i,j], 3))

# Plot 2: Social Context × Hierarchy - Exploit
exploit_matrix <- matrix(context_hierarchy_results$exploit, nrow = 3, ncol = 3)
rownames(exploit_matrix) <- c("Solo", "Duo", "Trio")
colnames(exploit_matrix) <- c("Dominant", "Intermediate", "Subordinate")

image(1:3, 1:3, exploit_matrix,
      main = "Exploitation by Context × Hierarchy", 
      xlab = "Hierarchy Level", ylab = "Social Context",
      xaxt = "n", yaxt = "n", col = heat.colors(20))
axis(1, at = 1:3, labels = colnames(exploit_matrix))
axis(2, at = 1:3, labels = rownames(exploit_matrix))
for(i in 1:3) for(j in 1:3) text(j, i, round(exploit_matrix[i,j], 3))

# Plot 3: Sex Differences in Exploration
sex_explore <- sex_combined[, c("sex", "social_context", "hierarchy_level", "explore")]
sex_means <- aggregate(explore ~ sex + social_context, data = sex_explore, mean)

barplot(matrix(sex_means$explore, nrow = 2), beside = TRUE,
        names.arg = unique(sex_means$social_context),
        main = "Sex Differences in Exploration",
        xlab = "Social Context", ylab = "Explore Probability",
        legend.text = c("Female", "Male"),
        col = c("pink", "lightblue"))

# Plot 4: Individual Monkey Exploration Profiles
monkey_explore <- individual_combined[, c("monkey_id", "social_context", "explore")]
monkey_means <- aggregate(explore ~ monkey_id + social_context, data = monkey_explore, mean)

# Reshape for plotting
solo_vals <- monkey_means$explore[monkey_means$social_context == "solo"]
duo_vals <- monkey_means$explore[monkey_means$social_context == "duo"] 
trio_vals <- monkey_means$explore[monkey_means$social_context == "trio"]

barplot(rbind(solo_vals, duo_vals, trio_vals), beside = TRUE,
        names.arg = unique(monkey_means$monkey_id),
        main = "Individual Exploration Profiles",
        xlab = "Monkey", ylab = "Explore Probability",
        legend.text = c("Solo", "Duo", "Trio"),
        col = c("green", "orange", "red"))

# Plots 5-8: Expected Explore Value effects by context
contexts <- c("solo", "duo", "trio")
colors <- c("green", "orange", "red")

for (i in 1:3) {
  context_data <- other_vars_combined[other_vars_combined$social_context == contexts[i], ]
  plot(context_data$y06_expected_explore, context_data$explore,
       type = "b", pch = 19, col = colors[i], lwd = 2,
       main = paste("Expected Explore Effect -", toupper(contexts[i])),
       xlab = "Expected Explore Value (y06)",
       ylab = "Explore Probability",
       ylim = c(0, 1))
  grid()
}

# Plot 9: None choice patterns
none_matrix <- matrix(context_hierarchy_results$none, nrow = 3, ncol = 3)
rownames(none_matrix) <- c("Solo", "Duo", "Trio")
colnames(none_matrix) <- c("Dominant", "Intermediate", "Subordinate")

image(1:3, 1:3, none_matrix,
      main = "No Choice by Context × Hierarchy",
      xlab = "Hierarchy Level", ylab = "Social Context", 
      xaxt = "n", yaxt = "n", col = heat.colors(20))
axis(1, at = 1:3, labels = colnames(none_matrix))
axis(2, at = 1:3, labels = rownames(none_matrix))
for(i in 1:3) for(j in 1:3) text(j, i, round(none_matrix[i,j], 3))

# Plots 10-12: Individual monkey sex/rank profiles
for (sex_type in c("Male", "Female")) {
  sex_monkeys <- individual_combined[individual_combined$sex == sex_type, ]
  
  plot(1:3, rep(0, 3), type = "n", ylim = c(0, 1),
       main = paste(sex_type, "Monkeys - Exploration"),
       xlab = "Social Context (1=Solo, 2=Duo, 3=Trio)",
       ylab = "Explore Probability")
  
  unique_monkeys <- unique(sex_monkeys$monkey_id)
  colors_sex <- rainbow(length(unique_monkeys))
  
  for (j in 1:length(unique_monkeys)) {
    monkey_data <- sex_monkeys[sex_monkeys$monkey_id == unique_monkeys[j], ]
    lines(1:3, monkey_data$explore, col = colors_sex[j], lwd = 2)
    points(1:3, monkey_data$explore, col = colors_sex[j], pch = 19)
  }
  legend("topright", legend = unique_monkeys, col = colors_sex, lwd = 2, cex = 0.8)
}

dev.off()

# CALCULATE KEY STATISTICS
cat("\n=== KEY STATISTICS ===\n")

# Social context effect sizes
solo_explore <- mean(context_hierarchy_results$explore[context_hierarchy_results$social_context == "solo"])
duo_explore <- mean(context_hierarchy_results$explore[context_hierarchy_results$social_context == "duo"])
trio_explore <- mean(context_hierarchy_results$explore[context_hierarchy_results$social_context == "trio"])

cat(sprintf("Social Context Effects on Exploration:\n"))
cat(sprintf("Solo: %.1f%%, Duo: %.1f%%, Trio: %.1f%%\n", 
            solo_explore*100, duo_explore*100, trio_explore*100))
cat(sprintf("Solo→Duo change: %.1f percentage points\n", (duo_explore - solo_explore)*100))
cat(sprintf("Duo→Trio change: %.1f percentage points\n", (trio_explore - duo_explore)*100))

# Hierarchy effect sizes
dom_explore <- mean(context_hierarchy_results$explore[context_hierarchy_results$hierarchy_level == "Dominant"])
int_explore <- mean(context_hierarchy_results$explore[context_hierarchy_results$hierarchy_level == "Intermediate"]) 
sub_explore <- mean(context_hierarchy_results$explore[context_hierarchy_results$hierarchy_level == "Subordinate"])

cat(sprintf("\nHierarchy Effects on Exploration:\n"))
cat(sprintf("Dominant: %.1f%%, Intermediate: %.1f%%, Subordinate: %.1f%%\n",
            dom_explore*100, int_explore*100, sub_explore*100))

# Sex differences
male_explore <- mean(sex_combined$explore[sex_combined$sex == "Male"])
female_explore <- mean(sex_combined$explore[sex_combined$sex == "Female"])

cat(sprintf("\nSex Differences:\n"))
cat(sprintf("Males: %.1f%%, Females: %.1f%%\n", male_explore*100, female_explore*100))
cat(sprintf("Female advantage: %.1f percentage points\n", (female_explore - male_explore)*100))

# Individual differences
individual_means <- aggregate(explore ~ monkey_id + sex + overall_rank, 
                            data = individual_combined, mean)
cat(sprintf("\nIndividual Monkey Profiles:\n"))
print(individual_means)

# Save results
write.csv(context_hierarchy_results, "social_context_hierarchy_effects.csv", row.names = FALSE)
write.csv(sex_combined, "sex_hierarchy_interactions.csv", row.names = FALSE)
write.csv(individual_combined, "individual_monkey_profiles.csv", row.names = FALSE)
write.csv(other_vars_combined, "other_variables_by_context.csv", row.names = FALSE)

cat("\nAnalysis complete! Generated:\n")
cat("- comprehensive_social_hierarchy_analysis.pdf\n")
cat("- social_context_hierarchy_effects.csv\n")
cat("- sex_hierarchy_interactions.csv\n")
cat("- individual_monkey_profiles.csv\n") 
cat("- other_variables_by_context.csv\n") 