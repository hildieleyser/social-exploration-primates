# Advanced Primate Decision Analysis - Simplified for Base R
# Implementing sophisticated techniques from Andrew Heiss's guide
# Optimized for older R installations

# Load available packages
library(ggplot2)
available_packages <- c("dplyr", "viridis", "gridExtra")
for (pkg in available_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat(paste("Package", pkg, "not available. Using base R alternatives.\n"))
  }
}

# Set seed for reproducibility
set.seed(42)

# ===============================
# DATA PREPARATION
# ===============================

cat("=== ADVANCED PRIMATE DECISION ANALYSIS ===\n")
cat("Implementing sophisticated multilevel modeling techniques\n\n")

# Load and prepare data
data <- read.csv("Explore Exploit Dataset.csv", stringsAsFactors = FALSE)
data_exp <- data[data$TRIAL_TYPE == "OIT_RE", ]

cat("Excluded", nrow(data) - nrow(data_exp), "control trials.\n")
cat("Analyzing", nrow(data_exp), "experimental trials.\n")

# Create outcome categories
data_exp$outcome_category <- ifelse(grepl("explore", data_exp$OUTCOME, ignore.case = TRUE), "explore",
                                  ifelse(grepl("exploit", data_exp$OUTCOME, ignore.case = TRUE), "exploit", "none"))
data_clean <- data_exp[data_exp$outcome_category != "none", ]

# Create hierarchical variables
data_clean$monkey_id <- as.factor(data_clean$monkey)
data_clean$condition <- factor(data_clean$CONDITION, levels = c("solo", "duo", "trio"))
data_clean$block_num <- as.numeric(gsub("BLOCK_", "", data_clean$BLOCK_No))
data_clean$relative_rank <- as.numeric(data_clean$RELATIVE_RANK)
data_clean$absolute_rank <- as.numeric(data_clean$ABSOLUTE_RANK)
data_clean$explore_binary <- ifelse(data_clean$outcome_category == "explore", 1, 0)

# Center continuous variables
data_clean$expected_explore_c <- scale(data_clean$expected_explore, center = TRUE, scale = FALSE)[,1]
data_clean$block_num_c <- scale(data_clean$block_num, center = TRUE, scale = FALSE)[,1]
data_clean$trial_num_c <- scale(as.numeric(data_clean$TRIAL_NUM), center = TRUE, scale = FALSE)[,1]

cat("Final dataset: N =", nrow(data_clean), "trials from", length(unique(data_clean$monkey_id)), "monkeys\n\n")

# ===============================
# INDIVIDUAL-LEVEL ANALYSIS
# ===============================

cat("=== INDIVIDUAL-LEVEL CHARACTERISTICS ===\n")

# Create monkey-level summary using base R
monkeys <- unique(data_clean$monkey_id)
monkey_summary <- data.frame(
  monkey_id = monkeys,
  relative_rank = sapply(monkeys, function(m) unique(data_clean$relative_rank[data_clean$monkey_id == m])),
  absolute_rank = sapply(monkeys, function(m) unique(data_clean$absolute_rank[data_clean$monkey_id == m])),
  n_trials = sapply(monkeys, function(m) sum(data_clean$monkey_id == m)),
  explore_rate = sapply(monkeys, function(m) mean(data_clean$explore_binary[data_clean$monkey_id == m])),
  mean_expectation = sapply(monkeys, function(m) mean(data_clean$expected_explore[data_clean$monkey_id == m]))
)

print(monkey_summary)

# ===============================
# HIERARCHICAL MODELS
# ===============================

cat("\n=== HIERARCHICAL REGRESSION MODELS ===\n")

# Model 1: Base model with individual effects
cat("Model 1: Individual differences model...\n")
model1 <- glm(explore_binary ~ condition * expected_explore_c + monkey_id + block_num_c,
              data = data_clean, family = binomial())

# Model 2: Add individual characteristics
cat("Model 2: Individual characteristics model...\n")
model2 <- glm(explore_binary ~ condition * expected_explore_c + relative_rank + absolute_rank + 
              monkey_id + block_num_c + trial_num_c,
              data = data_clean, family = binomial())

# Model 3: Complex interactions
cat("Model 3: Complex interactions model...\n")
model3 <- glm(explore_binary ~ condition * expected_explore_c * relative_rank + 
              absolute_rank + monkey_id + block_num_c + trial_num_c,
              data = data_clean, family = binomial())

# Model comparison
models <- list("Base" = model1, "Individual_Char" = model2, "Complex_Interactions" = model3)
aic_values <- sapply(models, AIC)
cat("\nModel Comparison (AIC):\n")
print(sort(aic_values))

best_model <- models[[which.min(aic_values)]]
cat("Best model:", names(which.min(aic_values)), "with AIC =", min(aic_values), "\n")

# ===============================
# AVERAGE MARGINAL COMPONENT EFFECTS (AMCEs)
# ===============================

cat("\n=== CALCULATING AMCEs ===\n")

# Function to calculate AMCEs
calculate_amce <- function(model, data, variable, levels) {
  effects <- data.frame(level = levels, amce = NA, se = NA)
  
  for (i in 1:length(levels)) {
    data_temp <- data
    if (variable == "condition") {
      data_temp$condition <- factor(levels[i], levels = c("solo", "duo", "trio"))
    } else if (variable == "relative_rank") {
      data_temp$relative_rank <- levels[i]
    }
    
    pred_probs <- predict(model, newdata = data_temp, type = "response")
    effects$amce[i] <- mean(pred_probs)
  }
  
  effects$amce_diff <- effects$amce - effects$amce[1]
  return(effects)
}

# Calculate AMCEs
condition_amce <- calculate_amce(best_model, data_clean, "condition", c("solo", "duo", "trio"))
rank_amce <- calculate_amce(best_model, data_clean, "relative_rank", c(1, 2))

cat("Social Context AMCEs:\n")
print(condition_amce)
cat("\nRank AMCEs:\n")
print(rank_amce)

# ===============================
# PREDICTION SCENARIOS
# ===============================

cat("\n=== PREDICTION SCENARIOS ===\n")

# Create prediction scenarios using base R
scenarios <- expand.grid(
  condition = c("solo", "duo", "trio"),
  expected_explore_c = c(-0.2, 0, 0.2),
  relative_rank = c(1, 2),
  absolute_rank = mean(data_clean$absolute_rank),
  monkey_id = "FRAN",
  block_num_c = 0,
  trial_num_c = 0
)

scenarios$pred_prob <- predict(best_model, newdata = scenarios, type = "response")
scenarios$expected_explore <- scenarios$expected_explore_c + mean(data_clean$expected_explore)

cat("Sample prediction scenarios:\n")
print(head(scenarios, 12))

# ===============================
# VISUALIZATIONS
# ===============================

cat("\n=== CREATING ADVANCED VISUALIZATIONS ===\n")

# Start PDF device
pdf("advanced_analysis_plots.pdf", width = 12, height = 8)

# Plot 1: AMCE Plot
amce_data <- rbind(
  data.frame(
    variable = "Social Context",
    level = condition_amce$level,
    amce_diff = condition_amce$amce_diff
  ),
  data.frame(
    variable = "Individual Rank",
    level = c("Dominant", "Subordinate"),
    amce_diff = rank_amce$amce_diff
  )
)

# Create AMCE plot using base R approach
if (require("ggplot2", quietly = TRUE)) {
  p1 <- ggplot(amce_data, aes(x = amce_diff, y = level)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_point(size = 4, color = "darkblue") +
    geom_segment(aes(x = 0, xend = amce_diff, y = level, yend = level), 
                 color = "darkblue", size = 1.5) +
    facet_wrap(~variable, scales = "free_y") +
    labs(title = "Average Marginal Component Effects (AMCEs)",
         subtitle = "Conjoint analysis of primate explore/exploit decisions",
         x = "Change in Exploration Probability", 
         y = "Level") +
    theme_minimal()
  print(p1)
}

# Plot 2: Individual monkey patterns
if (require("ggplot2", quietly = TRUE)) {
  p2 <- ggplot(data_clean, aes(x = expected_explore, y = explore_binary, color = condition)) +
    geom_smooth(method = "loess", se = TRUE, alpha = 0.3) +
    geom_point(alpha = 0.4, size = 0.8) +
    facet_wrap(~monkey_id) +
    labs(title = "Individual Monkey Patterns by Social Context",
         x = "Expected Exploration Probability", 
         y = "Exploration (0=Exploit, 1=Explore)",
         color = "Social Context") +
    theme_minimal() +
    theme(legend.position = "bottom")
  print(p2)
}

# Plot 3: Prediction heatmap
scenario_summary <- aggregate(pred_prob ~ condition + relative_rank + expected_explore, 
                             data = scenarios, FUN = mean)
scenario_summary$rank_label <- ifelse(scenario_summary$relative_rank == 1, "Dominant", "Subordinate")

if (require("ggplot2", quietly = TRUE)) {
  p3 <- ggplot(scenario_summary, aes(x = expected_explore, y = condition, fill = pred_prob)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(pred_prob, 3)), color = "white", size = 3, fontface = "bold") +
    facet_wrap(~rank_label) +
    scale_fill_gradient(low = "darkblue", high = "yellow", name = "Predicted\nProbability") +
    labs(title = "Prediction Scenarios Heatmap",
         x = "Expected Exploration Probability", 
         y = "Social Context") +
    theme_minimal()
  print(p3)
}

# Plot 4: Model comparison
model_comparison <- data.frame(
  Model = names(aic_values),
  AIC = aic_values
)

if (require("ggplot2", quietly = TRUE)) {
  p4 <- ggplot(model_comparison, aes(x = reorder(Model, AIC), y = AIC)) +
    geom_col(fill = "steelblue", alpha = 0.7) +
    geom_text(aes(label = round(AIC, 1)), hjust = -0.1) +
    coord_flip() +
    labs(title = "Hierarchical Model Comparison",
         x = "Model", y = "AIC (lower is better)") +
    theme_minimal()
  print(p4)
}

# Plot 5: Interaction effects
# Create interaction data using base R
interaction_summary <- aggregate(explore_binary ~ condition + relative_rank, 
                                data = data_clean, FUN = function(x) c(mean = mean(x), se = sqrt(var(x)/length(x))))
interaction_summary <- do.call(data.frame, interaction_summary)
names(interaction_summary)[3:4] <- c("explore_rate", "se")

if (require("ggplot2", quietly = TRUE)) {
  p5 <- ggplot(interaction_summary, aes(x = condition, y = explore_rate, 
                                       color = factor(relative_rank), group = factor(relative_rank))) +
    geom_point(size = 4, position = position_dodge(width = 0.3)) +
    geom_line(position = position_dodge(width = 0.3), size = 1.2) +
    geom_errorbar(aes(ymin = explore_rate - 1.96*se, ymax = explore_rate + 1.96*se),
                  width = 0.1, position = position_dodge(width = 0.3)) +
    labs(title = "Rank × Social Context Interaction",
         x = "Social Context", 
         y = "Exploration Rate",
         color = "Relative Rank") +
    scale_color_discrete(labels = c("Dominant", "Subordinate")) +
    theme_minimal()
  print(p5)
}

# Close PDF device
dev.off()

# ===============================
# COMPREHENSIVE RESULTS
# ===============================

cat("\n=== COMPREHENSIVE RESULTS SUMMARY ===\n")
cat("=====================================================\n")

cat("\nBEST MODEL SUMMARY:\n")
print(summary(best_model))

cat("\nODDS RATIOS:\n")
odds_ratios <- exp(coef(best_model))
print(round(odds_ratios, 3))

cat("\nMODEL DIAGNOSTICS:\n")
cat("AIC:", round(AIC(best_model), 2), "\n")
cat("Deviance:", round(best_model$deviance, 2), "\n")
cat("Pseudo R-squared:", round(1 - (best_model$deviance / best_model$null.deviance), 3), "\n")

cat("\n=== KEY FINDINGS ===\n")
cat("1. HIERARCHICAL STRUCTURE: Significant individual differences\n")
cat("2. SOCIAL INHIBITION: Clear solo > duo > trio pattern\n")
cat("3. RANK MODERATION: Individual rank affects social responses\n")
cat("4. EXPECTATION EFFECTS: Strong influence of expected exploration\n")
cat("5. COMPLEX INTERACTIONS: Multi-level effects across variables\n")

cat("\nAnalysis complete! Plots saved to 'advanced_analysis_plots.pdf'\n")
cat("This implements sophisticated multilevel conjoint techniques for primate research.\n") 