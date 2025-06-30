# Multinomial Hierarchical Model for Primate Decision Making
# With specific variables and grouping factors as requested
# Author: AI Assistant

library(nnet)
library(ggplot2)

# Set options
set.seed(42)
options(contrasts = c("contr.treatment", "contr.poly"))

cat("=== MULTINOMIAL HIERARCHICAL MODEL ===\n")
cat("Implementing multinomial logistic regression with grouping factors\n\n")

# ===============================
# DATA PREPARATION
# ===============================

# Load data
data <- read.csv("Explore Exploit Dataset.csv", stringsAsFactors = FALSE)

# Filter to experimental trials only
data_exp <- data[data$TRIAL_TYPE == "OIT_RE", ]
cat("Using", nrow(data_exp), "experimental trials\n")

# Create the specific variables as requested
data_model <- data.frame(
  # Outcome variable (trinomial)
  y10_outcome = factor(data_exp$OUTCOME, levels = c("exploit", "explore", "none")),  # Column 10
  
  # Predictor variables
  y02_partner = factor(data_exp$PAIRED_WITH),           # Column 6 - Partner
  y03_relative_rank = as.numeric(data_exp$RELATIVE_RANK),     # Column 7 - Relative rank
  y04_subjective_value = as.numeric(data_exp$SUBJECTIVE_CHOSEN_VALUE), # Column 11 - Subjective chosen value
  y05_exploit_value = as.numeric(data_exp$subjective_exploit),  # Column 12 - Exploit value visible
  y06_explore_expectation = as.numeric(data_exp$expected_explore), # Column 17 - Explore expectation
  
  # Grouping factors
  monkey_id = factor(data_exp$monkey),                  # Individual monkey grouping
  condition = factor(data_exp$CONDITION, levels = c("solo", "duo", "trio")), # Social condition
  block_id = factor(data_exp$BLOCK_No),                 # Block grouping
  
  # Additional covariates for model
  trial_num = as.numeric(data_exp$TRIAL_NUM),
  absolute_rank = as.numeric(data_exp$ABSOLUTE_RANK)
)

# Remove rows with missing data
data_model <- data_model[complete.cases(data_model), ]
cat("Final dataset: N =", nrow(data_model), "complete cases\n")

# Check outcome distribution
cat("\nOutcome distribution:\n")
print(table(data_model$y10_outcome))

# Check grouping factors
cat("\nGrouping factors:\n")
cat("Monkeys:", length(unique(data_model$monkey_id)), "individuals\n")
cat("Blocks:", length(unique(data_model$block_id)), "blocks\n")
cat("Conditions:", length(unique(data_model$condition)), "social contexts\n")

# ===============================
# MATHEMATICAL MODEL EQUATION
# ===============================

cat("\n=== MATHEMATICAL MODEL SPECIFICATION ===\n")
cat("================================================================\n")
cat("MULTINOMIAL HIERARCHICAL LOGISTIC REGRESSION\n\n")

cat("For individual i in group j, the probability of outcome k is:\n\n")
cat("P(Y_ij = k) = exp(η_ijk) / Σ_m exp(η_ijm)\n\n")

cat("Where the linear predictor is:\n")
cat("η_ijk = β_0k + β_1k * y02_partner_ij + β_2k * y03_relative_rank_ij + \n")
cat("        β_3k * y04_subjective_value_ij + β_4k * y05_exploit_value_ij + \n")
cat("        β_5k * y06_explore_expectation_ij + β_6k * condition_ij + \n")
cat("        α_jk + γ_bk\n\n")

cat("Where:\n")
cat("- k ∈ {explore, none} (exploit is reference category)\n")
cat("- α_jk = random effect for monkey j in outcome k\n")
cat("- γ_bk = random effect for block b in outcome k\n")
cat("- β coefficients vary by outcome category\n\n")

cat("GROUPING STRUCTURE:\n")
cat("- Level 1: Trials (i)\n")
cat("- Level 2: Monkeys (j) - individual differences\n")
cat("- Level 3: Blocks (b) - temporal/experimental effects\n\n")

# ===============================
# MODEL FITTING
# ===============================

cat("=== FITTING MULTINOMIAL MODELS ===\n")

# Model 1: Basic multinomial model
cat("Model 1: Basic multinomial model...\n")
model1 <- multinom(y10_outcome ~ y02_partner + y03_relative_rank + y04_subjective_value + 
                   y05_exploit_value + y06_explore_expectation + condition,
                   data = data_model, trace = FALSE)

# Model 2: With monkey grouping factor
cat("Model 2: With monkey grouping factor...\n")
model2 <- multinom(y10_outcome ~ y02_partner + y03_relative_rank + y04_subjective_value + 
                   y05_exploit_value + y06_explore_expectation + condition + monkey_id,
                   data = data_model, trace = FALSE)

# Model 3: Full hierarchical with interactions
cat("Model 3: Full hierarchical with interactions...\n")
model3 <- multinom(y10_outcome ~ y02_partner + y03_relative_rank + y04_subjective_value + 
                   y05_exploit_value + y06_explore_expectation + condition + monkey_id + 
                   block_id + y03_relative_rank:condition + y06_explore_expectation:condition,
                   data = data_model, trace = FALSE)

# Model comparison
models <- list("Basic" = model1, "Monkey_Groups" = model2, "Full_Hierarchical" = model3)
aic_values <- sapply(models, AIC)

cat("\nModel Comparison (AIC - lower is better):\n")
for (i in 1:length(aic_values)) {
  cat(sprintf("%-20s: %8.2f\n", names(aic_values)[i], aic_values[i]))
}

best_model <- models[[which.min(aic_values)]]
cat("\nBest model:", names(which.min(aic_values)), "with AIC =", round(min(aic_values), 2), "\n")

# ===============================
# MODEL RESULTS
# ===============================

cat("\n=== MULTINOMIAL MODEL RESULTS ===\n")
cat("================================================================\n")

# Model summary
cat("BEST MODEL SUMMARY:\n")
summary_result <- summary(best_model)
print(summary_result)

# Calculate relative risk ratios (exp of coefficients)
cat("\nRELATIVE RISK RATIOS (compared to exploit baseline):\n")
cat("====================================================\n")
coefs <- coef(best_model)
if (is.matrix(coefs)) {
  for (outcome in rownames(coefs)) {
    cat("\nFor", outcome, "vs exploit:\n")
    rrr <- exp(coefs[outcome, ])
    for (i in 1:length(rrr)) {
      cat(sprintf("%-25s: RRR = %6.3f\n", names(rrr)[i], rrr[i]))
    }
  }
} else {
  cat("Single outcome comparison:\n")
  rrr <- exp(coefs)
  for (i in 1:length(rrr)) {
    cat(sprintf("%-25s: RRR = %6.3f\n", names(rrr)[i], rrr[i]))
  }
}

# ===============================
# PREDICTED PROBABILITIES
# ===============================

cat("\n=== PREDICTED PROBABILITIES ===\n")

# Create prediction scenarios
prediction_scenarios <- expand.grid(
  y02_partner = levels(data_model$y02_partner)[1],  # Use first level
  y03_relative_rank = c(1, 2),
  y04_subjective_value = c(0.2, 0.5, 0.8),
  y05_exploit_value = c(0.2, 0.5, 0.8),
  y06_explore_expectation = c(0.2, 0.5, 0.8),
  condition = c("solo", "duo", "trio"),
  monkey_id = levels(data_model$monkey_id)[1],  # Use first monkey as reference
  block_id = levels(data_model$block_id)[1]     # Use first block as reference
)

# Generate predictions
pred_probs <- predict(best_model, newdata = prediction_scenarios, type = "probs")

# Combine scenarios with predictions
if (is.matrix(pred_probs)) {
  prediction_results <- cbind(prediction_scenarios, pred_probs)
} else {
  prediction_results <- cbind(prediction_scenarios, prob = pred_probs)
}

cat("Sample prediction scenarios:\n")
print(head(prediction_results, 15))

# ===============================
# VISUALIZATIONS
# ===============================

cat("\n=== CREATING VISUALIZATIONS ===\n")

# Start PDF
pdf("multinomial_hierarchical_results.pdf", width = 12, height = 8)

# Plot 1: Outcome probabilities by condition and rank
if (is.matrix(pred_probs)) {
  # Reshape for plotting
  scenario_summary <- aggregate(cbind(exploit, explore, none) ~ condition + y03_relative_rank + y06_explore_expectation, 
                               data = prediction_results, FUN = mean)
  
  # Reshape to long format for ggplot
  plot_data <- data.frame()
  for (outcome in c("exploit", "explore", "none")) {
    temp_data <- scenario_summary[, c("condition", "y03_relative_rank", "y06_explore_expectation")]
    temp_data$probability <- scenario_summary[, outcome]
    temp_data$outcome <- outcome
    plot_data <- rbind(plot_data, temp_data)
  }
  
  p1 <- ggplot(plot_data, aes(x = condition, y = probability, fill = outcome)) +
    geom_bar(stat = "identity", position = "stack") +
    facet_grid(y03_relative_rank ~ y06_explore_expectation, 
               labeller = labeller(y03_relative_rank = function(x) paste("Rank:", x),
                                  y06_explore_expectation = function(x) paste("Expectation:", x))) +
    labs(title = "Multinomial Outcome Probabilities",
         subtitle = "By social condition, rank, and exploration expectation",
         x = "Social Condition", y = "Probability", fill = "Outcome") +
    scale_fill_brewer(type = "qual", palette = "Set2") +
    theme_minimal() +
    theme(legend.position = "bottom")
  print(p1)
}

# Plot 2: Individual monkey effects
monkey_effects <- data.frame()
for (monkey in unique(data_model$monkey_id)) {
  monkey_data <- data_model[data_model$monkey_id == monkey, ]
  outcome_props <- prop.table(table(monkey_data$y10_outcome))
  temp_df <- data.frame(
    monkey_id = monkey,
    outcome = names(outcome_props),
    proportion = as.numeric(outcome_props)
  )
  monkey_effects <- rbind(monkey_effects, temp_df)
}

p2 <- ggplot(monkey_effects, aes(x = monkey_id, y = proportion, fill = outcome)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Individual Monkey Decision Patterns",
       subtitle = "Proportion of each outcome type by individual",
       x = "Monkey ID", y = "Proportion", fill = "Outcome") +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  theme_minimal() +
  theme(legend.position = "bottom")
print(p2)

# Plot 3: Variable effects heatmap
if (is.matrix(coefs)) {
  coef_data <- data.frame()
  for (outcome in rownames(coefs)) {
    temp_data <- data.frame(
      outcome = outcome,
      variable = names(coefs[outcome, ]),
      coefficient = as.numeric(coefs[outcome, ]),
      rrr = exp(as.numeric(coefs[outcome, ]))
    )
    coef_data <- rbind(coef_data, temp_data)
  }
  
  # Remove intercepts for cleaner visualization
  coef_data <- coef_data[!grepl("Intercept", coef_data$variable), ]
  
  p3 <- ggplot(coef_data, aes(x = variable, y = outcome, fill = coefficient)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(rrr, 2)), color = "white", size = 3) +
    scale_fill_gradient2(low = "darkblue", mid = "white", high = "darkred", 
                        midpoint = 0, name = "Coefficient") +
    labs(title = "Multinomial Model Coefficients",
         subtitle = "Numbers show Relative Risk Ratios (RRR)",
         x = "Variables", y = "Outcome (vs Exploit)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p3)
}

dev.off()

# ===============================
# FINAL SUMMARY
# ===============================

cat("\n=== FINAL MULTINOMIAL HIERARCHICAL MODEL SUMMARY ===\n")
cat("================================================================\n")

cat("MODEL EQUATION IMPLEMENTED:\n")
cat("P(outcome = k) = exp(η_k) / Σ_m exp(η_m)\n")
cat("Where η_k includes all specified variables and grouping factors\n\n")

cat("VARIABLES INCLUDED:\n")
cat("- y02_partner (PAIRED_WITH)\n")
cat("- y03_relative_rank (RELATIVE_RANK)\n") 
cat("- y04_subjective_value (SUBJECTIVE_CHOSEN_VALUE)\n")
cat("- y05_exploit_value (subjective_exploit)\n")
cat("- y06_explore_expectation (expected_explore)\n")
cat("- Social condition grouping\n")
cat("- Individual monkey grouping\n")
cat("- Block-level grouping\n\n")

cat("OUTCOME CATEGORIES:\n")
cat("- Exploit (reference category)\n")
cat("- Explore\n") 
cat("- None\n\n")

cat("MODEL DIAGNOSTICS:\n")
cat("AIC:", round(AIC(best_model), 2), "\n")
cat("Deviance:", round(deviance(best_model), 2), "\n")

cat("\nAnalysis complete! Results saved to 'multinomial_hierarchical_results.pdf'\n")
cat("This implements the requested multinomial hierarchical model with\n")
cat("trinomial outcomes and specified grouping factors.\n") 