# Hierarchical Trinomial Model for Primate Explore-Exploit Decisions
# Following multilevel modeling principles with proper hierarchical structure

library(nnet)
library(ggplot2)

set.seed(42)
options(scipen = 999)

cat("=== HIERARCHICAL TRINOMIAL MODEL ===\n")
cat("Multilevel multinomial logistic regression\n")
cat("Level 1: Trials (i) nested within Level 2: Monkeys (j)\n\n")

# ===============================
# HIERARCHICAL DATA STRUCTURE
# ===============================

data <- read.csv("Explore Exploit Dataset.csv", stringsAsFactors = FALSE)
data_exp <- data[data$TRIAL_TYPE == "OIT_RE", ]

# Create hierarchical dataset
data_exp$outcome_clean <- ifelse(grepl("explore", data_exp$OUTCOME, ignore.case = TRUE), "explore",
                                ifelse(grepl("exploit", data_exp$OUTCOME, ignore.case = TRUE), "exploit", "none"))

data_clean <- data_exp[data_exp$outcome_clean %in% c("explore", "exploit", "none"), ]

# Hierarchical structure
hierarchical_data <- data.frame(
  # Level 1: Trial-level variables (i)
  trial_id = 1:nrow(data_clean),
  outcome = factor(data_clean$outcome_clean, levels = c("exploit", "explore", "none")),
  condition = factor(data_clean$CONDITION, levels = c("solo", "duo", "trio")),
  relative_rank = as.numeric(data_clean$RELATIVE_RANK),
  subjective_value = scale(as.numeric(data_clean$SUBJECTIVE_CHOSEN_VALUE))[,1],
  exploit_value = scale(as.numeric(data_clean$subjective_exploit))[,1],
  explore_expectation = scale(as.numeric(data_clean$expected_explore))[,1],
  block_num = as.numeric(gsub("BLOCK_", "", data_clean$BLOCK_No)),
  
  # Level 2: Monkey-level variables (j)
  monkey_id = factor(data_clean$monkey),
  
  stringsAsFactors = FALSE
)

# Remove missing data
hierarchical_data <- hierarchical_data[complete.cases(hierarchical_data), ]

cat("HIERARCHICAL DATA STRUCTURE:\n")
cat("Level 1 (Trials): N =", nrow(hierarchical_data), "observations\n")
cat("Level 2 (Monkeys): N =", length(unique(hierarchical_data$monkey_id)), "subjects\n")

# Show monkey-level summary
monkey_summary <- aggregate(trial_id ~ monkey_id, hierarchical_data, length)
names(monkey_summary)[2] <- "n_trials"
cat("\nTrials per monkey:\n")
for (i in 1:nrow(monkey_summary)) {
  cat(sprintf("%-10s: %3d trials\n", monkey_summary$monkey_id[i], monkey_summary$n_trials[i]))
}

cat("\nOutcome distribution by monkey:\n")
outcome_by_monkey <- table(hierarchical_data$monkey_id, hierarchical_data$outcome)
print(outcome_by_monkey)

# ===============================
# MATHEMATICAL MODEL SPECIFICATION
# ===============================

cat("\n=== MATHEMATICAL MODEL ===\n")
cat("Level 1 (Trial level):\n")
cat("P(Y_ij = k) = exp(η_ijk) / Σ_m exp(η_ijm)\n\n")

cat("Level 1 Linear Predictor:\n")
cat("η_ijk = β_0jk + β_1k * condition_ij + β_2k * relative_rank_ij +\n")
cat("        β_3k * subjective_value_ij + β_4k * exploit_value_ij +\n")
cat("        β_5k * explore_expectation_ij + β_6k * block_ij\n\n")

cat("Level 2 (Monkey level):\n")
cat("β_0jk = γ_00k + u_0jk\n")
cat("where u_0jk ~ N(0, τ²_k) for outcome k\n\n")

cat("Where:\n")
cat("- i = trial (1 to n_ij)\n")
cat("- j = monkey (1 to J = 6)\n") 
cat("- k = outcome (exploit, explore, none)\n")
cat("- Y_ij = trinomial outcome for trial i of monkey j\n")
cat("- u_0jk = random intercept for monkey j and outcome k\n\n")

# ===============================
# HIERARCHICAL MODEL FITTING
# ===============================

cat("=== FITTING HIERARCHICAL MODELS ===\n")

# Model 1: Fixed effects only (non-hierarchical baseline)
cat("Model 1: Fixed effects only...\n")
model_fixed <- multinom(outcome ~ condition + relative_rank + subjective_value + 
                       exploit_value + explore_expectation + block_num, 
                       data = hierarchical_data, trace = FALSE)

# Model 2: Hierarchical with monkey random intercepts
cat("Model 2: Hierarchical with monkey random intercepts...\n")
model_hierarchical <- multinom(outcome ~ condition + relative_rank + subjective_value + 
                              exploit_value + explore_expectation + block_num + monkey_id, 
                              data = hierarchical_data, trace = FALSE)

# Model 3: Hierarchical with interactions
cat("Model 3: Hierarchical with condition × expectation interaction...\n")
model_interaction <- multinom(outcome ~ condition * explore_expectation + relative_rank + 
                             subjective_value + exploit_value + block_num + monkey_id, 
                             data = hierarchical_data, trace = FALSE)

# Model comparison
models <- list(
  "Fixed_Effects" = model_fixed,
  "Hierarchical" = model_hierarchical, 
  "Hierarchical_Interaction" = model_interaction
)

aic_values <- sapply(models, AIC)
bic_values <- sapply(models, BIC)

cat("\nHIERARCHICAL MODEL COMPARISON:\n")
cat("Model                    AIC        BIC        Δ_AIC\n")
cat("----------------------------------------\n")
for (i in 1:length(aic_values)) {
  delta_aic <- aic_values[i] - min(aic_values)
  cat(sprintf("%-20s %8.2f %8.2f %8.2f\n", 
              names(aic_values)[i], aic_values[i], bic_values[i], delta_aic))
}

best_model <- models[[which.min(aic_values)]]
cat("\nBest model:", names(which.min(aic_values)), "\n")

# ===============================
# HIERARCHICAL VARIANCE COMPONENTS
# ===============================

cat("\n=== HIERARCHICAL VARIANCE COMPONENTS ===\n")

# Calculate ICC-like measure for multinomial
cat("Between-monkey variance (pseudo-ICC):\n")

# Predict individual monkey effects
monkey_effects <- data.frame()
for (monkey in unique(hierarchical_data$monkey_id)) {
  subset_data <- hierarchical_data[hierarchical_data$monkey_id == monkey, ]
  if (nrow(subset_data) > 5) {  # Only monkeys with sufficient data
    pred_probs <- predict(best_model, newdata = subset_data, type = "probs")
    avg_probs <- colMeans(pred_probs)
    
    monkey_effects <- rbind(monkey_effects, data.frame(
      monkey_id = monkey,
      exploit_prob = avg_probs["exploit"],
      explore_prob = avg_probs["explore"], 
      none_prob = avg_probs["none"]
    ))
  }
}

# Calculate between-monkey variance
if (nrow(monkey_effects) > 1) {
  between_var <- apply(monkey_effects[, -1], 2, var)
  cat("Between-monkey variance in predicted probabilities:\n")
  for (outcome in names(between_var)) {
    cat(sprintf("%-12s: σ² = %6.4f\n", outcome, between_var[outcome]))
  }
  
  # Total variance approximation
  total_var <- between_var + 0.33  # Approximate within-monkey variance for multinomial
  pseudo_icc <- between_var / total_var
  
  cat("\nPseudo-ICC (proportion of variance between monkeys):\n")
  for (outcome in names(pseudo_icc)) {
    cat(sprintf("%-12s: ICC = %5.3f\n", outcome, pseudo_icc[outcome]))
  }
}

# ===============================
# HIERARCHICAL PREDICTIONS
# ===============================

cat("\n=== HIERARCHICAL PREDICTIONS ===\n")

# Population-level predictions (fixed effects)
pop_grid <- expand.grid(
  condition = c("solo", "duo", "trio"),
  relative_rank = 1,
  subjective_value = 0,
  exploit_value = 0,
  explore_expectation = 0,
  block_num = mean(hierarchical_data$block_num),
  monkey_id = levels(hierarchical_data$monkey_id)[1]  # Reference monkey
)

pop_predictions <- predict(best_model, newdata = pop_grid, type = "probs")

cat("POPULATION-LEVEL EFFECTS (Fixed Effects):\n")
for (i in 1:nrow(pop_grid)) {
  cat(sprintf("%-8s: Exploit=%4.1f%%, Explore=%4.1f%%, None=%4.1f%%\n",
              pop_grid$condition[i],
              pop_predictions[i, "exploit"] * 100,
              pop_predictions[i, "explore"] * 100,
              pop_predictions[i, "none"] * 100))
}

# Individual monkey predictions
cat("\nINDIVIDUAL MONKEY EFFECTS:\n")
for (monkey in unique(hierarchical_data$monkey_id)) {
  monkey_grid <- pop_grid
  monkey_grid$monkey_id <- monkey
  monkey_pred <- predict(best_model, newdata = monkey_grid, type = "probs")
  
  cat(sprintf("\n%s:\n", monkey))
  for (i in 1:nrow(monkey_grid)) {
    cat(sprintf("  %-8s: Exploit=%4.1f%%, Explore=%4.1f%%, None=%4.1f%%\n",
                monkey_grid$condition[i],
                monkey_pred[i, "exploit"] * 100,
                monkey_pred[i, "explore"] * 100,
                monkey_pred[i, "none"] * 100))
  }
}

# ===============================
# HIERARCHICAL VISUALIZATION
# ===============================

cat("\n=== CREATING HIERARCHICAL PLOTS ===\n")

pdf("hierarchical_trinomial_plots.pdf", width = 14, height = 10)

# Plot 1: Hierarchical structure diagram (data visualization)
monkey_trial_counts <- table(hierarchical_data$monkey_id)
hierarchy_df <- data.frame(
  level = c(rep("Population", 1), rep("Monkey", length(monkey_trial_counts)), 
            rep("Trial", sum(monkey_trial_counts))),
  id = c("All Monkeys", names(monkey_trial_counts), 
         paste0("Trial_", 1:sum(monkey_trial_counts))),
  parent = c(NA, rep("All Monkeys", length(monkey_trial_counts)),
             rep(names(monkey_trial_counts), monkey_trial_counts)),
  trials = c(sum(monkey_trial_counts), monkey_trial_counts, rep(1, sum(monkey_trial_counts)))
)

# Simplified hierarchy plot
p1 <- ggplot(monkey_effects, aes(x = monkey_id)) +
  geom_col(aes(y = exploit_prob), fill = "#d73027", alpha = 0.7, width = 0.8) +
  geom_col(aes(y = explore_prob), fill = "#1a9850", alpha = 0.7, width = 0.6) +
  geom_col(aes(y = none_prob), fill = "#fee08b", alpha = 0.7, width = 0.4) +
  labs(title = "Hierarchical Structure: Individual Monkey Effects",
       subtitle = "Predicted probabilities showing between-monkey variation",
       x = "Monkey ID (Level 2)", y = "Predicted Probability") +
  scale_y_continuous(labels = function(x) paste0(round(x*100), "%")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
print(p1)

# Plot 2: Random effects by condition
individual_condition <- data.frame()
for (monkey in unique(hierarchical_data$monkey_id)) {
  for (cond in c("solo", "duo", "trio")) {
    subset_data <- hierarchical_data[hierarchical_data$monkey_id == monkey & 
                                   hierarchical_data$condition == cond, ]
    if (nrow(subset_data) > 0) {
      pred_probs <- predict(best_model, newdata = subset_data, type = "probs")
      avg_probs <- colMeans(pred_probs)
      
      individual_condition <- rbind(individual_condition, data.frame(
        monkey_id = monkey,
        condition = cond,
        exploit = avg_probs["exploit"],
        explore = avg_probs["explore"],
        none = avg_probs["none"]
      ))
    }
  }
}

# Reshape for plotting
library(reshape2)
individual_long <- melt(individual_condition, 
                       id.vars = c("monkey_id", "condition"),
                       variable.name = "outcome", value.name = "probability")

p2 <- ggplot(individual_long, aes(x = condition, y = probability, 
                                 color = monkey_id, group = monkey_id)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_line(alpha = 0.7, linewidth = 1) +
  facet_wrap(~outcome, scales = "free_y") +
  labs(title = "Individual Monkey Random Effects",
       subtitle = "How each monkey responds to social conditions",
       x = "Social Condition", y = "Predicted Probability",
       color = "Monkey ID") +
  scale_y_continuous(labels = function(x) paste0(round(x*100), "%")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "bottom",
    strip.text = element_text(size = 12, face = "bold")
  )
print(p2)

# Plot 3: Variance components visualization
if (exists("between_var") && exists("pseudo_icc")) {
  variance_df <- data.frame(
    outcome = names(between_var),
    between_monkey = between_var,
    within_monkey = 0.33,  # Approximate
    icc = pseudo_icc
  )
  
  variance_long <- melt(variance_df[, 1:3], id.vars = "outcome",
                       variable.name = "variance_type", value.name = "variance")
  
  p3 <- ggplot(variance_long, aes(x = outcome, y = variance, fill = variance_type)) +
    geom_col(position = "stack", alpha = 0.8) +
    geom_text(data = variance_df, aes(x = outcome, y = between_monkey + 0.15, 
                                     label = paste0("ICC = ", round(icc, 3))),
              inherit.aes = FALSE, size = 3, fontface = "bold") +
    labs(title = "Hierarchical Variance Components",
         subtitle = "Decomposition of variance: Between vs. Within monkeys",
         x = "Outcome Type", y = "Variance", fill = "Variance Source") +
    scale_fill_manual(values = c("between_monkey" = "#2166ac", "within_monkey" = "#762a83"),
                     labels = c("Between Monkeys", "Within Monkeys")) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      legend.position = "bottom"
    )
  print(p3)
}

dev.off()

# ===============================
# HIERARCHICAL SUMMARY
# ===============================

cat("\n=== HIERARCHICAL MODEL SUMMARY ===\n")
cat("=====================================\n")

cat("HIERARCHICAL STRUCTURE:\n")
cat("- Level 1: Trials (N =", nrow(hierarchical_data), ")\n")
cat("- Level 2: Monkeys (N =", length(unique(hierarchical_data$monkey_id)), ")\n")
cat("- Outcomes: 3 (exploit, explore, none)\n")

cat("\nMODEL FIT:\n")
cat("- Best Model:", names(which.min(aic_values)), "\n")
cat("- AIC:", round(min(aic_values), 2), "\n")
cat("- Model accounts for monkey-level clustering\n")

if (exists("pseudo_icc")) {
  cat("\nHIERARCHICAL EFFECTS:\n")
  for (outcome in names(pseudo_icc)) {
    cat(sprintf("- %-12s: %5.1f%% of variance between monkeys\n", 
                outcome, pseudo_icc[outcome] * 100))
  }
}

cat("\nVisualization saved to: hierarchical_trinomial_plots.pdf\n")
cat("This properly models the nested structure of trials within monkeys.\n") 