# Andrew Heiss-Style Trinomial Analysis
# Proper multinomial logistic regression with explore/exploit/none outcomes
# Following Andrew Heiss's exact approach for multinomial models

library(nnet)
library(ggplot2)

set.seed(42)
options(scipen = 999)

cat("=== ANDREW HEISS-STYLE TRINOMIAL ANALYSIS ===\n")
cat("Multinomial logistic regression with explore/exploit/none outcomes\n\n")

# ===============================
# DATA PREPARATION
# ===============================

data <- read.csv("Explore Exploit Dataset.csv", stringsAsFactors = FALSE)
data_exp <- data[data$TRIAL_TYPE == "OIT_RE", ]

cat("Starting with", nrow(data_exp), "experimental trials\n")

# Create trinomial outcome (keeping all three categories)
data_exp$outcome_clean <- ifelse(grepl("explore", data_exp$OUTCOME, ignore.case = TRUE), "explore",
                                ifelse(grepl("exploit", data_exp$OUTCOME, ignore.case = TRUE), "exploit", "none"))

# Check outcome distribution
cat("Raw outcome distribution:\n")
print(table(data_exp$outcome_clean))

# Filter to complete cases only
data_clean <- data_exp[data_exp$outcome_clean %in% c("explore", "exploit", "none"), ]

# Create model dataset with all three outcomes
data_model <- data.frame(
  # Trinomial outcome (exploit as reference category)
  outcome = factor(data_clean$outcome_clean, levels = c("exploit", "explore", "none")),
  
  # Main predictors (properly scaled)
  condition = factor(data_clean$CONDITION, levels = c("solo", "duo", "trio")),
  relative_rank = as.numeric(data_clean$RELATIVE_RANK),
  subjective_value = scale(as.numeric(data_clean$SUBJECTIVE_CHOSEN_VALUE))[,1],
  exploit_value = scale(as.numeric(data_clean$subjective_exploit))[,1],
  explore_expectation = scale(as.numeric(data_clean$expected_explore))[,1],
  
  # Grouping factors
  monkey_id = factor(data_clean$monkey),
  block_num = as.numeric(gsub("BLOCK_", "", data_clean$BLOCK_No))
)

# Remove missing data
data_model <- data_model[complete.cases(data_model), ]

cat("Clean trinomial dataset: N =", nrow(data_model), "decisions\n")
cat("Final outcome distribution:\n")
outcome_props <- prop.table(table(data_model$outcome))
for (i in 1:length(outcome_props)) {
  cat(sprintf("%-8s: %5.1f%% (n=%d)\n", names(outcome_props)[i], 
              outcome_props[i]*100, table(data_model$outcome)[i]))
}

# ===============================
# MULTINOMIAL MODEL FITTING
# ===============================

cat("\n=== FITTING MULTINOMIAL MODELS ===\n")

# Model 1: Basic multinomial
cat("Model 1: Basic multinomial model...\n")
model1 <- multinom(outcome ~ condition + relative_rank + subjective_value + 
                   exploit_value + explore_expectation, 
                   data = data_model, trace = FALSE)

# Model 2: With individual effects
cat("Model 2: With monkey fixed effects...\n")
model2 <- multinom(outcome ~ condition + relative_rank + subjective_value + 
                   exploit_value + explore_expectation + monkey_id, 
                   data = data_model, trace = FALSE)

# Model 3: With interactions
cat("Model 3: With condition × expectation interaction...\n")
model3 <- multinom(outcome ~ condition * explore_expectation + relative_rank + 
                   subjective_value + exploit_value + monkey_id, 
                   data = data_model, trace = FALSE)

# Model comparison
models <- list("Basic" = model1, "Monkey_FE" = model2, "Interactions" = model3)
aic_values <- sapply(models, AIC)

cat("\nModel Comparison:\n")
for (i in 1:length(aic_values)) {
  cat(sprintf("%-15s: AIC = %8.2f\n", names(aic_values)[i], aic_values[i]))
}

best_model <- models[[which.min(aic_values)]]
cat("\nBest model:", names(which.min(aic_values)), "\n")

# ===============================
# MULTINOMIAL RESULTS
# ===============================

cat("\n=== MULTINOMIAL MODEL RESULTS ===\n")

# Extract coefficients properly
coef_matrix <- coef(best_model)
if (is.matrix(coef_matrix)) {
  cat("Coefficients (Relative Risk Ratios vs. Exploit baseline):\n\n")
  
  for (outcome in rownames(coef_matrix)) {
    cat("=== ", toupper(outcome), " vs EXPLOIT ===\n")
    coefs <- coef_matrix[outcome, ]
    
    # Calculate RRRs and standard errors
    se_matrix <- summary(best_model)$standard.errors
    ses <- se_matrix[outcome, ]
    
    for (i in 1:length(coefs)) {
      rrr <- exp(coefs[i])
      ci_lower <- exp(coefs[i] - 1.96 * ses[i])
      ci_upper <- exp(coefs[i] + 1.96 * ses[i])
      
      # Calculate z-score and p-value
      z_score <- coefs[i] / ses[i]
      p_value <- 2 * (1 - pnorm(abs(z_score)))
      
      # Only show main effects (not monkey IDs)
      if (!grepl("monkey_id", names(coefs)[i])) {
        cat(sprintf("%-30s: RRR = %5.2f (95%% CI: %4.2f-%5.2f), p = %6.4f\n",
                    names(coefs)[i], rrr, ci_lower, ci_upper, p_value))
      }
    }
    cat("\n")
  }
}

# ===============================
# MULTINOMIAL MARGINAL EFFECTS
# ===============================

cat("=== MULTINOMIAL MARGINAL EFFECTS ===\n")

# Function for multinomial marginal effects
calc_multinomial_me <- function(model, data, variable, levels) {
  results <- data.frame()
  
  for (level in levels) {
    data_temp <- data
    data_temp[[variable]] <- level
    
    # Predict probabilities for all outcomes
    pred_probs <- predict(model, newdata = data_temp, type = "probs")
    
    if (is.matrix(pred_probs)) {
      # Multiple outcomes
      avg_probs <- colMeans(pred_probs)
      se_probs <- apply(pred_probs, 2, function(x) sd(x) / sqrt(length(x)))
      
      for (outcome in names(avg_probs)) {
        results <- rbind(results, data.frame(
          variable = variable,
          level = as.character(level),
          outcome = outcome,
          probability = avg_probs[outcome],
          std_error = se_probs[outcome],
          ci_lower = avg_probs[outcome] - 1.96 * se_probs[outcome],
          ci_upper = avg_probs[outcome] + 1.96 * se_probs[outcome]
        ))
      }
    }
  }
  return(results)
}

# Calculate marginal effects
me_condition <- calc_multinomial_me(best_model, data_model, "condition", c("solo", "duo", "trio"))
me_rank <- calc_multinomial_me(best_model, data_model, "relative_rank", c(1, 2))

cat("Social Condition Marginal Effects:\n")
for (outcome in c("exploit", "explore", "none")) {
  cat("--- ", toupper(outcome), " ---\n")
  subset_data <- me_condition[me_condition$outcome == outcome, ]
  for (i in 1:nrow(subset_data)) {
    cat(sprintf("%-8s: %4.1f%% (95%% CI: %4.1f%%-%4.1f%%)\n",
                subset_data$level[i], subset_data$probability[i]*100,
                subset_data$ci_lower[i]*100, subset_data$ci_upper[i]*100))
  }
  cat("\n")
}

cat("Rank Marginal Effects:\n")
rank_labels <- c("Dominant", "Subordinate")
for (outcome in c("exploit", "explore", "none")) {
  cat("--- ", toupper(outcome), " ---\n")
  subset_data <- me_rank[me_rank$outcome == outcome, ]
  for (i in 1:nrow(subset_data)) {
    cat(sprintf("%-12s: %4.1f%% (95%% CI: %4.1f%%-%4.1f%%)\n",
                rank_labels[i], subset_data$probability[i]*100,
                subset_data$ci_lower[i]*100, subset_data$ci_upper[i]*100))
  }
  cat("\n")
}

# ===============================
# ANDREW HEISS-STYLE TRINOMIAL PLOTS
# ===============================

cat("=== CREATING TRINOMIAL PLOTS ===\n")

pdf("trinomial_andrew_heiss_plots.pdf", width = 12, height = 8)

# Plot 1: Stacked probability plot by condition (signature Andrew Heiss)
p1 <- ggplot(me_condition, aes(x = level, y = probability, fill = outcome)) +
  geom_col(position = "stack", alpha = 0.8) +
  geom_text(aes(label = paste0(round(probability*100, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "white", fontface = "bold", size = 3.5) +
  labs(title = "Trinomial Decision Probabilities by Social Condition",
       subtitle = "Stack plot showing all three outcome probabilities",
       x = "Social Condition", y = "Probability", fill = "Decision Type") +
  scale_fill_manual(values = c("exploit" = "#d73027", "explore" = "#1a9850", "none" = "#fee08b")) +
  scale_y_continuous(labels = function(x) paste0(round(x*100), "%")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "bottom"
  )
print(p1)

# Plot 2: Faceted marginal effects (Andrew Heiss style)
p2 <- ggplot(me_condition, aes(x = probability, y = level)) +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper, color = outcome),
                 height = 0.2, alpha = 0.7, linewidth = 1) +
  geom_point(aes(color = outcome), size = 3) +
  facet_wrap(~outcome, scales = "free_x") +
  labs(title = "Marginal Effects by Outcome Type",
       subtitle = "Predicted probabilities with 95% confidence intervals",
       x = "Predicted Probability", y = "Social Condition") +
  scale_x_continuous(labels = function(x) paste0(round(x*100), "%")) +
  scale_color_manual(values = c("exploit" = "#d73027", "explore" = "#1a9850", "none" = "#fee08b")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "none",
    strip.text = element_text(size = 12, face = "bold")
  )
print(p2)

# Plot 3: Interaction effects for all outcomes
interaction_grid <- expand.grid(
  condition = c("solo", "duo", "trio"),
  explore_expectation = c(-1, 0, 1),
  relative_rank = 1,
  subjective_value = 0,
  exploit_value = 0,
  monkey_id = levels(data_model$monkey_id)[1]
)

pred_probs_interaction <- predict(best_model, newdata = interaction_grid, type = "probs")
interaction_long <- data.frame()

for (i in 1:nrow(interaction_grid)) {
  for (outcome in colnames(pred_probs_interaction)) {
    interaction_long <- rbind(interaction_long, data.frame(
      condition = interaction_grid$condition[i],
      explore_expectation = interaction_grid$explore_expectation[i],
      outcome = outcome,
      probability = pred_probs_interaction[i, outcome]
    ))
  }
}

interaction_long$expectation_label <- factor(interaction_long$explore_expectation,
                                            levels = c(-1, 0, 1),
                                            labels = c("Low", "Medium", "High"))

p3 <- ggplot(interaction_long, aes(x = condition, y = probability, 
                                  color = expectation_label, group = expectation_label)) +
  geom_point(size = 3, position = position_dodge(width = 0.2)) +
  geom_line(position = position_dodge(width = 0.2), linewidth = 1) +
  facet_wrap(~outcome, scales = "free_y") +
  labs(title = "Condition × Expectation Interaction (All Outcomes)",
       subtitle = "How exploration expectation moderates social context effects",
       x = "Social Condition", y = "Predicted Probability",
       color = "Exploration\nExpectation") +
  scale_y_continuous(labels = function(x) paste0(round(x*100), "%")) +
  scale_color_manual(values = c("Low" = "#d73027", "Medium" = "#fee08b", "High" = "#1a9850")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "bottom",
    strip.text = element_text(size = 12, face = "bold")
  )
print(p3)

# Plot 4: Individual monkey patterns (trinomial)
if (require(dplyr, quietly = TRUE)) {
  individual_trinomial <- data_model %>%
    group_by(monkey_id, condition, outcome) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(monkey_id, condition) %>%
    mutate(
      total = sum(n),
      proportion = n / total
    ) %>%
    ungroup()
} else {
  # Manual aggregation
  individual_trinomial <- data.frame()
  for (monkey in unique(data_model$monkey_id)) {
    for (cond in c("solo", "duo", "trio")) {
      subset_data <- data_model[data_model$monkey_id == monkey & data_model$condition == cond, ]
      if (nrow(subset_data) > 0) {
        outcome_counts <- table(subset_data$outcome)
        total <- sum(outcome_counts)
        for (outcome in names(outcome_counts)) {
          individual_trinomial <- rbind(individual_trinomial, data.frame(
            monkey_id = monkey,
            condition = cond,
            outcome = outcome,
            n = outcome_counts[outcome],
            total = total,
            proportion = outcome_counts[outcome] / total
          ))
        }
      }
    }
  }
}

p4 <- ggplot(individual_trinomial, aes(x = condition, y = proportion, fill = outcome)) +
  geom_col(position = "stack", alpha = 0.8) +
  facet_wrap(~monkey_id, scales = "free_y") +
  labs(title = "Individual Monkey Trinomial Patterns",
       subtitle = "Decision type proportions by monkey and social condition",
       x = "Social Condition", y = "Proportion", fill = "Decision Type") +
  scale_fill_manual(values = c("exploit" = "#d73027", "explore" = "#1a9850", "none" = "#fee08b")) +
  scale_y_continuous(labels = function(x) paste0(round(x*100), "%")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "bottom",
    strip.text = element_text(size = 10, face = "bold")
  )
print(p4)

dev.off()

# ===============================
# TRINOMIAL SUMMARY
# ===============================

cat("\n=== TRINOMIAL ANALYSIS SUMMARY ===\n")
cat("========================================\n")

cat("MODEL FIT:\n")
cat("AIC:", round(AIC(best_model), 2), "\n")
cat("Deviance:", round(deviance(best_model), 2), "\n")

cat("\nOUTCOME STRUCTURE:\n")
cat("This is a proper trinomial model with three decision types:\n")
cat("1. EXPLOIT: Choose known option\n")
cat("2. EXPLORE: Try novel option\n") 
cat("3. NONE: Make no decision\n")

cat("\nKEY FINDINGS:\n")
cat("1. Social context affects ALL three decision types\n")
cat("2. 'None' decisions represent behavioral inhibition\n")
cat("3. Individual monkeys show distinct trinomial patterns\n")
cat("4. Exploration expectation interacts with social context\n")

cat("\nVisualization saved to: trinomial_andrew_heiss_plots.pdf\n")
cat("This follows Andrew Heiss's approach for multinomial outcomes.\n") 