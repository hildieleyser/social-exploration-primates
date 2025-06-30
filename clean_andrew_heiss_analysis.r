# Clean Andrew Heiss-Style Analysis
# Fixed version with proper error handling

library(ggplot2)

# Load available packages
if (!require(dplyr, quietly = TRUE)) {
  cat("dplyr not available, using base R\n")
}

set.seed(42)
options(scipen = 999)

cat("=== CLEAN ANDREW HEISS-STYLE ANALYSIS ===\n\n")

# ===============================
# DATA PREPARATION
# ===============================

data <- read.csv("Explore Exploit Dataset.csv", stringsAsFactors = FALSE)
data_exp <- data[data$TRIAL_TYPE == "OIT_RE", ]

# Create clean binary outcome
data_exp$outcome_clean <- ifelse(grepl("explore", data_exp$OUTCOME, ignore.case = TRUE), "explore",
                                ifelse(grepl("exploit", data_exp$OUTCOME, ignore.case = TRUE), "exploit", NA))

data_clean <- data_exp[!is.na(data_exp$outcome_clean), ]

# Create clean model dataset
data_model <- data.frame(
  outcome = factor(data_clean$outcome_clean, levels = c("exploit", "explore")),
  condition = factor(data_clean$CONDITION, levels = c("solo", "duo", "trio")),
  relative_rank = as.numeric(data_clean$RELATIVE_RANK),
  subjective_value = scale(as.numeric(data_clean$SUBJECTIVE_CHOSEN_VALUE))[,1],
  exploit_value = scale(as.numeric(data_clean$subjective_exploit))[,1],
  explore_expectation = scale(as.numeric(data_clean$expected_explore))[,1],
  monkey_id = factor(data_clean$monkey),
  block_num = as.numeric(gsub("BLOCK_", "", data_clean$BLOCK_No))
)

data_model <- data_model[complete.cases(data_model), ]

cat("Clean dataset: N =", nrow(data_model), "decisions\n")
cat("Explore rate:", round(mean(as.numeric(data_model$outcome) - 1), 3), "\n\n")

# ===============================
# MODEL FITTING
# ===============================

cat("=== FITTING MODELS ===\n")

# Best model from previous analysis
model <- glm(outcome ~ condition * explore_expectation + relative_rank + 
             subjective_value + exploit_value + monkey_id,
             data = data_model, family = binomial())

cat("Model fitted successfully\n")
cat("AIC:", round(AIC(model), 2), "\n")
cat("Pseudo R²:", round(1 - (model$deviance / model$null.deviance), 3), "\n\n")

# ===============================
# COEFFICIENT RESULTS
# ===============================

cat("=== MODEL RESULTS ===\n")

# Extract coefficients safely
coef_summary <- summary(model)$coefficients
coef_names <- rownames(coef_summary)

# Create coefficient dataframe manually
coef_df <- data.frame(
  term = coef_names,
  estimate = coef_summary[, 1],
  std_error = coef_summary[, 2],
  z_value = coef_summary[, 3],
  p_value = coef_summary[, 4],
  stringsAsFactors = FALSE
)

# Calculate odds ratios and CIs
coef_df$odds_ratio <- exp(coef_df$estimate)
coef_df$ci_lower <- exp(coef_df$estimate - 1.96 * coef_df$std_error)
coef_df$ci_upper <- exp(coef_df$estimate + 1.96 * coef_df$std_error)

# Print main coefficients (exclude monkey effects for clarity)
main_coefs <- coef_df[!grepl("monkey_id", coef_df$term), ]
cat("Main Effects (Odds Ratios):\n")
for (i in 1:nrow(main_coefs)) {
  cat(sprintf("%-30s: OR = %5.2f (95%% CI: %4.2f-%4.2f), p = %6.4f\n",
              main_coefs$term[i], main_coefs$odds_ratio[i],
              main_coefs$ci_lower[i], main_coefs$ci_upper[i], main_coefs$p_value[i]))
}

# ===============================
# MARGINAL EFFECTS
# ===============================

cat("\n=== MARGINAL EFFECTS ===\n")

# Function for marginal effects
calc_marginal_effects <- function(model, data, variable, levels) {
  results <- data.frame()
  
  for (level in levels) {
    data_temp <- data
    data_temp[[variable]] <- level
    pred_probs <- predict(model, newdata = data_temp, type = "response")
    avg_prob <- mean(pred_probs)
    se_prob <- sd(pred_probs) / sqrt(length(pred_probs))
    
    results <- rbind(results, data.frame(
      variable = variable,
      level = as.character(level),
      probability = avg_prob,
      std_error = se_prob,
      ci_lower = avg_prob - 1.96 * se_prob,
      ci_upper = avg_prob + 1.96 * se_prob
    ))
  }
  return(results)
}

# Calculate marginal effects
me_condition <- calc_marginal_effects(model, data_model, "condition", c("solo", "duo", "trio"))
me_rank <- calc_marginal_effects(model, data_model, "relative_rank", c(1, 2))

cat("Social Condition Effects:\n")
for (i in 1:nrow(me_condition)) {
  cat(sprintf("%-8s: %4.1f%% (95%% CI: %4.1f%%-%4.1f%%)\n",
              me_condition$level[i], me_condition$probability[i]*100,
              me_condition$ci_lower[i]*100, me_condition$ci_upper[i]*100))
}

cat("\nRank Effects:\n")
rank_labels <- c("Dominant", "Subordinate")
for (i in 1:nrow(me_rank)) {
  cat(sprintf("%-12s: %4.1f%% (95%% CI: %4.1f%%-%4.1f%%)\n",
              rank_labels[i], me_rank$probability[i]*100,
              me_rank$ci_lower[i]*100, me_rank$ci_upper[i]*100))
}

# ===============================
# ANDREW HEISS-STYLE PLOTS
# ===============================

cat("\n=== CREATING PLOTS ===\n")

pdf("clean_andrew_heiss_plots.pdf", width = 12, height = 8)

# Plot 1: Coefficient plot (Andrew Heiss style)
plot_coefs <- main_coefs[main_coefs$term != "(Intercept)", ]

p1 <- ggplot(plot_coefs, aes(x = estimate, y = reorder(term, estimate))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.8) +
  geom_errorbarh(aes(xmin = estimate - 1.96*std_error, xmax = estimate + 1.96*std_error),
                 height = 0.2, color = "darkblue", alpha = 0.7, linewidth = 1) +
  geom_point(size = 3, color = "darkblue") +
  labs(title = "Logistic Regression Coefficients",
       subtitle = "Effect on log-odds of exploration (95% confidence intervals)",
       x = "Coefficient Estimate", y = "Variable") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text.y = element_text(size = 11),
    panel.grid.minor.x = element_blank()
  )
print(p1)

# Plot 2: Marginal effects
me_all <- rbind(me_condition, me_rank)
me_all$level_clean <- paste0(me_all$variable, ": ", me_all$level)

p2 <- ggplot(me_all, aes(x = probability, y = reorder(level_clean, probability))) +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper),
                 height = 0.2, color = "darkred", alpha = 0.7, linewidth = 1) +
  geom_point(size = 3, color = "darkred") +
  labs(title = "Average Marginal Effects",
       subtitle = "Predicted probability of exploration by variable level",
       x = "Predicted Probability of Exploration", y = "Variable: Level") +
  scale_x_continuous(labels = function(x) paste0(round(x*100), "%")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text.y = element_text(size = 11),
    panel.grid.minor.x = element_blank()
  )
print(p2)

# Plot 3: Interaction effects
interaction_grid <- expand.grid(
  condition = c("solo", "duo", "trio"),
  explore_expectation = c(-1, 0, 1),
  relative_rank = 1,
  subjective_value = 0,
  exploit_value = 0,
  monkey_id = levels(data_model$monkey_id)[1]
)

interaction_grid$pred_prob <- predict(model, newdata = interaction_grid, type = "response")
interaction_grid$expectation_label <- factor(interaction_grid$explore_expectation,
                                            levels = c(-1, 0, 1),
                                            labels = c("Low", "Medium", "High"))

p3 <- ggplot(interaction_grid, aes(x = condition, y = pred_prob, 
                                  color = expectation_label, group = expectation_label)) +
  geom_point(size = 4, position = position_dodge(width = 0.2)) +
  geom_line(position = position_dodge(width = 0.2), linewidth = 1.2) +
  labs(title = "Condition × Expectation Interaction",
       subtitle = "Predicted exploration probability",
       x = "Social Condition", y = "Predicted Probability of Exploration",
       color = "Exploration\nExpectation") +
  scale_y_continuous(labels = function(x) paste0(round(x*100), "%")) +
  scale_color_manual(values = c("Low" = "#d73027", "Medium" = "#fee08b", "High" = "#1a9850")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "bottom"
  )
print(p3)

# Plot 4: Individual differences
if (require(dplyr, quietly = TRUE)) {
  individual_data <- data_model %>%
    group_by(monkey_id, condition) %>%
    summarise(
      explore_rate = mean(as.numeric(outcome) - 1),
      n = n(),
      se = sqrt(explore_rate * (1 - explore_rate) / n),
      .groups = "drop"
    )
} else {
  # Manual aggregation
  individual_data <- data.frame()
  for (monkey in unique(data_model$monkey_id)) {
    for (cond in c("solo", "duo", "trio")) {
      subset_data <- data_model[data_model$monkey_id == monkey & data_model$condition == cond, ]
      if (nrow(subset_data) > 0) {
        explore_rate <- mean(as.numeric(subset_data$outcome) - 1)
        n <- nrow(subset_data)
        se <- sqrt(explore_rate * (1 - explore_rate) / n)
        
        individual_data <- rbind(individual_data, data.frame(
          monkey_id = monkey,
          condition = cond,
          explore_rate = explore_rate,
          n = n,
          se = se
        ))
      }
    }
  }
}

p4 <- ggplot(individual_data, aes(x = condition, y = explore_rate, color = monkey_id)) +
  geom_point(size = 3, position = position_dodge(width = 0.3)) +
  geom_line(aes(group = monkey_id), position = position_dodge(width = 0.3), linewidth = 1) +
  geom_errorbar(aes(ymin = pmax(0, explore_rate - 1.96*se), 
                    ymax = pmin(1, explore_rate + 1.96*se)),
                width = 0.1, position = position_dodge(width = 0.3)) +
  labs(title = "Individual Monkey Patterns",
       subtitle = "Exploration rates by monkey and social condition",
       x = "Social Condition", y = "Exploration Rate",
       color = "Monkey ID") +
  scale_y_continuous(labels = function(x) paste0(round(x*100), "%")) +
  scale_color_brewer(type = "qual", palette = "Set2") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "bottom"
  )
print(p4)

dev.off()

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Andrew Heiss-style plots saved to: clean_andrew_heiss_plots.pdf\n")
cat("This provides clean, interpretable results with proper marginal effects.\n") 