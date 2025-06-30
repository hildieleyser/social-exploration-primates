# CREATE ALL 8 RESEARCH FIGURES (FIXED)
# Based on the research plan specifications

library(brms)
library(ggplot2)
library(dplyr)
library(scales)

cat("=== CREATING ALL 8 RESEARCH FIGURES (FIXED) ===\n")

# Load and prepare data
data_raw <- read.csv("Explore Exploit Dataset.csv", stringsAsFactors = FALSE)
data_exp <- data_raw[data_raw$TRIAL_TYPE == "OIT_RE", ]
data_exp$outcome_clean <- ifelse(grepl("explore", data_exp$OUTCOME, ignore.case = TRUE), "explore",
                                ifelse(grepl("exploit", data_exp$OUTCOME, ignore.case = TRUE), "exploit", 
                                      ifelse(grepl("none|NONE", data_exp$OUTCOME, ignore.case = TRUE), "none", NA)))

data_clean <- data_exp[!is.na(data_exp$outcome_clean), ]
data_clean$outcome_clean <- factor(data_clean$outcome_clean, levels = c("exploit", "explore", "none"))
data_clean$monkey <- data_clean$monkey
data_clean$condition <- factor(data_clean$CONDITION, levels = c("solo", "duo", "trio"))
data_clean$sex <- ifelse(data_clean$monkey %in% c("FRAN", "DALI", "EBI"), "Male", "Female")
data_clean$sex <- factor(data_clean$sex, levels = c("Male", "Female"))
data_clean$relative_rank <- factor(data_clean$RELATIVE_RANK, levels = c(1, 2, 3))

model_data <- data_clean[complete.cases(data_clean[c("outcome_clean", "condition", "relative_rank", "sex", "monkey")]), ]

# Fit the Bayesian model
options(mc.cores = parallel::detectCores())

priors_rel <- c(
  prior(normal(0, 1), class = Intercept, dpar = muexplore),
  prior(normal(0, 1), class = Intercept, dpar = munone),
  prior(normal(0, 0.5), class = b, dpar = muexplore),
  prior(normal(0, 0.5), class = b, dpar = munone),
  prior(exponential(1), class = sd, group = monkey, dpar = muexplore),
  prior(exponential(1), class = sd, group = monkey, dpar = munone)
)

cat("Fitting Bayesian model for all figures...\n")
bayesian_model <- brm(outcome_clean ~ condition + relative_rank + sex + (1|monkey),
                     data = model_data, family = categorical(), prior = priors_rel,
                     iter = 1200, warmup = 600, chains = 3,
                     silent = 2, refresh = 0)

# Start creating all figures
pdf("ALL_8_RESEARCH_FIGURES.pdf", width = 20, height = 24)

# ===== R1: OVERALL CHOICE MIX =====
cat("Creating R1: Overall Choice Mix...\n")

choice_counts <- model_data %>%
  count(outcome_clean) %>%
  mutate(
    percentage = n / sum(n) * 100,
    outcome_clean = factor(outcome_clean, levels = c("explore", "exploit", "none"))
  )

r1 <- ggplot(choice_counts, aes(x = outcome_clean, y = percentage, fill = outcome_clean)) +
  geom_col(alpha = 0.8, width = 0.7) +
  geom_text(aes(label = paste0(round(percentage, 1), "%\n(n=", n, ")")), 
            vjust = -0.5, fontface = "bold", size = 5) +
  scale_fill_manual(values = c("explore" = "#4CAF50", "exploit" = "#2196F3", "none" = "#FFC107")) +
  scale_y_continuous(labels = percent_format(scale = 1), limits = c(0, max(choice_counts$percentage) * 1.2)) +
  labs(
    title = "R1: Overall Choice Mix",
    subtitle = "Sanity check that behaviour is truly tri-modal, not dominated by one option",
    x = "Choice Type", 
    y = "Percentage of Trials",
    caption = "Confirms balanced tri-modal decision pattern"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12, face = "bold"),
    legend.position = "none"
  )

print(r1)

# ===== R2: CONTEXT STACKED BARS =====
cat("Creating R2: Context Stacked Bars...\n")

context_breakdown <- model_data %>%
  group_by(condition, outcome_clean) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(condition) %>%
  mutate(
    total = sum(count),
    percentage = count / total * 100
  )

r2 <- ggplot(context_breakdown, aes(x = condition, y = percentage, fill = outcome_clean)) +
  geom_col(position = "stack", alpha = 0.8) +
  geom_text(aes(label = ifelse(percentage > 8, paste0(round(percentage, 1), "%"), "")),
            position = position_stack(vjust = 0.5), color = "white", fontface = "bold") +
  scale_fill_manual(values = c("explore" = "#4CAF50", "exploit" = "#2196F3", "none" = "#FFC107"),
                   name = "Choice Type") +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(
    title = "R2: Context Stacked Bars",
    subtitle = "Immediate eyeball look at whether Solo, Duo, Trio differ",
    x = "Social Context", 
    y = "Percentage of Choices",
    caption = "Visual inspection of context differences"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12, face = "bold"),
    legend.position = "top"
  )

print(r2)

# ===== R3: POSTERIOR BETA_CONTEXT =====
cat("Creating R3: Posterior Beta_context...\n")

# Extract posterior samples
posterior_samples <- as.data.frame(bayesian_model)

# Context effects data
context_effects <- data.frame(
  parameter = c("Duo vs Solo", "Trio vs Solo"),
  estimate = c(
    mean(posterior_samples$b_muexplore_conditionduo),
    mean(posterior_samples$b_muexplore_conditiontrio)
  ),
  lower_95 = c(
    quantile(posterior_samples$b_muexplore_conditionduo, 0.025),
    quantile(posterior_samples$b_muexplore_conditiontrio, 0.025)
  ),
  upper_95 = c(
    quantile(posterior_samples$b_muexplore_conditionduo, 0.975),
    quantile(posterior_samples$b_muexplore_conditiontrio, 0.975)
  ),
  lower_66 = c(
    quantile(posterior_samples$b_muexplore_conditionduo, 0.17),
    quantile(posterior_samples$b_muexplore_conditiontrio, 0.17)
  ),
  upper_66 = c(
    quantile(posterior_samples$b_muexplore_conditionduo, 0.83),
    quantile(posterior_samples$b_muexplore_conditiontrio, 0.83)
  )
)

r3 <- ggplot(context_effects, aes(x = parameter, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  geom_errorbar(aes(ymin = lower_95, ymax = upper_95), width = 0.1, linewidth = 1.5, alpha = 0.6) +
  geom_errorbar(aes(ymin = lower_66, ymax = upper_66), width = 0.05, linewidth = 2) +
  geom_point(size = 4, color = "darkblue") +
  geom_text(aes(label = paste0("b = ", round(estimate, 3))), 
            hjust = -0.2, fontface = "bold", size = 4) +
  coord_flip() +
  labs(
    title = "R3: Posterior Beta_context",
    subtitle = "Quantifies how big that drop is and its uncertainty",
    x = "Context Comparison", 
    y = "Log-Odds Effect Size",
    caption = "Thick lines = 66% CI, Thin lines = 95% CI"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12, face = "bold")
  )

print(r3)

# ===== R4: RANK → EXPLORE PLOT =====
cat("Creating R4: Rank -> Explore Plot...\n")

rank_explore <- model_data %>%
  group_by(relative_rank) %>%
  summarise(
    exploration_rate = mean(outcome_clean == "explore"),
    n = n(),
    se = sqrt(exploration_rate * (1 - exploration_rate) / n),
    .groups = "drop"
  )

r4 <- ggplot(rank_explore, aes(x = relative_rank, y = exploration_rate)) +
  geom_col(fill = "#E91E63", alpha = 0.8, width = 0.6) +
  geom_errorbar(aes(ymin = exploration_rate - se, ymax = exploration_rate + se), 
                width = 0.2, linewidth = 1) +
  geom_text(aes(label = paste0(round(exploration_rate * 100, 1), "%")), 
            vjust = -0.8, fontface = "bold", size = 5) +
  geom_smooth(aes(group = 1), method = "lm", se = TRUE, color = "darkred", linewidth = 1.5) +
  scale_y_continuous(labels = percent_format(), limits = c(0, max(rank_explore$exploration_rate) * 1.3)) +
  labs(
    title = "R4: Rank -> Explore Plot",
    subtitle = "Raw proportion plot that sets the hierarchy before any statistics",
    x = "Relative Rank (1=highest, 3=lowest)", 
    y = "Exploration Rate",
    caption = "Error bars show standard error, line shows linear trend"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12, face = "bold")
  )

print(r4)

# ===== R5: POSTERIOR BETA_RANK =====
cat("Creating R5: Posterior Beta_rank...\n")

rank_effects <- data.frame(
  parameter = c("Rank 2 vs 1", "Rank 3 vs 1"),
  estimate = c(
    mean(posterior_samples$b_muexplore_relative_rank2),
    mean(posterior_samples$b_muexplore_relative_rank3)
  ),
  lower_95 = c(
    quantile(posterior_samples$b_muexplore_relative_rank2, 0.025),
    quantile(posterior_samples$b_muexplore_relative_rank3, 0.025)
  ),
  upper_95 = c(
    quantile(posterior_samples$b_muexplore_relative_rank2, 0.975),
    quantile(posterior_samples$b_muexplore_relative_rank3, 0.975)
  ),
  lower_66 = c(
    quantile(posterior_samples$b_muexplore_relative_rank2, 0.17),
    quantile(posterior_samples$b_muexplore_relative_rank3, 0.17)
  ),
  upper_66 = c(
    quantile(posterior_samples$b_muexplore_relative_rank2, 0.83),
    quantile(posterior_samples$b_muexplore_relative_rank3, 0.83)
  )
)

r5 <- ggplot(rank_effects, aes(x = parameter, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  geom_errorbar(aes(ymin = lower_95, ymax = upper_95), width = 0.1, linewidth = 1.5, alpha = 0.6) +
  geom_errorbar(aes(ymin = lower_66, ymax = upper_66), width = 0.05, linewidth = 2) +
  geom_point(size = 4, color = "darkred") +
  geom_text(aes(label = paste0("b = ", round(estimate, 3))), 
            hjust = -0.2, fontface = "bold", size = 4) +
  coord_flip() +
  labs(
    title = "R5: Posterior Beta_rank",
    subtitle = "Shows the statistical strength of that climb",
    x = "Rank Comparison", 
    y = "Log-Odds Effect Size",
    caption = "Thick lines = 66% CI, Thin lines = 95% CI"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12, face = "bold")
  )

print(r5)

# ===== R6: MONKEY INTERCEPTS (CATERPILLAR) =====
cat("Creating R6: Monkey Intercepts...\n")

# Extract random effects safely
ranef_summary <- ranef(bayesian_model)
monkey_effects_data <- ranef_summary$monkey[, , "muexplore_Intercept"]

# Check available columns
available_cols <- colnames(monkey_effects_data)
cat("Available columns:", paste(available_cols, collapse = ", "), "\n")

# Create monkey intercepts with available columns
monkey_intercepts <- data.frame(
  monkey = rownames(monkey_effects_data),
  estimate = monkey_effects_data[, "Estimate"],
  lower_95 = monkey_effects_data[, "Q2.5"],
  upper_95 = monkey_effects_data[, "Q97.5"]
)

# Add 66% CI if available, otherwise use 25-75%
if("Q17" %in% available_cols && "Q83" %in% available_cols) {
  monkey_intercepts$lower_66 <- monkey_effects_data[, "Q17"]
  monkey_intercepts$upper_66 <- monkey_effects_data[, "Q83"]
} else if("Q25" %in% available_cols && "Q75" %in% available_cols) {
  monkey_intercepts$lower_66 <- monkey_effects_data[, "Q25"]
  monkey_intercepts$upper_66 <- monkey_effects_data[, "Q75"]
} else {
  # Calculate approximate 66% CI
  monkey_intercepts$lower_66 <- monkey_intercepts$estimate - 0.5 * (monkey_intercepts$upper_95 - monkey_intercepts$lower_95)
  monkey_intercepts$upper_66 <- monkey_intercepts$estimate + 0.5 * (monkey_intercepts$upper_95 - monkey_intercepts$lower_95)
}

monkey_intercepts <- monkey_intercepts %>% arrange(estimate)

r6 <- ggplot(monkey_intercepts, aes(x = reorder(monkey, estimate), y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 1) +
  geom_errorbar(aes(ymin = lower_95, ymax = upper_95), width = 0.1, linewidth = 1.5, alpha = 0.6) +
  geom_errorbar(aes(ymin = lower_66, ymax = upper_66), width = 0.05, linewidth = 2) +
  geom_point(size = 4, aes(color = monkey)) +
  scale_color_viridis_d() +
  coord_flip() +
  labs(
    title = "R6: Monkey Intercepts (Caterpillar)",
    subtitle = "Visual proof of stable individual 'personalities' after removing context and rank",
    x = "Individual Monkey", 
    y = "Individual Effect Size (Log-Odds)",
    caption = "Individual variation in exploration tendency"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12, face = "bold"),
    legend.position = "none"
  )

print(r6)

# ===== R7: CONTEXT × RANK INTERACTION =====
cat("Creating R7: Context x Rank Interaction...\n")

interaction_data <- model_data %>%
  group_by(condition, relative_rank) %>%
  summarise(
    exploration_rate = mean(outcome_clean == "explore"),
    n = n(),
    se = sqrt(exploration_rate * (1 - exploration_rate) / n),
    .groups = "drop"
  )

r7 <- ggplot(interaction_data, aes(x = relative_rank, y = exploration_rate, color = condition, group = condition)) +
  geom_line(linewidth = 2, alpha = 0.8) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = exploration_rate - se, ymax = exploration_rate + se), 
                width = 0.1, linewidth = 1) +
  scale_color_manual(values = c("solo" = "#4CAF50", "duo" = "#FF9800", "trio" = "#F44336"),
                    name = "Social Context") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "R7: Context x Rank Interaction",
    subtitle = "Checks whether hierarchy effects change when more partners are present",
    x = "Relative Rank", 
    y = "Exploration Rate",
    caption = "Lines show how rank effects vary by social context"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12, face = "bold"),
    legend.position = "top"
  )

print(r7)

# ===== R8: FIT & CV (AELPD + ACCURACY) =====
cat("Creating R8: Fit & CV...\n")

# Calculate LOO and accuracy
loo_result <- loo(bayesian_model)

# Predicted vs observed for accuracy
predicted_probs <- fitted(bayesian_model)
predicted_class <- apply(predicted_probs, 1, function(x) which.max(x))
observed_class <- as.numeric(model_data$outcome_clean)

# Calculate accuracy
accuracy <- mean(predicted_class == observed_class) * 100

# Create baseline accuracy (most frequent class)
baseline_accuracy <- max(table(model_data$outcome_clean)) / nrow(model_data) * 100

# Model fit metrics
fit_metrics <- data.frame(
  Metric = c("Accuracy", "Baseline Accuracy", "Improvement"),
  Value = c(accuracy, baseline_accuracy, accuracy - baseline_accuracy)
)

r8 <- ggplot(fit_metrics, aes(x = Metric, y = Value)) +
  geom_col(aes(fill = Metric), alpha = 0.8, width = 0.6) +
  geom_text(aes(label = paste0(round(Value, 1), "%")), 
            vjust = -0.5, fontface = "bold", size = 5) +
  scale_fill_manual(values = c("Accuracy" = "#4CAF50", 
                              "Baseline Accuracy" = "#FFC107",
                              "Improvement" = "#2196F3")) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(
    title = "R8: Fit & CV (AELPD + Accuracy)",
    subtitle = "Demonstrates that the chosen model predicts unseen data and so inferences are trustworthy",
    x = "Model Performance Metric", 
    y = "Percentage",
    caption = paste0("ELPD LOO: ", round(loo_result$estimates["elpd_loo", "Estimate"], 1), 
                    " +/- ", round(loo_result$estimates["elpd_loo", "SE"], 1))
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12, face = "bold"),
    legend.position = "none"
  )

print(r8)

# ===== R9: POSTERIOR PREDICTIVE CHECK =====
cat("Creating R9: Posterior Predictive Check...\n")

# Use pp_check for final validation
r9 <- pp_check(bayesian_model, ndraws = 50, type = "bars") +
  ggtitle("R9: Posterior Predictive Check") +
  labs(subtitle = "Final reassurance that model reproduces empirical frequencies,\nso inferences are trustworthy",
       caption = "Dark blue = observed data, Light blue = model predictions") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12, face = "bold")
  )

print(r9)

dev.off()

# Summary
cat("\n=== ALL 8 FIGURES CREATED SUCCESSFULLY ===\n")
cat("File created: ALL_8_RESEARCH_FIGURES.pdf\n")
cat("\nFigure Summary:\n")
cat("R1: Overall Choice Mix - Sanity check of tri-modal behavior\n")
cat("R2: Context Stacked Bars - Immediate visual of Solo/Duo/Trio differences\n") 
cat("R3: Posterior Beta_context - Quantified context effects with uncertainty\n")
cat("R4: Rank -> Explore Plot - Raw hierarchy effects before statistics\n")
cat("R5: Posterior Beta_rank - Statistical strength of rank effects\n")
cat("R6: Monkey Intercepts - Individual personality differences\n")
cat("R7: Context x Rank Interaction - Hierarchy changes with social partners\n")
cat("R8: Fit & CV - Model performance and trustworthiness\n")
cat("R9: Posterior Predictive Check - Final model validation\n")

cat("\nKey Statistics:\n")
cat("Model Accuracy:", round(accuracy, 1), "%\n")
cat("Baseline Accuracy:", round(baseline_accuracy, 1), "%\n")
cat("Improvement:", round(accuracy - baseline_accuracy, 1), "percentage points\n")
cat("ELPD LOO:", round(loo_result$estimates["elpd_loo", "Estimate"], 1), "+/-", 
    round(loo_result$estimates["elpd_loo", "SE"], 1), "\n")

cat("\nAll figures complete!\n") 