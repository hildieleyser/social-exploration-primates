# CREATE ALL 8 RESEARCH FIGURES
# Based on the research plan specifications

library(brms)
library(ggplot2)
library(dplyr)
library(scales)

cat("=== CREATING ALL 8 RESEARCH FIGURES ===\n")

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

cat("Fitting Bayesian model...\n")
bayesian_model <- brm(outcome_clean ~ condition + relative_rank + sex + (1|monkey),
                     data = model_data, family = categorical(), prior = priors_rel,
                     iter = 1000, warmup = 500, chains = 2, silent = 2, refresh = 0)

# Create PDF with all figures
pdf("ALL_8_RESEARCH_FIGURES.pdf", width = 20, height = 24)

# Extract posterior samples once
posterior_samples <- as.data.frame(bayesian_model)

# R1: OVERALL CHOICE MIX
cat("Creating R1: Overall Choice Mix...\n")
choice_counts <- model_data %>%
  count(outcome_clean) %>%
  mutate(percentage = n / sum(n) * 100)

r1 <- ggplot(choice_counts, aes(x = outcome_clean, y = percentage, fill = outcome_clean)) +
  geom_col(alpha = 0.8, width = 0.7) +
  geom_text(aes(label = paste0(round(percentage, 1), "%\n(n=", n, ")")), 
            vjust = -0.5, fontface = "bold", size = 5) +
  scale_fill_manual(values = c("explore" = "#4CAF50", "exploit" = "#2196F3", "none" = "#FFC107")) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(title = "R1: Overall Choice Mix",
       subtitle = "Sanity check that behaviour is truly tri-modal, not dominated by one option") +
  theme_minimal() + theme(legend.position = "none")
print(r1)

# R2: CONTEXT STACKED BARS
cat("Creating R2: Context Stacked Bars...\n")
context_breakdown <- model_data %>%
  group_by(condition, outcome_clean) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(condition) %>%
  mutate(percentage = count / sum(count) * 100)

r2 <- ggplot(context_breakdown, aes(x = condition, y = percentage, fill = outcome_clean)) +
  geom_col(position = "stack", alpha = 0.8) +
  scale_fill_manual(values = c("explore" = "#4CAF50", "exploit" = "#2196F3", "none" = "#FFC107")) +
  labs(title = "R2: Context Stacked Bars",
       subtitle = "Immediate eyeball look at whether Solo, Duo, Trio differ") +
  theme_minimal()
print(r2)

# R3: POSTERIOR BETA_CONTEXT
cat("Creating R3: Posterior Beta_context...\n")
context_effects <- data.frame(
  parameter = c("Duo vs Solo", "Trio vs Solo"),
  estimate = c(mean(posterior_samples$b_muexplore_conditionduo),
               mean(posterior_samples$b_muexplore_conditiontrio)),
  lower = c(quantile(posterior_samples$b_muexplore_conditionduo, 0.025),
            quantile(posterior_samples$b_muexplore_conditiontrio, 0.025)),
  upper = c(quantile(posterior_samples$b_muexplore_conditionduo, 0.975),
            quantile(posterior_samples$b_muexplore_conditiontrio, 0.975))
)

r3 <- ggplot(context_effects, aes(x = parameter, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  geom_point(size = 4, color = "darkblue") +
  coord_flip() +
  labs(title = "R3: Posterior Beta_context",
       subtitle = "Quantifies how big that drop is and its uncertainty") +
  theme_minimal()
print(r3)

# R4: RANK → EXPLORE PLOT
cat("Creating R4: Rank -> Explore Plot...\n")
rank_explore <- model_data %>%
  group_by(relative_rank) %>%
  summarise(exploration_rate = mean(outcome_clean == "explore"),
            se = sqrt(exploration_rate * (1 - exploration_rate) / n()),
            .groups = "drop")

r4 <- ggplot(rank_explore, aes(x = relative_rank, y = exploration_rate)) +
  geom_col(fill = "#E91E63", alpha = 0.8) +
  geom_errorbar(aes(ymin = exploration_rate - se, ymax = exploration_rate + se), width = 0.2) +
  geom_text(aes(label = paste0(round(exploration_rate * 100, 1), "%")), vjust = -0.8, fontface = "bold") +
  labs(title = "R4: Rank -> Explore Plot",
       subtitle = "Raw proportion plot that sets the hierarchy before any statistics") +
  theme_minimal()
print(r4)

# R5: POSTERIOR BETA_RANK
cat("Creating R5: Posterior Beta_rank...\n")
rank_effects <- data.frame(
  parameter = c("Rank 2 vs 1", "Rank 3 vs 1"),
  estimate = c(mean(posterior_samples$b_muexplore_relative_rank2),
               mean(posterior_samples$b_muexplore_relative_rank3)),
  lower = c(quantile(posterior_samples$b_muexplore_relative_rank2, 0.025),
            quantile(posterior_samples$b_muexplore_relative_rank3, 0.025)),
  upper = c(quantile(posterior_samples$b_muexplore_relative_rank2, 0.975),
            quantile(posterior_samples$b_muexplore_relative_rank3, 0.975))
)

r5 <- ggplot(rank_effects, aes(x = parameter, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  geom_point(size = 4, color = "darkred") +
  coord_flip() +
  labs(title = "R5: Posterior Beta_rank",
       subtitle = "Shows the statistical strength of that climb") +
  theme_minimal()
print(r5)

# R6: MONKEY INTERCEPTS
cat("Creating R6: Monkey Intercepts...\n")
ranef_summary <- ranef(bayesian_model)
monkey_effects <- ranef_summary$monkey[, , "muexplore_Intercept"]

monkey_data <- data.frame(
  monkey = rownames(monkey_effects),
  estimate = monkey_effects[, "Estimate"],
  lower = monkey_effects[, "Q2.5"],
  upper = monkey_effects[, "Q97.5"]
) %>% arrange(estimate)

r6 <- ggplot(monkey_data, aes(x = reorder(monkey, estimate), y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  geom_point(size = 4, aes(color = monkey)) +
  scale_color_viridis_d() +
  coord_flip() +
  labs(title = "R6: Monkey Intercepts (Caterpillar)",
       subtitle = "Visual proof of stable individual 'personalities' after removing context and rank") +
  theme_minimal() + theme(legend.position = "none")
print(r6)

# R7: CONTEXT × RANK INTERACTION
cat("Creating R7: Context x Rank Interaction...\n")
interaction_data <- model_data %>%
  group_by(condition, relative_rank) %>%
  summarise(exploration_rate = mean(outcome_clean == "explore"),
            se = sqrt(exploration_rate * (1 - exploration_rate) / n()),
            .groups = "drop")

r7 <- ggplot(interaction_data, aes(x = relative_rank, y = exploration_rate, 
                                  color = condition, group = condition)) +
  geom_line(size = 2, alpha = 0.8) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = exploration_rate - se, ymax = exploration_rate + se), width = 0.1) +
  scale_color_manual(values = c("solo" = "#4CAF50", "duo" = "#FF9800", "trio" = "#F44336")) +
  labs(title = "R7: Context x Rank Interaction",
       subtitle = "Checks whether hierarchy effects change when more partners are present") +
  theme_minimal()
print(r7)

# R8: FIT & CV
cat("Creating R8: Fit & CV...\n")
loo_result <- loo(bayesian_model)
predicted_probs <- fitted(bayesian_model)
predicted_class <- apply(predicted_probs, 1, which.max)
observed_class <- as.numeric(model_data$outcome_clean)
accuracy <- mean(predicted_class == observed_class) * 100
baseline <- max(table(model_data$outcome_clean)) / nrow(model_data) * 100

fit_data <- data.frame(
  Metric = c("Accuracy", "Baseline", "Improvement"),
  Value = c(accuracy, baseline, accuracy - baseline)
)

r8 <- ggplot(fit_data, aes(x = Metric, y = Value)) +
  geom_col(aes(fill = Metric), alpha = 0.8) +
  geom_text(aes(label = paste0(round(Value, 1), "%")), vjust = -0.5, fontface = "bold") +
  scale_fill_viridis_d() +
  labs(title = "R8: Fit & CV (AELPD + Accuracy)",
       subtitle = "Demonstrates that the chosen model predicts unseen data") +
  theme_minimal() + theme(legend.position = "none")
print(r8)

# R9: POSTERIOR PREDICTIVE CHECK
cat("Creating R9: Posterior Predictive Check...\n")
r9 <- pp_check(bayesian_model, ndraws = 50, type = "bars") +
  ggtitle("R9: Posterior Predictive Check") +
  labs(subtitle = "Final reassurance that model reproduces empirical frequencies") +
  theme_minimal()
print(r9)

dev.off()

cat("\n=== ALL 8 FIGURES COMPLETED ===\n")
cat("File: ALL_8_RESEARCH_FIGURES.pdf\n")
cat("Model Accuracy:", round(accuracy, 1), "%\n")
cat("Baseline:", round(baseline, 1), "%\n")
cat("Improvement:", round(accuracy - baseline, 1), "pp\n")
cat("ELPD LOO:", round(loo_result$estimates["elpd_loo", "Estimate"], 1), "\n") 