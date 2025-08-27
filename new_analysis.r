# Load required libraries
library(brms)
library(dplyr)
library(readr)
library(marginaleffects)
library(ggplot2)
library(forcats)
library(scales)

# 1. Load and prepare the data
data <- read_csv("Explore Exploit Dataset.csv")

data <- data %>%
  mutate(
    OUTCOME_CAT = case_when(
      grepl("^explore$", OUTCOME, ignore.case = TRUE) ~ "explore",
      grepl("^exploit", OUTCOME, ignore.case = TRUE) ~ "exploit",
      grepl("^stop$|^none$", OUTCOME, ignore.case = TRUE) ~ "none",
      TRUE ~ NA_character_
    ),
    OUTCOME_CAT = factor(OUTCOME_CAT, levels = c("explore", "exploit", "none")),
    CONDITION = factor(CONDITION),              # y10: social context (column 5)
    PAIRED_WITH = factor(PAIRED_WITH),          # y02: partner (column 6)
    RELATIVE_RANK = as.numeric(RELATIVE_RANK),  # y03: relative rank (column 7)
    SUBJECTIVE_CHOSEN_VALUE = as.numeric(SUBJECTIVE_CHOSEN_VALUE), # y04: chosen value (col 11)
    subjective_exploit = as.numeric(subjective_exploit),           # y05: exploit value (col 12)
    expected_explore = as.numeric(expected_explore),               # y06: expectation (col 17)
    monkey = factor(monkey),                    # grouping factor
    BLOCK_No = factor(BLOCK_No),                # grouping factor
    TRIAL_NUM = factor(TRIAL_NUM)               # grouping factor (optional)
  ) %>%
  filter(!is.na(OUTCOME_CAT))

# 2. Fit the Bayesian hierarchical multinomial model
fit <- brm(
  OUTCOME_CAT ~ CONDITION + PAIRED_WITH + RELATIVE_RANK +
    SUBJECTIVE_CHOSEN_VALUE + subjective_exploit + expected_explore +
    (1 | monkey) + (1 | BLOCK_No),
  data = data,
  family = categorical(),
  chains = 4,
  iter = 2000,
  cores = 4
)

# 3. Model summary
summary(fit)

# 4. Plot predicted probabilities by expectation and condition
preds <- predictions(
  fit,
  newdata = datagrid(
    CONDITION = unique(data$CONDITION),
    expected_explore = seq(min(data$expected_explore, na.rm = TRUE),
                           max(data$expected_explore, na.rm = TRUE),
                           length.out = 20),
    monkey = NA,
    BLOCK_No = NA
  )
)

# Plot 1: Basic predicted probabilities
p1 <- ggplot(preds, aes(x = expected_explore, y = estimate, color = CONDITION)) +
  geom_line() +
  facet_wrap(~ OUTCOME_CAT) +
  labs(title = "Predicted Probability by Condition and Expectation",
       y = "Predicted Probability",
       x = "Expectation (expected_explore)") +
  theme_minimal()

print(p1)

# Plot 2: Enhanced plot with confidence intervals
p2 <- ggplot(preds, aes(x = expected_explore, y = estimate, color = CONDITION)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = CONDITION), alpha = 0.2) +
  facet_wrap(~ OUTCOME_CAT) +
  labs(title = "Predicted Probability by Condition and Expectation (with 95% CI)",
       y = "Predicted Probability",
       x = "Expectation (expected_explore)") +
  theme_minimal()

print(p2)

# Plot 3: Comparison at mean expectation level
preds_combos <- predictions(
  fit,
  newdata = expand.grid(
    CONDITION = unique(data$CONDITION),
    expected_explore = mean(data$expected_explore, na.rm = TRUE),
    monkey = NA,
    BLOCK_No = NA
  )
)

# Add custom labels for each combination
preds_combos <- preds_combos %>%
  mutate(label = paste("Condition:", CONDITION))

# Plot predicted probabilities for each outcome and combination
p3 <- ggplot(preds_combos, aes(x = estimate, y = label, color = OUTCOME_CAT)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  labs(
    title = "Predicted Probabilities by Condition and Outcome (at Mean Expectation)",
    x = "Predicted Probability",
    y = NULL
  ) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank())

print(p3)
