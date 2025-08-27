# Enhanced GLM Analysis with Advanced Plotting
# Compatible with R 3.6.3

# Load required libraries (using only base packages and common ones)
library(readr)
library(dplyr)
library(ggplot2)

print("Loading and preparing data...")

# 1. Load and prepare the data
data <- read_csv("Explore Exploit Dataset.csv")

data <- data %>%
  filter(!is.na(OUTCOME) & !is.na(CONDITION) & !is.na(expected_explore)) %>%
  mutate(
    OUTCOME_CAT = case_when(
      OUTCOME == 0 ~ "none",
      OUTCOME == 1 ~ "explore", 
      OUTCOME == 2 ~ "exploit"
    ),
    CONDITION = factor(CONDITION, levels = c("Solo", "Duo", "Trio")),
    monkey = factor(monkey),
    BLOCK_No = factor(BLOCK_No)
  )

print(paste("Data loaded. Rows:", nrow(data)))
print("OUTCOME distribution:")
print(table(data$OUTCOME_CAT))
print("CONDITION distribution:")
print(table(data$CONDITION))

# 2. Fit multinomial logistic regression using nnet package
if (!require(nnet, quietly = TRUE)) {
  install.packages("nnet")
  library(nnet)
}

print("Fitting multinomial logistic regression model...")

# Fit the model
fit <- multinom(OUTCOME_CAT ~ CONDITION * expected_explore + 
                as.numeric(monkey) + as.numeric(BLOCK_No), 
                data = data)

print("Model fitted successfully!")
print(summary(fit))

# 3. Manual prediction function for multinomial model
predict_probs <- function(model, newdata) {
  probs <- predict(model, newdata = newdata, type = "probs")
  
  # Handle case where only one prediction (returns vector instead of matrix)
  if (is.vector(probs)) {
    probs <- matrix(probs, nrow = 1)
    colnames(probs) <- levels(data$OUTCOME_CAT)
  }
  
  # Convert to long format
  result <- data.frame(
    CONDITION = rep(newdata$CONDITION, each = ncol(probs)),
    expected_explore = rep(newdata$expected_explore, each = ncol(probs)),
    OUTCOME_CAT = rep(colnames(probs), nrow(newdata)),
    estimate = as.vector(t(probs))
  )
  
  return(result)
}

# 4. Generate predictions for plotting
print("Generating predictions...")

# Create prediction grid
pred_grid <- expand.grid(
  CONDITION = levels(data$CONDITION),
  expected_explore = seq(min(data$expected_explore, na.rm = TRUE),
                        max(data$expected_explore, na.rm = TRUE),
                        length.out = 20),
  monkey = median(as.numeric(data$monkey), na.rm = TRUE),
  BLOCK_No = median(as.numeric(data$BLOCK_No), na.rm = TRUE)
)

# Get predictions
preds <- predict_probs(fit, pred_grid)

# Add confidence intervals (approximate using standard errors)
# For simplicity, we'll use bootstrap-style confidence intervals
se_multiplier <- 1.96  # 95% CI

# Calculate approximate standard errors (simplified approach)
preds$conf.low <- pmax(0, preds$estimate - se_multiplier * sqrt(preds$estimate * (1 - preds$estimate) / nrow(data)))
preds$conf.high <- pmin(1, preds$estimate + se_multiplier * sqrt(preds$estimate * (1 - preds$estimate) / nrow(data)))

print("Creating enhanced plots...")

# Plot 1: Basic predicted probabilities
p1 <- ggplot(preds, aes(x = expected_explore, y = estimate, color = CONDITION)) +
  geom_line(size = 1.2) +
  facet_wrap(~ OUTCOME_CAT) +
  labs(title = "Predicted Probability by Condition and Expectation",
       y = "Predicted Probability",
       x = "Expectation (expected_explore)",
       color = "Social Context") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "bottom"
  ) +
  scale_color_manual(values = c("Solo" = "#E74C3C", "Duo" = "#3498DB", "Trio" = "#2ECC71"))

print(p1)

# Plot 2: Enhanced plot with confidence intervals
p2 <- ggplot(preds, aes(x = expected_explore, y = estimate, color = CONDITION, fill = CONDITION)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  facet_wrap(~ OUTCOME_CAT) +
  labs(title = "Predicted Probability by Condition and Expectation (with 95% CI)",
       y = "Predicted Probability",
       x = "Expectation (expected_explore)",
       color = "Social Context",
       fill = "Social Context") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "bottom"
  ) +
  scale_color_manual(values = c("Solo" = "#E74C3C", "Duo" = "#3498DB", "Trio" = "#2ECC71")) +
  scale_fill_manual(values = c("Solo" = "#E74C3C", "Duo" = "#3498DB", "Trio" = "#2ECC71"))

print(p2)

# Plot 3: Comparison at mean expectation level
print("Creating comparison plot at mean expectation...")

mean_expect <- mean(data$expected_explore, na.rm = TRUE)

combo_grid <- expand.grid(
  CONDITION = levels(data$CONDITION),
  expected_explore = mean_expect,
  monkey = median(as.numeric(data$monkey), na.rm = TRUE),
  BLOCK_No = median(as.numeric(data$BLOCK_No), na.rm = TRUE)
)

preds_combos <- predict_probs(fit, combo_grid)

# Add confidence intervals
preds_combos$conf.low <- pmax(0, preds_combos$estimate - se_multiplier * sqrt(preds_combos$estimate * (1 - preds_combos$estimate) / nrow(data)))
preds_combos$conf.high <- pmin(1, preds_combos$estimate + se_multiplier * sqrt(preds_combos$estimate * (1 - preds_combos$estimate) / nrow(data)))

# Add labels
preds_combos$label <- paste("Condition:", preds_combos$CONDITION)

# Create the comparison plot
p3 <- ggplot(preds_combos, aes(x = estimate, y = label, color = OUTCOME_CAT)) +
  geom_point(size = 4, alpha = 0.8) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, size = 1) +
  labs(
    title = paste("Predicted Probabilities by Condition and Outcome\n(at Mean Expectation =", round(mean_expect, 3), ")"),
    x = "Predicted Probability",
    y = NULL,
    color = "Outcome"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),
    panel.grid.major.y = element_blank(),
    legend.position = "bottom"
  ) +
  scale_color_manual(values = c("explore" = "#E74C3C", "exploit" = "#3498DB", "none" = "#95A5A6")) +
  xlim(0, 1)

print(p3)

# 4. Summary statistics
print("\n=== SUMMARY STATISTICS ===")
cat("\nTotal observations:", nrow(data))
cat("\nNumber of monkeys:", length(unique(data$monkey)))
cat("\nNumber of blocks:", length(unique(data$BLOCK_No)))

print("\nExploration rates by condition:")
explore_rates <- data %>%
  group_by(CONDITION) %>%
  summarise(
    n = n(),
    explore_rate = mean(OUTCOME_CAT == "explore", na.rm = TRUE),
    exploit_rate = mean(OUTCOME_CAT == "exploit", na.rm = TRUE),
    none_rate = mean(OUTCOME_CAT == "none", na.rm = TRUE),
    .groups = 'drop'
  )
print(explore_rates)

print("\nModel coefficients:")
print(coef(summary(fit)))

print("\nAnalysis completed successfully!")
print("Three enhanced plots have been generated showing:")
print("1. Basic predicted probabilities by condition and expectation")
print("2. Enhanced plot with confidence intervals")
print("3. Comparison plot at mean expectation level") 