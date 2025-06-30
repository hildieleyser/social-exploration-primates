# CORRECT TRINOMIAL MODEL FOR PRIMATE DECISION-MAKING
# Using user's exact variable specifications with nnet package

# Load required libraries
library(nnet)
library(dplyr)
library(ggplot2)
library(tidyr)
library(broom)
library(gridExtra)

# Read the data
data <- read.csv("Explore Exploit Dataset.csv", stringsAsFactors = FALSE)

# Clean and prepare the data according to user specifications
data_clean <- data %>%
  # Remove rows with missing essential data
  filter(!is.na(OUTCOME) & !is.na(monkey) & !is.na(CONDITION)) %>%
  
  # Create the correct variable assignments as specified by user:
  mutate(
    # y10 = CONDITION (column 5) - social context
    y10 = factor(CONDITION, levels = c("solo", "duo", "trio")),
    
    # y02 = PAIRED_WITH (column 6) - partner information  
    y02 = PAIRED_WITH,
    
    # y03 = RELATIVE_RANK (column 7) - relative rank within context
    y03 = as.numeric(RELATIVE_RANK),
    
    # y04 = SUBJECTIVE_CHOSEN_VALUE (column 11) - subjective value of chosen option
    y04 = as.numeric(SUBJECTIVE_CHOSEN_VALUE),
    
    # y05 = subjective_exploit (column 12) - visible exploit value
    y05 = as.numeric(subjective_exploit),
    
    # y06 = expected_explore (column 17) - expectation for explore value
    y06 = as.numeric(expected_explore),
    
    # Outcome variable - ternary (explore/exploit/none)
    outcome_ternary = case_when(
      grepl("explore", OUTCOME, ignore.case = TRUE) ~ "explore",
      grepl("exploit", OUTCOME, ignore.case = TRUE) ~ "exploit",
      OUTCOME %in% c("none", "NONE", "stop") ~ "none",
      TRUE ~ "other"
    ),
    
    # Grouping factors
    monkey_id = factor(monkey),
    block_id = factor(BLOCK_No),
    trial_num = as.numeric(TRIAL_NUM)
  ) %>%
  
  # Remove rows where outcome couldn't be classified
  filter(outcome_ternary != "other") %>%
  
  # Ensure factors are properly set
  mutate(
    outcome_ternary = factor(outcome_ternary, levels = c("exploit", "explore", "none")),
    monkey_id = factor(monkey_id),
    y10 = factor(y10, levels = c("solo", "duo", "trio"))
  )

# Check the data structure
cat("=== DATA SUMMARY ===\n")
cat("Total observations:", nrow(data_clean), "\n")
cat("Outcome distribution:\n")
print(table(data_clean$outcome_ternary))
cat("\nMonkey distribution:\n")
print(table(data_clean$monkey_id))
cat("\nCondition distribution:\n")
print(table(data_clean$y10))

# Check for missing values in key variables
missing_check <- data_clean %>%
  summarise(
    y03_missing = sum(is.na(y03)),
    y04_missing = sum(is.na(y04)),
    y05_missing = sum(is.na(y05)),
    y06_missing = sum(is.na(y06))
  )
cat("\nMissing values check:\n")
print(missing_check)

# Remove rows with missing predictor values
data_final <- data_clean %>%
  filter(!is.na(y03) & !is.na(y04) & !is.na(y05) & !is.na(y06))

cat("\nFinal dataset size:", nrow(data_final), "\n")

# Print variable descriptions
cat("\n=== VARIABLE DEFINITIONS ===\n")
cat("y10 = CONDITION (social context: solo/duo/trio)\n")
cat("y02 = PAIRED_WITH (partner information)\n") 
cat("y03 = RELATIVE_RANK (rank within social context: 1=dominant, 2=middle, 3=subordinate)\n")
cat("y04 = SUBJECTIVE_CHOSEN_VALUE (subjective value of the choice made)\n")
cat("y05 = subjective_exploit (visible value of exploit option)\n")
cat("y06 = expected_explore (running expectation for explore value)\n")
cat("Outcome = ternary choice (exploit/explore/none)\n")

# TRINOMIAL LOGISTIC REGRESSION MODEL
cat("\n=== FITTING TRINOMIAL MODEL ===\n")

# Fit the multinomial model with exploit as reference category
model_trinomial <- multinom(
  outcome_ternary ~ y10 + y03 + y04 + y05 + y06 + monkey_id,
  data = data_final,
  trace = FALSE
)

# Model summary
cat("\nModel Summary:\n")
print(summary(model_trinomial))

# Calculate z-values and p-values
z_values <- summary(model_trinomial)$coefficients / summary(model_trinomial)$standard.errors
p_values <- (1 - pnorm(abs(z_values))) * 2

cat("\nZ-values:\n")
print(z_values)
cat("\nP-values:\n")
print(p_values)

# Calculate AIC
cat("\nModel AIC:", AIC(model_trinomial), "\n")

# GENERATE PREDICTIONS FOR VISUALIZATION
# Create prediction dataset for social conditions
pred_data <- expand.grid(
  y10 = factor(c("solo", "duo", "trio"), levels = c("solo", "duo", "trio")),
  y03 = mean(data_final$y03, na.rm = TRUE),
  y04 = mean(data_final$y04, na.rm = TRUE),
  y05 = mean(data_final$y05, na.rm = TRUE), 
  y06 = mean(data_final$y06, na.rm = TRUE),
  monkey_id = factor("FRAN", levels = levels(data_final$monkey_id))
)

# Generate predictions
predictions <- predict(model_trinomial, newdata = pred_data, type = "probs")
pred_df <- data.frame(
  condition = pred_data$y10,
  exploit = predictions[, "exploit"],
  explore = predictions[, "explore"],
  none = predictions[, "none"]
)

# Reshape for plotting
pred_long <- pred_df %>%
  pivot_longer(cols = c(exploit, explore, none), 
               names_to = "outcome", values_to = "probability")

# Plot 1: Social condition effects
p1 <- ggplot(pred_long, aes(x = condition, y = probability, fill = outcome)) +
  geom_col(position = "dodge", alpha = 0.8) +
  labs(
    title = "Trinomial Model Predictions by Social Condition",
    subtitle = "Model-based probabilities (not raw data)",
    x = "Social Condition (y10)", 
    y = "Predicted Probability",
    fill = "Outcome"
  ) +
  theme_minimal() +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Predictions by monkey
pred_monkey <- expand.grid(
  y10 = factor("solo", levels = c("solo", "duo", "trio")),
  y03 = mean(data_final$y03, na.rm = TRUE),
  y04 = mean(data_final$y04, na.rm = TRUE),
  y05 = mean(data_final$y05, na.rm = TRUE),
  y06 = mean(data_final$y06, na.rm = TRUE),
  monkey_id = levels(data_final$monkey_id)
)

pred_monkey_probs <- predict(model_trinomial, newdata = pred_monkey, type = "probs")
pred_monkey_df <- data.frame(
  monkey = pred_monkey$monkey_id,
  exploit = pred_monkey_probs[, "exploit"],
  explore = pred_monkey_probs[, "explore"], 
  none = pred_monkey_probs[, "none"]
)

pred_monkey_long <- pred_monkey_df %>%
  pivot_longer(cols = c(exploit, explore, none),
               names_to = "outcome", values_to = "probability")

# Plot 2: Individual differences
p2 <- ggplot(pred_monkey_long, aes(x = monkey, y = probability, fill = outcome)) +
  geom_col(position = "dodge", alpha = 0.8) +
  labs(
    title = "Individual Differences in Decision-Making",
    subtitle = "Model-based predictions for solo condition",
    x = "Monkey ID",
    y = "Predicted Probability", 
    fill = "Outcome"
  ) +
  theme_minimal() +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Predictions by rank
pred_rank <- expand.grid(
  y10 = factor("duo", levels = c("solo", "duo", "trio")),
  y03 = c(1, 2, 3),  # Dominant, intermediate, subordinate
  y04 = mean(data_final$y04, na.rm = TRUE),
  y05 = mean(data_final$y05, na.rm = TRUE),
  y06 = mean(data_final$y06, na.rm = TRUE),
  monkey_id = factor("FRAN", levels = levels(data_final$monkey_id))
)

pred_rank_probs <- predict(model_trinomial, newdata = pred_rank, type = "probs")
pred_rank_df <- data.frame(
  rank = factor(c("Dominant", "Intermediate", "Subordinate"), 
                levels = c("Dominant", "Intermediate", "Subordinate")),
  exploit = pred_rank_probs[, "exploit"],
  explore = pred_rank_probs[, "explore"],
  none = pred_rank_probs[, "none"]
)

pred_rank_long <- pred_rank_df %>%
  pivot_longer(cols = c(exploit, explore, none),
               names_to = "outcome", values_to = "probability")

# Plot 3: Rank effects
p3 <- ggplot(pred_rank_long, aes(x = rank, y = probability, fill = outcome)) +
  geom_col(position = "dodge", alpha = 0.8) +
  labs(
    title = "Rank Effects on Decision-Making",
    subtitle = "Model-based predictions for duo condition (y03)",
    x = "Relative Rank",
    y = "Predicted Probability",
    fill = "Outcome"
  ) +
  theme_minimal() +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Predictions by subjective values
value_range <- seq(0, 1, by = 0.2)
pred_values <- expand.grid(
  y10 = factor("solo", levels = c("solo", "duo", "trio")),
  y03 = mean(data_final$y03, na.rm = TRUE),
  y04 = value_range,  # Vary subjective chosen value
  y05 = mean(data_final$y05, na.rm = TRUE),
  y06 = mean(data_final$y06, na.rm = TRUE),
  monkey_id = factor("FRAN", levels = levels(data_final$monkey_id))
)

pred_values_probs <- predict(model_trinomial, newdata = pred_values, type = "probs")
pred_values_df <- data.frame(
  subjective_value = pred_values$y04,
  exploit = pred_values_probs[, "exploit"],
  explore = pred_values_probs[, "explore"],
  none = pred_values_probs[, "none"]
)

pred_values_long <- pred_values_df %>%
  pivot_longer(cols = c(exploit, explore, none),
               names_to = "outcome", values_to = "probability")

# Plot 4: Subjective value effects
p4 <- ggplot(pred_values_long, aes(x = subjective_value, y = probability, color = outcome)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Effect of Subjective Chosen Value (y04)",
    subtitle = "Model-based predictions",
    x = "Subjective Chosen Value",
    y = "Predicted Probability",
    color = "Outcome"
  ) +
  theme_minimal() +
  scale_color_brewer(type = "qual", palette = "Set2")

# Combine plots
combined_plot <- grid.arrange(p1, p2, p3, p4, ncol = 2)

# Print coefficient interpretation
cat("\n=== MODEL INTERPRETATION ===\n")
cat("Reference category: exploit\n")
cat("Coefficients represent log-odds ratios relative to exploit\n\n")

coefs <- summary(model_trinomial)$coefficients
cat("EXPLORE vs EXPLOIT:\n")
for(i in 1:ncol(coefs)) {
  if(colnames(coefs)[i] != "(Intercept)") {
    cat(sprintf("%s: %.3f (log-odds ratio)\n", colnames(coefs)[i], coefs["explore", i]))
  }
}

cat("\nNONE vs EXPLOIT:\n")
for(i in 1:ncol(coefs)) {
  if(colnames(coefs)[i] != "(Intercept)") {
    cat(sprintf("%s: %.3f (log-odds ratio)\n", colnames(coefs)[i], coefs["none", i]))
  }
}

# Calculate and display odds ratios
cat("\n=== ODDS RATIOS ===\n")
cat("EXPLORE vs EXPLOIT (odds ratios):\n")
explore_ors <- exp(coefs["explore", ])
for(i in 1:length(explore_ors)) {
  if(names(explore_ors)[i] != "(Intercept)") {
    cat(sprintf("%s: %.3f\n", names(explore_ors)[i], explore_ors[i]))
  }
}

cat("\nNONE vs EXPLOIT (odds ratios):\n") 
none_ors <- exp(coefs["none", ])
for(i in 1:length(none_ors)) {
  if(names(none_ors)[i] != "(Intercept)") {
    cat(sprintf("%s: %.3f\n", names(none_ors)[i], none_ors[i]))
  }
}

# Print model equations
cat("\n=== TRINOMIAL LOGISTIC MODEL EQUATIONS ===\n")
cat("Level 1: Y_ij ~ Multinomial(1, π_ij)\n")
cat("where π_ij = (π_exploit, π_explore, π_none)\n\n")

cat("Level 2 (Logit Links):\n")
cat("log(π_explore/π_exploit) = α₁ + β₁*y10 + β₂*y03 + β₃*y04 + β₄*y05 + β₅*y06 + Σγₖ*monkeyₖ\n")
cat("log(π_none/π_exploit) = α₂ + δ₁*y10 + δ₂*y03 + δ₃*y04 + δ₄*y05 + δ₅*y06 + Σηₖ*monkeyₖ\n\n")

cat("where:\n")
cat("y10 = CONDITION (social context: solo/duo/trio)\n")
cat("y02 = PAIRED_WITH (partner information)\n")
cat("y03 = RELATIVE_RANK (relative rank: 1=dominant, 2=intermediate, 3=subordinate)\n")
cat("y04 = SUBJECTIVE_CHOSEN_VALUE (subjective value of chosen option)\n")
cat("y05 = subjective_exploit (visible exploit option value)\n")
cat("y06 = expected_explore (expectation for explore value)\n")
cat("monkeyₖ = individual monkey fixed effects\n")

# Save results
write.csv(pred_df, "trinomial_predictions_by_condition.csv", row.names = FALSE)
write.csv(pred_monkey_df, "trinomial_predictions_by_monkey.csv", row.names = FALSE)
write.csv(pred_rank_df, "trinomial_predictions_by_rank.csv", row.names = FALSE)

# Save model
saveRDS(model_trinomial, "trinomial_model.rds")

cat("\nAnalysis complete! Results saved.\n") 