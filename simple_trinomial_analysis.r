# CORRECT TRINOMIAL MODEL FOR PRIMATE DECISION-MAKING
# Using user's exact variable specifications with minimal dependencies

# Load required libraries
library(nnet)
library(dplyr)
library(ggplot2)

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

cat("\n=== PREDICTIONS BY SOCIAL CONDITION ===\n")
print(pred_df)

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

cat("\n=== PREDICTIONS BY MONKEY (SOLO CONDITION) ===\n")
print(pred_monkey_df)

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
  rank = c("Dominant", "Intermediate", "Subordinate"),
  exploit = pred_rank_probs[, "exploit"],
  explore = pred_rank_probs[, "explore"],
  none = pred_rank_probs[, "none"]
)

cat("\n=== PREDICTIONS BY RANK (DUO CONDITION) ===\n")
print(pred_rank_df)

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

# Create simple plots
# Plot 1: Social condition effects
pdf("social_condition_effects.pdf", width = 10, height = 6)
par(mfrow = c(1, 3))

# Exploit probabilities
barplot(pred_df$exploit, names.arg = pred_df$condition, 
        main = "Exploit Probability by Condition", 
        ylab = "Probability", col = "lightblue")

# Explore probabilities  
barplot(pred_df$explore, names.arg = pred_df$condition,
        main = "Explore Probability by Condition",
        ylab = "Probability", col = "lightgreen")

# None probabilities
barplot(pred_df$none, names.arg = pred_df$condition,
        main = "None Probability by Condition", 
        ylab = "Probability", col = "lightcoral")

dev.off()

# Plot 2: Individual differences
pdf("individual_differences.pdf", width = 12, height = 6)
par(mfrow = c(1, 3))

# Exploit probabilities by monkey
barplot(pred_monkey_df$exploit, names.arg = pred_monkey_df$monkey,
        main = "Exploit Probability by Monkey",
        ylab = "Probability", col = "lightblue", las = 2)

# Explore probabilities by monkey
barplot(pred_monkey_df$explore, names.arg = pred_monkey_df$monkey,
        main = "Explore Probability by Monkey", 
        ylab = "Probability", col = "lightgreen", las = 2)

# None probabilities by monkey
barplot(pred_monkey_df$none, names.arg = pred_monkey_df$monkey,
        main = "None Probability by Monkey",
        ylab = "Probability", col = "lightcoral", las = 2)

dev.off()

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

cat("\nAnalysis complete! Results and plots saved.\n")
cat("Generated files:\n")
cat("- trinomial_predictions_by_condition.csv\n")
cat("- trinomial_predictions_by_monkey.csv\n") 
cat("- trinomial_predictions_by_rank.csv\n")
cat("- social_condition_effects.pdf\n")
cat("- individual_differences.pdf\n")
cat("- trinomial_model.rds\n") 