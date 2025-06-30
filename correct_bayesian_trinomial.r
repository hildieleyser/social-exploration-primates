# CORRECT BAYESIAN TRINOMIAL MODEL FOR PRIMATE DECISION-MAKING
# Following user's exact specifications for variable assignments

# Load required libraries
library(brms)
library(dplyr)
library(ggplot2)
library(tidyr)
library(bayesplot)

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
    trial_num = as.numeric(TRIAL_NUM),
    
    # Additional variables for context
    social_complexity = case_when(
      y10 == "solo" ~ 0,  # Individual
      y10 == "duo" ~ 1,   # Dyadic  
      y10 == "trio" ~ 2   # Triadic
    )
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
cat("Data Summary:\n")
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
print("Missing values check:")
print(missing_check)

# Remove rows with missing predictor values
data_final <- data_clean %>%
  filter(!is.na(y03) & !is.na(y04) & !is.na(y05) & !is.na(y06))

cat("\nFinal dataset size:", nrow(data_final), "\n")

# HIERARCHICAL BAYESIAN TRINOMIAL MODEL
# Level 1: Trinomial outcome ~ Multinomial(1, π_ijk)
# Level 2: Individual-level predictors with random effects
# Level 3: Population-level hyperpriors

# Define the model formula
model_formula <- bf(
  outcome_ternary ~ 
    # Fixed effects
    y10 +                    # Social condition (solo/duo/trio)
    y03 +                    # Relative rank
    y04 +                    # Subjective chosen value
    y05 +                    # Subjective exploit value  
    y06 +                    # Expected explore value
    
    # Random intercepts
    (1 | monkey_id) +        # Random intercept by monkey
    (1 | block_id),          # Random intercept by block
  
  family = categorical()
)

# Set priors
priors <- c(
  # Intercept priors (weakly informative)
  prior(normal(0, 1), class = Intercept, dpar = muexplore),
  prior(normal(0, 1), class = Intercept, dpar = munone),
  
  # Fixed effect priors (weakly informative)
  prior(normal(0, 0.5), class = b, dpar = muexplore),
  prior(normal(0, 0.5), class = b, dpar = munone),
  
  # Random effect standard deviations
  prior(exponential(1), class = sd, group = monkey_id),
  prior(exponential(1), class = sd, group = block_id)
)

# Fit the Bayesian model
cat("Fitting Bayesian trinomial model...\n")
cat("This may take several minutes...\n")

model_bayes <- brm(
  formula = model_formula,
  data = data_final,
  family = categorical(),
  prior = priors,
  chains = 4,
  iter = 2000,
  warmup = 1000,
  cores = 4,
  control = list(adapt_delta = 0.95, max_treedepth = 12),
  seed = 12345
)

# Model summary
print(summary(model_bayes))

# Check model convergence
print("R-hat values (should be < 1.01):")
print(rhat(model_bayes))

# Posterior predictive checks
pp_check(model_bayes, ndraws = 50)

# Extract and examine coefficients
posterior_samples <- posterior_samples(model_bayes)

# Create coefficient plots
plot(model_bayes)

# Generate predictions for visualization
newdata <- expand.grid(
  y10 = factor(c("solo", "duo", "trio"), levels = c("solo", "duo", "trio")),
  y03 = mean(data_final$y03, na.rm = TRUE),
  y04 = mean(data_final$y04, na.rm = TRUE), 
  y05 = mean(data_final$y05, na.rm = TRUE),
  y06 = mean(data_final$y06, na.rm = TRUE),
  monkey_id = factor("FRAN", levels = levels(data_final$monkey_id)),
  block_id = factor("BLOCK_1", levels = levels(data_final$block_id))
)

# Generate posterior predictions
predictions <- posterior_epred(model_bayes, newdata = newdata, allow_new_levels = TRUE)

# Calculate prediction summaries
pred_summary <- apply(predictions, c(2,3), function(x) {
  c(mean = mean(x), 
    lower = quantile(x, 0.025),
    upper = quantile(x, 0.975))
})

# Create visualization
pred_df <- data.frame(
  condition = rep(c("solo", "duo", "trio"), 3),
  outcome = rep(c("exploit", "explore", "none"), each = 3),
  mean = as.vector(pred_summary[1,,]),
  lower = as.vector(pred_summary[2,,]),
  upper = as.vector(pred_summary[3,,])
)

# Plot predictions
ggplot(pred_df, aes(x = condition, y = mean, fill = outcome)) +
  geom_col(position = "dodge", alpha = 0.7) +
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                position = position_dodge(width = 0.9), width = 0.2) +
  labs(
    title = "Bayesian Trinomial Model Predictions",
    subtitle = "Probability of each outcome by social condition",
    x = "Social Condition", 
    y = "Predicted Probability",
    fill = "Outcome"
  ) +
  theme_minimal() +
  scale_fill_brewer(type = "qual", palette = "Set2")

# Save the model
saveRDS(model_bayes, "bayesian_trinomial_model.rds")

# Print model equations
cat("\n=== HIERARCHICAL BAYESIAN TRINOMIAL MODEL EQUATIONS ===\n")
cat("Level 1 (Observation): Y_ijk ~ Multinomial(1, π_ijk)\n")
cat("where i = monkey, j = block, k = trial\n")
cat("π_ijk = (π_exploit, π_explore, π_none)\n\n")

cat("Level 2 (Linear Predictors):\n")
cat("logit(π_explore/π_exploit) = α_explore + β1*y10 + β2*y03 + β3*y04 + β4*y05 + β5*y06 + u_i + v_j\n")
cat("logit(π_none/π_exploit) = α_none + γ1*y10 + γ2*y03 + γ3*y04 + γ4*y05 + γ5*y06 + w_i + z_j\n\n")

cat("where:\n")
cat("y10 = CONDITION (social context: solo/duo/trio)\n")
cat("y03 = RELATIVE_RANK (rank within social context)\n") 
cat("y04 = SUBJECTIVE_CHOSEN_VALUE (subjective value of choice)\n")
cat("y05 = subjective_exploit (visible exploit option value)\n")
cat("y06 = expected_explore (expectation for explore value)\n")
cat("u_i, w_i ~ N(0, σ_monkey²) [random intercepts by monkey]\n")
cat("v_j, z_j ~ N(0, σ_block²) [random intercepts by block]\n\n")

cat("Level 3 (Hyperpriors):\n")
cat("α_explore, α_none ~ N(0, 1)\n")
cat("β1...β5, γ1...γ5 ~ N(0, 0.5)\n") 
cat("σ_monkey, σ_block ~ Exponential(1)\n")

cat("\nModel fitted successfully!\n") 