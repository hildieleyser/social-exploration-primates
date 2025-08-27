# =============================================================================
# Robustness Checks and Alternative Model Specifications
# Supplementary analysis for explore/exploit decision modeling
# =============================================================================

# Load required libraries and data
library(brms)
library(tidyverse)
library(bayesplot)
library(loo)

# Load the cleaned data (assumes main script has been run)
if(!exists("data_clean")) {
  data <- read.csv("Explore Exploit Dataset.csv", stringsAsFactors = FALSE)
  source("bayesian_explore_exploit_analysis.R")
}

# =============================================================================
# ALTERNATIVE MODEL SPECIFICATIONS
# =============================================================================

# Alternative 1: Multinomial model treating explore/exploit as categorical
# (instead of binary logistic)
cat("Fitting multinomial alternative model...\n")

# Prepare data for multinomial - include other outcomes
data_multi <- data %>%
  filter(!is.na(monkey) & !is.na(CONDITION) & !is.na(OUTCOME)) %>%
  mutate(
    decision_outcome = case_when(
      str_detect(tolower(OUTCOME), "explore") ~ "explore",
      str_detect(tolower(OUTCOME), "exploit") ~ "exploit",
      TRUE ~ "other"
    ),
    social_context = case_when(
      CONDITION == "solo" ~ "solo",
      CONDITION == "duo" ~ "duo", 
      CONDITION == "trio" ~ "trio",
      TRUE ~ "other"
    ),
    expected_explore_z = scale(expected_explore)[,1],
    subjective_exploit_z = scale(subjective_exploit)[,1],
    monkey_id = factor(monkey),
    block_id = factor(BLOCK_No)
  ) %>%
  filter(!is.na(expected_explore) & social_context != "other" & 
         decision_outcome != "other") %>%
  group_by(monkey_id, social_context) %>%
  filter(n() >= 10) %>%
  ungroup()

# Multinomial model
model_multinomial <- brm(
  decision_outcome ~ social_context + expected_explore_z + subjective_exploit_z +
                    (1 | monkey_id) + (1 | block_id),
  data = data_multi,
  family = categorical(),
  chains = 4,
  iter = 2000,
  cores = 4,
  seed = 123
)

# Alternative 2: Varying intercepts and slopes by trial type
cat("Fitting model with varying slopes by trial type...\n")

model_trial_varying <- brm(
  decision_type ~ social_context * expected_explore_z + subjective_exploit_z +
                 (1 + social_context | monkey_id) + 
                 (1 + expected_explore_z | trial_type_clean) +
                 (1 | block_id),
  data = data_clean,
  family = bernoulli(),
  chains = 4,
  iter = 2000,
  cores = 4,
  control = list(adapt_delta = 0.95),
  seed = 123
)

# Alternative 3: Non-linear effects using splines
cat("Fitting model with non-linear expectation effects...\n")

model_spline <- brm(
  decision_type ~ social_context + s(expected_explore, by = social_context) + 
                 subjective_exploit_z + (1 | monkey_id) + (1 | block_id),
  data = data_clean,
  family = bernoulli(),
  chains = 4,
  iter = 2000,
  cores = 4,
  control = list(adapt_delta = 0.95),
  seed = 123
)

# =============================================================================
# ROBUSTNESS CHECKS
# =============================================================================

# Robustness 1: Different prior specifications
cat("Testing sensitivity to priors...\n")

# Skeptical priors
priors_skeptical <- c(
  prior(normal(0, 0.5), class = Intercept),
  prior(normal(0, 0.25), class = b),
  prior(exponential(2), class = sd)
)

# Informative priors (assuming some exploration bias)
priors_informative <- c(
  prior(normal(0.2, 0.5), class = Intercept),  # Slight exploration bias
  prior(normal(0, 0.5), class = b),
  prior(exponential(1), class = sd)
)

model_skeptical <- brm(
  decision_type ~ social_context * expected_explore_z + subjective_exploit_z + 
                 trial_num_z + (1 + expected_explore_z | monkey_id) + (1 | block_id),
  data = data_clean,
  family = bernoulli(),
  prior = priors_skeptical,
  chains = 4,
  iter = 2000,
  cores = 4,
  control = list(adapt_delta = 0.95),
  seed = 123
)

model_informative <- brm(
  decision_type ~ social_context * expected_explore_z + subjective_exploit_z + 
                 trial_num_z + (1 + expected_explore_z | monkey_id) + (1 | block_id),
  data = data_clean,
  family = bernoulli(),
  prior = priors_informative,
  chains = 4,
  iter = 2000,
  cores = 4,
  control = list(adapt_delta = 0.95),
  seed = 123
)

# Robustness 2: Subset analyses by monkey
cat("Running subset analyses by individual monkeys...\n")

monkey_models <- list()
for(monkey in unique(data_clean$monkey_id)) {
  cat("Fitting model for monkey:", monkey, "\n")
  
  monkey_data <- data_clean %>% filter(monkey_id == monkey)
  
  if(nrow(monkey_data) > 50) {  # Only if sufficient data
    monkey_models[[monkey]] <- brm(
      decision_type ~ social_context + expected_explore_z + 
                     subjective_exploit_z + (1 | block_id),
      data = monkey_data,
      family = bernoulli(),
      chains = 4,
      iter = 2000,
      cores = 1,  # Reduce cores for individual models
      seed = 123,
      refresh = 0  # Suppress output
    )
  }
}

# Robustness 3: Temporal effects
cat("Testing for temporal trends...\n")

# Add temporal variables
data_temporal <- data_clean %>%
  arrange(monkey_id, date, TRIAL_NUM) %>%
  group_by(monkey_id) %>%
  mutate(
    trial_order = row_number(),
    trial_order_z = scale(trial_order)[,1],
    date_numeric = as.numeric(as.Date(date, format = "%d/%m/%Y")),
    date_z = scale(date_numeric)[,1]
  ) %>%
  ungroup()

model_temporal <- brm(
  decision_type ~ social_context * expected_explore_z + subjective_exploit_z + 
                 trial_order_z + date_z + (1 + expected_explore_z | monkey_id) + 
                 (1 | block_id),
  data = data_temporal,
  family = bernoulli(),
  chains = 4,
  iter = 2000,
  cores = 4,
  control = list(adapt_delta = 0.95),
  seed = 123
)

# =============================================================================
# MODEL COMPARISON AND VALIDATION
# =============================================================================

# Compare all models using LOO
cat("Comparing all models using LOO...\n")

loo_multinomial <- loo(model_multinomial)
loo_trial_varying <- loo(model_trial_varying)
loo_spline <- loo(model_spline)
loo_skeptical <- loo(model_skeptical)
loo_informative <- loo(model_informative)
loo_temporal <- loo(model_temporal)

# Model comparison
all_loos <- list(
  multinomial = loo_multinomial,
  trial_varying = loo_trial_varying,
  spline = loo_spline,
  skeptical = loo_skeptical,
  informative = loo_informative,
  temporal = loo_temporal
)

print("Extended Model Comparison:")
print(loo_compare(all_loos))

# =============================================================================
# CROSS-VALIDATION AND PREDICTION
# =============================================================================

# K-fold cross-validation for best model
cat("Performing k-fold cross-validation...\n")

# Choose best model from comparison
best_alternative <- model_temporal  # Update based on actual results

kfold_result <- kfold(best_alternative, K = 5, cores = 4)
print("K-fold cross-validation results:")
print(kfold_result)

# Out-of-sample prediction
# Leave one monkey out cross-validation
monkeys <- unique(data_clean$monkey_id)
predictions <- list()

for(i in seq_along(monkeys)) {
  cat("Leave-one-out for monkey:", monkeys[i], "\n")
  
  train_data <- data_clean %>% filter(monkey_id != monkeys[i])
  test_data <- data_clean %>% filter(monkey_id == monkeys[i])
  
  if(nrow(train_data) > 100 & nrow(test_data) > 10) {
    # Fit model on training data
    loo_model <- brm(
      decision_type ~ social_context * expected_explore_z + subjective_exploit_z + 
                     (1 + expected_explore_z | monkey_id) + (1 | block_id),
      data = train_data,
      family = bernoulli(),
      chains = 2,  # Reduced for speed
      iter = 1000,
      cores = 2,
      seed = 123,
      refresh = 0
    )
    
    # Predict on test data
    pred <- posterior_predict(loo_model, newdata = test_data, allow_new_levels = TRUE)
    predictions[[monkeys[i]]] <- list(
      observed = test_data$decision_type,
      predicted = apply(pred, 2, mean)
    )
  }
}

# Calculate prediction accuracy
prediction_accuracy <- sapply(predictions, function(x) {
  if(!is.null(x)) {
    observed_binary <- ifelse(x$observed == "explore", 1, 0)
    predicted_binary <- ifelse(x$predicted > 0.5, 1, 0)
    mean(observed_binary == predicted_binary)
  } else {
    NA
  }
})

cat("Prediction accuracies by monkey:\n")
print(prediction_accuracy)
cat("Mean prediction accuracy:", mean(prediction_accuracy, na.rm = TRUE), "\n")

# =============================================================================
# EFFECT SIZE ANALYSIS
# =============================================================================

# Calculate standardized effect sizes
cat("Calculating effect sizes...\n")

# Extract posterior samples from best model
post_samples <- posterior_samples(best_alternative)

# Convert to standardized effect sizes (Cohen's d equivalent for logistic regression)
effect_sizes <- list(
  social_duo = post_samples$b_social_contextduo * sqrt(3)/pi,
  social_trio = post_samples$b_social_contexttrio * sqrt(3)/pi,
  expectation = post_samples$b_expected_explore_z * sqrt(3)/pi,
  interaction_duo = post_samples$`b_social_contextduo:expected_explore_z` * sqrt(3)/pi,
  interaction_trio = post_samples$`b_social_contexttrio:expected_explore_z` * sqrt(3)/pi
)

# Summarize effect sizes
effect_summary <- sapply(effect_sizes, function(x) {
  c(mean = mean(x), 
    sd = sd(x),
    q025 = quantile(x, 0.025),
    q975 = quantile(x, 0.975))
})

print("Standardized Effect Sizes (Cohen's d equivalent):")
print(t(effect_summary))

# =============================================================================
# SAVE ROBUSTNESS RESULTS
# =============================================================================

robustness_results <- list(
  models = list(
    multinomial = model_multinomial,
    trial_varying = model_trial_varying,
    spline = model_spline,
    skeptical = model_skeptical,
    informative = model_informative,
    temporal = model_temporal
  ),
  model_comparison = loo_compare(all_loos),
  kfold_results = kfold_result,
  prediction_accuracy = prediction_accuracy,
  effect_sizes = effect_summary,
  individual_models = monkey_models
)

save(robustness_results, file = "robustness_analysis_results.RData")

cat("\n=============================================================================\n")
cat("ROBUSTNESS ANALYSIS COMPLETE\n")
cat("=============================================================================\n")
cat("Results saved as: robustness_analysis_results.RData\n")
cat("Mean out-of-sample prediction accuracy:", 
    round(mean(prediction_accuracy, na.rm = TRUE), 3), "\n") 