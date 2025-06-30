# RESEARCH QUESTION 1: BAYESIAN HIERARCHICAL TRINOMIAL MODEL (FIXED PRIORS)
# Using brms for proper Bayesian analysis

library(brms)
library(ggplot2)
library(dplyr)
library(bayesplot)
library(posterior)

cat("=== BAYESIAN RESEARCH QUESTION 1 ANALYSIS (FIXED) ===\n")
cat("Social reference frames and identity models in primate decision-making\n\n")

# Load and prepare data
data_raw <- read.csv("Explore Exploit Dataset.csv", stringsAsFactors = FALSE)
data_exp <- data_raw[data_raw$TRIAL_TYPE == "OIT_RE", ]
data_exp$outcome_clean <- ifelse(grepl("explore", data_exp$OUTCOME, ignore.case = TRUE), "explore",
                                ifelse(grepl("exploit", data_exp$OUTCOME, ignore.case = TRUE), "exploit", 
                                      ifelse(grepl("none|NONE", data_exp$OUTCOME, ignore.case = TRUE), "none", NA)))

# Clean data and prepare for Bayesian analysis
data_clean <- data_exp[!is.na(data_exp$outcome_clean), ]
data_clean$outcome_clean <- factor(data_clean$outcome_clean, levels = c("exploit", "explore", "none"))

# Prepare variables
data_clean$monkey <- data_clean$monkey
data_clean$condition <- factor(data_clean$CONDITION, levels = c("solo", "duo", "trio"))
data_clean$sex <- ifelse(data_clean$monkey %in% c("FRAN", "DALI", "EBI"), "Male", "Female")
data_clean$sex <- factor(data_clean$sex, levels = c("Male", "Female"))
data_clean$relative_rank <- factor(data_clean$RELATIVE_RANK, levels = c(1, 2, 3))
data_clean$absolute_rank <- factor(data_clean$ABSOLUTE_RANK, levels = c(1, 2, 3))

# Create model data
model_data <- data_clean[complete.cases(data_clean[c("outcome_clean", "condition", "relative_rank", "absolute_rank", "sex", "monkey")]), ]

cat("Model data prepared. Sample size:", nrow(model_data), "trials\n")
cat("Outcomes:\n")
print(table(model_data$outcome_clean))

# Set up brms options
options(mc.cores = parallel::detectCores())

# Check default priors first
cat("\n=== CHECKING DEFAULT PRIORS ===\n")
default_priors_rel <- get_prior(outcome_clean ~ condition + relative_rank + sex + (1|monkey),
                                data = model_data,
                                family = categorical())
print(default_priors_rel)

# Set proper priors for categorical model
priors_rel <- c(
  prior(normal(0, 1), class = Intercept, dpar = muexplore),
  prior(normal(0, 1), class = Intercept, dpar = munone),
  prior(normal(0, 0.5), class = b, dpar = muexplore),
  prior(normal(0, 0.5), class = b, dpar = munone),
  prior(exponential(1), class = sd, group = monkey, dpar = muexplore),
  prior(exponential(1), class = sd, group = monkey, dpar = munone)
)

cat("\n=== BAYESIAN HIERARCHICAL MULTINOMIAL MODELS ===\n")

# Model 1: Relative Rank Model
cat("\nFitting Relative Rank Bayesian Model...\n")

bayesian_relative <- tryCatch({
  brm(outcome_clean ~ condition + relative_rank + sex + (1|monkey),
      data = model_data,
      family = categorical(),
      prior = priors_rel,
      iter = 2000, warmup = 1000, chains = 4,
      control = list(adapt_delta = 0.95),
      silent = 2, refresh = 0)
}, error = function(e) {
  cat("Error fitting Bayesian relative rank model:", e$message, "\n")
  return(NULL)
})

# Set priors for absolute rank model
priors_abs <- c(
  prior(normal(0, 1), class = Intercept, dpar = muexplore),
  prior(normal(0, 1), class = Intercept, dpar = munone),
  prior(normal(0, 0.5), class = b, dpar = muexplore),
  prior(normal(0, 0.5), class = b, dpar = munone),
  prior(exponential(1), class = sd, group = monkey, dpar = muexplore),
  prior(exponential(1), class = sd, group = monkey, dpar = munone)
)

# Model 2: Absolute Rank Model  
cat("Fitting Absolute Rank Bayesian Model...\n")

bayesian_absolute <- tryCatch({
  brm(outcome_clean ~ condition + absolute_rank + sex + (1|monkey),
      data = model_data,
      family = categorical(),
      prior = priors_abs,
      iter = 2000, warmup = 1000, chains = 4,
      control = list(adapt_delta = 0.95),
      silent = 2, refresh = 0)
}, error = function(e) {
  cat("Error fitting Bayesian absolute rank model:", e$message, "\n")
  return(NULL)
})

# Model comparison if both models fitted successfully
if(!is.null(bayesian_relative) && !is.null(bayesian_absolute)) {
  
  cat("\n=== BAYESIAN MODEL COMPARISON ===\n")
  
  # LOO Cross-validation
  loo_relative <- loo(bayesian_relative)
  loo_absolute <- loo(bayesian_absolute)
  
  # Compare models
  loo_comparison <- loo_compare(loo_relative, loo_absolute)
  
  cat("LOO Model Comparison:\n")
  print(loo_comparison)
  
  # Model weights
  model_weights_vals <- model_weights(bayesian_relative, bayesian_absolute, weights = "loo")
  cat("\nModel Weights:\n")
  cat("Relative Rank Model:", round(model_weights_vals[1], 3), "\n")
  cat("Absolute Rank Model:", round(model_weights_vals[2], 3), "\n")
  
  # Posterior summaries
  cat("\n=== RELATIVE RANK MODEL SUMMARY ===\n")
  print(summary(bayesian_relative))
  
  cat("\n=== ABSOLUTE RANK MODEL SUMMARY ===\n")
  print(summary(bayesian_absolute))
  
  # Create Bayesian visualizations
  pdf("BAYESIAN_RESEARCH_QUESTION_1_RESULTS.pdf", width = 16, height = 12)
  
  # Create a 2x3 layout
  par(mfrow = c(2, 3), mar = c(4, 4, 3, 2))
  
  # Plot 1: Posterior distributions for relative rank effects (explore outcome)
  posterior_rel <- as.data.frame(bayesian_relative)
  
  # Extract rank effects for explore outcome
  rank_cols_rel <- grep("b_muexplore.*relative_rank", colnames(posterior_rel), value = TRUE)
  
  if(length(rank_cols_rel) > 0) {
    rank_effects_rel <- posterior_rel[, rank_cols_rel, drop = FALSE]
    boxplot(rank_effects_rel, 
            names = c("Rank 2 vs 1", "Rank 3 vs 1"),
            main = "A. Relative Rank Effects on Exploration\n(Posterior Distributions)",
            ylab = "Log-Odds Effect Size",
            col = c("#E8F4FD", "#81D4FA"),
            border = "black")
    abline(h = 0, lty = 2, col = "red")
  }
  
  # Plot 2: Posterior distributions for absolute rank effects (explore outcome)
  posterior_abs <- as.data.frame(bayesian_absolute)
  rank_cols_abs <- grep("b_muexplore.*absolute_rank", colnames(posterior_abs), value = TRUE)
  
  if(length(rank_cols_abs) > 0) {
    rank_effects_abs <- posterior_abs[, rank_cols_abs, drop = FALSE]
    boxplot(rank_effects_abs,
            names = c("Rank 2 vs 1", "Rank 3 vs 1"), 
            main = "B. Absolute Rank Effects on Exploration\n(Posterior Distributions)",
            ylab = "Log-Odds Effect Size",
            col = c("#F3E5F5", "#E1BEE7"),
            border = "black")
    abline(h = 0, lty = 2, col = "red")
  }
  
  # Plot 3: Model comparison
  barplot(c(model_weights_vals[1], model_weights_vals[2]),
          names.arg = c("Relative Rank", "Absolute Rank"),
          main = "C. Bayesian Model Weights",
          ylab = "Model Weight",
          col = c("#E8F4FD", "#F3E5F5"),
          border = "black",
          ylim = c(0, 1))
  
  # Plot 4: Condition effects from relative rank model
  cond_cols_rel <- grep("b_muexplore.*condition", colnames(posterior_rel), value = TRUE)
  if(length(cond_cols_rel) > 0) {
    cond_effects_rel <- posterior_rel[, cond_cols_rel, drop = FALSE]
    boxplot(cond_effects_rel,
            names = c("Duo vs Solo", "Trio vs Solo"),
            main = "D. Social Context Effects\n(Relative Rank Model)",
            ylab = "Log-Odds Effect Size",
            col = c("#E8F5E8", "#C8E6C9"),
            border = "black")
    abline(h = 0, lty = 2, col = "red")
  }
  
  # Plot 5: Sex effects comparison
  sex_rel <- grep("b_muexplore.*sexMale", colnames(posterior_rel), value = TRUE)
  sex_abs <- grep("b_muexplore.*sexMale", colnames(posterior_abs), value = TRUE)
  
  if(length(sex_rel) > 0 && length(sex_abs) > 0) {
    sex_effects <- data.frame(
      Relative = posterior_rel[, sex_rel],
      Absolute = posterior_abs[, sex_abs]
    )
    boxplot(sex_effects,
            main = "E. Male vs Female Effects\n(Both Models)",
            ylab = "Log-Odds Effect Size",
            col = c("#FFF3E0", "#FFE0B2"),
            border = "black")
    abline(h = 0, lty = 2, col = "red")
  }
  
  # Plot 6: LOO comparison visualization
  loo_diff <- loo_comparison[1, "elpd_diff"]
  loo_se <- loo_comparison[1, "se_diff"]
  
  barplot(c(loo_relative$estimates["elpd_loo", "Estimate"], 
           loo_absolute$estimates["elpd_loo", "Estimate"]),
          names.arg = c("Relative", "Absolute"),
          main = "F. Expected Log Predictive Density",
          ylab = "ELPD (higher = better)",
          col = c("#E8F4FD", "#F3E5F5"),
          border = "black")
  
  text(1.5, max(loo_relative$estimates["elpd_loo", "Estimate"], 
               loo_absolute$estimates["elpd_loo", "Estimate"]) * 0.95,
       paste("Difference:", round(loo_diff, 1), "±", round(loo_se, 1)),
       cex = 1.1, font = 2)
  
  dev.off()
  
  cat("\nCreated BAYESIAN_RESEARCH_QUESTION_1_RESULTS.pdf\n")
  
} else if(!is.null(bayesian_relative) || !is.null(bayesian_absolute)) {
  
  # At least one model fitted
  working_model <- if(!is.null(bayesian_relative)) bayesian_relative else bayesian_absolute
  model_name <- if(!is.null(bayesian_relative)) "Relative Rank" else "Absolute Rank"
  
  cat("\nOnly", model_name, "model fitted successfully.\n")
  cat("Model Summary:\n")
  print(summary(working_model))
  
} else {
  # Fallback to frequentist analysis
  cat("\nBayesian models failed. Running frequentist analysis as fallback...\n")
  
  library(nnet)
  
  # Frequentist multinomial models
  model_relative_freq <- multinom(outcome_clean ~ condition + relative_rank + sex, 
                                 data = model_data, trace = FALSE)
  
  model_absolute_freq <- multinom(outcome_clean ~ condition + absolute_rank + sex, 
                                 data = model_data, trace = FALSE)
  
  # Model comparison
  aic_relative <- AIC(model_relative_freq)
  aic_absolute <- AIC(model_absolute_freq)
  
  cat("\nFrequentist Model Comparison:\n")
  cat("Relative Rank Model AIC:", round(aic_relative, 2), "\n")
  cat("Absolute Rank Model AIC:", round(aic_absolute, 2), "\n")
  cat("Best Model:", ifelse(aic_relative < aic_absolute, "Relative Rank", "Absolute Rank"), "\n")
}

# Summary regardless of method used
cat("\n=== RESEARCH QUESTION 1 SUMMARY ===\n")

# Calculate basic statistics
rank_effects <- model_data %>%
  group_by(relative_rank) %>%
  summarise(exploration_rate = mean(outcome_clean == "explore") * 100, .groups = "drop") %>%
  filter(!is.na(relative_rank))

gender_effects <- model_data %>%
  group_by(sex) %>%
  summarise(exploration_rate = mean(outcome_clean == "explore") * 100, .groups = "drop")

individual_effects <- model_data %>%
  group_by(monkey) %>%
  summarise(exploration_rate = mean(outcome_clean == "explore") * 100, .groups = "drop")

# Effect sizes
rank_range <- max(rank_effects$exploration_rate) - min(rank_effects$exploration_rate)
gender_range <- max(gender_effects$exploration_rate) - min(gender_effects$exploration_rate)
individual_range <- max(individual_effects$exploration_rate) - min(individual_effects$exploration_rate)

cat("1. RANK vs GENDER IMPORTANCE:\n")
cat("   - Rank effect range:", round(rank_range, 1), "%\n")
cat("   - Gender effect range:", round(gender_range, 1), "%\n")
cat("   - RANK IS", ifelse(rank_range > gender_range, "MORE", "LESS"), "IMPORTANT THAN GENDER\n")

cat("\n2. RANK vs INDIVIDUAL DIFFERENCES:\n")
cat("   - Rank effect range:", round(rank_range, 1), "%\n")
cat("   - Individual differences range:", round(individual_range, 1), "%\n")
cat("   - INDIVIDUAL DIFFERENCES ARE", ifelse(individual_range > rank_range, "MORE", "LESS"), "IMPORTANT THAN RANK\n")

if(!is.null(bayesian_relative) && !is.null(bayesian_absolute)) {
  cat("\n3. BAYESIAN MODEL COMPARISON:\n")
  winner <- ifelse(model_weights_vals[1] > model_weights_vals[2], "RELATIVE RANK", "ABSOLUTE RANK")
  cat("   - Winner:", winner, "MODEL\n")
  cat("   - Model weights: Relative", round(model_weights_vals[1], 3), 
      ", Absolute", round(model_weights_vals[2], 3), "\n")
}

cat("\nBayesian analysis complete!\n") 