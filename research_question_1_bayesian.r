# RESEARCH QUESTION 1: BAYESIAN HIERARCHICAL TRINOMIAL MODEL
# Using brms for proper Bayesian analysis

# Load libraries with error handling
libraries_needed <- c("brms", "ggplot2", "dplyr", "bayesplot", "posterior")
missing_libs <- c()

for(lib in libraries_needed) {
  if(!require(lib, character.only = TRUE, quietly = TRUE)) {
    missing_libs <- c(missing_libs, lib)
  }
}

if(length(missing_libs) > 0) {
  cat("Missing libraries:", paste(missing_libs, collapse = ", "), "\n")
  cat("Attempting to install...\n")
  install.packages(missing_libs, repos = "https://cran.rstudio.com/")
  
  # Try loading again
  for(lib in missing_libs) {
    if(!require(lib, character.only = TRUE, quietly = TRUE)) {
      cat("FAILED to install", lib, "- falling back to basic analysis\n")
    }
  }
}

cat("=== BAYESIAN RESEARCH QUESTION 1 ANALYSIS ===\n")
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

# Check if brms is available for Bayesian analysis
if(require(brms, quietly = TRUE)) {
  
  cat("\n=== BAYESIAN HIERARCHICAL MULTINOMIAL MODELS ===\n")
  
  # Set up brms options
  options(mc.cores = parallel::detectCores())
  
  # Model 1: Relative Rank Model
  cat("\nFitting Relative Rank Bayesian Model...\n")
  
  bayesian_relative <- tryCatch({
    brm(outcome_clean ~ condition + relative_rank + sex + (1|monkey),
        data = model_data,
        family = categorical(),
        prior = c(
          prior(normal(0, 1), class = Intercept),
          prior(normal(0, 0.5), class = b),
          prior(exponential(1), class = sd)
        ),
        iter = 2000, warmup = 1000, chains = 4,
        control = list(adapt_delta = 0.95),
        silent = 2, refresh = 0)
  }, error = function(e) {
    cat("Error fitting Bayesian relative rank model:", e$message, "\n")
    return(NULL)
  })
  
  # Model 2: Absolute Rank Model  
  cat("Fitting Absolute Rank Bayesian Model...\n")
  
  bayesian_absolute <- tryCatch({
    brm(outcome_clean ~ condition + absolute_rank + sex + (1|monkey),
        data = model_data,
        family = categorical(),
        prior = c(
          prior(normal(0, 1), class = Intercept),
          prior(normal(0, 0.5), class = b),
          prior(exponential(1), class = sd)
        ),
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
    model_weights <- model_weights(bayesian_relative, bayesian_absolute, weights = "loo")
    cat("\nModel Weights:\n")
    cat("Relative Rank Model:", round(model_weights[1], 3), "\n")
    cat("Absolute Rank Model:", round(model_weights[2], 3), "\n")
    
    # Posterior summaries
    cat("\n=== RELATIVE RANK MODEL SUMMARY ===\n")
    print(summary(bayesian_relative))
    
    cat("\n=== ABSOLUTE RANK MODEL SUMMARY ===\n")
    print(summary(bayesian_absolute))
    
    # Create Bayesian visualizations
    if(require(bayesplot, quietly = TRUE)) {
      
      pdf("BAYESIAN_RESEARCH_QUESTION_1_RESULTS.pdf", width = 16, height = 12)
      
      # Layout for multiple plots
      layout(matrix(c(1,2,3,4,5,6), nrow = 2, ncol = 3, byrow = TRUE))
      par(mar = c(4, 4, 3, 2))
      
      # Plot 1: Posterior distributions for relative rank effects
      posterior_relative <- posterior_samples(bayesian_relative)
      
      # Extract rank effects for explore outcome
      rank_effects_rel <- posterior_relative[, grepl("b_muexplore.*relative_rank", colnames(posterior_relative))]
      
      if(ncol(rank_effects_rel) > 0) {
        boxplot(rank_effects_rel, 
                names = c("Rank 2", "Rank 3"),
                main = "A. Relative Rank Effects on Exploration\n(Posterior Distributions)",
                ylab = "Effect Size",
                col = c("#E8F4FD", "#81D4FA"),
                border = "black")
        abline(h = 0, lty = 2, col = "red")
      }
      
      # Plot 2: Posterior distributions for absolute rank effects
      posterior_absolute <- posterior_samples(bayesian_absolute)
      rank_effects_abs <- posterior_absolute[, grepl("b_muexplore.*absolute_rank", colnames(posterior_absolute))]
      
      if(ncol(rank_effects_abs) > 0) {
        boxplot(rank_effects_abs,
                names = c("Rank 2", "Rank 3"), 
                main = "B. Absolute Rank Effects on Exploration\n(Posterior Distributions)",
                ylab = "Effect Size",
                col = c("#F3E5F5", "#E1BEE7"),
                border = "black")
        abline(h = 0, lty = 2, col = "red")
      }
      
      # Plot 3: Model comparison
      barplot(c(model_weights[1], model_weights[2]),
              names.arg = c("Relative Rank", "Absolute Rank"),
              main = "C. Bayesian Model Weights",
              ylab = "Model Weight",
              col = c("#E8F4FD", "#F3E5F5"),
              border = "black",
              ylim = c(0, 1))
      
      # Plot 4: MCMC diagnostics for relative rank model
      plot(bayesian_relative, ask = FALSE)
      title("D. MCMC Diagnostics - Relative Rank Model")
      
      # Plot 5: MCMC diagnostics for absolute rank model  
      plot(bayesian_absolute, ask = FALSE)
      title("E. MCMC Diagnostics - Absolute Rank Model")
      
      # Plot 6: Posterior predictive checks
      pp_check(bayesian_relative, type = "bars") + 
        ggtitle("F. Posterior Predictive Check - Relative Rank Model")
      
      dev.off()
      
      cat("\nCreated BAYESIAN_RESEARCH_QUESTION_1_RESULTS.pdf\n")
    }
    
  } else {
    cat("Could not fit both Bayesian models. Proceeding with available results.\n")
  }
  
} else {
  # Fallback to frequentist analysis if brms not available
  cat("\nbrms not available. Running frequentist analysis as fallback...\n")
  
  if(require(nnet, quietly = TRUE)) {
    
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
    
    # Create basic visualizations
    pdf("FREQUENTIST_RESEARCH_QUESTION_1_RESULTS.pdf", width = 12, height = 8)
    
    layout(matrix(c(1,2,3,4), nrow = 2, ncol = 2, byrow = TRUE))
    par(mar = c(4, 4, 3, 2))
    
    # Basic analysis plots
    rank_data <- model_data %>%
      group_by(relative_rank) %>%
      summarise(exploration_rate = mean(outcome_clean == "explore") * 100, .groups = "drop") %>%
      filter(!is.na(relative_rank))
    
    barplot(rank_data$exploration_rate, 
            names.arg = paste("Rank", rank_data$relative_rank),
            col = c("#E8F4FD", "#B3E5FC", "#81D4FA"),
            main = "A. Relative Rank Effect",
            ylab = "Exploration Rate (%)")
    
    gender_data <- model_data %>%
      group_by(sex) %>%
      summarise(exploration_rate = mean(outcome_clean == "explore") * 100, .groups = "drop")
    
    barplot(gender_data$exploration_rate,
            names.arg = gender_data$sex,
            col = c("#FFF3E0", "#FFE0B2"),
            main = "B. Gender Effect",
            ylab = "Exploration Rate (%)")
    
    # Model comparison
    barplot(c(aic_relative, aic_absolute),
            names.arg = c("Relative", "Absolute"),
            main = "C. AIC Comparison",
            ylab = "AIC (lower = better)",
            col = c("#E8F4FD", "#F3E5F5"))
    
    # Individual differences
    ind_data <- model_data %>%
      group_by(monkey) %>%
      summarise(exploration_rate = mean(outcome_clean == "explore") * 100, .groups = "drop") %>%
      arrange(desc(exploration_rate))
    
    barplot(ind_data$exploration_rate,
            names.arg = ind_data$monkey,
            col = rainbow(nrow(ind_data)),
            main = "D. Individual Differences",
            ylab = "Exploration Rate (%)",
            las = 2)
    
    dev.off()
    
    cat("\nCreated FREQUENTIST_RESEARCH_QUESTION_1_RESULTS.pdf\n")
  }
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

cat("\nAnalysis complete!\n") 