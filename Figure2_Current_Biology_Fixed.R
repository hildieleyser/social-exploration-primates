# Figure 2: Current Biology/Nature Communications Standard Analysis
# Fixed version with proper error handling

library(ggplot2)
library(dplyr)

# Load optional packages
suppressPackageStartupMessages({
  patchwork_available <- require(patchwork, quietly = TRUE)
  if(!patchwork_available) {
    cat("Note: patchwork not available, will save individual panels\n")
  }
})

cat("=== CURRENT BIOLOGY / NATURE COMMUNICATIONS STANDARD ANALYSIS ===\n")
cat("Model comparison, cross-validation, individual differences\n\n")

# ============================================================================
# DATA PREPARATION
# ============================================================================

data <- read.csv("Explore Exploit Dataset.csv", stringsAsFactors = FALSE)

# Comprehensive data preparation
data_analysis <- data %>%
  filter(TRIAL_TYPE == "OIT_RE") %>%
  mutate(
    choice = case_when(
      grepl("explore", tolower(OUTCOME)) ~ "explore",
      grepl("exploit", tolower(OUTCOME)) ~ "exploit", 
      grepl("none|stop|NONE", tolower(OUTCOME)) | OUTCOME == "" ~ "abstain",
      TRUE ~ NA_character_
    ),
    
    monkey_id = factor(monkey),
    block_id = factor(BLOCK_No),
    social_context = factor(CONDITION, levels = c("solo", "duo", "trio")),
    social_complexity = as.numeric(social_context),
    
    sex = case_when(
      monkey %in% c("FRAN", "DALI", "EBI") ~ "Male",
      monkey %in% c("ANEMONE", "CHOCOLAT", "ICE") ~ "Female"
    ),
    
    # Standardized predictors
    expected_explore_z = as.numeric(scale(expected_explore)),
    subjective_exploit_z = as.numeric(scale(subjective_exploit)),
    chosen_value_z = as.numeric(scale(SUBJECTIVE_CHOSEN_VALUE)),
    trial_in_block_z = as.numeric(scale(TRIAL_NUM)),
    
    abstain = ifelse(choice == "abstain", 1, 0)
  ) %>%
  filter(!is.na(choice), !is.na(expected_explore), !is.na(subjective_exploit), 
         !is.na(SUBJECTIVE_CHOSEN_VALUE)) %>%
  arrange(monkey_id, block_id, TRIAL_NUM)

# Dataset summary
n_trials <- nrow(data_analysis)
n_monkeys <- n_distinct(data_analysis$monkey_id)
n_blocks <- n_distinct(data_analysis$block_id)

cat(sprintf("DATASET SUMMARY:\n"))
cat(sprintf("- Total trials: %d\n", n_trials))
cat(sprintf("- Monkeys: %d\n", n_monkeys))
cat(sprintf("- Blocks: %d\n", n_blocks))
cat(sprintf("- Trials per monkey: %.1f ± %.1f\n", 
            mean(table(data_analysis$monkey_id)), 
            sd(table(data_analysis$monkey_id))))

# ============================================================================
# MODEL COMPARISON
# ============================================================================

cat("\n=== MODEL COMPARISON FRAMEWORK ===\n")

# Fit models with error handling
fit_model_safely <- function(formula, data) {
  tryCatch({
    model <- glm(formula, data = data, family = binomial)
    if(model$converged) {
      return(model)
    } else {
      cat("Warning: Model did not converge\n")
      return(model)
    }
  }, error = function(e) {
    cat("Error fitting model:", e$message, "\n")
    return(NULL)
  })
}

# Model 1: Null model
model_null <- fit_model_safely(abstain ~ 1, data_analysis)

# Model 2: Social context only
model_social <- fit_model_safely(abstain ~ social_complexity, data_analysis)

# Model 3: Social + behavioral (simplified to avoid convergence issues)
model_behavioral <- fit_model_safely(abstain ~ social_complexity + expected_explore_z, data_analysis)

# Model 4: Full model
model_full <- fit_model_safely(abstain ~ social_complexity + expected_explore_z + 
                              subjective_exploit_z + chosen_value_z, data_analysis)

# Model 5: With monkey effects
model_monkey <- fit_model_safely(abstain ~ social_complexity + expected_explore_z + monkey_id, data_analysis)

# Create model comparison table
models <- list(
  "Null" = model_null,
  "Social" = model_social, 
  "Social + Behavioral" = model_behavioral,
  "Full" = model_full,
  "Monkey Effects" = model_monkey
)

# Remove NULL models
models <- models[!sapply(models, is.null)]

if(length(models) > 0) {
  model_comparison <- data.frame(
    Model = names(models),
    AIC = sapply(models, function(m) if(!is.null(m)) AIC(m) else NA),
    BIC = sapply(models, function(m) if(!is.null(m)) BIC(m) else NA),
    LogLik = sapply(models, function(m) if(!is.null(m)) as.numeric(logLik(m)) else NA),
    df = sapply(models, function(m) if(!is.null(m)) attr(logLik(m), "df") else NA),
    stringsAsFactors = FALSE
  )
  
  # Calculate AIC weights
  model_comparison$Delta_AIC <- model_comparison$AIC - min(model_comparison$AIC, na.rm = TRUE)
  model_comparison$AIC_weight <- exp(-0.5 * model_comparison$Delta_AIC) / sum(exp(-0.5 * model_comparison$Delta_AIC), na.rm = TRUE)
  
  # Sort by AIC
  model_comparison <- model_comparison[order(model_comparison$AIC), ]
  
  cat("\nMODEL COMPARISON TABLE:\n")
  print(model_comparison)
  
  best_model <- models[[model_comparison$Model[1]]]
  cat(sprintf("\nBest model: %s (ΔAIC = 0, weight = %.3f)\n", 
              model_comparison$Model[1], model_comparison$AIC_weight[1]))
} else {
  cat("Error: No models fitted successfully\n")
  best_model <- NULL
}

# ============================================================================
# CROSS-VALIDATION
# ============================================================================

if(!is.null(best_model)) {
  cat("\n=== CROSS-VALIDATION ===\n")
  
  monkeys <- unique(data_analysis$monkey_id)
  cv_results <- data.frame()
  
  for(monkey in monkeys) {
    train_data <- data_analysis %>% filter(monkey_id != monkey)
    test_data <- data_analysis %>% filter(monkey_id == monkey)
    
    # Use simplified model for CV to avoid convergence issues
    cv_model <- fit_model_safely(abstain ~ social_complexity + expected_explore_z, train_data)
    
    if(!is.null(cv_model)) {
      test_data$predicted_prob <- predict(cv_model, newdata = test_data, type = "response")
      test_data$predicted_class <- ifelse(test_data$predicted_prob > 0.5, 1, 0)
      accuracy <- mean(test_data$abstain == test_data$predicted_class, na.rm = TRUE)
      
      cv_results <- rbind(cv_results, data.frame(
        monkey = monkey,
        accuracy = accuracy,
        n_trials = nrow(test_data)
      ))
    }
  }
  
  if(nrow(cv_results) > 0) {
    mean_cv_accuracy <- mean(cv_results$accuracy)
    cat(sprintf("Cross-validation accuracy: %.3f ± %.3f\n", 
                mean_cv_accuracy, sd(cv_results$accuracy)))
  }
}

# ============================================================================
# INDIVIDUAL DIFFERENCES
# ============================================================================

cat("\n=== INDIVIDUAL DIFFERENCES ===\n")

individual_analysis <- data_analysis %>%
  group_by(monkey_id, sex) %>%
  summarise(
    n_trials = n(),
    baseline_abstention = mean(abstain),
    mean_explore_expectation = mean(expected_explore_z),
    mean_exploit_value = mean(subjective_exploit_z),
    .groups = "drop"
  )

cat("INDIVIDUAL DIFFERENCES:\n")
print(individual_analysis)

# Test sex differences
sex_test <- t.test(baseline_abstention ~ sex, data = individual_analysis)
cat(sprintf("\nSex difference in abstention: t = %.2f, p = %.4f\n", 
            sex_test$statistic, sex_test$p.value))

# ============================================================================
# EFFECT SIZES
# ============================================================================

if(!is.null(best_model)) {
  cat("\n=== EFFECT SIZES ===\n")
  
  coef_summary <- summary(best_model)$coefficients
  
  # Get confidence intervals safely
  tryCatch({
    conf_int <- confint(best_model)
    
    fixed_effects <- data.frame(
      term = rownames(coef_summary),
      estimate = coef_summary[, "Estimate"],
      std_error = coef_summary[, "Std. Error"],
      z_value = coef_summary[, "z value"],
      p_value = coef_summary[, "Pr(>|z|)"],
      stringsAsFactors = FALSE
    )
    
    if(nrow(conf_int) == nrow(fixed_effects)) {
      fixed_effects$conf_low <- conf_int[, 1]
      fixed_effects$conf_high <- conf_int[, 2]
      fixed_effects$odds_ratio <- exp(fixed_effects$estimate)
      fixed_effects$or_lower <- exp(fixed_effects$conf_low)
      fixed_effects$or_upper <- exp(fixed_effects$conf_high)
    }
    
    cat("FIXED EFFECTS:\n")
    print(fixed_effects)
    
  }, error = function(e) {
    cat("Error calculating confidence intervals:", e$message, "\n")
    print(coef_summary)
  })
}

# ============================================================================
# PUBLICATION-QUALITY FIGURES
# ============================================================================

# Current Biology theme
theme_current_biology <- function() {
  theme_classic(base_size = 8) +
    theme(
      axis.line = element_line(size = 0.3),
      axis.ticks = element_line(size = 0.3),
      axis.text = element_text(size = 7),
      axis.title = element_text(size = 8, face = "bold"),
      plot.title = element_text(size = 9, face = "bold"),
      plot.subtitle = element_text(size = 7, color = "grey30"),
      legend.title = element_text(size = 7, face = "bold"),
      legend.text = element_text(size = 6),
      plot.margin = margin(4, 4, 4, 4),
      panel.grid = element_blank()
    )
}

# Panel A: Social complexity effect
observed_data <- data_analysis %>%
  group_by(social_context) %>%
  summarise(
    observed = mean(abstain),
    se = sqrt(observed * (1 - observed) / n()),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(social_complexity = as.numeric(social_context))

panel_a <- ggplot(observed_data, aes(x = social_complexity, y = observed)) +
  geom_point(size = 2, color = "#2166ac") +
  geom_errorbar(aes(ymin = observed - 1.96*se, ymax = observed + 1.96*se),
                width = 0.1, color = "#2166ac") +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), 
              se = TRUE, color = "#d73027", linetype = "dashed") +
  scale_x_continuous(breaks = 1:3, labels = c("Solo", "Duo", "Trio")) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 0.6)) +
  labs(
    title = "A",
    subtitle = "Social complexity effect",
    x = "Social Context",
    y = "Abstention Rate"
  ) +
  theme_current_biology()

# Panel B: Individual differences
panel_b <- individual_analysis %>%
  ggplot(aes(x = reorder(monkey_id, baseline_abstention), y = baseline_abstention, 
             fill = sex)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = c("Male" = "#d95f02", "Female" = "#1b9e77"),
                   name = "Sex") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "B", 
    subtitle = "Individual differences",
    x = "Individual",
    y = "Abstention Rate"
  ) +
  theme_current_biology() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save figures
ggsave("Figure2_Current_Biology_Panel_A.png", panel_a,
       width = 3.5, height = 3, dpi = 300, bg = "white")
ggsave("Figure2_Current_Biology_Panel_B.png", panel_b,
       width = 3.5, height = 3, dpi = 300, bg = "white")

# ============================================================================
# SUMMARY
# ============================================================================

cat("\n")
cat(paste(rep("=", 60), collapse = ""))
cat("\nCURRENT BIOLOGY ANALYSIS COMPLETE\n")
cat(paste(rep("=", 60), collapse = ""))

cat("\n\nKEY FINDINGS:\n")
cat(sprintf("- Dataset: %d trials from %d monkeys\n", n_trials, n_monkeys))
cat(sprintf("- Abstention rates: Solo %.1f%%, Duo %.1f%%, Trio %.1f%%\n",
            observed_data$observed[1]*100, observed_data$observed[2]*100, observed_data$observed[3]*100))

if(exists("mean_cv_accuracy")) {
  cat(sprintf("- Cross-validation accuracy: %.1f%%\n", mean_cv_accuracy * 100))
}

cat(sprintf("- Sex differences: p = %.4f\n", sex_test$p.value))

cat("\n✅ PUBLICATION-READY ANALYSIS FOR CURRENT BIOLOGY/NATURE COMMUNICATIONS\n")
cat("\nFigures saved:\n")
cat("- Figure2_Current_Biology_Panel_A.png (Social complexity effect)\n")
cat("- Figure2_Current_Biology_Panel_B.png (Individual differences)\n") 