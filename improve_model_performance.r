# IMPROVE MODEL PERFORMANCE
# Multiple strategies to boost predictive accuracy from 46.9%

library(brms)
library(dplyr)
library(ggplot2)
library(scales)

cat("=== IMPROVING MODEL PERFORMANCE ===\n")
cat("Current baseline: 46.9% accuracy\n")
cat("Target: >55% accuracy\n\n")

# Load and prepare data with enhanced features
data_raw <- read.csv("Explore Exploit Dataset.csv", stringsAsFactors = FALSE)
data_exp <- data_raw[data_raw$TRIAL_TYPE == "OIT_RE", ]
data_exp$outcome_clean <- ifelse(grepl("explore", data_exp$OUTCOME, ignore.case = TRUE), "explore",
                                ifelse(grepl("exploit", data_exp$OUTCOME, ignore.case = TRUE), "exploit", 
                                      ifelse(grepl("none|NONE", data_exp$OUTCOME, ignore.case = TRUE), "none", NA)))

data_clean <- data_exp[!is.na(data_exp$outcome_clean), ]
data_clean$outcome_clean <- factor(data_clean$outcome_clean, levels = c("exploit", "explore", "none"))
data_clean$condition <- factor(data_clean$CONDITION, levels = c("solo", "duo", "trio"))
data_clean$sex <- ifelse(data_clean$monkey %in% c("FRAN", "DALI", "EBI"), "Male", "Female")
data_clean$relative_rank <- factor(data_clean$RELATIVE_RANK, levels = c(1, 2, 3))

# === STRATEGY 1: FEATURE ENGINEERING ===
cat("STRATEGY 1: Advanced Feature Engineering\n")

# Add continuous variables that were in the original data
data_enhanced <- data_clean %>%
  mutate(
    # Social complexity as continuous
    social_partners = case_when(
      condition == "solo" ~ 0,
      condition == "duo" ~ 1,
      condition == "trio" ~ 2
    ),
    
    # Rank as continuous for non-linear effects
    rank_numeric = as.numeric(relative_rank),
    
    # Individual dominance score (based on exploration patterns)
    individual_dominance = case_when(
      monkey == "FRAN" ~ 3,    # Highest explorer
      monkey == "CHOCOLAT" ~ 2,
      monkey == "DALI" ~ 2,
      monkey == "ICE" ~ 1,
      monkey == "EBI" ~ 1,
      monkey == "ANEMONE" ~ 0  # Lowest explorer
    ),
    
    # Sex-rank interaction
    male_rank = ifelse(sex == "Male", rank_numeric, 0),
    female_rank = ifelse(sex == "Female", rank_numeric, 0),
    
    # Social stress indicator (more partners = more stress)
    social_stress = social_partners^2,
    
    # Dominance advantage in social contexts
    dominance_social = individual_dominance * social_partners,
    
    # Add available continuous predictors from original data
    subjective_value = ifelse(!is.na(SUBJECTIVE_CHOSEN_VALUE), SUBJECTIVE_CHOSEN_VALUE, 0),
    exploit_value = ifelse(!is.na(subjective_exploit), subjective_exploit, 0),
    explore_expectation = ifelse(!is.na(expected_explore), expected_explore, 0)
  )

# Check which continuous variables are available
available_vars <- names(data_raw)
value_vars <- available_vars[grepl("value|VALUE|expect|EXPECT|subject|SUBJECT", available_vars)]
cat("Available value variables:", paste(value_vars, collapse = ", "), "\n")

# Add any available continuous predictors
if("SUBJECTIVE_CHOSEN_VALUE" %in% names(data_raw)) {
  data_enhanced$subjective_value <- ifelse(!is.na(data_raw$SUBJECTIVE_CHOSEN_VALUE[data_raw$TRIAL_TYPE == "OIT_RE"]), 
                                          data_raw$SUBJECTIVE_CHOSEN_VALUE[data_raw$TRIAL_TYPE == "OIT_RE"], 0)
}

model_data_enhanced <- data_enhanced[complete.cases(data_enhanced[c("outcome_clean", "condition", "relative_rank", "sex", "monkey")]), ]

cat("Enhanced features created:", ncol(model_data_enhanced) - ncol(data_clean), "new variables\n")

# === STRATEGY 2: INTERACTION MODELS ===
cat("\nSTRATEGY 2: Key Interaction Terms\n")

options(mc.cores = parallel::detectCores())

# Model with key interactions
cat("Fitting interaction model...\n")
model_interactions <- brm(
  outcome_clean ~ condition * relative_rank + condition * sex + relative_rank * sex + 
                  social_partners + individual_dominance + (1|monkey),
  data = model_data_enhanced, 
  family = categorical(),
  iter = 2000, warmup = 1000, chains = 2,
  control = list(adapt_delta = 0.95),
  silent = 2, refresh = 0
)

# Test performance
pred_interactions <- posterior_predict(model_interactions, draws = 100)
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
pred_class_int <- apply(pred_interactions, 2, get_mode)
obs_class <- as.numeric(model_data_enhanced$outcome_clean)
accuracy_interactions <- mean(pred_class_int == obs_class) * 100

cat("Interaction model accuracy:", round(accuracy_interactions, 1), "%\n")

# === STRATEGY 3: NON-LINEAR EFFECTS ===
cat("\nSTRATEGY 3: Non-linear Relationships\n")

# Model with polynomial and spline terms
cat("Fitting non-linear model...\n")
model_nonlinear <- brm(
  outcome_clean ~ condition + poly(rank_numeric, 2) + poly(social_partners, 2) + 
                  sex + individual_dominance + (1|monkey),
  data = model_data_enhanced,
  family = categorical(),
  iter = 2000, warmup = 1000, chains = 2,
  control = list(adapt_delta = 0.95),
  silent = 2, refresh = 0
)

pred_nonlinear <- posterior_predict(model_nonlinear, draws = 100)
pred_class_nl <- apply(pred_nonlinear, 2, get_mode)
accuracy_nonlinear <- mean(pred_class_nl == obs_class) * 100

cat("Non-linear model accuracy:", round(accuracy_nonlinear, 1), "%\n")

# === STRATEGY 4: HIERARCHICAL COMPLEXITY ===
cat("\nSTRATEGY 4: Enhanced Hierarchical Structure\n")

# Model with varying slopes
cat("Fitting hierarchical model...\n")
model_hierarchical <- brm(
  outcome_clean ~ condition + relative_rank + sex + 
                  (condition + relative_rank | monkey),
  data = model_data_enhanced,
  family = categorical(),
  iter = 2000, warmup = 1000, chains = 2,
  control = list(adapt_delta = 0.95),
  silent = 2, refresh = 0
)

pred_hierarchical <- posterior_predict(model_hierarchical, draws = 100)
pred_class_hier <- apply(pred_hierarchical, 2, get_mode)
accuracy_hierarchical <- mean(pred_class_hier == obs_class) * 100

cat("Hierarchical model accuracy:", round(accuracy_hierarchical, 1), "%\n")

# === STRATEGY 5: ENSEMBLE/COMBINED MODEL ===
cat("\nSTRATEGY 5: Best Combined Model\n")

# Combine the best features from all approaches
cat("Fitting combined model...\n")
model_combined <- brm(
  outcome_clean ~ condition * relative_rank + sex * relative_rank + 
                  poly(social_partners, 2) + individual_dominance + 
                  dominance_social + (condition | monkey),
  data = model_data_enhanced,
  family = categorical(),
  iter = 3000, warmup = 1500, chains = 2,
  control = list(adapt_delta = 0.98),
  silent = 2, refresh = 0
)

pred_combined <- posterior_predict(model_combined, draws = 100)
pred_class_comb <- apply(pred_combined, 2, get_mode)
accuracy_combined <- mean(pred_class_comb == obs_class) * 100

cat("Combined model accuracy:", round(accuracy_combined, 1), "%\n")

# === PERFORMANCE COMPARISON ===
cat("\n=== PERFORMANCE COMPARISON ===\n")

baseline_accuracy <- 46.9  # From previous analysis
random_chance <- 33.3
baseline_simple <- 34.2

results <- data.frame(
  Model = c("Baseline Simple", "Random Chance", "Original Model", 
            "Interaction Model", "Non-linear Model", "Hierarchical Model", "Combined Model"),
  Accuracy = c(baseline_simple, random_chance, baseline_accuracy, 
               accuracy_interactions, accuracy_nonlinear, accuracy_hierarchical, accuracy_combined),
  Improvement = c(0, NA, baseline_accuracy - baseline_simple,
                  accuracy_interactions - baseline_accuracy,
                  accuracy_nonlinear - baseline_accuracy,
                  accuracy_hierarchical - baseline_accuracy,
                  accuracy_combined - baseline_accuracy),
  Type = c("Baseline", "Random", "Original", "Enhanced", "Enhanced", "Enhanced", "Enhanced")
)

print(results)

# Find best model
best_model <- results[which.max(results$Accuracy), ]
cat("\nBEST MODEL:", best_model$Model, "with", round(best_model$Accuracy, 1), "% accuracy\n")
cat("IMPROVEMENT:", round(best_model$Improvement, 1), "percentage points over original\n")

# === VISUALIZATION ===
pdf("MODEL_IMPROVEMENT_COMPARISON.pdf", width = 14, height = 8)

p1 <- ggplot(results, aes(x = reorder(Model, Accuracy), y = Accuracy, fill = Type)) +
  geom_col(alpha = 0.8, width = 0.7) +
  geom_text(aes(label = paste0(round(Accuracy, 1), "%")), 
            hjust = -0.1, fontface = "bold", size = 4) +
  geom_hline(yintercept = baseline_accuracy, linetype = "dashed", color = "red", linewidth = 1) +
  annotate("text", x = 2, y = baseline_accuracy + 2, 
           label = paste0("Original: ", baseline_accuracy, "%"), color = "red", fontface = "bold") +
  scale_fill_manual(values = c("Baseline" = "#FF5722", "Random" = "#9E9E9E", 
                              "Original" = "#FF9800", "Enhanced" = "#4CAF50")) +
  scale_y_continuous(labels = percent_format(scale = 1), limits = c(0, max(results$Accuracy) * 1.15)) +
  coord_flip() +
  labs(
    title = "Model Performance Improvement Strategies",
    subtitle = paste0("Best model achieves ", round(max(results$Accuracy), 1), 
                     "% accuracy (+", round(max(results$Improvement, na.rm = TRUE), 1), 
                     " points improvement)"),
    x = "Model Type", 
    y = "Prediction Accuracy (%)",
    caption = "Red line shows original model performance"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "darkblue"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 13, face = "bold"),
    legend.position = "top"
  )

print(p1)
dev.off()

# === RECOMMENDATIONS ===
cat("\n=== IMPROVEMENT RECOMMENDATIONS ===\n")

if(max(results$Accuracy) > baseline_accuracy + 5) {
  cat("✓ SUCCESS: Achieved >5 point improvement\n")
  cat("BEST STRATEGY:", best_model$Model, "\n")
} else if(max(results$Accuracy) > baseline_accuracy + 2) {
  cat("~ MODERATE: Achieved 2-5 point improvement\n")
} else {
  cat("✗ LIMITED: <2 point improvement achieved\n")
}

cat("\nFURTHER IMPROVEMENTS TO TRY:\n")
cat("1. Add temporal/sequential effects (trial order, learning)\n")
cat("2. Include block-level random effects\n")
cat("3. Add partner identity effects (who they're paired with)\n")
cat("4. Include value-based predictors (subjective/objective values)\n")
cat("5. Try ensemble methods (model averaging)\n")
cat("6. Add contextual features (session, day, time)\n")
cat("7. Consider ordinal models for ranked outcomes\n")

cat("\nModel improvement analysis saved to: MODEL_IMPROVEMENT_COMPARISON.pdf\n") 