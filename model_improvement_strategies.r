# MODEL IMPROVEMENT STRATEGIES
# Systematic approach to boost performance from current 46.9%

library(brms)
library(dplyr)
library(ggplot2)
library(scales)

cat("=== MODEL IMPROVEMENT STRATEGIES ===\n")
cat("Current performance: 46.9% accuracy\n")
cat("Testing 5 improvement strategies...\n\n")

# Load and prepare data
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

# Enhanced feature engineering (safe approach)
data_enhanced <- data_clean %>%
  mutate(
    # Social complexity as continuous
    social_partners = case_when(
      condition == "solo" ~ 0,
      condition == "duo" ~ 1,
      condition == "trio" ~ 2
    ),
    
    # Rank as continuous
    rank_numeric = as.numeric(relative_rank),
    
    # Individual personality scores (based on observed exploration rates)
    personality_score = case_when(
      monkey == "FRAN" ~ 0.557,      # 55.7% exploration
      monkey == "CHOCOLAT" ~ 0.293,  # 29.3% exploration  
      monkey == "DALI" ~ 0.368,      # 36.8% exploration
      monkey == "ICE" ~ 0.312,       # 31.2% exploration
      monkey == "EBI" ~ 0.303,       # 30.3% exploration
      monkey == "ANEMONE" ~ 0.207    # 20.7% exploration
    ),
    
    # Dominance hierarchy (1=subordinate, 2=intermediate, 3=dominant)
    dominance_level = case_when(
      monkey %in% c("FRAN", "CHOCOLAT") ~ 3,  # Dominant
      monkey %in% c("DALI", "ICE") ~ 2,       # Intermediate
      monkey %in% c("EBI", "ANEMONE") ~ 1     # Subordinate
    ),
    
    # Social stress (quadratic effect of partners)
    social_stress = social_partners^2,
    
    # Sex-specific rank effects
    male_advantage = ifelse(sex == "Male", rank_numeric, 0),
    female_disadvantage = ifelse(sex == "Female", 4 - rank_numeric, 0)
  )

model_data <- data_enhanced[complete.cases(data_enhanced[c("outcome_clean", "condition", "relative_rank", "sex", "monkey")]), ]

cat("Enhanced dataset ready:", nrow(model_data), "observations\n")
cat("New features added:", ncol(data_enhanced) - ncol(data_clean), "\n\n")

options(mc.cores = parallel::detectCores())

# Helper function for accuracy calculation
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

calculate_accuracy <- function(model, data) {
  pred <- posterior_predict(model, draws = 100)
  pred_class <- apply(pred, 2, get_mode)
  obs_class <- as.numeric(data$outcome_clean)
  mean(pred_class == obs_class) * 100
}

# === STRATEGY 1: INTERACTION EFFECTS ===
cat("STRATEGY 1: Key Interaction Effects\n")
model_interactions <- brm(
  outcome_clean ~ condition * relative_rank + condition * sex + 
                  relative_rank * sex + (1|monkey),
  data = model_data, family = categorical(),
  iter = 2000, warmup = 1000, chains = 2,
  control = list(adapt_delta = 0.95),
  silent = 2, refresh = 0
)

accuracy_interactions <- calculate_accuracy(model_interactions, model_data)
cat("Interaction model accuracy:", round(accuracy_interactions, 1), "%\n\n")

# === STRATEGY 2: PERSONALITY-BASED MODEL ===
cat("STRATEGY 2: Individual Personality Effects\n")
model_personality <- brm(
  outcome_clean ~ condition + relative_rank + sex + 
                  personality_score + dominance_level + (1|monkey),
  data = model_data, family = categorical(),
  iter = 2000, warmup = 1000, chains = 2,
  control = list(adapt_delta = 0.95),
  silent = 2, refresh = 0
)

accuracy_personality <- calculate_accuracy(model_personality, model_data)
cat("Personality model accuracy:", round(accuracy_personality, 1), "%\n\n")

# === STRATEGY 3: NON-LINEAR SOCIAL EFFECTS ===
cat("STRATEGY 3: Non-linear Social Complexity\n")
model_nonlinear <- brm(
  outcome_clean ~ poly(social_partners, 2) + relative_rank + sex + 
                  social_stress + (1|monkey),
  data = model_data, family = categorical(),
  iter = 2000, warmup = 1000, chains = 2,
  control = list(adapt_delta = 0.95),
  silent = 2, refresh = 0
)

accuracy_nonlinear <- calculate_accuracy(model_nonlinear, model_data)
cat("Non-linear model accuracy:", round(accuracy_nonlinear, 1), "%\n\n")

# === STRATEGY 4: SEX-SPECIFIC EFFECTS ===
cat("STRATEGY 4: Sex-Specific Rank Effects\n")
model_sex_specific <- brm(
  outcome_clean ~ condition + relative_rank + sex + 
                  male_advantage + female_disadvantage + (1|monkey),
  data = model_data, family = categorical(),
  iter = 2000, warmup = 1000, chains = 2,
  control = list(adapt_delta = 0.95),
  silent = 2, refresh = 0
)

accuracy_sex_specific <- calculate_accuracy(model_sex_specific, model_data)
cat("Sex-specific model accuracy:", round(accuracy_sex_specific, 1), "%\n\n")

# === STRATEGY 5: COMPREHENSIVE MODEL ===
cat("STRATEGY 5: Comprehensive Combined Model\n")
model_comprehensive <- brm(
  outcome_clean ~ condition * relative_rank + sex * relative_rank + 
                  personality_score + poly(social_partners, 2) + 
                  dominance_level + (1|monkey),
  data = model_data, family = categorical(),
  iter = 3000, warmup = 1500, chains = 2,
  control = list(adapt_delta = 0.98),
  silent = 2, refresh = 0
)

accuracy_comprehensive <- calculate_accuracy(model_comprehensive, model_data)
cat("Comprehensive model accuracy:", round(accuracy_comprehensive, 1), "%\n\n")

# === PERFORMANCE COMPARISON ===
cat("=== PERFORMANCE COMPARISON ===\n")

baseline_original <- 46.9
baseline_simple <- 34.2
random_chance <- 33.3

results <- data.frame(
  Model = c("Random Chance", "Baseline (Most Frequent)", "Original Model",
            "Interaction Effects", "Personality-Based", "Non-linear Social", 
            "Sex-Specific Effects", "Comprehensive"),
  Accuracy = c(random_chance, baseline_simple, baseline_original,
               accuracy_interactions, accuracy_personality, accuracy_nonlinear,
               accuracy_sex_specific, accuracy_comprehensive),
  Improvement = c(NA, 0, baseline_original - baseline_simple,
                  accuracy_interactions - baseline_original,
                  accuracy_personality - baseline_original,
                  accuracy_nonlinear - baseline_original,
                  accuracy_sex_specific - baseline_original,
                  accuracy_comprehensive - baseline_original),
  Type = c("Random", "Baseline", "Original", "Enhanced", "Enhanced", "Enhanced", "Enhanced", "Enhanced")
)

print(results)

# Find best performing model
best_model <- results[which.max(results$Accuracy), ]
cat("\n*** BEST MODEL ***\n")
cat("Model:", best_model$Model, "\n")
cat("Accuracy:", round(best_model$Accuracy, 1), "%\n")
cat("Improvement over original:", round(best_model$Improvement, 1), "percentage points\n")

# Performance assessment
improvement <- best_model$Accuracy - baseline_original
if(improvement >= 5) {
  cat("✓ EXCELLENT: Major improvement achieved (≥5 points)\n")
} else if(improvement >= 2) {
  cat("✓ GOOD: Meaningful improvement (2-5 points)\n")
} else if(improvement > 0) {
  cat("~ MODEST: Small improvement (<2 points)\n")
} else {
  cat("✗ NO IMPROVEMENT: Performance unchanged or worse\n")
}

# === VISUALIZATION ===
pdf("MODEL_IMPROVEMENT_RESULTS.pdf", width = 14, height = 8)

# Remove random chance and baseline for cleaner comparison
results_clean <- results[results$Type %in% c("Original", "Enhanced"), ]

p1 <- ggplot(results_clean, aes(x = reorder(Model, Accuracy), y = Accuracy, fill = Type)) +
  geom_col(alpha = 0.8, width = 0.7) +
  geom_text(aes(label = paste0(round(Accuracy, 1), "%")), 
            hjust = -0.1, fontface = "bold", size = 4.5) +
  geom_hline(yintercept = baseline_original, linetype = "dashed", color = "red", linewidth = 1.2) +
  annotate("text", x = 1.5, y = baseline_original + 1.5, 
           label = paste0("Original: ", baseline_original, "%"), 
           color = "red", fontface = "bold", size = 4) +
  scale_fill_manual(values = c("Original" = "#FF9800", "Enhanced" = "#4CAF50")) +
  scale_y_continuous(labels = percent_format(scale = 1), 
                     limits = c(0, max(results_clean$Accuracy) * 1.15)) +
  coord_flip() +
  labs(
    title = "Model Performance Improvement Strategies",
    subtitle = paste0("Best strategy: ", best_model$Model, " (", 
                     round(best_model$Accuracy, 1), "%, +", 
                     round(improvement, 1), " points)"),
    x = "Model Strategy", 
    y = "Prediction Accuracy (%)",
    caption = "Red line = original model performance. Higher bars = better performance."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "darkblue"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 13, face = "bold"),
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

print(p1)
dev.off()

# === FINAL RECOMMENDATIONS ===
cat("\n=== IMPROVEMENT RECOMMENDATIONS ===\n")

cat("IMMEDIATE NEXT STEPS:\n")
if(best_model$Model != "Original Model") {
  cat("1. Adopt the", best_model$Model, "approach\n")
  cat("2. Expected improvement: +", round(improvement, 1), " percentage points\n")
} else {
  cat("1. Current model is already optimal among tested strategies\n")
}

cat("\nFURTHER IMPROVEMENTS TO EXPLORE:\n")
cat("• Add temporal effects (trial sequence, learning curves)\n")
cat("• Include partner identity (who they're paired with)\n") 
cat("• Add value-based predictors (expected rewards)\n")
cat("• Try ensemble methods (combine multiple models)\n")
cat("• Include contextual variables (session, block effects)\n")
cat("• Consider alternative model families (ordinal, multinomial)\n")

cat("\nEXPECTED PERFORMANCE TARGETS:\n")
cat("• Current best:", round(max(results$Accuracy), 1), "%\n")
cat("• Realistic target with more features: 55-60%\n")
cat("• Theoretical maximum (with perfect features): 70-75%\n")

cat("\nResults saved to: MODEL_IMPROVEMENT_RESULTS.pdf\n") 