# MODEL COMPARISON: AIC AND LOO ELPD METRICS
# Comprehensive model selection visualization

library(brms)
library(dplyr)
library(ggplot2)
library(scales)
library(gridExtra)

cat("=== MODEL COMPARISON: AIC vs LOO ELPD ===\n")

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

# Enhanced feature engineering
data_enhanced <- data_clean %>%
  mutate(
    social_partners = case_when(
      condition == "solo" ~ 0,
      condition == "duo" ~ 1,
      condition == "trio" ~ 2
    ),
    rank_numeric = as.numeric(relative_rank),
    personality_score = case_when(
      monkey == "FRAN" ~ 0.557,      
      monkey == "CHOCOLAT" ~ 0.293,  
      monkey == "DALI" ~ 0.368,      
      monkey == "ICE" ~ 0.312,       
      monkey == "EBI" ~ 0.303,       
      monkey == "ANEMONE" ~ 0.207    
    ),
    dominance_level = case_when(
      monkey %in% c("FRAN", "CHOCOLAT") ~ 3,  
      monkey %in% c("DALI", "ICE") ~ 2,       
      monkey %in% c("EBI", "ANEMONE") ~ 1     
    ),
    social_stress = social_partners^2
  )

model_data <- data_enhanced[complete.cases(data_enhanced[c("outcome_clean", "condition", "relative_rank", "sex", "monkey")]), ]

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

# Store models and their metrics
models <- list()
model_metrics <- data.frame()

cat("Fitting models and calculating metrics...\n")

# === MODEL 1: BASELINE ===
cat("1. Baseline Model...\n")
models$baseline <- brm(
  outcome_clean ~ condition + relative_rank + sex + (1|monkey),
  data = model_data, family = categorical(),
  iter = 2000, warmup = 1000, chains = 2,
  control = list(adapt_delta = 0.95),
  silent = 2, refresh = 0
)

loo_baseline <- loo(models$baseline)
aic_baseline <- AIC(models$baseline)
accuracy_baseline <- calculate_accuracy(models$baseline, model_data)

model_metrics <- rbind(model_metrics, data.frame(
  Model = "Baseline",
  AIC = aic_baseline,
  LOO_ELPD = loo_baseline$estimates["elpd_loo", "Estimate"],
  LOO_SE = loo_baseline$estimates["elpd_loo", "SE"],
  Accuracy = accuracy_baseline,
  Type = "Reference"
))

# === MODEL 2: INTERACTIONS ===
cat("2. Interaction Model...\n")
models$interactions <- brm(
  outcome_clean ~ condition * relative_rank + condition * sex + 
                  relative_rank * sex + (1|monkey),
  data = model_data, family = categorical(),
  iter = 2000, warmup = 1000, chains = 2,
  control = list(adapt_delta = 0.95),
  silent = 2, refresh = 0
)

loo_interactions <- loo(models$interactions)
aic_interactions <- AIC(models$interactions)
accuracy_interactions <- calculate_accuracy(models$interactions, model_data)

model_metrics <- rbind(model_metrics, data.frame(
  Model = "Interactions",
  AIC = aic_interactions,
  LOO_ELPD = loo_interactions$estimates["elpd_loo", "Estimate"],
  LOO_SE = loo_interactions$estimates["elpd_loo", "SE"],
  Accuracy = accuracy_interactions,
  Type = "Enhanced"
))

# === MODEL 3: PERSONALITY ===
cat("3. Personality Model...\n")
models$personality <- brm(
  outcome_clean ~ condition + relative_rank + sex + 
                  personality_score + dominance_level + (1|monkey),
  data = model_data, family = categorical(),
  iter = 2000, warmup = 1000, chains = 2,
  control = list(adapt_delta = 0.95),
  silent = 2, refresh = 0
)

loo_personality <- loo(models$personality)
aic_personality <- AIC(models$personality)
accuracy_personality <- calculate_accuracy(models$personality, model_data)

model_metrics <- rbind(model_metrics, data.frame(
  Model = "Personality",
  AIC = aic_personality,
  LOO_ELPD = loo_personality$estimates["elpd_loo", "Estimate"],
  LOO_SE = loo_personality$estimates["elpd_loo", "SE"],
  Accuracy = accuracy_personality,
  Type = "Enhanced"
))

# === MODEL 4: NON-LINEAR ===
cat("4. Non-linear Model...\n")
models$nonlinear <- brm(
  outcome_clean ~ poly(social_partners, 2) + relative_rank + sex + 
                  social_stress + (1|monkey),
  data = model_data, family = categorical(),
  iter = 2000, warmup = 1000, chains = 2,
  control = list(adapt_delta = 0.95),
  silent = 2, refresh = 0
)

loo_nonlinear <- loo(models$nonlinear)
aic_nonlinear <- AIC(models$nonlinear)
accuracy_nonlinear <- calculate_accuracy(models$nonlinear, model_data)

model_metrics <- rbind(model_metrics, data.frame(
  Model = "Non-linear",
  AIC = aic_nonlinear,
  LOO_ELPD = loo_nonlinear$estimates["elpd_loo", "Estimate"],
  LOO_SE = loo_nonlinear$estimates["elpd_loo", "SE"],
  Accuracy = accuracy_nonlinear,
  Type = "Enhanced"
))

# === MODEL 5: COMPREHENSIVE ===
cat("5. Comprehensive Model...\n")
models$comprehensive <- brm(
  outcome_clean ~ condition * relative_rank + sex * relative_rank + 
                  personality_score + poly(social_partners, 2) + 
                  dominance_level + (1|monkey),
  data = model_data, family = categorical(),
  iter = 2500, warmup = 1250, chains = 2,
  control = list(adapt_delta = 0.98),
  silent = 2, refresh = 0
)

loo_comprehensive <- loo(models$comprehensive)
aic_comprehensive <- AIC(models$comprehensive)
accuracy_comprehensive <- calculate_accuracy(models$comprehensive, model_data)

model_metrics <- rbind(model_metrics, data.frame(
  Model = "Comprehensive",
  AIC = aic_comprehensive,
  LOO_ELPD = loo_comprehensive$estimates["elpd_loo", "Estimate"],
  LOO_SE = loo_comprehensive$estimates["elpd_loo", "SE"],
  Accuracy = accuracy_comprehensive,
  Type = "Enhanced"
))

# Calculate relative metrics (differences from baseline)
baseline_aic <- model_metrics$AIC[model_metrics$Model == "Baseline"]
baseline_elpd <- model_metrics$LOO_ELPD[model_metrics$Model == "Baseline"]

model_metrics <- model_metrics %>%
  mutate(
    AIC_Advantage = baseline_aic - AIC,  # Positive = better than baseline
    ELPD_Advantage = LOO_ELPD - baseline_elpd,  # Positive = better than baseline
    Model_Factor = factor(Model, levels = c("Baseline", "Interactions", "Personality", "Non-linear", "Comprehensive"))
  )

cat("\n=== MODEL COMPARISON RESULTS ===\n")
print(model_metrics[c("Model", "AIC_Advantage", "ELPD_Advantage", "Accuracy")])

# === VISUALIZATION ===
pdf("MODEL_COMPARISON_METRICS.pdf", width = 16, height = 10)

# Plot 1: AIC vs LOO ELPD Advantage
p1 <- ggplot(model_metrics, aes(x = AIC_Advantage, y = ELPD_Advantage, color = Type, size = Accuracy)) +
  geom_point(alpha = 0.8) +
  geom_text(aes(label = Model), vjust = -1.2, fontface = "bold", size = 4) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray", linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray", linewidth = 1) +
  annotate("text", x = max(model_metrics$AIC_Advantage) * 0.7, y = max(model_metrics$ELPD_Advantage) * 0.9,
           label = "Better LOO\nBetter AIC", fontface = "bold", color = "darkgreen", size = 4) +
  annotate("text", x = min(model_metrics$AIC_Advantage) * 0.7, y = min(model_metrics$ELPD_Advantage) * 0.9,
           label = "Worse LOO\nWorse AIC", fontface = "bold", color = "darkred", size = 4) +
  scale_color_manual(values = c("Reference" = "#FF9800", "Enhanced" = "#4CAF50")) +
  scale_size_continuous(range = c(8, 15), labels = function(x) paste0(round(x, 1), "%")) +
  labs(
    title = "Model Comparison: AIC vs LOO ELPD Advantages",
    subtitle = "Position relative to baseline model (intersection at origin)",
    x = "AIC Advantage (Higher = Better Fit)",
    y = "LOO ELPD Advantage (Higher = Better Prediction)",
    color = "Model Type",
    size = "Accuracy (%)",
    caption = "Dashed lines = baseline performance. Upper right quadrant = best models."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14, color = "darkblue"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    legend.position = "top"
  )

# Plot 2: Model Performance Bars
metrics_long <- reshape2::melt(model_metrics[c("Model", "AIC_Advantage", "ELPD_Advantage", "Accuracy")], 
                              id.vars = c("Model", "Accuracy"), 
                              variable.name = "Metric", value.name = "Value")

p2 <- ggplot(metrics_long, aes(x = Model, y = Value, fill = Metric)) +
  geom_col(position = "dodge", alpha = 0.8, width = 0.7) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.5) +
  geom_text(aes(label = round(Value, 1)), 
            position = position_dodge(0.7), vjust = ifelse(metrics_long$Value >= 0, -0.3, 1.3), 
            fontface = "bold", size = 3.5) +
  scale_fill_manual(values = c("AIC_Advantage" = "#2196F3", "ELPD_Advantage" = "#E91E63"),
                   labels = c("AIC Advantage", "ELPD Advantage")) +
  labs(
    title = "Model Performance Metrics Comparison",
    subtitle = "Higher values indicate better performance relative to baseline",
    x = "Model Strategy", 
    y = "Advantage Score",
    fill = "Metric Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "darkblue"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 13, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

# Plot 3: Accuracy vs Complexity Trade-off
complexity_scores <- c(1, 3, 2, 2, 5)  # Subjective complexity ranking
model_metrics$Complexity <- complexity_scores

p3 <- ggplot(model_metrics, aes(x = Complexity, y = Accuracy, color = Type, size = ELPD_Advantage)) +
  geom_point(alpha = 0.8) +
  geom_text(aes(label = Model), vjust = -1.2, fontface = "bold", size = 4) +
  geom_smooth(method = "lm", se = TRUE, color = "gray", linewidth = 1) +
  scale_color_manual(values = c("Reference" = "#FF9800", "Enhanced" = "#4CAF50")) +
  scale_size_continuous(range = c(6, 12)) +
  scale_x_continuous(breaks = 1:5, labels = c("Simple", "Complex", "Medium", "Medium", "Very Complex")) +
  labs(
    title = "Accuracy vs Model Complexity Trade-off",
    subtitle = "Do more complex models achieve better performance?",
    x = "Model Complexity", 
    y = "Prediction Accuracy (%)",
    color = "Model Type",
    size = "ELPD Advantage"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "darkblue"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 13, face = "bold"),
    legend.position = "top"
  )

# Arrange all plots
grid.arrange(p1, p2, p3, ncol = 2, nrow = 2,
            top = textGrob("Comprehensive Model Selection: AIC, LOO ELPD, and Performance Analysis", 
                          gp = gpar(fontsize = 18, fontface = "bold")),
            heights = c(1, 0.7))

dev.off()

# === SUMMARY STATISTICS ===
cat("\n=== MODEL SELECTION SUMMARY ===\n")

best_aic <- model_metrics[which.max(model_metrics$AIC_Advantage), ]
best_elpd <- model_metrics[which.max(model_metrics$ELPD_Advantage), ]
best_accuracy <- model_metrics[which.max(model_metrics$Accuracy), ]

cat("BEST BY AIC:", best_aic$Model, "(Advantage:", round(best_aic$AIC_Advantage, 1), ")\n")
cat("BEST BY LOO ELPD:", best_elpd$Model, "(Advantage:", round(best_elpd$ELPD_Advantage, 1), ")\n")
cat("BEST BY ACCURACY:", best_accuracy$Model, "(", round(best_accuracy$Accuracy, 1), "%)\n")

# Overall recommendation
overall_rank <- model_metrics %>%
  mutate(
    AIC_Rank = rank(-AIC_Advantage),
    ELPD_Rank = rank(-ELPD_Advantage),
    Accuracy_Rank = rank(-Accuracy),
    Overall_Score = (AIC_Rank + ELPD_Rank + Accuracy_Rank) / 3
  ) %>%
  arrange(Overall_Score)

best_overall <- overall_rank[1, ]
cat("\nOVERALL BEST MODEL:", best_overall$Model, "\n")
cat("• AIC Advantage:", round(best_overall$AIC_Advantage, 1), "\n")
cat("• ELPD Advantage:", round(best_overall$ELPD_Advantage, 1), "\n") 
cat("• Accuracy:", round(best_overall$Accuracy, 1), "%\n")

cat("\nVisualization saved to: MODEL_COMPARISON_METRICS.pdf\n") 