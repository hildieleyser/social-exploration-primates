# QUICK DEMO: AIC vs LOO ELPD MODEL COMPARISON
# Demonstration of model selection metrics

library(ggplot2)
library(dplyr)
library(scales)

cat("=== QUICK MODEL METRICS DEMONSTRATION ===\n")

# Simulate realistic model comparison results based on typical Bayesian model performance
model_results <- data.frame(
  Model = c("Baseline", "Interactions", "Personality", "Non-linear", "Comprehensive"),
  
  # AIC values (lower is better, so we'll show advantage = baseline_aic - model_aic)
  AIC_Advantage = c(0, 5.2, 3.8, 2.1, 8.7),
  
  # LOO ELPD advantages (higher is better)
  ELPD_Advantage = c(0, 2.3, 1.7, 0.9, 4.1),
  
  # Model accuracy (%)
  Accuracy = c(46.9, 48.2, 47.6, 47.1, 49.3),
  
  # Model complexity (subjective 1-5 scale)
  Complexity = c(1, 3, 2, 2, 5),
  
  # Model type
  Type = c("Reference", "Enhanced", "Enhanced", "Enhanced", "Enhanced")
)

# Calculate improvement metrics
model_results <- model_results %>%
  mutate(
    Accuracy_Improvement = Accuracy - 46.9,
    Model_Factor = factor(Model, levels = Model),
    Performance_Score = (AIC_Advantage + ELPD_Advantage + Accuracy_Improvement) / 3
  )

cat("Model comparison results:\n")
print(model_results[c("Model", "AIC_Advantage", "ELPD_Advantage", "Accuracy")])

# === VISUALIZATION ===
pdf("QUICK_MODEL_METRICS_DEMO.pdf", width = 16, height = 10)

# Plot 1: AIC vs LOO ELPD Advantage Scatter
p1 <- ggplot(model_results, aes(x = AIC_Advantage, y = ELPD_Advantage, 
                               color = Type, size = Accuracy)) +
  geom_point(alpha = 0.8) +
  geom_text(aes(label = Model), vjust = -1.5, fontface = "bold", size = 4) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray", linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray", linewidth = 1) +
  annotate("text", x = 6, y = 3.5, 
           label = "Better Prediction\n& Better Fit", 
           fontface = "bold", color = "darkgreen", size = 5) +
  annotate("text", x = 1, y = 0.3, 
           label = "Baseline\nPerformance", 
           fontface = "bold", color = "red", size = 4) +
  scale_color_manual(values = c("Reference" = "#FF9800", "Enhanced" = "#4CAF50")) +
  scale_size_continuous(range = c(8, 15), 
                       labels = function(x) paste0(round(x, 1), "%")) +
  labs(
    title = "Model Selection: AIC vs LOO ELPD Trade-offs",
    subtitle = "Upper right quadrant = models that improve both fit and prediction",
    x = "AIC Advantage (Better Model Fit →)",
    y = "LOO ELPD Advantage (Better Prediction →)",
    color = "Model Type",
    size = "Accuracy (%)",
    caption = "Baseline at origin (0,0). Distance from origin = overall improvement."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14, color = "darkblue"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.position = "top"
  )

# Plot 2: Metrics Comparison Bars
metrics_long <- model_results %>%
  select(Model, AIC_Advantage, ELPD_Advantage, Accuracy_Improvement) %>%
  reshape2::melt(id.vars = "Model", variable.name = "Metric", value.name = "Value")

p2 <- ggplot(metrics_long, aes(x = Model, y = Value, fill = Metric)) +
  geom_col(position = "dodge", alpha = 0.8, width = 0.7) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.5) +
  geom_text(aes(label = round(Value, 1)), 
            position = position_dodge(0.7), 
            vjust = ifelse(metrics_long$Value >= 0, -0.3, 1.3), 
            fontface = "bold", size = 3.5) +
  scale_fill_manual(values = c("AIC_Advantage" = "#2196F3", 
                              "ELPD_Advantage" = "#E91E63",
                              "Accuracy_Improvement" = "#4CAF50"),
                   labels = c("AIC Advantage", "ELPD Advantage", "Accuracy Gain (%)")) +
  labs(
    title = "Model Performance Metrics: Advantages Over Baseline",
    subtitle = "Higher bars = better performance. All metrics positive = improvement.",
    x = "Model Strategy", 
    y = "Improvement Score",
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

# Plot 3: Complexity vs Performance Trade-off
p3 <- ggplot(model_results, aes(x = Complexity, y = Performance_Score, 
                               color = Type, size = ELPD_Advantage)) +
  geom_point(alpha = 0.8) +
  geom_text(aes(label = Model), vjust = -1.2, fontface = "bold", size = 4) +
  geom_smooth(method = "lm", se = TRUE, color = "darkgray", linewidth = 1) +
  scale_color_manual(values = c("Reference" = "#FF9800", "Enhanced" = "#4CAF50")) +
  scale_size_continuous(range = c(6, 12)) +
  scale_x_continuous(breaks = 1:5, 
                    labels = c("Simple", "Complex", "Medium", "Medium", "Very Complex")) +
  labs(
    title = "Model Complexity vs Overall Performance",
    subtitle = "Does adding complexity improve model performance?",
    x = "Model Complexity Level", 
    y = "Overall Performance Score",
    color = "Model Type",
    size = "ELPD Advantage",
    caption = "Performance Score = Average of AIC, ELPD, and Accuracy advantages"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "darkblue"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 13, face = "bold"),
    legend.position = "top"
  )

# Plot 4: Mathematical Enhancement Summary
enhancement_types <- data.frame(
  Enhancement = c("Interaction\nTerms", "Personality\nScores", "Non-linear\nEffects", "Comprehensive\nModel"),
  Mathematical_Form = c("A × B", "Individual\nTraits", "Poly(x,2)", "Combined\nApproach"),
  AIC_Gain = c(5.2, 3.8, 2.1, 8.7),
  ELPD_Gain = c(2.3, 1.7, 0.9, 4.1),
  Type = c("Interaction", "Individual", "Non-linear", "Combined")
)

p4 <- ggplot(enhancement_types, aes(x = AIC_Gain, y = ELPD_Gain, color = Type)) +
  geom_point(size = 8, alpha = 0.8) +
  geom_text(aes(label = Enhancement), color = "white", fontface = "bold", size = 3) +
  scale_color_manual(values = c("Interaction" = "#2196F3", "Individual" = "#FF9800", 
                               "Non-linear" = "#E91E63", "Combined" = "#4CAF50")) +
  labs(
    title = "Mathematical Enhancement Effectiveness",
    subtitle = "Which mathematical improvements provide the best gains?",
    x = "AIC Advantage",
    y = "LOO ELPD Advantage",
    color = "Enhancement Type"
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
gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)

dev.off()

# === SUMMARY ===
cat("\n=== MODEL SELECTION INSIGHTS ===\n")

best_overall <- model_results[which.max(model_results$Performance_Score), ]
cat("BEST OVERALL MODEL:", best_overall$Model, "\n")
cat("• AIC Advantage:", round(best_overall$AIC_Advantage, 1), "\n")
cat("• ELPD Advantage:", round(best_overall$ELPD_Advantage, 1), "\n")
cat("• Accuracy:", round(best_overall$Accuracy, 1), "%\n")
cat("• Improvement:", round(best_overall$Accuracy_Improvement, 1), "percentage points\n")

cat("\nKEY MATHEMATICAL INSIGHTS:\n")
cat("1. INTERACTION EFFECTS: Capture emergent social dynamics (A × B terms)\n")
cat("2. PERSONALITY SCORES: Individual behavioral signatures beyond random effects\n") 
cat("3. NON-LINEAR TERMS: Polynomial social complexity effects (quadratic stress)\n")
cat("4. COMPREHENSIVE MODEL: Combines all enhancements for maximum performance\n")

cat("\nMODEL SELECTION TRADE-OFFS:\n")
complexity_performance_cor <- cor(model_results$Complexity, model_results$Performance_Score)
cat("• Complexity-Performance Correlation:", round(complexity_performance_cor, 3), "\n")
if(complexity_performance_cor > 0.5) {
  cat("• FINDING: More complex models generally perform better\n")
} else {
  cat("• FINDING: Complexity doesn't guarantee better performance\n")
}

cat("\nVisualization saved to: QUICK_MODEL_METRICS_DEMO.pdf\n") 