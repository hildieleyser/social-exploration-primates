# ABSOLUTE VS RELATIVE RANK MODEL COMPARISON
# Compare how absolute vs relative rank models perform and their effect patterns

library(brms)
library(dplyr)
library(ggplot2)
library(scales)
library(gridExtra)

cat("=== ABSOLUTE VS RELATIVE RANK COMPARISON ===\n")

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

# Create both rank systems
data_enhanced <- data_clean %>%
  mutate(
    # RELATIVE RANK (context-dependent, 1-3 within each group)
    relative_rank = factor(RELATIVE_RANK, levels = c(1, 2, 3)),
    
    # ABSOLUTE RANK (fixed hierarchy across all contexts, 1-6)
    absolute_rank = case_when(
      monkey == "FRAN" ~ 1,      # Highest overall explorer
      monkey == "CHOCOLAT" ~ 2,  # Second highest  
      monkey == "DALI" ~ 3,      # Third
      monkey == "ICE" ~ 4,       # Fourth
      monkey == "EBI" ~ 5,       # Fifth
      monkey == "ANEMONE" ~ 6    # Lowest explorer
    ),
    absolute_rank = factor(absolute_rank, levels = 1:6)
  )

model_data <- data_enhanced[complete.cases(data_enhanced[c("outcome_clean", "condition", "relative_rank", "sex", "monkey")]), ]

cat("Data prepared with both ranking systems\n")
cat("Relative rank levels:", levels(model_data$relative_rank), "\n")
cat("Absolute rank levels:", levels(model_data$absolute_rank), "\n\n")

options(mc.cores = parallel::detectCores())

# Helper functions
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

# === MODEL 1: RELATIVE RANK ===
cat("Fitting RELATIVE RANK model...\n")
model_relative <- brm(
  outcome_clean ~ condition + relative_rank + sex + (1|monkey),
  data = model_data, family = categorical(),
  iter = 2000, warmup = 1000, chains = 2,
  control = list(adapt_delta = 0.95),
  silent = 2, refresh = 0
)

accuracy_relative <- calculate_accuracy(model_relative, model_data)
cat("Relative rank model accuracy:", round(accuracy_relative, 1), "%\n")

# === MODEL 2: ABSOLUTE RANK ===
cat("Fitting ABSOLUTE RANK model...\n")
model_absolute <- brm(
  outcome_clean ~ condition + absolute_rank + sex + (1|monkey),
  data = model_data, family = categorical(),
  iter = 2000, warmup = 1000, chains = 2,
  control = list(adapt_delta = 0.95),
  silent = 2, refresh = 0
)

accuracy_absolute <- calculate_accuracy(model_absolute, model_data)
cat("Absolute rank model accuracy:", round(accuracy_absolute, 1), "%\n\n")

# === MODEL COMPARISON ===
cat("=== MODEL COMPARISON METRICS ===\n")

# Calculate LOO for model comparison
loo_relative <- loo(model_relative)
loo_absolute <- loo(model_absolute)
loo_compare <- loo_compare(loo_relative, loo_absolute)

cat("LOO Comparison:\n")
print(loo_compare)

# Extract effect sizes from both models
posterior_relative <- as.data.frame(model_relative)
posterior_absolute <- as.data.frame(model_absolute)

# Relative rank effects
rel_rank2_effect <- mean(posterior_relative$b_muexplore_relative_rank2)
rel_rank3_effect <- mean(posterior_relative$b_muexplore_relative_rank3)

# Absolute rank effects (comparing each level to rank 1)
abs_rank2_effect <- mean(posterior_absolute$b_muexplore_absolute_rank2)
abs_rank3_effect <- mean(posterior_absolute$b_muexplore_absolute_rank3)
abs_rank4_effect <- mean(posterior_absolute$b_muexplore_absolute_rank4)
abs_rank5_effect <- mean(posterior_absolute$b_muexplore_absolute_rank5)
abs_rank6_effect <- mean(posterior_absolute$b_muexplore_absolute_rank6)

# === VISUALIZATION ===
pdf("ABSOLUTE_VS_RELATIVE_RANK_COMPARISON.pdf", width = 16, height = 10)

# Plot 1: Model Performance Comparison
performance_data <- data.frame(
  Model = c("Relative Rank", "Absolute Rank", "Baseline"),
  Accuracy = c(accuracy_relative, accuracy_absolute, 34.2),
  ELPD = c(loo_relative$estimates["elpd_loo", "Estimate"],
           loo_absolute$estimates["elpd_loo", "Estimate"],
           NA),
  Type = c("Relative", "Absolute", "Baseline")
)

p1 <- ggplot(performance_data[1:2, ], aes(x = Model, y = Accuracy, fill = Type)) +
  geom_col(alpha = 0.8, width = 0.6) +
  geom_text(aes(label = paste0(round(Accuracy, 1), "%")), 
            vjust = -0.3, fontface = "bold", size = 6) +
  scale_fill_manual(values = c("Relative" = "#4CAF50", "Absolute" = "#FF9800")) +
  scale_y_continuous(labels = percent_format(scale = 1), 
                     limits = c(0, max(performance_data$Accuracy[1:2]) * 1.15)) +
  labs(
    title = "Model Performance: Relative vs Absolute Rank",
    subtitle = paste0("Winner: ", ifelse(accuracy_relative > accuracy_absolute, "Relative", "Absolute"), 
                     " rank (+", round(abs(accuracy_relative - accuracy_absolute), 1), " points)"),
    x = "Rank Model Type", 
    y = "Prediction Accuracy (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "darkblue"),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 14, face = "bold"),
    legend.position = "none"
  )

# Plot 2: Effect Size Comparison
effect_data <- data.frame(
  Rank_Position = c("Rank 2 vs 1", "Rank 3 vs 1", "Rank 4 vs 1", "Rank 5 vs 1", "Rank 6 vs 1"),
  Relative_Model = c(rel_rank2_effect, rel_rank3_effect, NA, NA, NA),
  Absolute_Model = c(abs_rank2_effect, abs_rank3_effect, abs_rank4_effect, abs_rank5_effect, abs_rank6_effect)
)

effect_long <- reshape2::melt(effect_data, id.vars = "Rank_Position", 
                             variable.name = "Model_Type", value.name = "Effect_Size")
effect_long <- effect_long[!is.na(effect_long$Effect_Size), ]

p2 <- ggplot(effect_long, aes(x = Rank_Position, y = Effect_Size, fill = Model_Type)) +
  geom_col(position = "dodge", alpha = 0.8, width = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_text(aes(label = round(Effect_Size, 3)), 
            position = position_dodge(0.7), vjust = -0.3, fontface = "bold", size = 4) +
  scale_fill_manual(values = c("Relative_Model" = "#4CAF50", "Absolute_Model" = "#FF9800"),
                   labels = c("Relative Rank", "Absolute Rank")) +
  labs(
    title = "Rank Effect Sizes: Relative vs Absolute Models",
    subtitle = "Log-odds effects compared to highest rank (negative = less exploration)",
    x = "Rank Comparison", 
    y = "Effect Size (Log-Odds)",
    fill = "Model Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "darkblue"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

# Plot 3: Raw Exploration Rates by Rank System
exploration_by_relative <- model_data %>%
  group_by(relative_rank) %>%
  summarise(exploration_rate = mean(outcome_clean == "explore"), .groups = "drop") %>%
  mutate(rank_system = "Relative", rank_label = paste("Rank", relative_rank))

exploration_by_absolute <- model_data %>%
  group_by(absolute_rank) %>%
  summarise(exploration_rate = mean(outcome_clean == "explore"), .groups = "drop") %>%
  mutate(rank_system = "Absolute", rank_label = paste("Rank", absolute_rank))

combined_exploration <- rbind(
  data.frame(rank_system = exploration_by_relative$rank_system,
             rank_label = exploration_by_relative$rank_label,
             exploration_rate = exploration_by_relative$exploration_rate),
  data.frame(rank_system = exploration_by_absolute$rank_system,
             rank_label = exploration_by_absolute$rank_label,
             exploration_rate = exploration_by_absolute$exploration_rate)
)

p3 <- ggplot(combined_exploration, aes(x = rank_label, y = exploration_rate, fill = rank_system)) +
  geom_col(alpha = 0.8, width = 0.7) +
  geom_text(aes(label = paste0(round(exploration_rate * 100, 1), "%")), 
            vjust = -0.3, fontface = "bold", size = 4) +
  scale_fill_manual(values = c("Relative" = "#4CAF50", "Absolute" = "#FF9800")) +
  scale_y_continuous(labels = percent_format()) +
  facet_wrap(~rank_system, scales = "free_x") +
  labs(
    title = "Raw Exploration Rates by Rank System",
    subtitle = "How do different ranking systems capture behavioral patterns?",
    x = "Rank Position", 
    y = "Exploration Rate",
    fill = "Rank System"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "darkblue"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    strip.text = element_text(size = 14, face = "bold"),
    legend.position = "none"
  )

# Plot 4: Individual Monkey Mapping
monkey_ranks <- model_data %>%
  select(monkey, relative_rank, absolute_rank, sex) %>%
  distinct() %>%
  arrange(absolute_rank)

p4 <- ggplot(monkey_ranks, aes(x = absolute_rank, y = relative_rank, color = sex)) +
  geom_point(size = 8, alpha = 0.8) +
  geom_text(aes(label = monkey), color = "white", fontface = "bold", size = 3) +
  geom_line(aes(group = sex), linewidth = 2, alpha = 0.6) +
  scale_color_manual(values = c("Male" = "#2196F3", "Female" = "#E91E63")) +
  scale_x_continuous(breaks = 1:6, labels = paste("Abs", 1:6)) +
  scale_y_continuous(breaks = 1:3, labels = paste("Rel", 1:3)) +
  labs(
    title = "Rank System Mapping: Individual Monkeys",
    subtitle = "How does each monkey's position change between ranking systems?",
    x = "Absolute Rank (1 = highest overall)", 
    y = "Relative Rank (1 = highest within group)",
    color = "Sex"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "darkblue"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    legend.position = "top"
  )

# Arrange all plots
grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2,
            top = "Comprehensive Comparison: Absolute vs Relative Rank Models")

dev.off()

# === FINAL SUMMARY ===
cat("\n=== FINAL COMPARISON SUMMARY ===\n")

better_model <- ifelse(accuracy_relative > accuracy_absolute, "RELATIVE", "ABSOLUTE")
accuracy_diff <- abs(accuracy_relative - accuracy_absolute)
elpd_diff <- abs(loo_relative$estimates["elpd_loo", "Estimate"] - 
                 loo_absolute$estimates["elpd_loo", "Estimate"])

cat("WINNER:", better_model, "RANK MODEL\n")
cat("Performance difference:", round(accuracy_diff, 1), "percentage points\n")
cat("ELPD difference:", round(elpd_diff, 1), "\n")

if(better_model == "RELATIVE") {
  cat("\nRELATIVE RANK ADVANTAGES:\n")
  cat("• Captures within-group social dynamics\n")
  cat("• Context-dependent hierarchy effects\n")
  cat("• Better predictive performance\n")
} else {
  cat("\nABSOLUTE RANK ADVANTAGES:\n")
  cat("• Fixed individual differences\n")
  cat("• Cross-context consistency\n")
  cat("• Better predictive performance\n")
}

cat("\nKEY INSIGHTS:\n")
cat("• Relative rank effect (Rank 3 vs 1):", round(rel_rank3_effect, 3), "\n")
cat("• Absolute rank effect (Rank 6 vs 1):", round(abs_rank6_effect, 3), "\n")
cat("• Model selection supports:", better_model, "rank approach\n")

cat("\nVisualization saved to: ABSOLUTE_VS_RELATIVE_RANK_COMPARISON.pdf\n") 