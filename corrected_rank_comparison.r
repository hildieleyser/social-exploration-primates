# CORRECTED ABSOLUTE VS RELATIVE RANK COMPARISON
# Using the actual ranking systems from the dataset

library(brms)
library(dplyr)
library(ggplot2)
library(scales)
library(gridExtra)

cat("=== CORRECTED ABSOLUTE VS RELATIVE RANK COMPARISON ===\n")

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

# Check what ranking columns we have
cat("Available columns:\n")
rank_cols <- names(data_clean)[grepl("rank|RANK", names(data_clean), ignore.case = TRUE)]
cat("Rank-related columns:", paste(rank_cols, collapse = ", "), "\n")

# Show unique values for each rank column
for(col in rank_cols) {
  if(col %in% names(data_clean)) {
    cat(col, "unique values:", paste(sort(unique(data_clean[[col]])), collapse = ", "), "\n")
  }
}

# Create both rank systems from dataset
data_enhanced <- data_clean %>%
  mutate(
    # RELATIVE RANK (from RELATIVE_RANK column)
    relative_rank = factor(RELATIVE_RANK, levels = c(1, 2, 3)),
    
    # ABSOLUTE RANK (should be within-sex ranking: 1,2,3,1,2,3)
    # Check if there's an ABSOLUTE_RANK column, otherwise create it
    absolute_rank = if("ABSOLUTE_RANK" %in% names(data_clean)) {
      factor(ABSOLUTE_RANK, levels = c(1, 2, 3))
    } else {
      # Create absolute rank as within-sex ranking
      case_when(
        monkey == "FRAN" ~ 1,      # Male rank 1
        monkey == "DALI" ~ 2,      # Male rank 2  
        monkey == "EBI" ~ 3,       # Male rank 3
        monkey == "CHOCOLAT" ~ 1,  # Female rank 1
        monkey == "ICE" ~ 2,       # Female rank 2
        monkey == "ANEMONE" ~ 3    # Female rank 3
      ) %>% factor(levels = c(1, 2, 3))
    }
  )

model_data <- data_enhanced[complete.cases(data_enhanced[c("outcome_clean", "condition", "relative_rank", "sex", "monkey")]), ]

cat("\nData prepared with corrected ranking systems\n")
cat("Sample of data:\n")
sample_data <- model_data %>% 
  select(monkey, sex, relative_rank, absolute_rank) %>% 
  distinct() %>% 
  arrange(sex, absolute_rank)
print(sample_data)

cat("\nRelative rank distribution:\n")
print(table(model_data$relative_rank))
cat("Absolute rank distribution:\n") 
print(table(model_data$absolute_rank))

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
cat("\nFitting RELATIVE RANK model...\n")
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
cat("Absolute rank model accuracy:", round(accuracy_absolute, 1), "%\n")

# === MODEL COMPARISON ===
cat("\n=== MODEL COMPARISON METRICS ===\n")

# Calculate LOO for model comparison
loo_relative <- loo(model_relative)
loo_absolute <- loo(model_absolute)
loo_compare <- loo_compare(loo_relative, loo_absolute)

cat("LOO Comparison:\n")
print(loo_compare)

# Extract effect sizes
posterior_relative <- as.data.frame(model_relative)
posterior_absolute <- as.data.frame(model_absolute)

# Relative rank effects
rel_rank2_effect <- mean(posterior_relative$b_muexplore_relative_rank2)
rel_rank3_effect <- mean(posterior_relative$b_muexplore_relative_rank3)

# Absolute rank effects
abs_rank2_effect <- mean(posterior_absolute$b_muexplore_absolute_rank2)
abs_rank3_effect <- mean(posterior_absolute$b_muexplore_absolute_rank3)

# === VISUALIZATION ===
pdf("CORRECTED_RANK_COMPARISON.pdf", width = 16, height = 10)

# Plot 1: Model Performance
performance_data <- data.frame(
  Model = c("Relative Rank", "Absolute Rank"),
  Accuracy = c(accuracy_relative, accuracy_absolute),
  ELPD = c(loo_relative$estimates["elpd_loo", "Estimate"],
           loo_absolute$estimates["elpd_loo", "Estimate"]),
  Type = c("Relative", "Absolute")
)

p1 <- ggplot(performance_data, aes(x = Model, y = Accuracy, fill = Type)) +
  geom_col(alpha = 0.8, width = 0.6) +
  geom_text(aes(label = paste0(round(Accuracy, 1), "%")), 
            vjust = -0.3, fontface = "bold", size = 6) +
  scale_fill_manual(values = c("Relative" = "#4CAF50", "Absolute" = "#FF9800")) +
  scale_y_continuous(labels = percent_format(scale = 1), 
                     limits = c(0, max(performance_data$Accuracy) * 1.15)) +
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
  Rank_Position = c("Rank 2 vs 1", "Rank 3 vs 1"),
  Relative_Model = c(rel_rank2_effect, rel_rank3_effect),
  Absolute_Model = c(abs_rank2_effect, abs_rank3_effect)
)

effect_long <- reshape2::melt(effect_data, id.vars = "Rank_Position", 
                             variable.name = "Model_Type", value.name = "Effect_Size")

p2 <- ggplot(effect_long, aes(x = Rank_Position, y = Effect_Size, fill = Model_Type)) +
  geom_col(position = "dodge", alpha = 0.8, width = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_text(aes(label = round(Effect_Size, 3)), 
            position = position_dodge(0.7), vjust = -0.3, fontface = "bold", size = 5) +
  scale_fill_manual(values = c("Relative_Model" = "#4CAF50", "Absolute_Model" = "#FF9800"),
                   labels = c("Relative Rank", "Absolute Rank")) +
  labs(
    title = "Rank Effect Sizes: Relative vs Absolute Models",
    subtitle = "Log-odds effects compared to rank 1 (negative = less exploration)",
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
    legend.position = "top"
  )

# Plot 3: Raw Exploration by Both Rank Systems
exploration_by_relative <- model_data %>%
  group_by(relative_rank, sex) %>%
  summarise(exploration_rate = mean(outcome_clean == "explore"), .groups = "drop") %>%
  mutate(rank_system = "Relative",
         rank_char = as.character(relative_rank))

exploration_by_absolute <- model_data %>%
  group_by(absolute_rank, sex) %>%
  summarise(exploration_rate = mean(outcome_clean == "explore"), .groups = "drop") %>%
  mutate(rank_system = "Absolute",
         rank_char = as.character(absolute_rank))

# Combine properly
combined_exploration <- rbind(
  data.frame(rank_system = exploration_by_relative$rank_system,
             rank = exploration_by_relative$rank_char,
             sex = exploration_by_relative$sex,
             exploration_rate = exploration_by_relative$exploration_rate),
  data.frame(rank_system = exploration_by_absolute$rank_system,
             rank = exploration_by_absolute$rank_char,
             sex = exploration_by_absolute$sex,
             exploration_rate = exploration_by_absolute$exploration_rate)
)

# Ensure rank is treated as character/factor for discrete scale
combined_exploration$rank <- factor(combined_exploration$rank, levels = c("1", "2", "3"))

p3 <- ggplot(combined_exploration, aes(x = rank, y = exploration_rate, fill = sex)) +
  geom_col(position = "dodge", alpha = 0.8, width = 0.7) +
  geom_text(aes(label = paste0(round(exploration_rate * 100, 1), "%")), 
            position = position_dodge(0.7), vjust = -0.3, fontface = "bold", size = 4) +
  scale_fill_manual(values = c("Male" = "#2196F3", "Female" = "#E91E63")) +
  scale_y_continuous(labels = percent_format()) +
  facet_wrap(~rank_system, scales = "free") +
  labs(
    title = "Exploration Rates by Rank System and Sex",
    subtitle = "How do different ranking systems capture sex-specific patterns?",
    x = "Rank Position (1 = highest)", 
    y = "Exploration Rate",
    fill = "Sex"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "darkblue"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    strip.text = element_text(size = 14, face = "bold"),
    legend.position = "top"
  )

# Plot 4: Individual Monkey Comparison
monkey_comparison <- model_data %>%
  select(monkey, sex, relative_rank, absolute_rank) %>%
  distinct() %>%
  mutate(
    relative_rank_num = as.numeric(as.character(relative_rank)),
    absolute_rank_num = as.numeric(as.character(absolute_rank))
  ) %>%
  arrange(sex, monkey)

p4 <- ggplot(monkey_comparison, aes(x = absolute_rank_num, y = relative_rank_num, color = sex)) +
  geom_point(size = 8, alpha = 0.8) +
  geom_text(aes(label = monkey), color = "white", fontface = "bold", size = 3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray", linewidth = 1) +
  scale_color_manual(values = c("Male" = "#2196F3", "Female" = "#E91E63")) +
  scale_x_continuous(breaks = 1:3, labels = paste("Abs", 1:3)) +
  scale_y_continuous(breaks = 1:3, labels = paste("Rel", 1:3)) +
  labs(
    title = "Rank System Comparison: Individual Monkeys",
    subtitle = "Diagonal line = perfect agreement between systems",
    x = "Absolute Rank (within-sex)", 
    y = "Relative Rank (cross-sex)",
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
            top = "Corrected Comparison: Absolute vs Relative Rank Models")

dev.off()

# === FINAL SUMMARY ===
cat("\n=== CORRECTED COMPARISON SUMMARY ===\n")

better_model <- ifelse(accuracy_relative > accuracy_absolute, "RELATIVE", "ABSOLUTE")
accuracy_diff <- abs(accuracy_relative - accuracy_absolute)
elpd_diff <- loo_compare[1, "elpd_diff"]

cat("WINNER:", better_model, "RANK MODEL\n")
cat("Performance difference:", round(accuracy_diff, 1), "percentage points\n")
cat("ELPD difference:", round(abs(elpd_diff), 1), "\n")

cat("\nRANK SYSTEM DEFINITIONS:\n")
cat("• RELATIVE RANK: Cross-sex comparison (males vs females)\n")
cat("• ABSOLUTE RANK: Within-sex comparison (males: 1,2,3; females: 1,2,3)\n")

cat("\nEFFECT SIZES:\n")
cat("• Relative rank (Rank 3 vs 1):", round(rel_rank3_effect, 3), "\n")
cat("• Absolute rank (Rank 3 vs 1):", round(abs_rank3_effect, 3), "\n")

if(better_model == "RELATIVE") {
  cat("\nRELATIVE RANK CAPTURES:\n")
  cat("• Cross-sex hierarchical dynamics\n")
  cat("• Global social positioning\n")
  cat("• Mixed-group social contexts\n")
} else {
  cat("\nABSOLUTE RANK CAPTURES:\n")
  cat("• Within-sex hierarchical dynamics\n")
  cat("• Sex-specific social positioning\n")
  cat("• Same-sex group contexts\n")
}

cat("\nVisualization saved to: CORRECTED_RANK_COMPARISON.pdf\n") 