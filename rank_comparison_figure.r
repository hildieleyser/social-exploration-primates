# CLEAN FIGURE: ABSOLUTE VS RELATIVE RANK COMPARISON
# Publication-ready visualization of ranking system effects

library(brms)
library(dplyr)
library(ggplot2)
library(scales)
library(gridExtra)
library(grid)

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
    relative_rank = factor(RELATIVE_RANK, levels = c(1, 2, 3)),
    absolute_rank = factor(ABSOLUTE_RANK, levels = c(1, 2, 3))
  )

model_data <- data_enhanced[complete.cases(data_enhanced[c("outcome_clean", "condition", "relative_rank", "absolute_rank", "sex", "monkey")]), ]

# Calculate exploration rates for visualization
exploration_by_relative <- model_data %>%
  group_by(relative_rank, sex) %>%
  summarise(exploration_rate = mean(outcome_clean == "explore"), 
           count = n(), .groups = "drop") %>%
  mutate(rank_system = "Relative Rank\n(Cross-Sex)",
         rank_label = paste("Rank", relative_rank))

exploration_by_absolute <- model_data %>%
  group_by(absolute_rank, sex) %>%
  summarise(exploration_rate = mean(outcome_clean == "explore"), 
           count = n(), .groups = "drop") %>%
  mutate(rank_system = "Absolute Rank\n(Within-Sex)",
         rank_label = paste("Rank", absolute_rank))

# Individual monkey data
monkey_data <- model_data %>%
  group_by(monkey, sex, relative_rank, absolute_rank) %>%
  summarise(exploration_rate = mean(outcome_clean == "explore"), 
           count = n(), .groups = "drop") %>%
  mutate(
    relative_rank_num = as.numeric(as.character(relative_rank)),
    absolute_rank_num = as.numeric(as.character(absolute_rank))
  )

# Model performance data (from previous results)
performance_data <- data.frame(
  Model = c("Relative Rank", "Absolute Rank"),
  Accuracy = c(47.2, 47.3),
  ELPD_Advantage = c(6.7, 0),
  Type = c("Cross-Sex", "Within-Sex")
)

# Effect sizes (from previous results)
effect_data <- data.frame(
  Rank_System = c("Relative\n(Cross-Sex)", "Absolute\n(Within-Sex)"),
  Rank_3_vs_1_Effect = c(0.603, -0.399),
  Direction = c("Subordinates explore MORE", "Subordinates explore LESS")
)

# === CREATE COMPREHENSIVE FIGURE ===
pdf("RANK_COMPARISON_FIGURE.pdf", width = 20, height = 12)

# Color schemes
colors_sex <- c("Male" = "#2196F3", "Female" = "#E91E63")
colors_rank <- c("Cross-Sex" = "#4CAF50", "Within-Sex" = "#FF9800")

# Plot 1: Exploration Rates by Both Rank Systems
combined_exploration <- rbind(
  data.frame(rank_system = exploration_by_relative$rank_system,
             rank = as.character(exploration_by_relative$relative_rank),
             sex = exploration_by_relative$sex,
             exploration_rate = exploration_by_relative$exploration_rate,
             count = exploration_by_relative$count),
  data.frame(rank_system = exploration_by_absolute$rank_system,
             rank = as.character(exploration_by_absolute$absolute_rank),
             sex = exploration_by_absolute$sex,
             exploration_rate = exploration_by_absolute$exploration_rate,
             count = exploration_by_absolute$count)
)

combined_exploration$rank <- factor(combined_exploration$rank, levels = c("1", "2", "3"))

p1 <- ggplot(combined_exploration, aes(x = rank, y = exploration_rate, fill = sex)) +
  geom_col(position = "dodge", alpha = 0.8, width = 0.7) +
  geom_text(aes(label = paste0(round(exploration_rate * 100, 1), "%")), 
            position = position_dodge(0.7), vjust = -0.3, fontface = "bold", size = 5) +
  scale_fill_manual(values = colors_sex) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 0.7)) +
  facet_wrap(~rank_system, scales = "free_x") +
  labs(
    title = "A. Exploration Rates by Ranking System",
    subtitle = "How do cross-sex vs within-sex hierarchies predict behavior?",
    x = "Rank Position (1 = Highest)", 
    y = "Exploration Rate",
    fill = "Sex"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14, color = "darkblue"),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16, face = "bold"),
    strip.text = element_text(size = 15, face = "bold"),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    legend.position = "top"
  )

# Plot 2: Effect Size Comparison
p2 <- ggplot(effect_data, aes(x = Rank_System, y = Rank_3_vs_1_Effect, 
                              fill = Rank_System)) +
  geom_col(alpha = 0.8, width = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  geom_text(aes(label = round(Rank_3_vs_1_Effect, 3)), 
            vjust = ifelse(effect_data$Rank_3_vs_1_Effect > 0, -0.3, 1.3), 
            fontface = "bold", size = 6) +
  geom_text(aes(label = Direction, y = Rank_3_vs_1_Effect + ifelse(Rank_3_vs_1_Effect > 0, 0.15, -0.15)), 
            fontface = "bold", size = 4, color = "darkblue") +
  scale_fill_manual(values = colors_rank) +
  scale_y_continuous(limits = c(-0.8, 0.8)) +
  labs(
    title = "B. Effect Sizes: Rank 3 vs Rank 1",
    subtitle = "Log-odds effects show opposite patterns between ranking systems",
    x = "Ranking System", 
    y = "Effect Size (Log-Odds)",
    fill = "System Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14, color = "darkblue"),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16, face = "bold"),
    legend.position = "none"
  )

# Plot 3: Model Performance Comparison
performance_long <- reshape2::melt(performance_data[c("Model", "Accuracy", "ELPD_Advantage")], 
                                  id.vars = "Model", variable.name = "Metric", value.name = "Value")

p3 <- ggplot(performance_long, aes(x = Model, y = Value, fill = Metric)) +
  geom_col(position = "dodge", alpha = 0.8, width = 0.7) +
  geom_text(aes(label = ifelse(Metric == "Accuracy", paste0(round(Value, 1), "%"), 
                              paste0("+", round(Value, 1)))), 
            position = position_dodge(0.7), vjust = -0.3, fontface = "bold", size = 5) +
  scale_fill_manual(values = c("Accuracy" = "#9C27B0", "ELPD_Advantage" = "#607D8B"),
                   labels = c("Prediction Accuracy (%)", "ELPD Advantage")) +
  labs(
    title = "C. Model Performance Comparison",
    subtitle = "Accuracy similar, but LOO favors relative rank",
    x = "Ranking Model", 
    y = "Performance Metric",
    fill = "Metric Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14, color = "darkblue"),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16, face = "bold"),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    legend.position = "top"
  )

# Plot 4: Individual Monkey Positions
p4 <- ggplot(monkey_data, aes(x = absolute_rank_num, y = relative_rank_num, color = sex, size = exploration_rate)) +
  geom_point(alpha = 0.8) +
  geom_text(aes(label = monkey), color = "white", fontface = "bold", size = 3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray", linewidth = 1) +
  scale_color_manual(values = colors_sex) +
  scale_size_continuous(range = c(8, 15), labels = percent_format()) +
  scale_x_continuous(breaks = 1:3, labels = paste("Abs", 1:3)) +
  scale_y_continuous(breaks = 1:3, labels = paste("Rel", 1:3)) +
  labs(
    title = "D. Individual Monkey Rank Positions",
    subtitle = "Diagonal = perfect agreement between systems",
    x = "Absolute Rank (Within-Sex)", 
    y = "Relative Rank (Cross-Sex)",
    color = "Sex",
    size = "Exploration Rate"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14, color = "darkblue"),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16, face = "bold"),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    legend.position = "top"
  )

# Arrange all plots
grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2,
            top = textGrob("Absolute vs Relative Rank Comparison: Social Identity Effects on Primate Exploration", 
                          gp = gpar(fontsize = 20, fontface = "bold")))

dev.off()

# === CREATE SUMMARY FIGURE ===
pdf("RANK_COMPARISON_SUMMARY.pdf", width = 16, height = 8)

# Summary plot showing key insight
summary_data <- data.frame(
  Ranking_System = c("Relative Rank\n(Cross-Sex)", "Absolute Rank\n(Within-Sex)"),
  Subordinate_Effect = c("MORE Exploration", "LESS Exploration"),
  Effect_Size = c(0.603, -0.399),
  Interpretation = c("Subordinates take risks\nwhen competing globally", 
                    "Subordinates are inhibited\nwithin their own sex")
)

p_summary <- ggplot(summary_data, aes(x = Ranking_System, y = Effect_Size, fill = Ranking_System)) +
  geom_col(alpha = 0.8, width = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  geom_text(aes(label = paste0(ifelse(Effect_Size > 0, "+", ""), round(Effect_Size, 3))), 
            vjust = ifelse(summary_data$Effect_Size > 0, -0.3, 1.3), 
            fontface = "bold", size = 8) +
  geom_text(aes(label = Subordinate_Effect, y = Effect_Size + ifelse(Effect_Size > 0, 0.2, -0.2)), 
            fontface = "bold", size = 6, color = "darkblue") +
  geom_text(aes(label = Interpretation, y = ifelse(Effect_Size > 0, -0.4, 0.4)), 
            fontface = "italic", size = 5, color = "darkred") +
  scale_fill_manual(values = colors_rank) +
  scale_y_continuous(limits = c(-0.8, 0.8)) +
  labs(
    title = "Key Finding: Ranking Systems Reveal Opposite Social Dynamics",
    subtitle = "Subordinate primates show different exploration strategies depending on social reference frame",
    x = "Social Ranking System", 
    y = "Subordinate Effect Size (Log-Odds)",
    caption = "Data: 1,443 trials from 6 primates in social decision-making task"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 16, color = "darkblue", hjust = 0.5),
    plot.caption = element_text(size = 12, color = "gray50"),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 18, face = "bold"),
    legend.position = "none"
  )

print(p_summary)
dev.off()

cat("Figures saved:\n")
cat("• RANK_COMPARISON_FIGURE.pdf (comprehensive 4-panel figure)\n")
cat("• RANK_COMPARISON_SUMMARY.pdf (key finding summary)\n")

# Print key statistics
cat("\n=== KEY STATISTICS ===\n")
cat("Relative Rank Effect (Rank 3 vs 1): +0.603 log-odds\n")
cat("Absolute Rank Effect (Rank 3 vs 1): -0.399 log-odds\n")
cat("Model Performance: Nearly identical (~47% accuracy)\n")
cat("LOO Preference: Relative rank (+6.7 ELPD advantage)\n")
cat("\n=== INTERPRETATION ===\n")
cat("• RELATIVE RANK: Subordinates explore MORE (global competition)\n")
cat("• ABSOLUTE RANK: Subordinates explore LESS (within-sex inhibition)\n")
cat("• Social reference frame matters more than absolute hierarchy!\n") 