# Load required libraries
library(ggplot2)
library(dplyr)
library(stringr)
library(forcats)

# Set theme for publication-quality plots
theme_pub <- function() {
  theme_minimal() +
    theme(
      text = element_text(family = "Arial", size = 12),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_blank(),
      strip.text = element_text(size = 12, face = "bold"),
      plot.margin = margin(1, 1, 1, 1, "cm")
    )
}

# Read the data
df <- read.csv("Explore Exploit Dataset.csv")

# Data preparation
df <- df %>%
  mutate(OUTCOME = factor(OUTCOME, 
                         levels = c("explore", "exploit_pink", "exploit_blue", 
                                  "exploit_red", "exploit_green", "exploit_yellow")),
         monkey = factor(monkey),
         BLOCK_No = factor(BLOCK_No),
         TRIAL_TYPE = factor(TRIAL_TYPE))

# Create unique trial identifier
df$trial_id <- paste(df$monkey, df$BLOCK_No, df$TRIAL_NUM, sep = "_")

# Statistical Analysis
# 1. Chi-square test for outcome distribution
outcome_counts <- table(df$OUTCOME)
chi_square_test <- chisq.test(outcome_counts)

# 2. ANOVA for relative rank differences between outcomes
rank_anova <- aov(RELATIVE_RANK ~ OUTCOME, data = df)
rank_anova_summary <- summary(rank_anova)
tukey_test <- TukeyHSD(rank_anova)

# 3. Time series analysis
time_series_stats <- df %>%
  group_by(monkey, TRIAL_NUM) %>%
  summarise(
    explore_rate = mean(OUTCOME == "explore", na.rm = TRUE),
    exploit_rate = mean(OUTCOME != "explore" & !is.na(OUTCOME), na.rm = TRUE),
    .groups = "drop"
  )

# Create enhanced visualizations

# 1. Distribution of outcomes with statistical significance
p1 <- ggplot(df, aes(x = OUTCOME)) +
  geom_bar(fill = "steelblue", alpha = 0.8) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  theme_pub() +
  labs(title = "Distribution of Behavioral Outcomes",
       x = "Outcome",
       y = "Count",
       caption = paste("Chi-square test: χ² =", round(chi_square_test$statistic, 2),
                      ", p =", format.pval(chi_square_test$p.value, digits = 3)))

# 2. Outcomes by relative rank with statistical significance
p2 <- ggplot(df, aes(x = RELATIVE_RANK, fill = OUTCOME)) +
  geom_bar(position = "fill", alpha = 0.8) +
  theme_pub() +
  labs(title = "Outcome Distribution by Relative Rank",
       x = "Relative Rank",
       y = "Proportion",
       caption = paste("ANOVA: F =", round(rank_anova_summary[[1]]$F[1], 2),
                      ", p =", format.pval(rank_anova_summary[[1]]$`Pr(>F)`[1], digits = 3))) +
  scale_fill_brewer(palette = "Set3")

# 3. Time series of outcomes with trend lines
p3 <- ggplot(time_series_stats, aes(x = TRIAL_NUM)) +
  geom_line(aes(y = explore_rate, color = "Exploration"), size = 1) +
  geom_line(aes(y = exploit_rate, color = "Exploitation"), size = 1) +
  geom_smooth(aes(y = explore_rate, color = "Exploration"), method = "loess", se = TRUE) +
  geom_smooth(aes(y = exploit_rate, color = "Exploitation"), method = "loess", se = TRUE) +
  facet_wrap(~monkey) +
  theme_pub() +
  labs(title = "Exploration vs. Exploitation Rates Over Time",
       x = "Trial Number",
       y = "Rate",
       color = "Behavior") +
  scale_color_brewer(palette = "Set1")

# 4. Heatmap of outcomes by block and trial type
p4 <- ggplot(df, aes(x = BLOCK_No, y = TRIAL_TYPE, fill = OUTCOME)) +
  geom_tile(alpha = 0.8) +
  theme_pub() +
  labs(title = "Outcome Distribution by Block and Trial Type",
       x = "Block Number",
       y = "Trial Type") +
  scale_fill_brewer(palette = "Set3") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save plots with high resolution
ggsave("outcome_distribution.png", p1, width = 10, height = 6, dpi = 300)
ggsave("outcome_by_rank.png", p2, width = 10, height = 6, dpi = 300)
ggsave("outcome_time_series.png", p3, width = 12, height = 8, dpi = 300)
ggsave("outcome_heatmap.png", p4, width = 10, height = 6, dpi = 300)

# Save statistical results
sink("statistical_results.txt")
cat("Chi-square Test Results:\n")
print(chi_square_test)
cat("\nANOVA Results for Relative Rank:\n")
print(rank_anova_summary)
cat("\nTukey's HSD Test Results:\n")
print(tukey_test)
sink()

# Calculate and save summary statistics
summary_stats <- df %>%
  group_by(OUTCOME) %>%
  summarise(
    mean_rank = mean(RELATIVE_RANK, na.rm = TRUE),
    sd_rank = sd(RELATIVE_RANK, na.rm = TRUE),
    count = n(),
    .groups = "drop"
  )

write.csv(summary_stats, "summary_statistics.csv", row.names = FALSE) 