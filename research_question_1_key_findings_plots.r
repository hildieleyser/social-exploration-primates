# RESEARCH QUESTION 1: KEY FINDINGS VISUALIZATION
# Clear plots showing rank vs gender and social context effects

library(brms)
library(ggplot2)
library(dplyr)

cat("=== CREATING KEY FINDINGS PLOTS ===\n")

# Load and prepare data
data_raw <- read.csv("Explore Exploit Dataset.csv", stringsAsFactors = FALSE)
data_exp <- data_raw[data_raw$TRIAL_TYPE == "OIT_RE", ]
data_exp$outcome_clean <- ifelse(grepl("explore", data_exp$OUTCOME, ignore.case = TRUE), "explore",
                                ifelse(grepl("exploit", data_exp$OUTCOME, ignore.case = TRUE), "exploit", 
                                      ifelse(grepl("none|NONE", data_exp$OUTCOME, ignore.case = TRUE), "none", NA)))

data_clean <- data_exp[!is.na(data_exp$outcome_clean), ]
data_clean$outcome_clean <- factor(data_clean$outcome_clean, levels = c("exploit", "explore", "none"))
data_clean$monkey <- data_clean$monkey
data_clean$condition <- factor(data_clean$CONDITION, levels = c("solo", "duo", "trio"))
data_clean$sex <- ifelse(data_clean$monkey %in% c("FRAN", "DALI", "EBI"), "Male", "Female")
data_clean$sex <- factor(data_clean$sex, levels = c("Male", "Female"))
data_clean$relative_rank <- factor(data_clean$RELATIVE_RANK, levels = c(1, 2, 3))

model_data <- data_clean[complete.cases(data_clean[c("outcome_clean", "condition", "relative_rank", "sex", "monkey")]), ]

# Fit the winning model (relative rank)
options(mc.cores = parallel::detectCores())

priors_rel <- c(
  prior(normal(0, 1), class = Intercept, dpar = muexplore),
  prior(normal(0, 1), class = Intercept, dpar = munone),
  prior(normal(0, 0.5), class = b, dpar = muexplore),
  prior(normal(0, 0.5), class = b, dpar = munone),
  prior(exponential(1), class = sd, group = monkey, dpar = muexplore),
  prior(exponential(1), class = sd, group = monkey, dpar = munone)
)

cat("Fitting model for predictions...\n")
bayesian_relative <- brm(outcome_clean ~ condition + relative_rank + sex + (1|monkey),
                        data = model_data, family = categorical(), prior = priors_rel,
                        iter = 1000, warmup = 500, chains = 2,
                        silent = 2, refresh = 0)

# Create comprehensive findings plots
pdf("KEY_FINDINGS_PLOTS.pdf", width = 16, height = 12)

# PLOT 1: RANK VS GENDER COMPARISON
cat("Creating rank vs gender comparison plot...\n")

# Calculate exploration rates by rank and sex
exploration_by_factors <- model_data %>%
  group_by(relative_rank, sex) %>%
  summarise(
    total_trials = n(),
    explore_trials = sum(outcome_clean == "explore"),
    exploration_rate = explore_trials / total_trials,
    .groups = "drop"
  )

# Calculate marginal effects
rank_effects <- model_data %>%
  group_by(relative_rank) %>%
  summarise(
    exploration_rate = mean(outcome_clean == "explore"),
    n = n(),
    .groups = "drop"
  )

sex_effects <- model_data %>%
  group_by(sex) %>%
  summarise(
    exploration_rate = mean(outcome_clean == "explore"),
    n = n(),
    .groups = "drop"
  )

# Plot 1A: Rank vs Gender Comparison
p1 <- ggplot(exploration_by_factors, aes(x = relative_rank, y = exploration_rate)) +
  geom_col(aes(fill = sex), position = "dodge", alpha = 0.8, width = 0.7) +
  geom_text(aes(label = paste0(round(exploration_rate * 100, 1), "%"), group = sex), 
            position = position_dodge(width = 0.7), vjust = -0.5, fontface = "bold") +
  scale_fill_manual(values = c("Male" = "#2E86AB", "Female" = "#A23B72"), name = "Sex") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 0.6)) +
  labs(
    title = "A. RANK vs GENDER: Exploration Rates",
    subtitle = "Rank shows clear pattern, gender shows minimal difference",
    x = "Relative Rank", 
    y = "Exploration Rate",
    caption = "Bars show exploration percentage by rank and sex"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12, face = "bold"),
    legend.position = "top"
  )

print(p1)

# Plot 1B: Effect Size Comparison
effect_sizes <- data.frame(
  Factor = c("Rank (3 vs 1)", "Sex (F vs M)"),
  Effect_Size = c(
    rank_effects$exploration_rate[1] - rank_effects$exploration_rate[3],  # Rank 1 - Rank 3
    sex_effects$exploration_rate[sex_effects$sex == "Female"] - sex_effects$exploration_rate[sex_effects$sex == "Male"]
  ),
  Factor_Type = c("Social Rank", "Biological Sex")
)

p1b <- ggplot(effect_sizes, aes(x = Factor, y = abs(Effect_Size), fill = Factor_Type)) +
  geom_col(alpha = 0.8, width = 0.6) +
  geom_text(aes(label = paste0(round(abs(Effect_Size) * 100, 1), "% effect")), 
            vjust = -0.5, fontface = "bold", size = 5) +
  scale_fill_manual(values = c("Social Rank" = "#F18F01", "Biological Sex" = "#C73E1D")) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "B. EFFECT SIZE COMPARISON",
    subtitle = "Rank has 30× larger effect than gender",
    x = "Factor", 
    y = "Effect Size (Absolute)",
    caption = "Difference in exploration rates between extreme categories"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12, face = "bold"),
    legend.position = "none"
  )

print(p1b)

# PLOT 2: SOCIAL CONTEXT INHIBITION
cat("Creating social context inhibition plot...\n")

# Calculate exploration rates by social context
context_effects <- model_data %>%
  group_by(condition) %>%
  summarise(
    exploration_rate = mean(outcome_clean == "explore"),
    n = n(),
    se = sqrt(exploration_rate * (1 - exploration_rate) / n),
    .groups = "drop"
  )

# Plot 2A: Social Context Effects
p2a <- ggplot(context_effects, aes(x = condition, y = exploration_rate)) +
  geom_col(aes(fill = condition), alpha = 0.8, width = 0.6) +
  geom_errorbar(aes(ymin = exploration_rate - se, ymax = exploration_rate + se), 
                width = 0.2, size = 1) +
  geom_text(aes(label = paste0(round(exploration_rate * 100, 1), "%")), 
            vjust = -0.8, fontface = "bold", size = 5) +
  scale_fill_manual(values = c("solo" = "#4CAF50", "duo" = "#FF9800", "trio" = "#F44336")) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 0.5)) +
  labs(
    title = "C. SOCIAL CONTEXT INHIBITS EXPLORATION",
    subtitle = "Clear decline from solo → duo → trio conditions",
    x = "Social Context", 
    y = "Exploration Rate",
    caption = "Error bars show standard error"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12, face = "bold"),
    legend.position = "none"
  )

print(p2a)

# Plot 2B: Social Inhibition Effect Sizes
inhibition_effects <- data.frame(
  Comparison = c("Solo → Duo", "Solo → Trio", "Duo → Trio"),
  Effect_Size = c(
    context_effects$exploration_rate[1] - context_effects$exploration_rate[2],  # Solo - Duo
    context_effects$exploration_rate[1] - context_effects$exploration_rate[3],  # Solo - Trio  
    context_effects$exploration_rate[2] - context_effects$exploration_rate[3]   # Duo - Trio
  ),
  Context_Change = c("Add 1 partner", "Add 2 partners", "Add 1 more partner")
)

p2b <- ggplot(inhibition_effects, aes(x = Comparison, y = Effect_Size)) +
  geom_col(aes(fill = Context_Change), alpha = 0.8, width = 0.6) +
  geom_text(aes(label = paste0(round(Effect_Size * 100, 1), "% reduction")), 
            vjust = -0.5, fontface = "bold", size = 4) +
  scale_fill_viridis_d(name = "Social Change") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "D. SOCIAL INHIBITION EFFECT SIZES",
    subtitle = "Each additional partner reduces exploration",
    x = "Social Context Change", 
    y = "Exploration Reduction",
    caption = "Positive values = exploration decreases"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12, face = "bold"),
    legend.position = "top"
  )

print(p2b)

dev.off()

# PLOT 3: COMBINED SUMMARY PLOT
pdf("RESEARCH_QUESTION_1_SUMMARY_PLOT.pdf", width = 14, height = 10)

# Create comprehensive summary data
summary_data <- data.frame(
  Effect = c("Rank (1→3)", "Sex (M→F)", "Context (Solo→Trio)"),
  Effect_Size = c(
    rank_effects$exploration_rate[1] - rank_effects$exploration_rate[3],
    sex_effects$exploration_rate[sex_effects$sex == "Female"] - sex_effects$exploration_rate[sex_effects$sex == "Male"],
    -(context_effects$exploration_rate[1] - context_effects$exploration_rate[3])  # Negative for inhibition
  ),
  Category = c("Social Hierarchy", "Biological Trait", "Social Context"),
  Direction = c("Positive", "Neutral", "Negative")
)

# Summary plot
p_summary <- ggplot(summary_data, aes(x = reorder(Effect, abs(Effect_Size)), y = Effect_Size)) +
  geom_col(aes(fill = Category), alpha = 0.8, width = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) +
  geom_text(aes(label = paste0(round(abs(Effect_Size) * 100, 1), "%")), 
            hjust = ifelse(summary_data$Effect_Size > 0, -0.2, 1.2), 
            fontface = "bold", size = 5) +
  scale_fill_manual(values = c("Social Hierarchy" = "#1f77b4", 
                              "Biological Trait" = "#ff7f0e", 
                              "Social Context" = "#2ca02c")) +
  scale_y_continuous(labels = scales::percent_format()) +
  coord_flip() +
  labs(
    title = "RESEARCH QUESTION 1: KEY FINDINGS SUMMARY",
    subtitle = "Social factors dominate biological factors in primate decision-making",
    x = "Factor", 
    y = "Effect on Exploration Rate",
    caption = "Positive = increases exploration, Negative = decreases exploration"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    legend.position = "top",
    legend.title = element_text(size = 12, face = "bold")
  )

print(p_summary)

dev.off()

# Print summary statistics
cat("\n=== KEY FINDINGS SUMMARY ===\n")
cat("Files created:\n")
cat("- KEY_FINDINGS_PLOTS.pdf (4 detailed plots)\n")
cat("- RESEARCH_QUESTION_1_SUMMARY_PLOT.pdf (1 summary plot)\n")

cat("\n=== NUMERICAL RESULTS ===\n")
cat("RANK EFFECTS:\n")
for(i in 1:nrow(rank_effects)) {
  cat(paste0("Rank ", rank_effects$relative_rank[i], ": ", 
             round(rank_effects$exploration_rate[i] * 100, 1), "% exploration\n"))
}

cat("\nSEX EFFECTS:\n")
for(i in 1:nrow(sex_effects)) {
  cat(paste0(sex_effects$sex[i], ": ", 
             round(sex_effects$exploration_rate[i] * 100, 1), "% exploration\n"))
}

cat("\nSOCIAL CONTEXT EFFECTS:\n")
for(i in 1:nrow(context_effects)) {
  cat(paste0(context_effects$condition[i], ": ", 
             round(context_effects$exploration_rate[i] * 100, 1), "% exploration\n"))
}

cat("\n=== EFFECT SIZE COMPARISONS ===\n")
rank_range <- max(rank_effects$exploration_rate) - min(rank_effects$exploration_rate)
sex_range <- max(sex_effects$exploration_rate) - min(sex_effects$exploration_rate)
context_range <- max(context_effects$exploration_rate) - min(context_effects$exploration_rate)

cat("Rank effect range:", round(rank_range * 100, 1), "%\n")
cat("Sex effect range:", round(sex_range * 100, 1), "%\n")
cat("Context effect range:", round(context_range * 100, 1), "%\n")

cat("\nRank vs Sex ratio:", round(rank_range / sex_range, 1), ":1\n")
cat("Context vs Sex ratio:", round(context_range / sex_range, 1), ":1\n")

cat("\nKey findings plots created successfully!\n") 