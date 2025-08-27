#!/usr/bin/env Rscript

# Behavioral Figure Panels (A-D) as separate PNGs
# Matches the style and content of the provided example

library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(viridis)

# Load data
raw <- read.csv("Explore Exploit Dataset.csv")
choices <- c("exploit", "explore", "none")
choice_colors <- c("explore" = "#4CAF50", "exploit" = "#E57373", "none" = "#64B5F6")
context_names <- c("solo", "duo", "trio")

# Add sex and dominance rank (edit as needed)
monkey_sex <- c(F = "F", C = "F", D = "M", E = "M", F = "M", I = "F", ANEMONE = "F", CHOCOLAT = "F", DALI = "M", EBI = "M", FRAN = "M", ICE = "F")
monkey_rank <- c(F = 1, C = 1, D = 2, I = 2, E = 3, A = 3, ANEMONE = 3, CHOCOLAT = 1, DALI = 2, EBI = 2, FRAN = 1, ICE = 3)

# Data prep
raw <- raw %>%
  mutate(
    context = factor(CONDITION, levels = context_names),
    choice = case_when(
      OUTCOME == "explore" ~ "explore",
      OUTCOME %in% c("exploit_pink", "exploit_blue") ~ "exploit",
      TRUE ~ "none"
    ),
    choice = factor(choice, levels = choices),
    monkey_initial = substr(monkey, 1, 1),
    sex = monkey_sex[monkey_initial],
    rank = monkey_rank[monkey_initial]
  )

# PANEL A: Social vs Non-Social Context
raw$context_binary <- ifelse(raw$context == "solo", "Non-Social", "Social")
A_df <- raw %>%
  group_by(context_binary, choice) %>%
  summarise(n = n(), .groups = 'drop') %>%
  group_by(context_binary) %>%
  mutate(prop = n / sum(n))

pA <- ggplot(A_df, aes(x = context_binary, y = prop, fill = choice)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  scale_fill_manual(values = choice_colors, name = "Choice Type") +
  labs(title = NULL, x = "Social Context", y = "Proportion of Choices") +
  theme_classic(base_size = 18) +
  theme(legend.position = "top")
ggsave("panel_A_behavioral.png", pA, width = 7, height = 6, dpi = 400)
# INTERPRETATION: Shows how the presence of social partners affects the distribution of choices.

# PANEL B: Choice Proportions by Social Complexity
B_df <- raw %>%
  group_by(context, choice) %>%
  summarise(n = n(), .groups = 'drop') %>%
  group_by(context) %>%
  mutate(prop = n / sum(n))

pB <- ggplot(B_df, aes(x = context, y = prop, fill = choice)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  scale_fill_manual(values = choice_colors, name = "Choice Type") +
  labs(title = NULL, x = "Social Complexity", y = "Proportion of Choices") +
  theme_classic(base_size = 18) +
  theme(legend.position = "top")
ggsave("panel_B_behavioral.png", pB, width = 7, height = 6, dpi = 400)
# INTERPRETATION: Shows how choice proportions change as social complexity increases (solo, duo, trio).

# PANEL C: Individual Choice Patterns by Social Complexity
C_df <- raw %>%
  group_by(monkey_initial, sex, rank, context, choice) %>%
  summarise(n = n(), .groups = 'drop') %>%
  group_by(monkey_initial, context) %>%
  mutate(prop = n / sum(n))

# Order panels: males (F D E), females (C I A) or as in your data
C_df$monkey_initial <- factor(C_df$monkey_initial, levels = c("F", "D", "E", "C", "I", "A"))
C_df$sex <- factor(C_df$sex, levels = c("M", "F"))
C_df$rank <- factor(C_df$rank, levels = c(1,2,3))

pC <- ggplot(C_df, aes(x = context, y = prop, fill = choice)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  scale_fill_manual(values = choice_colors, name = "Choice Type") +
  facet_wrap(~monkey_initial, nrow = 2, labeller = label_both) +
  labs(title = NULL, x = "Social Complexity", y = "Proportion of Choices") +
  theme_classic(base_size = 16) +
  theme(legend.position = "none", strip.background = element_rect(fill = "#e0e0e0"))
ggsave("panel_C_behavioral.png", pC, width = 12, height = 6, dpi = 400)
# INTERPRETATION: Shows how each individual's choice pattern changes with social context.

# PANEL D: Individual Exploration Rates by Sex
D_df <- raw %>%
  group_by(monkey_initial, sex) %>%
  summarise(explore_rate = mean(choice == "explore"), .groups = 'drop')
D_df$monkey_initial <- factor(D_df$monkey_initial, levels = c("F", "D", "E", "C", "I", "A"))
D_df$sex <- factor(D_df$sex, levels = c("M", "F"))

pD <- ggplot(D_df, aes(x = monkey_initial, y = explore_rate, fill = sex)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_manual(values = c("M" = "#FF7043", "F" = "#BA68C8"), name = "Sex", labels = c("Male", "Female")) +
  geom_vline(xintercept = 3.5, linetype = "dashed", color = "grey40") +
  labs(title = NULL, x = "Individual (by Sex)", y = "Exploration Rate") +
  theme_classic(base_size = 18) +
  theme(legend.position = "top") +
  annotate("text", x = 2, y = max(D_df$explore_rate) * 1.05, label = "Males", color = "#FF7043", size = 6, fontface = "bold") +
  annotate("text", x = 5.5, y = max(D_df$explore_rate) * 1.05, label = "Females", color = "#BA68C8", size = 6, fontface = "bold")
ggsave("panel_D_behavioral.png", pD, width = 8, height = 6, dpi = 400)
# INTERPRETATION: Shows each individual's exploration rate, grouped by sex.

cat('Panels saved as panel_A_behavioral.png, panel_B_behavioral.png, panel_C_behavioral.png, panel_D_behavioral.png\n') 