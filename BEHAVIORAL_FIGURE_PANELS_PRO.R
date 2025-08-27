#!/usr/bin/env Rscript

library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(scales)

# Correct mapping from user
monkey_info <- data.frame(
  monkey_initial = c('F','D','E','C','I','A'),
  name = c('Fran','Dali','Ebi','Chocolat','Ice','Anemone'),
  sex = c('Male','Male','Male','Female','Female','Female'),
  rank = c(1,2,3,1,2,3),
  stringsAsFactors = FALSE
)

# Color palette from user
choice_colors <- c("explore" = "#4CAF50", "exploit" = "#E57373", "none" = "#64B5F6")
context_names <- c("solo", "duo", "trio")

# Load data
raw <- read.csv("Explore Exploit Dataset.csv")
choices <- c("exploit", "explore", "none")

raw <- raw %>%
  mutate(
    context = factor(CONDITION, levels = context_names),
    choice = case_when(
      grepl("^explore", OUTCOME) ~ "explore",
      OUTCOME %in% c("exploit_pink", "exploit_blue") ~ "exploit",
      TRUE ~ "none"
    ),
    choice = factor(choice, levels = choices),
    monkey_initial = substr(monkey, 1, 1)
  ) %>%
  left_join(monkey_info, by = "monkey_initial")

# PANEL A: Social vs Non-Social Context
raw$context_binary <- ifelse(raw$context == "solo", "Non-Social", "Social")
A_df <- raw %>%
  group_by(context_binary, choice) %>%
  summarise(n = n(), .groups = 'drop') %>%
  group_by(context_binary) %>%
  mutate(prop = n / sum(n))
cat("\nPANEL A SUMMARY TABLE:\n")
print(A_df)

pA <- ggplot(A_df, aes(x = context_binary, y = prop, fill = choice)) +
  geom_col(width = 0.7, color = "black", position = position_stack()) +
  scale_fill_manual(values = choice_colors, name = "Choice Type") +
  labs(title = "A. Social vs Non-Social Context", x = "Social Context", y = "Proportion of Choices") +
  theme_classic(base_size = 18) +
  theme(legend.position = "top", plot.title = element_text(face = "bold", hjust = 0.5))
ggsave("panel_A_behavioral_PRO.png", pA, width = 7, height = 6, dpi = 400)
ggsave("panel_A_behavioral_PRO.pdf", pA, width = 7, height = 6)

# PANEL B: Choice Proportions by Social Complexity
B_df <- raw %>%
  group_by(context, choice) %>%
  summarise(n = n(), .groups = 'drop') %>%
  group_by(context) %>%
  mutate(prop = n / sum(n))
cat("\nPANEL B SUMMARY TABLE:\n")
print(B_df)

pB <- ggplot(B_df, aes(x = context, y = prop, fill = choice)) +
  geom_col(width = 0.7, color = "black", position = position_stack()) +
  scale_fill_manual(values = choice_colors, name = "Choice Type") +
  labs(title = "B. Choice Proportions by Social Complexity", x = "Social Complexity", y = "Proportion of Choices") +
  theme_classic(base_size = 18) +
  theme(legend.position = "top", plot.title = element_text(face = "bold", hjust = 0.5))
ggsave("panel_B_behavioral_PRO.png", pB, width = 7, height = 6, dpi = 400)
ggsave("panel_B_behavioral_PRO.pdf", pB, width = 7, height = 6)

# PANEL C: Individual Choice Patterns by Social Complexity
C_df <- raw %>%
  group_by(monkey_initial, sex, rank, context, choice) %>%
  summarise(n = n(), .groups = 'drop') %>%
  group_by(monkey_initial, context) %>%
  mutate(prop = n / sum(n))
cat("\nPANEL C SUMMARY TABLE (first 20 rows):\n")
print(head(C_df, 20))

# Order panels: males (F, D, E), females (C, I, A), each by rank
C_df$monkey_initial <- factor(C_df$monkey_initial, levels = c("F", "D", "E", "C", "I", "A"))
C_df$sex <- factor(C_df$sex, levels = c("Male", "Female"))
C_df$rank <- factor(C_df$rank, levels = c(1,2,3), labels = c("Dominant","Intermediate","Submissive"))

pC <- ggplot(C_df, aes(x = context, y = prop, fill = choice)) +
  geom_col(width = 0.7, color = "black", position = position_stack()) +
  facet_wrap(~monkey_initial, nrow = 2, labeller = as_labeller(setNames(
    paste0(monkey_info$monkey_initial, "\n", monkey_info$sex, ", Rank ", monkey_info$rank),
    monkey_info$monkey_initial))) +
  scale_fill_manual(values = choice_colors, name = "Choice Type") +
  labs(title = "C. Individual Choice Patterns by Social Complexity", x = "Social Complexity", y = "Proportion of Choices") +
  theme_classic(base_size = 16) +
  theme(legend.position = "top", plot.title = element_text(face = "bold", hjust = 0.5), strip.background = element_rect(fill = "#e0e0e0"))
ggsave("panel_C_behavioral_PRO.png", pC, width = 12, height = 6, dpi = 400)
ggsave("panel_C_behavioral_PRO.pdf", pC, width = 12, height = 6)

# PANEL D: Individual Exploration Rates by Sex
D_df <- raw %>%
  group_by(monkey_initial, sex) %>%
  summarise(explore_rate = mean(choice == "explore"), .groups = 'drop')

# Ensure all individuals are present, even if their rate is zero
D_df <- monkey_info %>%
  select(monkey_initial, sex) %>%
  left_join(D_df, by = c("monkey_initial", "sex")) %>%
  mutate(explore_rate = ifelse(is.na(explore_rate), 0, explore_rate))

cat("\nPANEL D SUMMARY TABLE (all individuals):\n")
print(D_df)

D_df$monkey_initial <- factor(D_df$monkey_initial, levels = c("F", "D", "E", "C", "I", "A"))
D_df$sex <- factor(D_df$sex, levels = c("Male", "Female"))

pD <- ggplot(D_df, aes(x = monkey_initial, y = explore_rate, fill = sex)) +
  geom_col(width = 0.7, color = "black") +
  scale_fill_manual(values = c("Male" = "#E57373", "Female" = "#64B5F6"), name = "Sex", labels = c("Male", "Female")) +
  geom_vline(xintercept = 3.5, linetype = "dashed", color = "grey40") +
  labs(title = "D. Individual Exploration Rates by Sex", x = "Individual (by Sex)", y = "Exploration Rate") +
  theme_classic(base_size = 18) +
  theme(legend.position = "top", plot.title = element_text(face = "bold", hjust = 0.5)) +
  annotate("text", x = 2, y = max(D_df$explore_rate) * 1.05 + 0.01, label = "Males", color = "#E57373", size = 6, fontface = "bold") +
  annotate("text", x = 5.5, y = max(D_df$explore_rate) * 1.05 + 0.01, label = "Females", color = "#64B5F6", size = 6, fontface = "bold")
ggsave("panel_D_behavioral_PRO.png", pD, width = 8, height = 6, dpi = 400)
ggsave("panel_D_behavioral_PRO.pdf", pD, width = 8, height = 6)

cat("\nCHOICE COUNTS BY SEX:\n")
print(table(raw$choice, raw$sex))

cat("\nCHOICE COUNTS FOR EACH FEMALE (C, I, A):\n")
print(raw %>% filter(sex == "Female") %>% count(monkey_initial, choice))

cat('Panels and summary tables complete.\n') 