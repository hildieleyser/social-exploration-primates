# REPRODUCE_FIGURE_PANELS.R
# Publication-style behavioral panels (A–D) for Explore Exploit Dataset.csv

library(tidyverse)
library(scales)

# 1. Load Data
data <- read_csv('Explore Exploit Dataset.csv')

# 2. Add SEX and INDIVIDUAL_CODE columns
male_monkeys <- c("FRAN", "DALI", "EBI")
female_monkeys <- c("CHOCOLAT", "ICE", "ANEMONE")
data <- data %>%
  mutate(
    SEX = case_when(
      monkey %in% male_monkeys ~ "Male",
      monkey %in% female_monkeys ~ "Female",
      TRUE ~ NA_character_
    ),
    INDIVIDUAL_CODE = case_when(
      monkey == "FRAN" ~ "F",
      monkey == "DALI" ~ "D",
      monkey == "EBI" ~ "E",
      monkey == "CHOCOLAT" ~ "C",
      monkey == "ANEMONE" ~ "A",
      monkey == "ICE" ~ "I",
      TRUE ~ monkey
    )
  )

# 3. Clean OUTCOME
data <- data %>%
  mutate(
    OUTCOME_CLEAN = case_when(
      str_starts(tolower(OUTCOME), "explore") ~ "Explore",
      str_starts(tolower(OUTCOME), "exploit") ~ "Exploit",
      tolower(OUTCOME) == "none" ~ "No Choice",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(OUTCOME_CLEAN))

# 4. Add Social Context
data <- data %>%
  mutate(
    SOCIAL_CONTEXT = if_else(CONDITION == "solo", "Non-Social", "Social"),
    SOCIAL_COMPLEXITY = factor(CONDITION, levels = c("solo", "duo", "trio"))
  )

# 5. Color Palettes
choice_palette <- c("Explore" = "#3CB371", "Exploit" = "#CD5C5C", "No Choice" = "#4682B4")
sex_palette <- c("Male" = "#FF7F0E", "Female" = "#9467BD")

# 6. Panel A: Social vs Non-Social (Stacked Bar)
panel_A_data <- data %>%
  count(SOCIAL_CONTEXT, OUTCOME_CLEAN) %>%
  group_by(SOCIAL_CONTEXT) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

panel_A <- ggplot(panel_A_data, aes(x = SOCIAL_CONTEXT, y = prop, fill = OUTCOME_CLEAN)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = choice_palette, name = "Choice Type") +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "A", x = "Social Context", y = "Proportion of Choices") +
  theme_classic(base_size = 14) +
  theme(legend.position = "top")

ggsave("panel_A_behavioral_PRO.png", panel_A, width = 6, height = 5, dpi = 300)

# 7. Panel B: Social Complexity (Grouped Bar)
panel_B_data <- data %>%
  count(SOCIAL_COMPLEXITY, OUTCOME_CLEAN) %>%
  group_by(SOCIAL_COMPLEXITY) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

panel_B <- ggplot(panel_B_data, aes(x = SOCIAL_COMPLEXITY, y = prop, fill = OUTCOME_CLEAN)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = choice_palette, name = "Choice Type") +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "B", x = "Social Complexity", y = "Proportion of Choices") +
  theme_classic(base_size = 14) +
  theme(legend.position = "top")

ggsave("panel_B_behavioral_PRO.png", panel_B, width = 8, height = 5, dpi = 300)

# 8. Panel C: Individual Patterns (Faceted Stacked Bar)
panel_C_data <- data %>%
  count(INDIVIDUAL_CODE, SEX, ABSOLUTE_RANK, SOCIAL_COMPLEXITY, OUTCOME_CLEAN) %>%
  group_by(INDIVIDUAL_CODE, SOCIAL_COMPLEXITY) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

panel_C <- ggplot(panel_C_data, aes(x = SOCIAL_COMPLEXITY, y = prop, fill = OUTCOME_CLEAN)) +
  geom_col(width = 0.7) +
  facet_wrap(~INDIVIDUAL_CODE, ncol = 3) +
  scale_fill_manual(values = choice_palette, name = "Choice Type") +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "C", x = "Social Complexity", y = "Proportion of Choices") +
  theme_classic(base_size = 12) +
  theme(legend.position = "top", strip.text = element_text(face = "bold"))

ggsave("panel_C_behavioral_PRO.png", panel_C, width = 12, height = 8, dpi = 300)

# 9. Panel D: Individual Exploration Rates by Sex (Bar Plot)
panel_D_data <- data %>%
  group_by(INDIVIDUAL_CODE, SEX) %>%
  summarise(explore_rate = mean(OUTCOME_CLEAN == "Explore"), .groups = "drop") %>%
  arrange(SEX, INDIVIDUAL_CODE)

panel_D <- ggplot(panel_D_data, aes(x = INDIVIDUAL_CODE, y = explore_rate, fill = SEX)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = sex_palette, name = "Sex") +
  scale_y_continuous(labels = percent_format(), limits = c(0, 0.5)) +
  labs(title = "D", x = "Individual (by Sex)", y = "Exploration Rate") +
  theme_classic(base_size = 14) +
  theme(legend.position = "top")

ggsave("panel_D_behavioral_PRO.png", panel_D, width = 8, height = 5, dpi = 300) 