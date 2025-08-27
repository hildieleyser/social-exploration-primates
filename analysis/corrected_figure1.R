# Corrected Figure 1: Behavioral Measurements
library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)

cat("Creating corrected Figure 1...\n")

# Load data
data_raw <- read.csv("data/Explore Exploit Dataset.csv", stringsAsFactors = FALSE)
data_clean <- data_raw[data_raw$TRIAL_TYPE == "OIT_RE", ]

# Clean outcomes
outcome_clean <- character(nrow(data_clean))
outcome_clean[grepl("explore", tolower(data_clean$OUTCOME))] <- "explore"
outcome_clean[grepl("exploit", tolower(data_clean$OUTCOME))] <- "exploit"
outcome_clean[grepl("none|stop", tolower(data_clean$OUTCOME))] <- "none"

data_clean$outcome_clean <- outcome_clean
data_clean$social_condition <- ifelse(data_clean$CONDITION == "solo", "Non-Social", "Social")
data_clean$social_complexity <- factor(data_clean$CONDITION, levels = c("solo", "duo", "trio"))
data_clean$monkey_id <- factor(data_clean$monkey)

# Remove missing
data_clean <- data_clean[!is.na(data_clean$outcome_clean), ]
data_clean <- data_clean[data_clean$outcome_clean != "", ]
data_clean <- data_clean[complete.cases(data_clean[c("RELATIVE_RANK", "SUBJECTIVE_CHOSEN_VALUE")]), ]

# Add sex and initial information
sex_info <- data.frame(
  monkey = c("FRAN", "DALI", "EBI", "ANEMONE", "CHOCOLAT", "ICE"),
  sex = c("Male", "Male", "Male", "Female", "Female", "Female"),
  initial = c("F", "D", "E", "A", "C", "I"),
  stringsAsFactors = FALSE
)

data_clean <- merge(data_clean, sex_info, by.x = "monkey", by.y = "monkey", all.x = TRUE)

# Colors
bio_colors <- c("explore" = "#D55E00", "exploit" = "#009E73", "none" = "#0072B2")
rank_colors <- c("1" = "#E31A1C", "2" = "#FF7F00", "3" = "#1F78B4")

# Panel A: Overall proportions by social vs non-social condition
panel_a_data <- data_clean %>%
  group_by(social_condition, outcome_clean) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(social_condition) %>%
  mutate(proportion = count / sum(count),
         se = sqrt(proportion * (1 - proportion) / sum(count)))

panel_a <- ggplot(panel_a_data, aes(x = social_condition, y = proportion, fill = outcome_clean)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7, 
           color = "black", linewidth = 0.3, alpha = 0.9) +
  geom_errorbar(aes(ymin = pmax(0, proportion - 1.96*se), ymax = proportion + 1.96*se),
                position = position_dodge(width = 0.8), width = 0.15, linewidth = 0.4) +
  scale_fill_manual(values = bio_colors[c("explore", "exploit", "none")],
                    name = "Choice Type",
                    labels = c("Explore", "Exploit", "No Choice")) +
  scale_y_continuous(labels = function(x) paste0(round(x*100), "%"),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(title = "A", subtitle = "Choice proportions by social context",
       x = "Social Context", y = "Proportion of Choices") +
  theme_classic(base_size = 10) +
  theme(plot.title = element_text(face = "bold", size = 12, hjust = -0.1),
        legend.position = "bottom")

# Panel B: ALL choices by social complexity (CORRECTED TITLE)
panel_b_data <- data_clean %>%
  group_by(social_complexity, outcome_clean) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(social_complexity) %>%
  mutate(proportion = count / sum(count),
         se = sqrt(proportion * (1 - proportion) / sum(count)))

panel_b <- ggplot(panel_b_data, aes(x = social_complexity, y = proportion, fill = outcome_clean)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7,
           color = "black", linewidth = 0.3, alpha = 0.9) +
  geom_errorbar(aes(ymin = pmax(0, proportion - 1.96*se), ymax = proportion + 1.96*se),
                position = position_dodge(width = 0.8), width = 0.15, linewidth = 0.4) +
  scale_fill_manual(values = bio_colors[c("explore", "exploit", "none")],
                    name = "Choice Type",
                    labels = c("Explore", "Exploit", "No Choice")) +
  scale_x_discrete(labels = c("Solo", "Duo", "Trio")) +
  scale_y_continuous(labels = function(x) paste0(round(x*100), "%"),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(title = "B", subtitle = "Choice Proportions by Social Complexity",
       x = "Social Complexity", y = "Proportion of Choices") +
  theme_classic(base_size = 10) +
  theme(plot.title = element_text(face = "bold", size = 12, hjust = -0.1),
        legend.position = "bottom")

# Panel C: Individual choice patterns by rank (CORRECTED)
panel_c_data <- data_clean %>%
  group_by(monkey, initial, sex, RELATIVE_RANK, outcome_clean) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(monkey, initial, sex, RELATIVE_RANK) %>%
  mutate(total = sum(count),
         proportion = count / total,
         se = sqrt(proportion * (1 - proportion) / total),
         rank_factor = factor(RELATIVE_RANK))

# Order by sex (Males first, then Females)
panel_c_data$initial <- factor(panel_c_data$initial, levels = c("F", "D", "E", "A", "C", "I"))

panel_c <- ggplot(panel_c_data, aes(x = initial, y = proportion, fill = rank_factor)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7,
           color = "black", linewidth = 0.3, alpha = 0.9) +
  geom_errorbar(aes(ymin = pmax(0, proportion - 1.96*se), ymax = proportion + 1.96*se),
                position = position_dodge(width = 0.8), width = 0.15, linewidth = 0.4) +
  # Add vertical line to separate sexes  
  geom_vline(xintercept = 3.5, linetype = "dashed", color = "grey60", linewidth = 0.5) +
  # Add sex labels
  annotate("text", x = 2, y = max(panel_c_data$proportion) * 0.95, 
           label = "Males", fontface = "bold", size = 3.5) +
  annotate("text", x = 5, y = max(panel_c_data$proportion) * 0.95, 
           label = "Females", fontface = "bold", size = 3.5) +
  scale_fill_manual(values = rank_colors,
                    name = "Ingroup Rank",
                    labels = c("Rank 1", "Rank 2", "Rank 3")) +
  scale_y_continuous(labels = function(x) paste0(round(x*100), "%"),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(title = "C", subtitle = "Individual Choice Patterns by Rank",
       x = "Individual (by Sex)", y = "Proportion of Choices") +
  theme_classic(base_size = 10) +
  theme(plot.title = element_text(face = "bold", size = 12, hjust = -0.1),
        legend.position = "bottom")

# Panel D: Individual exploration rates by sex (CORRECTED SEX LABELS)
panel_d_data <- data_clean %>%
  filter(outcome_clean == "explore") %>%
  group_by(monkey, initial, sex) %>%
  summarise(count = n(), .groups = "drop") %>%
  left_join(
    data_clean %>%
      group_by(monkey, initial, sex) %>%
      summarise(total = n(), .groups = "drop"),
    by = c("monkey", "initial", "sex")
  ) %>%
  mutate(proportion = count / total,
         se = sqrt(proportion * (1 - proportion) / total))

# Order correctly (Males first: F,D,E then Females: A,C,I)
panel_d_data$initial <- factor(panel_d_data$initial, levels = c("F", "D", "E", "A", "C", "I"))

panel_d <- ggplot(panel_d_data, aes(x = initial, y = proportion, fill = sex)) +
  geom_col(width = 0.7, color = "black", linewidth = 0.3, alpha = 0.9) +
  geom_errorbar(aes(ymin = pmax(0, proportion - 1.96*se), ymax = proportion + 1.96*se),
                width = 0.15, linewidth = 0.4) +
  # Add vertical line to separate sexes
  geom_vline(xintercept = 3.5, linetype = "dashed", color = "grey60", linewidth = 0.5) +
  # CORRECTED sex labels (Males on left, Females on right)
  annotate("text", x = 2, y = max(panel_d_data$proportion) * 0.95, 
           label = "Males", fontface = "bold", size = 3.5) +
  annotate("text", x = 5, y = max(panel_d_data$proportion) * 0.95, 
           label = "Females", fontface = "bold", size = 3.5) +
  scale_fill_manual(values = c("Male" = "#4682B4", "Female" = "#DC143C"), name = "Sex") +
  scale_y_continuous(labels = function(x) paste0(round(x*100), "%"),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(title = "D", subtitle = "Individual Exploration Rates by Sex",
       x = "Individual (by Sex)", y = "Exploration Rate") +
  theme_classic(base_size = 10) +
  theme(plot.title = element_text(face = "bold", size = 12, hjust = -0.1),
        legend.position = "bottom")

# Save individual panels
ggsave("results/figures/Figure1_Panel_A_Corrected.png", panel_a, 
       width = 180, height = 120, units = "mm", dpi = 300)
ggsave("results/figures/Figure1_Panel_B_Corrected.png", panel_b, 
       width = 180, height = 120, units = "mm", dpi = 300)
ggsave("results/figures/Figure1_Panel_C_Corrected.png", panel_c, 
       width = 180, height = 120, units = "mm", dpi = 300)
ggsave("results/figures/Figure1_Panel_D_Corrected.png", panel_d, 
       width = 180, height = 120, units = "mm", dpi = 300)

cat("Corrected Figure 1 complete with all requested fixes!\n")
cat("- Panel B title: Choice Proportions by Social Complexity\n")
cat("- Panel C title: Individual Choice Patterns by Rank\n")
cat("- Panel C: Different colors for ranks 1,2,3 with legend\n")
cat("- Panel C: Sex grouping with Males (top) and Females (bottom)\n")
cat("- Panel D title: Individual Exploration Rates by Sex\n")
cat("- Panel D: Corrected sex labels (Males left, Females right)\n")
