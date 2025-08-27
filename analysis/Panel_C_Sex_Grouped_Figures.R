# =============================================================================
# Panel C with Sex-Grouped Monkeys and Initials
# Social Frames of Reference in Explore-Exploit Decision-Making
# =============================================================================

# Load required libraries
library(nnet)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(viridis)
library(grid)

# Set publication-quality parameters
fig_width <- 200  # mm
fig_height <- 140 # mm 
fig_dpi <- 300
base_font_size <- 14
title_font_size <- 16
axis_font_size <- 12

# Professional color palette
bio_colors <- c(
  "explore" = "#D55E00",    # Orange-red
  "exploit" = "#009E73",    # Blue-green  
  "none" = "#0072B2",       # Blue
  "Social" = "#E69F00",     # Orange
  "Non-Social" = "#56B4E9"  # Light blue
)

# =============================================================================
# 1. DATA LOADING AND PREPARATION
# =============================================================================

cat("Loading and preparing data with sex information...\n")

# Load dataset
data_raw <- read.csv("data/Explore Exploit Dataset.csv", stringsAsFactors = FALSE)
data_clean <- data_raw[data_raw$TRIAL_TYPE == "OIT_RE", ]

# Clean outcome variable
outcome_clean <- character(nrow(data_clean))
outcome_clean[grepl("explore", tolower(data_clean$OUTCOME))] <- "explore"
outcome_clean[grepl("exploit", tolower(data_clean$OUTCOME))] <- "exploit"
outcome_clean[grepl("none|stop|NONE", tolower(data_clean$OUTCOME))] <- "none"

# Remove rows with empty/missing outcomes
data_clean$outcome_clean <- outcome_clean
data_clean <- data_clean[data_clean$outcome_clean != "", ]
data_clean <- data_clean[!is.na(data_clean$outcome_clean), ]

# Create social vs non-social conditions
data_clean$social_condition <- ifelse(data_clean$CONDITION == "solo", "Non-Social", "Social")
data_clean$monkey_id <- factor(data_clean$monkey)

# ADD SEX INFORMATION based on your input
data_clean$sex <- case_when(
  data_clean$monkey %in% c("FRAN", "DALI", "EBI") ~ "Male",
  data_clean$monkey %in% c("ANEMONE", "CHOCOLAT", "ICE") ~ "Female",
  TRUE ~ "Unknown"
)

# CREATE INITIALS
data_clean$monkey_initial <- case_when(
  data_clean$monkey == "FRAN" ~ "F",
  data_clean$monkey == "DALI" ~ "D", 
  data_clean$monkey == "EBI" ~ "E",
  data_clean$monkey == "ANEMONE" ~ "A",
  data_clean$monkey == "CHOCOLAT" ~ "C",
  data_clean$monkey == "ICE" ~ "I",
  TRUE ~ substr(data_clean$monkey, 1, 1)
)

# Create combined sex-initial factor for ordering
data_clean$sex_initial <- paste0(data_clean$sex, ": ", data_clean$monkey_initial)
data_clean$sex_initial <- factor(data_clean$sex_initial, 
                                levels = c("Male: F", "Male: D", "Male: E", 
                                          "Female: A", "Female: C", "Female: I"))

# Remove missing data
data_clean <- data_clean[complete.cases(data_clean[c("RELATIVE_RANK", "SUBJECTIVE_CHOSEN_VALUE")]), ]

cat("Data preparation complete.\n")
cat("Final sample size:", nrow(data_clean), "trials\n")
cat("Males: FRAN (F), DALI (D), EBI (E)\n")
cat("Females: ANEMONE (A), CHOCOLAT (C), ICE (I)\n")

# =============================================================================
# 2. PANEL C: SEX-GROUPED INDIVIDUAL EXPLORATION RATES
# =============================================================================

cat("Creating Panel C: Sex-grouped individual exploration rates...\n")

# Panel C: Individual differences grouped by sex
panel_c_data <- data_clean %>%
  filter(outcome_clean == "explore") %>%
  group_by(monkey_initial, sex, social_condition) %>%
  summarise(count = n(), .groups = "drop") %>%
  # Calculate total trials per monkey per condition
  left_join(
    data_clean %>%
      group_by(monkey_initial, sex, social_condition) %>%
      summarise(total = n(), .groups = "drop"),
    by = c("monkey_initial", "sex", "social_condition")
  ) %>%
  mutate(proportion = count / total,
         se = sqrt(proportion * (1 - proportion) / total))

# Create sex-ordered factor for plotting
panel_c_data$monkey_ordered <- factor(
  paste0(panel_c_data$sex, ": ", panel_c_data$monkey_initial),
  levels = c("Male: F", "Male: D", "Male: E", "Female: A", "Female: C", "Female: I")
)

# Create the panel with sex grouping
panel_c <- ggplot(panel_c_data, aes(x = monkey_ordered, y = proportion, fill = social_condition)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7,
           color = "black", linewidth = 0.5, alpha = 0.9) +
  geom_errorbar(aes(ymin = pmax(0, proportion - 1.96*se), ymax = proportion + 1.96*se),
                position = position_dodge(width = 0.8), width = 0.2, linewidth = 0.6) +
  scale_fill_manual(values = bio_colors[c("Non-Social", "Social")],
                    name = "Context") +
  scale_x_discrete(labels = function(x) {
    # Extract just the initial from "Sex: Initial" format
    sapply(strsplit(x, ": "), function(parts) parts[2])
  }) +
  scale_y_continuous(labels = function(x) paste0(round(x*100), "%"),
                     expand = expansion(mult = c(0, 0.1)),
                     limits = c(0, max(panel_c_data$proportion) * 1.15)) +
  
  # Add sex group labels
  annotate("text", x = 2, y = max(panel_c_data$proportion) * 1.05, 
           label = "Males", size = 5, fontface = "bold") +
  annotate("text", x = 5, y = max(panel_c_data$proportion) * 1.05, 
           label = "Females", size = 5, fontface = "bold") +
  
  # Add vertical separator between sexes
  geom_vline(xintercept = 3.5, linetype = "dashed", color = "grey60", linewidth = 0.8) +
  
  labs(title = "C", subtitle = "Individual exploration rates by sex",
       x = "Individual (grouped by sex)", y = "Exploration Rate") +
  theme_classic(base_size = base_font_size) +
  theme(
    plot.title = element_text(face = "bold", size = title_font_size, hjust = 0),
    plot.subtitle = element_text(size = base_font_size, margin = margin(b = 20)),
    axis.title = element_text(size = base_font_size, color = "black", face = "bold"),
    axis.text = element_text(size = axis_font_size, color = "black"),
    axis.text.x = element_text(size = axis_font_size + 2, face = "bold"),  # Larger initials
    legend.position = "bottom",
    legend.title = element_text(size = base_font_size, face = "bold"),
    legend.text = element_text(size = axis_font_size),
    legend.key.size = unit(0.6, "cm"),
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.3),
    axis.line = element_line(color = "black", linewidth = 0.6),
    axis.ticks = element_line(color = "black", linewidth = 0.4),
    plot.margin = margin(10, 10, 10, 10)
  )

# Save the improved Panel C
ggsave("results/figures/Figure1_Panel_C_SexGrouped.png", panel_c,
       width = fig_width, height = fig_height, units = "mm", dpi = fig_dpi)

# Print summary statistics by sex
cat("\n=== EXPLORATION RATES BY SEX ===\n")
sex_summary <- panel_c_data %>%
  group_by(sex, social_condition) %>%
  summarise(
    mean_exploration = mean(proportion),
    se_exploration = sd(proportion) / sqrt(n()),
    .groups = "drop"
  )
print(sex_summary)

# =============================================================================
# 3. CREATE UPDATED COMPLETE FIGURE 1 WITH SEX-GROUPED PANEL C
# =============================================================================

cat("Creating complete Figure 1 with sex-grouped Panel C...\n")

# Panel A: Overall proportions (reuse from fixed version)
panel_a_data <- data_clean %>%
  group_by(social_condition, outcome_clean) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(social_condition) %>%
  mutate(proportion = count / sum(count),
         se = sqrt(proportion * (1 - proportion) / sum(count)))

panel_a <- ggplot(panel_a_data, aes(x = social_condition, y = proportion, fill = outcome_clean)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7, 
           color = "black", linewidth = 0.5, alpha = 0.9) +
  geom_errorbar(aes(ymin = pmax(0, proportion - 1.96*se), ymax = proportion + 1.96*se),
                position = position_dodge(width = 0.8), width = 0.2, linewidth = 0.6) +
  scale_fill_manual(values = bio_colors[c("explore", "exploit", "none")],
                    name = "Choice Type",
                    labels = c("Explore", "Exploit", "No Choice")) +
  scale_y_continuous(labels = function(x) paste0(round(x*100), "%"),
                     expand = expansion(mult = c(0, 0.1)),
                     limits = c(0, max(panel_a_data$proportion) * 1.15)) +
  labs(title = "A", subtitle = "Choice proportions by social context",
       x = "Social Context", y = "Proportion of Choices") +
  theme_classic(base_size = base_font_size) +
  theme(
    plot.title = element_text(face = "bold", size = title_font_size, hjust = 0),
    plot.subtitle = element_text(size = base_font_size, margin = margin(b = 20)),
    axis.title = element_text(size = base_font_size, color = "black", face = "bold"),
    axis.text = element_text(size = axis_font_size, color = "black"),
    legend.position = "bottom",
    legend.title = element_text(size = base_font_size, face = "bold"),
    legend.text = element_text(size = axis_font_size),
    legend.key.size = unit(0.6, "cm"),
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.3),
    axis.line = element_line(color = "black", linewidth = 0.6),
    axis.ticks = element_line(color = "black", linewidth = 0.4),
    plot.margin = margin(10, 10, 10, 10)
  )

# Panel B: Social complexity (reuse from fixed version)
panel_b_data <- data_clean %>%
  filter(outcome_clean %in% c("explore", "exploit")) %>%
  group_by(CONDITION, outcome_clean) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(CONDITION) %>%
  mutate(proportion = count / sum(count),
         se = sqrt(proportion * (1 - proportion) / sum(count)))

panel_b <- ggplot(panel_b_data, aes(x = factor(CONDITION, levels = c("solo", "duo", "trio")), 
                                    y = proportion, fill = outcome_clean)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7,
           color = "black", linewidth = 0.5, alpha = 0.9) +
  geom_errorbar(aes(ymin = pmax(0, proportion - 1.96*se), ymax = proportion + 1.96*se),
                position = position_dodge(width = 0.8), width = 0.2, linewidth = 0.6) +
  scale_fill_manual(values = bio_colors[c("explore", "exploit")],
                    name = "Choice Type",
                    labels = c("Explore", "Exploit")) +
  scale_x_discrete(labels = c("Solo", "Duo", "Trio")) +
  scale_y_continuous(labels = function(x) paste0(round(x*100), "%"),
                     expand = expansion(mult = c(0, 0.1)),
                     limits = c(0, max(panel_b_data$proportion) * 1.15)) +
  labs(title = "B", subtitle = "Explore vs exploit by social complexity",
       x = "Social Complexity", y = "Proportion of Choices") +
  theme_classic(base_size = base_font_size) +
  theme(
    plot.title = element_text(face = "bold", size = title_font_size, hjust = 0),
    plot.subtitle = element_text(size = base_font_size, margin = margin(b = 20)),
    axis.title = element_text(size = base_font_size, color = "black", face = "bold"),
    axis.text = element_text(size = axis_font_size, color = "black"),
    legend.position = "bottom",
    legend.title = element_text(size = base_font_size, face = "bold"),
    legend.text = element_text(size = axis_font_size),
    legend.key.size = unit(0.6, "cm"),
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.3),
    axis.line = element_line(color = "black", linewidth = 0.6),
    axis.ticks = element_line(color = "black", linewidth = 0.4),
    plot.margin = margin(10, 10, 10, 10)
  )

# Combine all panels
fig1_sex_grouped <- grid.arrange(panel_a, panel_b, panel_c, nrow = 1,
                                top = textGrob("Figure 1. Behavioral Measurements (Sex-Grouped)", 
                                              gp = gpar(fontsize = title_font_size + 2, fontface = "bold")))

# Save the complete sex-grouped figure
ggsave("results/figures/Figure1_SexGrouped_Final.png", fig1_sex_grouped,
       width = fig_width*3, height = fig_height, units = "mm", dpi = fig_dpi)

ggsave("results/figures/Figure1_SexGrouped_Final.tiff", fig1_sex_grouped,
       width = fig_width*3, height = fig_height, units = "mm", dpi = fig_dpi,
       compression = "lzw")

cat("\n=============================================================================\n")
cat("SEX-GROUPED FIGURE 1 COMPLETE!\n") 
cat("=============================================================================\n")
cat("Generated figures:\n")
cat("- Figure1_SexGrouped_Final.png/.tiff (complete figure with sex grouping)\n")
cat("- Figure1_Panel_C_SexGrouped.png (individual Panel C)\n")
cat("\nPanel C improvements:\n")
cat("- Monkeys grouped by sex (Males: F, D, E | Females: A, C, I)\n")
cat("- Only initials shown on x-axis\n")
cat("- Clear visual separation between sex groups\n") 
cat("- Sex labels added above groups\n")
cat("- Larger, bold initials for better readability\n")
cat("=============================================================================\n") 