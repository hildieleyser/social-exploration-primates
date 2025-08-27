# Much simpler layout approach that's easy to understand and adjust
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)

cat("Creating simple, adjustable layout...\n")

# Load and prepare data
data_clean <- read.csv("data/Explore Exploit Dataset.csv") %>%
  mutate(
    social_complexity = factor(CONDITION, levels = c("solo", "duo", "trio")),
    outcome_clean = case_when(
      grepl("explore", tolower(OUTCOME)) ~ "explore",
      grepl("exploit", tolower(OUTCOME)) ~ "exploit", 
      tolower(OUTCOME) %in% c("none", "nonne", "non", "stop") | OUTCOME == "" | is.na(OUTCOME) ~ "none",
      TRUE ~ "none"
    ),
    initial = case_when(
      monkey == "FRAN" ~ "F",
      monkey == "DALI" ~ "D", 
      monkey == "EBI" ~ "E",
      monkey == "ANEMONE" ~ "A",
      monkey == "CHOCOLAT" ~ "C",
      monkey == "ICE" ~ "I"
    ),
    sex = case_when(
      monkey %in% c("FRAN", "DALI", "EBI") ~ "Male",
      monkey %in% c("ANEMONE", "CHOCOLAT", "ICE") ~ "Female"
    )
  ) %>%
  filter(!is.na(outcome_clean) & outcome_clean != "" & 
         !is.na(social_complexity) & 
         !is.na(monkey) & monkey != "")

# Define colors
bio_colors <- c("explore" = "#CD5C5C", "exploit" = "#2E8B57", "none" = "#4682B4")
rank_colors <- c("1" = "#003366", "2" = "#4682B4", "3" = "#87CEEB")

# PANEL A: Social vs Non-Social Context
panel_1a_data <- data_clean %>%
  mutate(social_type = ifelse(social_complexity == "solo", "Non-Social", "Social")) %>%
  group_by(social_type, outcome_clean) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(social_type) %>%
  mutate(total = sum(count),
         proportion = count / total)

panel_1a <- ggplot(panel_1a_data, aes(x = social_type, y = proportion, fill = outcome_clean)) +
  geom_col(width = 0.6, color = "black", linewidth = 0.3, alpha = 0.9) +
  scale_fill_manual(values = bio_colors, name = "Choice Type",
                    labels = c("Explore", "Exploit", "No Choice")) +
  scale_y_continuous(labels = function(x) paste0(round(x*100), "%"), 
                     expand = expansion(mult = c(0, 0.05))) +
  labs(title = "A", subtitle = "Social vs Non-Social Context",
       x = "Social Context", y = "Proportion of Choices") +
  theme_classic(base_size = 18) +
  theme(
    plot.title = element_text(face = "bold", size = 20, hjust = 0),
    plot.subtitle = element_text(size = 16, hjust = 0),
    legend.position = "top",
    legend.title = element_text(face = "bold", size = 15),
    legend.text = element_text(size = 14),
    axis.title = element_text(face = "bold", size = 15),
    axis.text = element_text(size = 14),
    plot.margin = margin(5, 5, 5, 5, "pt")
  )

# PANEL B: Choice Proportions by Social Complexity
panel_1b_data <- data_clean %>%
  group_by(social_complexity, outcome_clean) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(social_complexity) %>%
  mutate(total = sum(count),
         proportion = count / total)

panel_1b <- ggplot(panel_1b_data, aes(x = social_complexity, y = proportion, fill = outcome_clean)) +
  geom_col(width = 0.6, color = "black", linewidth = 0.3, alpha = 0.9, position = "dodge") +
  scale_fill_manual(values = bio_colors, name = "Choice Type",
                    labels = c("Explore", "Exploit", "No Choice")) +
  scale_x_discrete(labels = c("Solo", "Duo", "Trio")) +
  scale_y_continuous(labels = function(x) paste0(round(x*100), "%"),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(title = "B", subtitle = "Choice Proportions by Social Complexity",
       x = "Social Complexity", y = "Proportion of Choices") +
  theme_classic(base_size = 18) +
  theme(
    plot.title = element_text(face = "bold", size = 20, hjust = 0),
    plot.subtitle = element_text(size = 16, hjust = 0),
    legend.position = "top",
    legend.title = element_text(face = "bold", size = 15),
    legend.text = element_text(size = 14),
    axis.title = element_text(face = "bold", size = 15),
    axis.text = element_text(size = 14),
    plot.margin = margin(5, 5, 5, 5, "pt")
  )

# PANEL C: Individual Choice Patterns - SIMPLE APPROACH
panel_1c_data <- data_clean %>%
  group_by(monkey, initial, sex, ABSOLUTE_RANK, social_complexity, outcome_clean) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(monkey, initial, sex, ABSOLUTE_RANK, social_complexity) %>%
  mutate(total = sum(count),
         proportion = count / total) %>%
  ungroup()

panel_1c_data$initial <- factor(panel_1c_data$initial, levels = c("F", "D", "E", "C", "I", "A"))

# Create individual plots for each monkey with colored backgrounds
create_monkey_plot <- function(monkey_initial, bg_color) {
  monkey_data <- panel_1c_data %>% filter(initial == monkey_initial)
  
  ggplot(monkey_data, aes(x = social_complexity, y = proportion, fill = outcome_clean)) +
    geom_col(width = 0.7, color = "black", linewidth = 0.2, alpha = 0.9) +
    scale_fill_manual(values = bio_colors) +
    scale_x_discrete(labels = c("solo", "duo", "trio")) +
    scale_y_continuous(labels = function(x) paste0(round(x*100), "%"),
                       expand = expansion(mult = c(0, 0.05)), limits = c(0, 1)) +
    labs(title = monkey_initial) +
    theme_classic(base_size = 15) +
    theme(
      plot.title = element_text(face = "bold", size = 18, color = "white", hjust = 0.5,
                               margin = margin(2, 2, 2, 2)),
      plot.background = element_rect(fill = bg_color, color = "black"),
      panel.background = element_rect(fill = "white"),
      legend.position = "none",
      axis.title = element_blank(),
      axis.text = element_text(size = 13),
      plot.margin = margin(1, 1, 1, 1, "pt")
    )
}

# Create plots for each monkey
f_plot <- create_monkey_plot("F", "#003366")
d_plot <- create_monkey_plot("D", "#4682B4")
e_plot <- create_monkey_plot("E", "#87CEEB")
c_plot <- create_monkey_plot("C", "#003366")
i_plot <- create_monkey_plot("I", "#4682B4")
a_plot <- create_monkey_plot("A", "#87CEEB")

# SIMPLE APPROACH: Build Panel C step by step

# Step 1: Create just the plots in a 2x3 grid
plots_only <- arrangeGrob(
  f_plot, d_plot, e_plot,
  c_plot, i_plot, a_plot,
  nrow = 2, ncol = 3
)

# Step 2: Add title and subtitle above the plots
title_area <- arrangeGrob(
  textGrob("C", gp = gpar(fontsize = 20, fontface = "bold"), 
           x = 0, y = 0.8, hjust = 0, vjust = 1),
  textGrob("Individual Choice Patterns by Social Complexity", 
           gp = gpar(fontsize = 16), x = 0, y = 0.3, hjust = 0, vjust = 1),
  nrow = 1
)

# Step 3: Combine title and plots
plots_with_title <- arrangeGrob(
  title_area,
  plots_only,
  nrow = 2, 
  heights = c(0.15, 0.85)  # ADJUST THESE: title takes 15%, plots take 85%
)

# Step 4: Add Y-axis label to the left
with_y_label <- arrangeGrob(
  textGrob("Proportion of Choices", gp = gpar(fontsize = 15, fontface = "bold"), 
           x = 0.5, y = 0.5, hjust = 0.5, vjust = 0.5, rot = 90),
  plots_with_title,
  nrow = 1,
  widths = c(0.05, 0.95)  # ADJUST THESE: Y-label takes 8%, plots take 92%
)

# Step 5: Add X-axis label at the bottom
with_x_label <- arrangeGrob(
  with_y_label,
  textGrob("Social Complexity", gp = gpar(fontsize = 15, fontface = "bold"),
           x = 0.5, y = 0.5, hjust = 0.5, vjust = 0.5),
  nrow = 2,
  heights = c(0.95, 0.05)  # ADJUST THESE: plots take 92%, X-label takes 8%
)

# Step 6: Add Male/Female labels to the right
with_sex_labels <- arrangeGrob(
  with_x_label,
  arrangeGrob(
    textGrob("Male", gp = gpar(fontsize = 15, fontface = "bold"), 
             x = 0.5, y = 0.5, hjust = 0.5, vjust = 0.5, rot = 270),
    textGrob("Female", gp = gpar(fontsize = 15, fontface = "bold"), 
             x = 0.5, y = 0.5, hjust = 0.5, vjust = 0.5, rot = 270),
    nrow = 2,
    heights = c(0.5, 0.5)  # ADJUST THESE: Male takes 50%, Female takes 50%
  ),
  nrow = 1,
  widths = c(0.92, 0.08)  # ADJUST THESE: plots take 88%, sex labels take 12%
)

# Step 7: Add rank legend
rank_legend <- ggplot() +
  geom_rect(aes(xmin = 0, xmax = 0.3, ymin = 2.7, ymax = 3), 
            fill = rank_colors["1"], color = "black", linewidth = 0.3) +
  geom_rect(aes(xmin = 0, xmax = 0.3, ymin = 1.8, ymax = 2.1), 
            fill = rank_colors["2"], color = "black", linewidth = 0.3) +
  geom_rect(aes(xmin = 0, xmax = 0.3, ymin = 0.9, ymax = 1.2), 
            fill = rank_colors["3"], color = "black", linewidth = 0.3) +
  annotate("text", x = 0.4, y = 2.85, label = "Rank 1 (Dominant)", hjust = 0, size = 5, fontface = "bold") +
  annotate("text", x = 0.4, y = 1.95, label = "Rank 2 (Intermediate)", hjust = 0, size = 5, fontface = "bold") +
  annotate("text", x = 0.4, y = 1.05, label = "Rank 3 (Submissive)", hjust = 0, size = 5, fontface = "bold") +
  scale_x_continuous(limits = c(0, 3)) +
  scale_y_continuous(limits = c(0.5, 3.5)) +
  labs(title = "Dominance Rank") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0))

# Final Panel C: Combine main panel with legend
panel_1c_complete <- arrangeGrob(
  with_sex_labels, 
  rank_legend, 
  nrow = 1, 
  widths = c(0.77, 0.23)  # ADJUST THESE: main panel 77%, legend 23%
)

# PANEL D: Individual Exploration Rates by Sex
panel_1d_data <- data_clean %>%
  group_by(monkey, initial, sex, ABSOLUTE_RANK, outcome_clean) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(monkey, initial, sex, ABSOLUTE_RANK) %>%
  mutate(total = sum(count),
         proportion = count / total) %>%
  ungroup() %>%
  filter(outcome_clean == "explore") %>%
  mutate(
    display_order = case_when(
      initial == "F" ~ 1,
      initial == "D" ~ 2,
      initial == "E" ~ 3,
      initial == "C" ~ 4,
      initial == "I" ~ 5,
      initial == "A" ~ 6
    ),
    individual_label = initial
  ) %>%
  arrange(display_order)

panel_1d_data$individual_label <- factor(panel_1d_data$individual_label, 
                                         levels = c("F", "D", "E", "C", "I", "A"))

panel_1d <- ggplot(panel_1d_data, aes(x = individual_label, y = proportion, fill = sex)) +
  geom_col(width = 0.6, color = "black", linewidth = 0.3, alpha = 0.9) +
  scale_fill_manual(values = c("Male" = "#FF6B35", "Female" = "#A855F7"), name = "Sex") +
  scale_y_continuous(labels = function(x) paste0(round(x*100), "%"),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(title = "D", subtitle = "Individual Exploration Rates by Sex",
       x = "Individual (by Sex)", y = "Exploration Rate") +
  theme_classic(base_size = 18) +
  theme(
    plot.title = element_text(face = "bold", size = 20, hjust = 0),
    plot.subtitle = element_text(size = 16, hjust = 0),
    legend.position = "top",
    legend.title = element_text(face = "bold", size = 15),
    legend.text = element_text(size = 14),
    axis.title = element_text(face = "bold", size = 15),
    axis.text = element_text(size = 14),
    plot.margin = margin(5, 5, 5, 5, "pt")
  ) +
  geom_vline(xintercept = 3.5, linetype = "dashed", color = "gray50", alpha = 0.7) +
  annotate("text", x = 2, y = max(panel_1d_data$proportion) * 0.95, label = "Males", 
           fontface = "bold", size = 6, color = "#FF6B35") +
  annotate("text", x = 5, y = max(panel_1d_data$proportion) * 0.95, label = "Females", 
           fontface = "bold", size = 6, color = "#A855F7")

# Combine all panels
figure_1_complete <- arrangeGrob(
  arrangeGrob(panel_1a, panel_1b, nrow = 1, widths = c(1, 1)),
  arrangeGrob(panel_1c_complete, panel_1d, nrow = 1, widths = c(2.5, 1)),
  nrow = 2, heights = c(1, 1.2)
)

# Add overall figure title
figure_1_final <- arrangeGrob(
  textGrob("Figure 1. Behavioral Measurements Across Social Contexts", 
           gp = gpar(fontsize = 20, fontface = "bold")),
  figure_1_complete,
  nrow = 2, heights = c(0.05, 0.95)
)

# Save complete figure
ggsave("results/figures/Figure1_SIMPLE_LAYOUT.png", figure_1_final, 
       width = 500, height = 380, units = "mm", dpi = 300)

ggsave("results/figures/Figure1_SIMPLE_LAYOUT.tiff", figure_1_final, 
       width = 500, height = 380, units = "mm", dpi = 300, compression = "lzw")

cat("\n=============================================================================\n")
cat("SIMPLE LAYOUT CREATED!\n") 
cat("=============================================================================\n")
cat("No complex matrix! Easy adjustments:\n")
cat("\n")
cat("TO ADJUST POSITIONING, CHANGE THESE VALUES:\n")
cat("\n")
cat("1. TITLE HEIGHT:\n")
cat("   Line ~150: heights = c(0.15, 0.85)\n")
cat("   - First number = title space (0.15 = 15%)\n")
cat("   - Second number = plots space (0.85 = 85%)\n")
cat("\n")
cat("2. Y-AXIS LABEL WIDTH:\n")
cat("   Line ~157: widths = c(0.08, 0.92)\n")
cat("   - First number = Y-label space (0.08 = 8%)\n")
cat("   - Second number = plots space (0.92 = 92%)\n")
cat("\n")
cat("3. X-AXIS LABEL HEIGHT:\n")
cat("   Line ~164: heights = c(0.92, 0.08)\n")
cat("   - First number = plots space (0.92 = 92%)\n")
cat("   - Second number = X-label space (0.08 = 8%)\n")
cat("\n")
cat("4. MALE/FEMALE LABEL WIDTH:\n")
cat("   Line ~178: widths = c(0.88, 0.12)\n")
cat("   - First number = plots space (0.88 = 88%)\n")
cat("   - Second number = sex labels space (0.12 = 12%)\n")
cat("\n")
cat("5. MALE vs FEMALE SPLIT:\n")
cat("   Line ~174: heights = c(0.5, 0.5)\n")
cat("   - First number = Male space (0.5 = 50%)\n")
cat("   - Second number = Female space (0.5 = 50%)\n")
cat("\n")
cat("6. LEGEND WIDTH:\n")
cat("   Line ~199: widths = c(0.77, 0.23)\n")
cat("   - First number = main panel (0.77 = 77%)\n")
cat("   - Second number = legend (0.23 = 23%)\n")
cat("\n")
cat("✓ All numbers must add up to 1.0\n")
cat("✓ Larger number = more space\n")
cat("✓ Smaller number = less space\n")
cat("=============================================================================\n")
