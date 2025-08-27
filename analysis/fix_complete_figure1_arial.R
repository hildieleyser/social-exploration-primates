# Complete Figure 1 with Arial font and proper formatting like Current Biology
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(extrafont)

cat("Creating publication-quality Figure 1 with Arial font...\n")

# Check if Arial is available, if not use sans
font_family <- "Arial"
if(!"Arial" %in% fonts()) {
  font_family <- "sans"
  cat("Arial not available, using sans font\n")
} else {
  cat("Using Arial font\n")
}

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

# Define colors - Current Biology style
bio_colors <- c("explore" = "#CD5C5C", "exploit" = "#2E8B57", "none" = "#4682B4")
rank_colors <- c("1" = "#003366", "2" = "#4682B4", "3" = "#87CEEB")

# PANEL A: Social vs Non-social comparison (stacked bars)
panel_1a_data <- data_clean %>%
  mutate(social_type = ifelse(social_complexity == "solo", "Non-Social", "Social")) %>%
  group_by(social_type, outcome_clean) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(social_type) %>%
  mutate(total = sum(count),
         proportion = count / total)

panel_1a <- ggplot(panel_1a_data, aes(x = social_type, y = proportion, fill = outcome_clean)) +
  geom_col(width = 0.6, color = "black", linewidth = 0.3, alpha = 0.9, position = "stack") +
  scale_fill_manual(values = bio_colors, name = "Choice Type",
                    labels = c("Explore", "Exploit", "No Choice")) +
  scale_y_continuous(labels = function(x) paste0(round(x*100), "%"), 
                     expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Social Context", y = "Proportion of Choices") +
  theme_classic(base_size = 11, base_family = font_family) +
  theme(
    legend.position = "top",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9),
    axis.title = element_text(face = "bold", size = 10),
    axis.text = element_text(size = 9),
    plot.margin = margin(5, 5, 5, 5, "pt")
  ) +
  # Add panel letter A
  annotate("text", x = -Inf, y = Inf, label = "A", 
           hjust = -0.5, vjust = 1.5, size = 14, fontface = "bold", family = font_family)

# PANEL B: Linear trend across complexity levels
panel_1b_data <- data_clean %>%
  group_by(social_complexity, outcome_clean) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(social_complexity) %>%
  mutate(total = sum(count),
         proportion = count / total) %>%
  filter(outcome_clean == "explore")

panel_1b <- ggplot(panel_1b_data, aes(x = social_complexity, y = proportion)) +
  geom_line(aes(group = 1), color = "#CD5C5C", linewidth = 1.5, alpha = 0.8) +
  geom_point(color = "#CD5C5C", size = 4, alpha = 0.9) +
  scale_x_discrete(labels = c("Solo", "Duo", "Trio")) +
  scale_y_continuous(labels = function(x) paste0(round(x*100), "%"),
                     expand = expansion(mult = c(0.1, 0.1))) +
  labs(x = "Social Complexity", y = "Proportion of Choices") +
  theme_classic(base_size = 11, base_family = font_family) +
  theme(
    axis.title = element_text(face = "bold", size = 10),
    axis.text = element_text(size = 9),
    plot.margin = margin(5, 5, 5, 5, "pt")
  ) +
  # Add panel letter B
  annotate("text", x = -Inf, y = Inf, label = "B", 
           hjust = -0.5, vjust = 1.5, size = 14, fontface = "bold", family = font_family)

# PANEL C: Individual patterns with colored backgrounds
panel_1c_data <- data_clean %>%
  group_by(monkey, initial, sex, ABSOLUTE_RANK, social_complexity, outcome_clean) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(monkey, initial, sex, ABSOLUTE_RANK, social_complexity) %>%
  mutate(total = sum(count),
         proportion = count / total) %>%
  ungroup()

panel_1c_data$initial <- factor(panel_1c_data$initial, levels = c("F", "D", "E", "C", "I", "A"))

# Create individual plots for each monkey with colored backgrounds
create_monkey_plot <- function(monkey_initial, rank, bg_color) {
  monkey_data <- panel_1c_data %>% filter(initial == monkey_initial)
  
  ggplot(monkey_data, aes(x = social_complexity, y = proportion, fill = outcome_clean)) +
    geom_col(width = 0.7, color = "black", linewidth = 0.2, alpha = 0.9) +
    scale_fill_manual(values = bio_colors) +
    scale_x_discrete(labels = c("solo", "duo", "trio")) +
    scale_y_continuous(labels = function(x) paste0(round(x*100), "%"),
                       expand = expansion(mult = c(0, 0.05)), limits = c(0, 1)) +
    labs(title = monkey_initial) +
    theme_classic(base_size = 10, base_family = font_family) +
    theme(
      plot.title = element_text(face = "bold", size = 12, color = "white", hjust = 0.5,
                               margin = margin(3, 3, 3, 3), family = font_family),
      plot.background = element_rect(fill = bg_color, color = "black"),
      panel.background = element_rect(fill = "white"),
      legend.position = "none",
      axis.title = element_blank(),
      axis.text = element_text(size = 8),
      plot.margin = margin(1, 1, 1, 1, "pt")
    )
}

# Create plots for each monkey
f_plot <- create_monkey_plot("F", 1, "#003366")
d_plot <- create_monkey_plot("D", 2, "#4682B4")
e_plot <- create_monkey_plot("E", 3, "#87CEEB")
c_plot <- create_monkey_plot("C", 1, "#003366")
i_plot <- create_monkey_plot("I", 2, "#4682B4")
a_plot <- create_monkey_plot("A", 3, "#87CEEB")

# Arrange Panel C
top_row <- arrangeGrob(f_plot, d_plot, e_plot, nrow = 1)
bottom_row <- arrangeGrob(c_plot, i_plot, a_plot, nrow = 1)

# Create Panel C with title and axes
panel_1c_plots <- arrangeGrob(top_row, bottom_row, nrow = 2)

# Add Panel C title and labels
panel_1c_with_labels <- arrangeGrob(
  # Panel letter C
  textGrob("C", gp = gpar(fontsize = 14, fontface = "bold", fontfamily = font_family), 
           x = 0.02, y = 0.95, hjust = 0, vjust = 1),
  # Main plots
  panel_1c_plots,
  # X-axis label
  textGrob("Social Complexity", gp = gpar(fontsize = 10, fontface = "bold", fontfamily = font_family)),
  # Y-axis label  
  textGrob("Proportion of Choices", gp = gpar(fontsize = 10, fontface = "bold", fontfamily = font_family), rot = 90),
  # Sex labels
  textGrob("Male", gp = gpar(fontsize = 10, fontface = "bold", fontfamily = font_family), 
           x = 0.95, y = 0.75, hjust = 0.5, vjust = 0.5, rot = 270),
  textGrob("Female", gp = gpar(fontsize = 10, fontface = "bold", fontfamily = font_family), 
           x = 0.95, y = 0.25, hjust = 0.5, vjust = 0.5, rot = 270),
  layout_matrix = matrix(c(1, 1, 1, 1, 1, 5,
                          4, 2, 2, 2, 2, 5,
                          4, 2, 2, 2, 2, 6,
                          NA, 3, 3, 3, 3, NA), nrow = 4, byrow = TRUE),
  heights = c(0.1, 0.4, 0.4, 0.1),
  widths = c(0.08, 0.22, 0.22, 0.22, 0.22, 0.04)
)

# Rank legend for Panel C
rank_legend <- ggplot() +
  geom_rect(aes(xmin = 0, xmax = 0.3, ymin = 2.7, ymax = 3), 
            fill = rank_colors["1"], color = "black", linewidth = 0.3) +
  geom_rect(aes(xmin = 0, xmax = 0.3, ymin = 1.8, ymax = 2.1), 
            fill = rank_colors["2"], color = "black", linewidth = 0.3) +
  geom_rect(aes(xmin = 0, xmax = 0.3, ymin = 0.9, ymax = 1.2), 
            fill = rank_colors["3"], color = "black", linewidth = 0.3) +
  annotate("text", x = 0.4, y = 2.85, label = "Rank 1", hjust = 0, size = 3, fontface = "bold", family = font_family) +
  annotate("text", x = 0.4, y = 1.95, label = "Rank 2", hjust = 0, size = 3, fontface = "bold", family = font_family) +
  annotate("text", x = 0.4, y = 1.05, label = "Rank 3", hjust = 0, size = 3, fontface = "bold", family = font_family) +
  scale_x_continuous(limits = c(0, 2)) +
  scale_y_continuous(limits = c(0.5, 3.5)) +
  labs(title = "Dominance Rank") +
  theme_void(base_family = font_family) +
  theme(plot.title = element_text(face = "bold", size = 9, hjust = 0, family = font_family))

panel_1c_complete <- arrangeGrob(panel_1c_with_labels, rank_legend, 
                                nrow = 1, widths = c(4.5, 1))

# PANEL D: Sex differences  
panel_1d_data <- data_clean %>%
  group_by(monkey, sex, social_complexity, outcome_clean) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(monkey, sex, social_complexity) %>%
  mutate(total = sum(count),
         proportion = count / total) %>%
  ungroup() %>%
  filter(outcome_clean == "explore") %>%
  group_by(sex, social_complexity) %>%
  summarise(
    mean_explore = mean(proportion),
    se_explore = sd(proportion) / sqrt(n()),
    .groups = "drop"
  )

panel_1d <- ggplot(panel_1d_data, aes(x = social_complexity, y = mean_explore, color = sex, group = sex)) +
  geom_line(linewidth = 1.2, alpha = 0.8) +
  geom_point(size = 3, alpha = 0.9) +
  geom_errorbar(aes(ymin = mean_explore - se_explore, ymax = mean_explore + se_explore),
                width = 0.1, linewidth = 0.8, alpha = 0.8) +
  scale_color_manual(values = c("Male" = "#4682B4", "Female" = "#CD5C5C"), name = "Sex") +
  scale_x_discrete(labels = c("Solo", "Duo", "Trio")) +
  scale_y_continuous(labels = function(x) paste0(round(x*100), "%"),
                     expand = expansion(mult = c(0.1, 0.1))) +
  labs(x = "Social Complexity", y = "Exploration Rate") +
  theme_classic(base_size = 11, base_family = font_family) +
  theme(
    legend.position = "top",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9),
    axis.title = element_text(face = "bold", size = 10),
    axis.text = element_text(size = 9),
    plot.margin = margin(5, 5, 5, 5, "pt")
  ) +
  # Add panel letter D
  annotate("text", x = -Inf, y = Inf, label = "D", 
           hjust = -0.5, vjust = 1.5, size = 14, fontface = "bold", family = font_family)

# Combine all panels with proper spacing
figure_1_complete <- arrangeGrob(
  # Top row: A and B
  arrangeGrob(panel_1a, panel_1b, nrow = 1, widths = c(1, 1)),
  # Bottom row: C and D
  arrangeGrob(panel_1c_complete, panel_1d, nrow = 1, widths = c(3, 1)),
  nrow = 2, heights = c(1, 1.3)
)

# Add overall figure title
figure_1_final <- arrangeGrob(
  textGrob("Figure 1. Behavioral Measurements Across Social Contexts", 
           gp = gpar(fontsize = 12, fontface = "bold", fontfamily = font_family)),
  figure_1_complete,
  nrow = 2, heights = c(0.05, 0.95)
)

# Save complete figure
ggsave("results/figures/Figure1_CURRENT_BIOLOGY_STYLE.png", figure_1_final, 
       width = 500, height = 380, units = "mm", dpi = 300)

ggsave("results/figures/Figure1_CURRENT_BIOLOGY_STYLE.tiff", figure_1_final, 
       width = 500, height = 380, units = "mm", dpi = 300, compression = "lzw")

cat("\n=============================================================================\n")
cat("CURRENT BIOLOGY STYLE FIGURE 1 COMPLETE!\n") 
cat("=============================================================================\n")
cat("✓ Arial font throughout (or sans if Arial unavailable)\n")
cat("✓ Clear panel letters A, B, C, D visible\n")
cat("✓ Panel A: Stacked bars for social vs non-social\n")
cat("✓ Panel B: Line plot showing exploration decline\n") 
cat("✓ Panel C: Individual patterns with colored backgrounds\n")
cat("✓ Panel D: Sex differences with error bars\n")
cat("✓ Professional Current Biology formatting\n")
cat("✓ Saved as PNG and TIFF\n")
cat("=============================================================================\n")
