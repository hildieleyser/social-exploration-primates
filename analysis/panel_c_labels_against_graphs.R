# Make all Panel C labels right up against the graphs with minimal spacing
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)

cat("Making Panel C labels right up against the graphs...\n")

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
  theme_classic(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0),
    plot.subtitle = element_text(size = 10, hjust = 0),
    legend.position = "top",
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 8),
    axis.title = element_text(face = "bold", size = 9),
    axis.text = element_text(size = 8),
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
  theme_classic(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0),
    plot.subtitle = element_text(size = 10, hjust = 0),
    legend.position = "top",
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 8),
    axis.title = element_text(face = "bold", size = 9),
    axis.text = element_text(size = 8),
    plot.margin = margin(5, 5, 5, 5, "pt")
  )

# PANEL C: Individual Choice Patterns - ALL labels RIGHT AGAINST graphs
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
    theme_classic(base_size = 9) +
    theme(
      plot.title = element_text(face = "bold", size = 11, color = "white", hjust = 0.5,
                               margin = margin(2, 2, 2, 2)),
      plot.background = element_rect(fill = bg_color, color = "black"),
      panel.background = element_rect(fill = "white"),
      legend.position = "none",
      axis.title = element_blank(),
      axis.text = element_text(size = 7),
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

# Arrange Panel C
top_row <- arrangeGrob(f_plot, d_plot, e_plot, nrow = 1)
bottom_row <- arrangeGrob(c_plot, i_plot, a_plot, nrow = 1)
panel_1c_plots <- arrangeGrob(top_row, bottom_row, nrow = 2)

# Create Panel C with ALL labels RIGHT UP AGAINST the graphs
panel_1c_titled <- arrangeGrob(
  # Title and subtitle
  textGrob("C", gp = gpar(fontsize = 12, fontface = "bold"), 
           x = 0.02, y = 0.95, hjust = 0, vjust = 1),
  textGrob("Individual Choice Patterns by Social Complexity", 
           gp = gpar(fontsize = 10), x = 0.02, y = 0.88, hjust = 0, vjust = 1),
  # Y-axis label - RIGHT AGAINST the graphs (minimal x)
  textGrob("Proportion of Choices", gp = gpar(fontsize = 9, fontface = "bold"), 
           x = 0.01, y = 0.45, hjust = 0.5, vjust = 0.5, rot = 90),
  # Main plots
  panel_1c_plots,
  # X-axis label - RIGHT AGAINST the graphs (minimal y)
  textGrob("Social Complexity", gp = gpar(fontsize = 9, fontface = "bold"),
           x = 0.5, y = 0.01, hjust = 0.5, vjust = 0),
  # Sex labels - RIGHT AGAINST the graphs (maximal x)
  textGrob("Male", gp = gpar(fontsize = 9, fontface = "bold"), 
           x = 0.99, y = 0.72, hjust = 0.5, vjust = 0.5, rot = 270),
  textGrob("Female", gp = gpar(fontsize = 9, fontface = "bold"), 
           x = 0.99, y = 0.28, hjust = 0.5, vjust = 0.5, rot = 270),
  layout_matrix = matrix(c(1, 1, 1, 1, 1, 6,
                          2, 2, 2, 2, 2, 6,
                          3, 4, 4, 4, 4, 6,
                          3, 4, 4, 4, 4, 7,
                          NA, 5, 5, 5, 5, NA), nrow = 5, byrow = TRUE),
  heights = c(0.08, 0.08, 0.38, 0.38, 0.08),
  widths = c(0.02, 0.23, 0.23, 0.23, 0.23, 0.04)
)

# Rank legend for Panel C
rank_legend <- ggplot() +
  geom_rect(aes(xmin = 0, xmax = 0.3, ymin = 2.7, ymax = 3), 
            fill = rank_colors["1"], color = "black", linewidth = 0.3) +
  geom_rect(aes(xmin = 0, xmax = 0.3, ymin = 1.8, ymax = 2.1), 
            fill = rank_colors["2"], color = "black", linewidth = 0.3) +
  geom_rect(aes(xmin = 0, xmax = 0.3, ymin = 0.9, ymax = 1.2), 
            fill = rank_colors["3"], color = "black", linewidth = 0.3) +
  annotate("text", x = 0.4, y = 2.85, label = "Rank 1 (Dominant)", hjust = 0, size = 2.5, fontface = "bold") +
  annotate("text", x = 0.4, y = 1.95, label = "Rank 2 (Intermediate)", hjust = 0, size = 2.5, fontface = "bold") +
  annotate("text", x = 0.4, y = 1.05, label = "Rank 3 (Submissive)", hjust = 0, size = 2.5, fontface = "bold") +
  scale_x_continuous(limits = c(0, 3)) +
  scale_y_continuous(limits = c(0.5, 3.5)) +
  labs(title = "Dominance Rank") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 8, hjust = 0))

panel_1c_complete <- arrangeGrob(panel_1c_titled, rank_legend, 
                                nrow = 1, widths = c(4, 1.2))

# PANEL D: Individual Exploration Rates by Sex
panel_1d_data <- data_clean %>%
  group_by(monkey, initial, sex, ABSOLUTE_RANK, outcome_clean) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(monkey, initial, sex, ABSOLUTE_RANK) %>%
  mutate(total = sum(count),
         proportion = count / total) %>%
  ungroup() %>%
  filter(outcome_clean == "explore") %>%
  # Create ordered factor for proper display: F D E (Males), then C I A (Females)
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

# Set factor levels for proper ordering
panel_1d_data$individual_label <- factor(panel_1d_data$individual_label, 
                                         levels = c("F", "D", "E", "C", "I", "A"))

# Use different colors that won't confuse with Panel C
panel_1d <- ggplot(panel_1d_data, aes(x = individual_label, y = proportion, fill = sex)) +
  geom_col(width = 0.6, color = "black", linewidth = 0.3, alpha = 0.9) +
  scale_fill_manual(values = c("Male" = "#FF6B35", "Female" = "#A855F7"), name = "Sex") +
  scale_y_continuous(labels = function(x) paste0(round(x*100), "%"),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(title = "D", subtitle = "Individual Exploration Rates by Sex",
       x = "Individual (by Sex)", y = "Exploration Rate") +
  theme_classic(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12, hjust = 0),
    plot.subtitle = element_text(size = 10, hjust = 0),
    legend.position = "top",
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 8),
    axis.title = element_text(face = "bold", size = 9),
    axis.text = element_text(size = 8),
    plot.margin = margin(5, 5, 5, 5, "pt")
  ) +
  # Add visual separator between males and females
  geom_vline(xintercept = 3.5, linetype = "dashed", color = "gray50", alpha = 0.7) +
  # Add group labels
  annotate("text", x = 2, y = max(panel_1d_data$proportion) * 0.95, label = "Males", 
           fontface = "bold", size = 3.5, color = "#FF6B35") +
  annotate("text", x = 5, y = max(panel_1d_data$proportion) * 0.95, label = "Females", 
           fontface = "bold", size = 3.5, color = "#A855F7")

# Combine all panels
figure_1_complete <- arrangeGrob(
  # Top row: A and B
  arrangeGrob(panel_1a, panel_1b, nrow = 1, widths = c(1, 1)),
  # Bottom row: C and D
  arrangeGrob(panel_1c_complete, panel_1d, nrow = 1, widths = c(2.5, 1)),
  nrow = 2, heights = c(1, 1.2)
)

# Add overall figure title
figure_1_final <- arrangeGrob(
  textGrob("Figure 1. Behavioral Measurements Across Social Contexts", 
           gp = gpar(fontsize = 12, fontface = "bold")),
  figure_1_complete,
  nrow = 2, heights = c(0.05, 0.95)
)

# Save complete figure
ggsave("results/figures/Figure1_LABELS_AGAINST_GRAPHS.png", figure_1_final, 
       width = 500, height = 380, units = "mm", dpi = 300)

ggsave("results/figures/Figure1_LABELS_AGAINST_GRAPHS.tiff", figure_1_final, 
       width = 500, height = 380, units = "mm", dpi = 300, compression = "lzw")

cat("\n=============================================================================\n")
cat("ALL PANEL C LABELS NOW RIGHT UP AGAINST THE GRAPHS!\n") 
cat("=============================================================================\n")
cat("✓ Y-axis label at x=0.01 (right against left side of graphs)\n")
cat("✓ X-axis label at y=0.01 (right against bottom of graphs)\n")
cat("✓ Male/Female labels at x=0.99 (right against right side of graphs)\n")
cat("✓ Layout matrix minimized: left width=0.02, right width=0.04\n")
cat("✓ Maximum possible proximity to graphs achieved\n")
cat("✓ All other formatting maintained\n")
cat("✓ Saved as PNG and TIFF\n")
cat("=============================================================================\n")
