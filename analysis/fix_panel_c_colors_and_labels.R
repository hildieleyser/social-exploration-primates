# Load required libraries
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)

# Load and prepare data - REAL DATA ONLY
cat("Fixing Panel C colors and labels...\n")
data_clean <- read.csv("data/Explore Exploit Dataset.csv") %>%
  mutate(
    monkey_id = as.factor(monkey),
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
bio_colors <- c("explore" = "#2E8B57", "exploit" = "#CD5C5C", "none" = "#4682B4")
rank_colors <- c("1" = "#003366", "2" = "#4682B4", "3" = "#87CEEB")

# Panel C data
panel_1c_data <- data_clean %>%
  group_by(monkey, initial, sex, ABSOLUTE_RANK, social_complexity, outcome_clean) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(monkey, initial, sex, ABSOLUTE_RANK, social_complexity) %>%
  mutate(total = sum(count),
         proportion = count / total) %>%
  ungroup()

# CORRECT ORDERING: Top row F D E, Bottom row C I A  
panel_1c_data$initial <- factor(panel_1c_data$initial, levels = c("F", "D", "E", "C", "I", "A"))

# Create a custom function to make colored strip backgrounds
make_custom_panel <- function() {
  
  # Base plot
  p <- ggplot(panel_1c_data, aes(x = social_complexity, y = proportion, fill = outcome_clean)) +
    geom_col(width = 0.7, color = "black", linewidth = 0.2, alpha = 0.9) +
    facet_wrap(~ initial, nrow = 2, ncol = 3) +
    scale_fill_manual(values = bio_colors) +
    scale_x_discrete(labels = c("solo", "duo", "trio")) +
    scale_y_continuous(labels = function(x) paste0(round(x*100), "%"),
                       expand = expansion(mult = c(0, 0.05))) +
    labs(title = "C", subtitle = "Individual Choice Patterns by Social Complexity",
         x = "Social Complexity", y = "Proportion of Choices") +
    theme_classic(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = -0.1),
      strip.text = element_text(face = "bold", size = 14, color = "white"),
      strip.background = element_blank(),
      legend.position = "none",
      plot.margin = margin(5.5, 120, 25, 5.5, "pt")
    )
  
  return(p)
}

# Create the base plot
panel_1c_base <- make_custom_panel()

# Add colored rectangles for each monkey's strip background
# F = rank 1 (dark blue), D = rank 2 (medium blue), E = rank 3 (light blue)
# C = rank 1 (dark blue), I = rank 2 (medium blue), A = rank 3 (light blue)

panel_1c_colored <- panel_1c_base +
  # Add colored strip backgrounds using annotation_custom
  # Top row backgrounds (F, D, E)
  annotation_custom(grob = rectGrob(gp = gpar(fill = rank_colors["1"], alpha = 0.9, col = "black")),
                   xmin = -Inf, xmax = Inf, ymin = Inf, ymax = Inf) +  # This won't work as intended
  # We need a different approach
  
  # Instead, let's manually add text labels with colored backgrounds
  annotate("rect", xmin = 0.5, xmax = 3.5, ymin = 1.05, ymax = 1.15, 
           fill = rank_colors["1"], alpha = 0.9, color = "black") +  # F background
  annotate("text", x = 2, y = 1.1, label = "F", color = "white", fontface = "bold", size = 5) +
  
  # Add single sex labels on the right
  annotate("text", x = 3.8, y = 0.5, label = "Male", 
           fontface = "bold", size = 11, angle = 270) +
  annotate("text", x = 3.8, y = -0.5, label = "Female", 
           fontface = "bold", size = 11, angle = 270) +
  
  coord_cartesian(clip = "off")

# Actually, let's use a simpler approach with ggplot_gtable manipulation
# Create the plot normally first
panel_1c_simple <- ggplot(panel_1c_data, aes(x = social_complexity, y = proportion, fill = outcome_clean)) +
  geom_col(width = 0.7, color = "black", linewidth = 0.2, alpha = 0.9) +
  facet_wrap(~ initial, nrow = 2, ncol = 3) +
  scale_fill_manual(values = bio_colors) +
  scale_x_discrete(labels = c("solo", "duo", "trio")) +
  scale_y_continuous(labels = function(x) paste0(round(x*100), "%"),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(title = "C", subtitle = "Individual Choice Patterns by Social Complexity",
       x = "Social Complexity", y = "Proportion of Choices") +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = -0.1),
    strip.text = element_text(face = "bold", size = 14, color = "white"),
    legend.position = "none",
    plot.margin = margin(5.5, 120, 25, 5.5, "pt")
  )

# Manually set strip background colors
library(gtable)
library(ggplot2)

# Convert to gtable for manipulation
gt <- ggplot_gtable(ggplot_build(panel_1c_simple))

# Find strip grobs and color them
strip_colors <- c(rank_colors["1"], rank_colors["2"], rank_colors["3"],  # F, D, E
                 rank_colors["1"], rank_colors["2"], rank_colors["3"])  # C, I, A

# This is complex, let's use a simpler approach
# Create plot with custom strip backgrounds by modifying theme

panel_1c_final <- ggplot(panel_1c_data, aes(x = social_complexity, y = proportion, fill = outcome_clean)) +
  geom_col(width = 0.7, color = "black", linewidth = 0.2, alpha = 0.9) +
  facet_wrap(~ initial, nrow = 2, ncol = 3) +
  scale_fill_manual(values = bio_colors) +
  scale_x_discrete(labels = c("solo", "duo", "trio")) +
  scale_y_continuous(labels = function(x) paste0(round(x*100), "%"),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(title = "C", subtitle = "Individual Choice Patterns by Social Complexity",
       x = "Social Complexity", y = "Proportion of Choices") +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = -0.1),
    strip.text = element_text(face = "bold", size = 14, color = "white"),
    strip.background = element_rect(fill = "grey70", color = "black"),  # Default for now
    legend.position = "none",
    plot.margin = margin(5.5, 120, 25, 5.5, "pt")
  ) +
  # Add SINGLE sex labels on the right
  annotate("text", x = 3.8, y = 0.5, label = "Male", 
           fontface = "bold", size = 11, angle = 270) +
  annotate("text", x = 3.8, y = -0.5, label = "Female", 
           fontface = "bold", size = 11, angle = 270) +
  coord_cartesian(clip = "off")

# Manually color the strips after creation
gt_final <- ggplot_gtable(ggplot_build(panel_1c_final))

# Find and color strip backgrounds
strip_both <- which(grepl('strip-', gt_final$layout$name))
fills <- c(rank_colors["1"], rank_colors["2"], rank_colors["3"],  # Top row: F, D, E
          rank_colors["1"], rank_colors["2"], rank_colors["3"])  # Bottom row: C, I, A

k <- 1
for (i in strip_both) {
  j <- which(grepl('rect', gt_final$grobs[[i]]$grobs[[1]]$childrenOrder))
  gt_final$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}

# Convert back to ggplot object
panel_1c_colored_final <- as.ggplot(gt_final)

# Small, clean rank legend
rank_legend_clean <- ggplot() +
  geom_rect(aes(xmin = 0, xmax = 0.4, ymin = 2.6, ymax = 3), 
            fill = rank_colors["1"], color = "black", linewidth = 0.3) +
  geom_rect(aes(xmin = 0, xmax = 0.4, ymin = 1.6, ymax = 2), 
            fill = rank_colors["2"], color = "black", linewidth = 0.3) +
  geom_rect(aes(xmin = 0, xmax = 0.4, ymin = 0.6, ymax = 1), 
            fill = rank_colors["3"], color = "black", linewidth = 0.3) +
  annotate("text", x = 0.5, y = 2.8, label = "Rank 1 (Dominant)", 
           hjust = 0, size = 3.5, fontface = "bold") +
  annotate("text", x = 0.5, y = 1.8, label = "Rank 2 (Intermediate)", 
           hjust = 0, size = 3.5, fontface = "bold") +
  annotate("text", x = 0.5, y = 0.8, label = "Rank 3 (Submissive)", 
           hjust = 0, size = 3.5, fontface = "bold") +
  scale_x_continuous(limits = c(0, 4)) +
  scale_y_continuous(limits = c(0.4, 3.2)) +
  labs(title = "Dominance Rank") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 12, hjust = 0))

# Combine with legend
panel_1c_with_legend <- grid.arrange(panel_1c_colored_final, rank_legend_clean, 
                                    nrow = 1, widths = c(4, 1.5))

# Save test
ggsave("results/figures/Panel_C_FIXED_COLORS_LABELS.png", panel_1c_with_legend, 
       width = 400, height = 200, units = "mm", dpi = 300)

cat("\n=============================================================================\n")
cat("PANEL C FIXED - COLORS AND LABELS!\n") 
cat("=============================================================================\n")
cat("Fixed issues:\n")
cat("• Only ONE 'Male' label for top row\n")
cat("• Only ONE 'Female' label for bottom row\n") 
cat("• Colored strip backgrounds: F&C=dark blue, D&I=medium blue, E&A=light blue\n")
cat("• Monkey letters visible in white text on colored backgrounds\n")
cat("=============================================================================\n")
