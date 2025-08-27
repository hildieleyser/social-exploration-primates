# Load required libraries
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)

# Load and prepare data - REAL DATA ONLY
cat("Creating final fixed Panel C...\n")
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
    ),
    rank_color = case_when(
      ABSOLUTE_RANK == 1 ~ "#003366",  # Dark blue
      ABSOLUTE_RANK == 2 ~ "#4682B4",  # Medium blue  
      ABSOLUTE_RANK == 3 ~ "#87CEEB"   # Light blue
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
  group_by(monkey, initial, sex, ABSOLUTE_RANK, rank_color, social_complexity, outcome_clean) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(monkey, initial, sex, ABSOLUTE_RANK, rank_color, social_complexity) %>%
  mutate(total = sum(count),
         proportion = count / total) %>%
  ungroup()

# CORRECT ORDERING: Top row F D E, Bottom row C I A  
panel_1c_data$initial <- factor(panel_1c_data$initial, levels = c("F", "D", "E", "C", "I", "A"))

# Get rank and color for each monkey
monkey_info <- panel_1c_data %>%
  select(initial, ABSOLUTE_RANK, rank_color) %>%
  distinct() %>%
  arrange(initial)

cat("Monkey info:\n")
print(monkey_info)

# Create the plot using a faceted approach but with custom strip colors
# We'll use a simpler method - create one plot and modify it

panel_1c_base <- ggplot(panel_1c_data, aes(x = social_complexity, y = proportion, fill = outcome_clean)) +
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
    strip.background = element_rect(fill = "grey70", color = "black"),
    legend.position = "none",
    plot.margin = margin(5.5, 120, 25, 5.5, "pt")
  )

# Use ggplot_build to modify strip colors
library(gtable)
gt <- ggplot_gtable(ggplot_build(panel_1c_base))

# Find strips and set their colors
strips <- which(grepl('strip-', gt$layout$name))
fills <- c("#003366", "#4682B4", "#87CEEB",    # F, D, E (ranks 1, 2, 3)
          "#003366", "#4682B4", "#87CEEB")     # C, I, A (ranks 1, 2, 3)

for (i in seq_along(strips)) {
  gt$grobs[[strips[i]]]$grobs[[1]]$children[[1]]$gp$fill <- fills[i]
}

# Add sex labels - convert gtable back to grob
panel_with_strips <- as.grob(gt)

# Create sex labels
male_label <- textGrob("Male", gp = gpar(fontsize = 11, fontface = "bold"), rot = 270)
female_label <- textGrob("Female", gp = gpar(fontsize = 11, fontface = "bold"), rot = 270)

# Combine with labels  
panel_1c_with_labels <- grid.arrange(
  panel_with_strips,
  arrangeGrob(male_label, nullGrob(), female_label, nrow = 3, heights = c(1, 0.2, 1)),
  nrow = 1, widths = c(5, 0.3)
)

# Small rank legend
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
complete_panel_c <- grid.arrange(panel_1c_with_labels, rank_legend_clean, 
                                nrow = 1, widths = c(4, 1.5))

# Save
ggsave("results/figures/Panel_C_COLORS_LABELS_FIXED.png", complete_panel_c, 
       width = 450, height = 200, units = "mm", dpi = 300)

cat("\n=============================================================================\n")
cat("PANEL C COLORS AND LABELS FIXED!\n") 
cat("=============================================================================\n")
cat("✓ Strip backgrounds colored by rank:\n")
cat("  - F & C: Dark blue (Rank 1)\n")
cat("  - D & I: Medium blue (Rank 2)\n") 
cat("  - E & A: Light blue (Rank 3)\n")
cat("✓ White monkey letters on colored backgrounds\n")
cat("✓ Only ONE 'Male' label for top row\n")
cat("✓ Only ONE 'Female' label for bottom row\n")
cat("✓ Clean rank legend\n")
cat("=============================================================================\n")
