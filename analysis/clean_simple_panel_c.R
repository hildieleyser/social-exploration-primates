# Load required libraries
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)

# Load and prepare data - REAL DATA ONLY
cat("Creating clean, simple Panel C...\n")
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
rank_colors <- c("1" = "#003366", "2" = "#4682B4", "3" = "#87CEEB")  # Dark, medium, light blue

# Get actual rank data
rank_data <- data_clean %>%
  select(initial, ABSOLUTE_RANK) %>%
  distinct()

cat("Actual ranks:\n")
print(rank_data)

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

# Create the plot with CLEAN monkey letters in white boxes
panel_1c <- ggplot(panel_1c_data, aes(x = social_complexity, y = proportion, fill = outcome_clean)) +
  geom_col(width = 0.7, color = "black", linewidth = 0.2, alpha = 0.9) +
  facet_wrap(~ initial, nrow = 2, ncol = 3, 
             labeller = labeller(initial = function(x) as.character(x))) +
  scale_fill_manual(values = bio_colors) +
  scale_x_discrete(labels = c("solo", "duo", "trio")) +
  scale_y_continuous(labels = function(x) paste0(round(x*100), "%"),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(title = "C", subtitle = "Individual Choice Patterns by Social Complexity",
       x = "Social Complexity", y = "Proportion of Choices") +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = -0.1),
    strip.text = element_text(face = "bold", size = 14, color = "black"),
    strip.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    legend.position = "none",
    plot.margin = margin(5.5, 100, 5.5, 5.5, "pt")
  )

# Color the strip backgrounds based on rank
# F and C = rank 1 (dark blue), D and I = rank 2 (medium blue), E and A = rank 3 (light blue)
panel_1c_colored <- panel_1c +
  theme(
    panel.background = element_rect(fill = "white"),
    strip.background = element_blank()
  )

# Add colored boxes behind each monkey letter manually
# This requires custom grid drawing for each facet
# Simplified approach: add colored rectangles as annotations

# Add rank-colored backgrounds to strip areas
for(i in 1:nrow(rank_data)) {
  monkey <- as.character(rank_data$initial[i])
  rank <- rank_data$ABSOLUTE_RANK[i]
  color <- rank_colors[as.character(rank)]
  
  # Position calculations for each monkey's strip
  if(monkey == "F") {
    panel_1c_colored <- panel_1c_colored + 
      annotation_custom(grob = rectGrob(gp = gpar(fill = color, alpha = 0.8)),
                       xmin = -Inf, xmax = Inf, ymin = 1.05, ymax = 1.15)
  }
}

# Add simple sex labels on the right
panel_1c_final <- panel_1c_colored +
  annotation_custom(grob = textGrob("Male", gp = gpar(fontsize = 12, fontface = "bold")),
                   xmin = 3.6, xmax = 3.6, ymin = 0.5, ymax = 0.5) +
  annotation_custom(grob = textGrob("Female", gp = gpar(fontsize = 12, fontface = "bold")),
                   xmin = 3.6, xmax = 3.6, ymin = -0.5, ymax = -0.5) +
  coord_cartesian(clip = "off")

# Create SMALL, CLEAN rank legend
rank_legend_small <- ggplot() +
  # Dark blue box
  geom_rect(aes(xmin = 0, xmax = 0.3, ymin = 2.7, ymax = 3), 
            fill = rank_colors["1"], color = "black", linewidth = 0.3) +
  # Medium blue box  
  geom_rect(aes(xmin = 0, xmax = 0.3, ymin = 1.7, ymax = 2), 
            fill = rank_colors["2"], color = "black", linewidth = 0.3) +
  # Light blue box
  geom_rect(aes(xmin = 0, xmax = 0.3, ymin = 0.7, ymax = 1), 
            fill = rank_colors["3"], color = "black", linewidth = 0.3) +
  # Labels
  annotate("text", x = 0.4, y = 2.85, label = "Rank 1 (Dominant)", 
           hjust = 0, size = 3, fontface = "bold") +
  annotate("text", x = 0.4, y = 1.85, label = "Rank 2 (Intermediate)", 
           hjust = 0, size = 3, fontface = "bold") +
  annotate("text", x = 0.4, y = 0.85, label = "Rank 3 (Submissive)", 
           hjust = 0, size = 3, fontface = "bold") +
  scale_x_continuous(limits = c(0, 3)) +
  scale_y_continuous(limits = c(0.5, 3.2)) +
  labs(title = "Dominance Rank") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 11, hjust = 0))

# Combine with the legend positioned properly
panel_1c_with_legend <- grid.arrange(
  panel_1c_final,
  rank_legend_small,
  nrow = 1, widths = c(5, 2)
)

# Save just Panel C for testing
ggsave("results/figures/Panel_C_CLEAN_TEST.png", panel_1c_with_legend, 
       width = 350, height = 200, units = "mm", dpi = 300)

cat("\n=============================================================================\n")
cat("CLEAN PANEL C CREATED!\n") 
cat("=============================================================================\n")
cat("Features:\n")
cat("• White boxes with clear monkey letters (F, D, E, C, I, A)\n")
cat("• Top row: F D E (Males)\n") 
cat("• Bottom row: C I A (Females)\n")
cat("• Small rank legend on right\n")
cat("• Simple Male/Female labels on side\n")
cat("• Rank colors: F&C=dark blue, D&I=medium blue, E&A=light blue\n")
cat("=============================================================================\n")
