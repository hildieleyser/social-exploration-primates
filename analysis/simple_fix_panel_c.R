# Load required libraries
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)

# Load and prepare data - REAL DATA ONLY
cat("Creating simple fixed Panel C...\n")
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

# Get rank for each monkey
monkey_ranks <- data_clean %>%
  select(initial, ABSOLUTE_RANK) %>%
  distinct()

cat("Monkey ranks:\n")
print(monkey_ranks)

# Create individual plots for each monkey with correct colors
plots_list <- list()

for(monkey in c("F", "D", "E", "C", "I", "A")) {
  monkey_data <- panel_1c_data %>% filter(initial == monkey)
  rank <- monkey_ranks$ABSOLUTE_RANK[monkey_ranks$initial == monkey]
  strip_color <- rank_colors[as.character(rank)]
  
  p <- ggplot(monkey_data, aes(x = social_complexity, y = proportion, fill = outcome_clean)) +
    geom_col(width = 0.7, color = "black", linewidth = 0.2, alpha = 0.9) +
    scale_fill_manual(values = bio_colors) +
    scale_x_discrete(labels = c("solo", "duo", "trio")) +
    scale_y_continuous(labels = function(x) paste0(round(x*100), "%"),
                       expand = expansion(mult = c(0, 0.05))) +
    labs(title = monkey) +
    theme_classic(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14, color = "white", hjust = 0.5,
                               margin = margin(5, 5, 5, 5)),
      plot.background = element_rect(fill = strip_color, color = "black"),
      panel.background = element_rect(fill = "white"),
      legend.position = "none",
      axis.title = element_blank(),
      plot.margin = margin(2, 2, 2, 2, "pt")
    )
  
  plots_list[[monkey]] <- p
}

# Arrange in grid: F D E (top), C I A (bottom)
top_row <- grid.arrange(plots_list[["F"]], plots_list[["D"]], plots_list[["E"]], nrow = 1)
bottom_row <- grid.arrange(plots_list[["C"]], plots_list[["I"]], plots_list[["A"]], nrow = 1)
plots_grid <- grid.arrange(top_row, bottom_row, nrow = 2)

# Add overall title and axis labels
panel_1c_final <- grid.arrange(
  textGrob("C", gp = gpar(fontsize = 14, fontface = "bold"), x = 0.02, hjust = 0),
  textGrob("Individual Choice Patterns by Social Complexity", 
           gp = gpar(fontsize = 12), x = 0.5, hjust = 0.5),
  plots_grid,
  textGrob("Social Complexity", gp = gpar(fontsize = 11, fontface = "bold")),
  textGrob("Proportion of Choices", gp = gpar(fontsize = 11, fontface = "bold"), rot = 90),
  layout_matrix = matrix(c(1, 2, 2, 2,
                          5, 3, 3, 3,
                          5, 3, 3, 3,
                          NA, 4, 4, 4), nrow = 4, byrow = TRUE),
  heights = c(0.1, 0.4, 0.4, 0.1),
  widths = c(0.1, 0.3, 0.3, 0.3)
)

# Add sex labels
panel_1c_with_labels <- grid.arrange(
  panel_1c_final,
  textGrob("Male", gp = gpar(fontsize = 11, fontface = "bold"), rot = 270),
  textGrob("Female", gp = gpar(fontsize = 11, fontface = "bold"), rot = 270),
  layout_matrix = matrix(c(1, 2,
                          1, NA,
                          1, 3), nrow = 3, byrow = TRUE),
  heights = c(0.33, 0.34, 0.33),
  widths = c(0.9, 0.1)
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
ggsave("results/figures/Panel_C_PROPERLY_FIXED.png", complete_panel_c, 
       width = 450, height = 200, units = "mm", dpi = 300)

cat("\n=============================================================================\n")
cat("PANEL C PROPERLY FIXED!\n") 
cat("=============================================================================\n")
cat("Fixed:\n")
cat("• Colored backgrounds: F&C=dark blue, D&I=medium blue, E&A=light blue\n")
cat("• White monkey letters on colored backgrounds\n")
cat("• Only ONE 'Male' label (top row)\n")
cat("• Only ONE 'Female' label (bottom row)\n")
cat("• Small clean rank legend\n")
cat("• All real data used\n")
cat("=============================================================================\n")
