# Load required libraries
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)

# Load and prepare data
cat("Creating ultra-simple Panel C fix...\n")
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

# Define custom labels that include rank info for coloring
panel_1c_data <- panel_1c_data %>%
  mutate(
    monkey_rank = paste0(initial, " (Rank ", ABSOLUTE_RANK, ")"),
    rank_color = case_when(
      ABSOLUTE_RANK == 1 ~ "#003366",
      ABSOLUTE_RANK == 2 ~ "#4682B4", 
      ABSOLUTE_RANK == 3 ~ "#87CEEB"
    )
  )

# CORRECT ORDERING: Top row F D E, Bottom row C I A  
panel_1c_data$initial <- factor(panel_1c_data$initial, levels = c("F", "D", "E", "C", "I", "A"))
panel_1c_data$monkey_rank <- factor(panel_1c_data$monkey_rank, 
                                   levels = c("F (Rank 1)", "D (Rank 2)", "E (Rank 3)",
                                             "C (Rank 1)", "I (Rank 2)", "A (Rank 3)"))

# Create the plot with colored strip backgrounds manually
panel_1c_plot <- ggplot(panel_1c_data, aes(x = social_complexity, y = proportion, fill = outcome_clean)) +
  geom_col(width = 0.7, color = "black", linewidth = 0.2, alpha = 0.9) +
  facet_wrap(~ initial, nrow = 2, ncol = 3, 
             labeller = labeller(initial = c("F" = "F", "D" = "D", "E" = "E", 
                                            "C" = "C", "I" = "I", "A" = "A"))) +
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
    strip.background = element_rect(fill = "grey40", color = "black"),  
    legend.position = "none",
    plot.margin = margin(5.5, 120, 25, 5.5, "pt")
  )

# Create custom strip colors by creating separate plots and arranging
# F plot (rank 1 - dark blue)
f_data <- panel_1c_data %>% filter(initial == "F")
f_plot <- ggplot(f_data, aes(x = social_complexity, y = proportion, fill = outcome_clean)) +
  geom_col(width = 0.7, color = "black", linewidth = 0.2, alpha = 0.9) +
  scale_fill_manual(values = bio_colors) +
  scale_x_discrete(labels = c("solo", "duo", "trio")) +
  scale_y_continuous(labels = function(x) paste0(round(x*100), "%"),
                     expand = expansion(mult = c(0, 0.05)), limits = c(0, 1)) +
  labs(title = "F") +
  theme_classic(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 12, color = "white", hjust = 0.5,
                             margin = margin(3, 3, 3, 3)),
    plot.background = element_rect(fill = "#003366", color = "black"),
    panel.background = element_rect(fill = "white"),
    legend.position = "none",
    axis.title = element_blank(),
    plot.margin = margin(1, 1, 1, 1, "pt")
  )

# D plot (rank 2 - medium blue)
d_data <- panel_1c_data %>% filter(initial == "D")
d_plot <- ggplot(d_data, aes(x = social_complexity, y = proportion, fill = outcome_clean)) +
  geom_col(width = 0.7, color = "black", linewidth = 0.2, alpha = 0.9) +
  scale_fill_manual(values = bio_colors) +
  scale_x_discrete(labels = c("solo", "duo", "trio")) +
  scale_y_continuous(labels = function(x) paste0(round(x*100), "%"),
                     expand = expansion(mult = c(0, 0.05)), limits = c(0, 1)) +
  labs(title = "D") +
  theme_classic(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 12, color = "white", hjust = 0.5,
                             margin = margin(3, 3, 3, 3)),
    plot.background = element_rect(fill = "#4682B4", color = "black"),
    panel.background = element_rect(fill = "white"),
    legend.position = "none",
    axis.title = element_blank(),
    plot.margin = margin(1, 1, 1, 1, "pt")
  )

# E plot (rank 3 - light blue)
e_data <- panel_1c_data %>% filter(initial == "E")
e_plot <- ggplot(e_data, aes(x = social_complexity, y = proportion, fill = outcome_clean)) +
  geom_col(width = 0.7, color = "black", linewidth = 0.2, alpha = 0.9) +
  scale_fill_manual(values = bio_colors) +
  scale_x_discrete(labels = c("solo", "duo", "trio")) +
  scale_y_continuous(labels = function(x) paste0(round(x*100), "%"),
                     expand = expansion(mult = c(0, 0.05)), limits = c(0, 1)) +
  labs(title = "E") +
  theme_classic(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 12, color = "white", hjust = 0.5,
                             margin = margin(3, 3, 3, 3)),
    plot.background = element_rect(fill = "#87CEEB", color = "black"),
    panel.background = element_rect(fill = "white"),
    legend.position = "none",
    axis.title = element_blank(),
    plot.margin = margin(1, 1, 1, 1, "pt")
  )

# C plot (rank 1 - dark blue)
c_data <- panel_1c_data %>% filter(initial == "C")
c_plot <- ggplot(c_data, aes(x = social_complexity, y = proportion, fill = outcome_clean)) +
  geom_col(width = 0.7, color = "black", linewidth = 0.2, alpha = 0.9) +
  scale_fill_manual(values = bio_colors) +
  scale_x_discrete(labels = c("solo", "duo", "trio")) +
  scale_y_continuous(labels = function(x) paste0(round(x*100), "%"),
                     expand = expansion(mult = c(0, 0.05)), limits = c(0, 1)) +
  labs(title = "C") +
  theme_classic(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 12, color = "white", hjust = 0.5,
                             margin = margin(3, 3, 3, 3)),
    plot.background = element_rect(fill = "#003366", color = "black"),
    panel.background = element_rect(fill = "white"),
    legend.position = "none",
    axis.title = element_blank(),
    plot.margin = margin(1, 1, 1, 1, "pt")
  )

# I plot (rank 2 - medium blue)
i_data <- panel_1c_data %>% filter(initial == "I")
i_plot <- ggplot(i_data, aes(x = social_complexity, y = proportion, fill = outcome_clean)) +
  geom_col(width = 0.7, color = "black", linewidth = 0.2, alpha = 0.9) +
  scale_fill_manual(values = bio_colors) +
  scale_x_discrete(labels = c("solo", "duo", "trio")) +
  scale_y_continuous(labels = function(x) paste0(round(x*100), "%"),
                     expand = expansion(mult = c(0, 0.05)), limits = c(0, 1)) +
  labs(title = "I") +
  theme_classic(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 12, color = "white", hjust = 0.5,
                             margin = margin(3, 3, 3, 3)),
    plot.background = element_rect(fill = "#4682B4", color = "black"),
    panel.background = element_rect(fill = "white"),
    legend.position = "none",
    axis.title = element_blank(),
    plot.margin = margin(1, 1, 1, 1, "pt")
  )

# A plot (rank 3 - light blue)
a_data <- panel_1c_data %>% filter(initial == "A")
a_plot <- ggplot(a_data, aes(x = social_complexity, y = proportion, fill = outcome_clean)) +
  geom_col(width = 0.7, color = "black", linewidth = 0.2, alpha = 0.9) +
  scale_fill_manual(values = bio_colors) +
  scale_x_discrete(labels = c("solo", "duo", "trio")) +
  scale_y_continuous(labels = function(x) paste0(round(x*100), "%"),
                     expand = expansion(mult = c(0, 0.05)), limits = c(0, 1)) +
  labs(title = "A") +
  theme_classic(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 12, color = "white", hjust = 0.5,
                             margin = margin(3, 3, 3, 3)),
    plot.background = element_rect(fill = "#87CEEB", color = "black"),
    panel.background = element_rect(fill = "white"),
    legend.position = "none",
    axis.title = element_blank(),
    plot.margin = margin(1, 1, 1, 1, "pt")
  )

# Arrange in grid
top_row <- arrangeGrob(f_plot, d_plot, e_plot, nrow = 1)
bottom_row <- arrangeGrob(c_plot, i_plot, a_plot, nrow = 1)

# Add title, subtitle, and labels
panel_1c_titled <- arrangeGrob(
  textGrob("C", gp = gpar(fontsize = 14, fontface = "bold"), x = 0.02, hjust = 0),
  textGrob("Individual Choice Patterns by Social Complexity", 
           gp = gpar(fontsize = 12), x = 0.5, hjust = 0.5),
  top_row,
  bottom_row,
  textGrob("Social Complexity", gp = gpar(fontsize = 11, fontface = "bold")),
  layout_matrix = matrix(c(1, 2, 2, 2,
                          NA, 3, 3, 3,
                          NA, 4, 4, 4,  
                          NA, 5, 5, 5), nrow = 4, byrow = TRUE),
  heights = c(0.1, 0.4, 0.4, 0.1)
)

# Add y-axis label
panel_1c_with_y <- arrangeGrob(
  textGrob("Proportion of Choices", gp = gpar(fontsize = 11, fontface = "bold"), rot = 90),
  panel_1c_titled,
  nrow = 1, widths = c(0.05, 0.95)
)

# Add sex labels  
panel_1c_with_sex <- arrangeGrob(
  panel_1c_with_y,
  arrangeGrob(
    textGrob("Male", gp = gpar(fontsize = 11, fontface = "bold"), rot = 270),
    nullGrob(),
    textGrob("Female", gp = gpar(fontsize = 11, fontface = "bold"), rot = 270),
    nrow = 3, heights = c(0.45, 0.1, 0.45)
  ),
  nrow = 1, widths = c(0.92, 0.08)
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
complete_panel_c <- arrangeGrob(panel_1c_with_sex, rank_legend_clean, 
                               nrow = 1, widths = c(4, 1.5))

# Save
ggsave("results/figures/Panel_C_FINAL_COLORED.png", complete_panel_c, 
       width = 450, height = 200, units = "mm", dpi = 300)

cat("\n=============================================================================\n")
cat("PANEL C FINAL VERSION WITH COLORS!\n") 
cat("=============================================================================\n")
cat("✓ Colored backgrounds by rank:\n")
cat("  - F & C: Dark blue (#003366) - Rank 1\n")
cat("  - D & I: Medium blue (#4682B4) - Rank 2\n") 
cat("  - E & A: Light blue (#87CEEB) - Rank 3\n")
cat("✓ White monkey letters on colored backgrounds\n")
cat("✓ SINGLE 'Male' label for top row only\n")
cat("✓ SINGLE 'Female' label for bottom row only\n")
cat("✓ Clean rank legend showing color meanings\n")
cat("✓ All real data, no dummy data\n")
cat("=============================================================================\n")
