# Complete Figure 1 with FIXED Panel C (colored backgrounds, single sex labels)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(viridis)

cat("Creating complete Figure 1 with fixed Panel C...\n")

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
bio_colors <- c("explore" = "#2E8B57", "exploit" = "#CD5C5C", "none" = "#4682B4")
rank_colors <- c("1" = "#003366", "2" = "#4682B4", "3" = "#87CEEB")

# PANEL A: Social vs Non-social comparison
panel_1a_data <- data_clean %>%
  mutate(social_type = ifelse(social_complexity == "solo", "Non-social", "Social")) %>%
  group_by(social_type, outcome_clean) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(social_type) %>%
  mutate(total = sum(count),
         proportion = count / total)

panel_1a <- ggplot(panel_1a_data, aes(x = social_type, y = proportion, fill = outcome_clean)) +
  geom_col(width = 0.6, color = "black", linewidth = 0.3, alpha = 0.9) +
  scale_fill_manual(values = bio_colors, name = "Choice Type",
                    labels = c("Explore", "Exploit", "None")) +
  scale_y_continuous(labels = function(x) paste0(round(x*100), "%"), 
                     expand = expansion(mult = c(0, 0.05))) +
  labs(title = "A", subtitle = "Social Context Effect on Choice Behavior",
       x = "Context", y = "Proportion of Choices") +
  theme_classic(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = -0.1),
        legend.position = "top",
        legend.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold"))

# PANEL B: Linear trend across complexity levels
panel_1b_data <- data_clean %>%
  group_by(social_complexity, outcome_clean) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(social_complexity) %>%
  mutate(total = sum(count),
         proportion = count / total) %>%
  filter(outcome_clean == "explore")

panel_1b <- ggplot(panel_1b_data, aes(x = social_complexity, y = proportion)) +
  geom_line(aes(group = 1), color = "#2E8B57", linewidth = 1.2, alpha = 0.8) +
  geom_point(color = "#2E8B57", size = 4, alpha = 0.9) +
  scale_y_continuous(labels = function(x) paste0(round(x*100), "%"),
                     expand = expansion(mult = c(0.1, 0.1))) +
  labs(title = "B", subtitle = "Exploration Decreases with Social Complexity",
       x = "Social Complexity", y = "Exploration Rate") +
  theme_classic(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = -0.1),
        axis.title = element_text(face = "bold"))

# PANEL C: Individual patterns with FIXED colors and labels
panel_1c_data <- data_clean %>%
  group_by(monkey, initial, sex, ABSOLUTE_RANK, social_complexity, outcome_clean) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(monkey, initial, sex, ABSOLUTE_RANK, social_complexity) %>%
  mutate(total = sum(count),
         proportion = count / total) %>%
  ungroup()

panel_1c_data$initial <- factor(panel_1c_data$initial, levels = c("F", "D", "E", "C", "I", "A"))

# Create individual plots for each monkey with colored backgrounds
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

# Arrange Panel C
top_row <- arrangeGrob(f_plot, d_plot, e_plot, nrow = 1)
bottom_row <- arrangeGrob(c_plot, i_plot, a_plot, nrow = 1)

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

panel_1c_with_y <- arrangeGrob(
  textGrob("Proportion of Choices", gp = gpar(fontsize = 11, fontface = "bold"), rot = 90),
  panel_1c_titled,
  nrow = 1, widths = c(0.05, 0.95)
)

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

# Rank legend for Panel C
rank_legend_clean <- ggplot() +
  geom_rect(aes(xmin = 0, xmax = 0.4, ymin = 2.6, ymax = 3), 
            fill = rank_colors["1"], color = "black", linewidth = 0.3) +
  geom_rect(aes(xmin = 0, xmax = 0.4, ymin = 1.6, ymax = 2), 
            fill = rank_colors["2"], color = "black", linewidth = 0.3) +
  geom_rect(aes(xmin = 0, xmax = 0.4, ymin = 0.6, ymax = 1), 
            fill = rank_colors["3"], color = "black", linewidth = 0.3) +
  annotate("text", x = 0.5, y = 2.8, label = "Rank 1", hjust = 0, size = 3.5, fontface = "bold") +
  annotate("text", x = 0.5, y = 1.8, label = "Rank 2", hjust = 0, size = 3.5, fontface = "bold") +
  annotate("text", x = 0.5, y = 0.8, label = "Rank 3", hjust = 0, size = 3.5, fontface = "bold") +
  scale_x_continuous(limits = c(0, 2.5)) +
  scale_y_continuous(limits = c(0.4, 3.2)) +
  labs(title = "Dominance Rank") +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 10, hjust = 0))

panel_1c_complete <- arrangeGrob(panel_1c_with_sex, rank_legend_clean, 
                                nrow = 1, widths = c(4, 1))

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
  geom_point(size = 4, alpha = 0.9) +
  geom_errorbar(aes(ymin = mean_explore - se_explore, ymax = mean_explore + se_explore),
                width = 0.1, linewidth = 1, alpha = 0.8) +
  scale_color_manual(values = c("Male" = "#1f77b4", "Female" = "#ff7f0e"), name = "Sex") +
  scale_y_continuous(labels = function(x) paste0(round(x*100), "%"),
                     expand = expansion(mult = c(0.1, 0.1))) +
  labs(title = "D", subtitle = "Sex Differences in Exploration",
       x = "Social Complexity", y = "Mean Exploration Rate") +
  theme_classic(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 14, hjust = -0.1),
        legend.position = "top",
        legend.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold"))

# Combine all panels
figure_1_complete <- arrangeGrob(
  arrangeGrob(panel_1a, panel_1b, nrow = 1),
  arrangeGrob(panel_1c_complete, panel_1d, nrow = 1, widths = c(3, 1)),
  nrow = 2, heights = c(1, 1.2)
)

# Save complete figure
ggsave("results/figures/Figure1_COMPLETE_FIXED_COLORS_LABELS.png", figure_1_complete, 
       width = 500, height = 400, units = "mm", dpi = 300)

ggsave("results/figures/Figure1_COMPLETE_FIXED_COLORS_LABELS.tiff", figure_1_complete, 
       width = 500, height = 400, units = "mm", dpi = 300, compression = "lzw")

cat("\n=============================================================================\n")
cat("COMPLETE FIGURE 1 WITH FIXED PANEL C!\n") 
cat("=============================================================================\n")
cat("✓ Panel A: Social vs non-social comparison\n")
cat("✓ Panel B: Linear decline across complexity\n") 
cat("✓ Panel C: Individual patterns with FIXED:\n")
cat("  - Colored backgrounds by rank (F&C=dark blue, D&I=medium, E&A=light)\n")
cat("  - White monkey letters on colored backgrounds\n")
cat("  - SINGLE 'Male' and 'Female' labels\n")
cat("  - Clean rank legend\n")
cat("✓ Panel D: Sex differences with error bars\n")
cat("✓ Saved as PNG and TIFF\n")
cat("=============================================================================\n")
