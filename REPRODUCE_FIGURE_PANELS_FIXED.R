# Minimal script for panel D only
library(ggplot2)
library(scales)

panel_D_data <- data.frame(
  INDIVIDUAL_CODE = c('F', 'D', 'E', 'C', 'I', 'A'),
  SEX = c('Male', 'Male', 'Male', 'Female', 'Female', 'Female'),
  explore_rate = c(0.507, 0.368, 0.278, 0.289, 0.305, 0.191)
)

sex_palette <- c('Male' = '#ff7f0e', 'Female' = '#9467bd')

panel_D_data$INDIVIDUAL_CODE <- factor(panel_D_data$INDIVIDUAL_CODE, levels = c('F', 'D', 'E', 'C', 'I', 'A'))

print(panel_D_data)

panel_D <- ggplot(panel_D_data, aes(x = INDIVIDUAL_CODE, y = explore_rate, fill = SEX)) +
  geom_col(width = 0.7) +
  scale_x_discrete(drop = FALSE) +
  scale_fill_manual(values = sex_palette, name = 'Sex') +
  scale_y_continuous(labels = percent_format(), limits = c(0, 0.5)) +
  labs(title = 'D', x = 'Individual (by Sex)', y = 'Exploration Rate') +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = 'bold', size = 14),
    axis.title = element_text(face = 'bold'),
    legend.position = 'bottom'
  )

ggsave('panel_D_behavioral_PRO.png', panel_D, width = 8, height = 5, dpi = 300) 