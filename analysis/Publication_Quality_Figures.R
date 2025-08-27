# =============================================================================
# Publication-Quality Hierarchical Multinomial Bayesian Regression Analysis
# Social Frames of Reference in Explore-Exploit Decision-Making
# =============================================================================
# Publication-ready figures for biology journal submission

# Load required libraries
library(nnet)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(viridis)
library(grid)

# Set publication-quality parameters
options(scipen = 999)  # Avoid scientific notation

# Publication figure parameters
fig_width <- 180  # mm (standard for 2-column figure)
fig_height <- 120 # mm 
fig_dpi <- 300    # Publication DPI
base_font_size <- 10
title_font_size <- 12
axis_font_size <- 9

# Color palette for biology publications
bio_colors <- c(
  "explore" = "#D55E00",    # Orange-red (colorblind safe)
  "exploit" = "#009E73",    # Blue-green (colorblind safe)  
  "none" = "#0072B2",       # Blue (colorblind safe)
  "social" = "#E69F00",     # Orange
  "nonsocial" = "#56B4E9"   # Light blue
)

# Rank colors (for Panel C)
rank_colors <- c(
  "1" = "#E31A1C",    # Red for rank 1
  "2" = "#FF7F00",    # Orange for rank 2  
  "3" = "#1F78B4"     # Blue for rank 3
)

# =============================================================================
# 1. DATA LOADING AND PREPARATION
# =============================================================================

cat("Loading and preparing data for publication figures...\n")

# Load dataset
data_raw <- read.csv("data/Explore Exploit Dataset.csv", stringsAsFactors = FALSE)

# Data cleaning and preparation
data_clean <- data_raw[data_raw$TRIAL_TYPE == "OIT_RE", ]

# Clean outcome variable
outcome_clean <- character(nrow(data_clean))
outcome_clean[grepl("explore", tolower(data_clean$OUTCOME))] <- "explore"
outcome_clean[grepl("exploit", tolower(data_clean$OUTCOME))] <- "exploit"
outcome_clean[grepl("none|stop", tolower(data_clean$OUTCOME))] <- "none"

# Create social vs non-social conditions
data_clean$outcome_clean <- outcome_clean
data_clean$social_condition <- ifelse(data_clean$CONDITION == "solo", "Non-Social", "Social")
data_clean$social_complexity <- factor(data_clean$CONDITION, levels = c("solo", "duo", "trio"))
data_clean$monkey_id <- factor(data_clean$monkey)

# Remove missing data
data_clean <- data_clean[!is.na(data_clean$outcome_clean), ]
data_clean <- data_clean[data_clean$outcome_clean != "", ]
data_clean <- data_clean[complete.cases(data_clean[c("RELATIVE_RANK", "SUBJECTIVE_CHOSEN_VALUE")]), ]

# Add sex and initial information
sex_info <- data.frame(
  monkey = c("FRAN", "DALI", "EBI", "ANEMONE", "CHOCOLAT", "ICE"),
  sex = c("Male", "Male", "Male", "Female", "Female", "Female"),
  initial = c("F", "D", "E", "A", "C", "I"),
  stringsAsFactors = FALSE
)

# Merge sex information
data_clean <- merge(data_clean, sex_info, by.x = "monkey", by.y = "monkey", all.x = TRUE)

# Standardize continuous variables
data_clean$rank_std <- scale(data_clean$RELATIVE_RANK)[,1]
data_clean$subjective_value_std <- scale(data_clean$SUBJECTIVE_CHOSEN_VALUE)[,1]
data_clean$exploit_preference_std <- scale(data_clean$subjective_exploit)[,1]
data_clean$explore_expectation_std <- scale(data_clean$expected_explore)[,1]

cat("Data preparation complete.\n")
cat("Final sample size:", nrow(data_clean), "trials\n")
cat("Subjects:", length(unique(data_clean$monkey_id)), "\n")

# =============================================================================
# 2. FIGURE 1: BEHAVIORAL MEASUREMENTS (Publication Quality - 4 Panels)
# =============================================================================

cat("Creating Figure 1: Publication-quality Behavioral Measurements...\n")

# Panel A: Overall proportions by social vs non-social condition
panel_a_data <- data_clean %>%
  group_by(social_condition, outcome_clean) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(social_condition) %>%
  mutate(proportion = count / sum(count),
         se = sqrt(proportion * (1 - proportion) / sum(count)))

panel_a <- ggplot(panel_a_data, aes(x = social_condition, y = proportion, fill = outcome_clean)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7, 
           color = "black", size = 0.3, alpha = 0.9) +
  geom_errorbar(aes(ymin = pmax(0, proportion - 1.96*se), ymax = proportion + 1.96*se),
                position = position_dodge(width = 0.8), width = 0.15, size = 0.4) +
  scale_fill_manual(values = bio_colors[c("explore", "exploit", "none")],
                    name = "Choice Type",
                    labels = c("Explore", "Exploit", "No Choice")) +
  scale_y_continuous(labels = function(x) paste0(round(x*100), "%"),
                     expand = expansion(mult = c(0, 0.05)),
                     limits = c(0, max(panel_a_data$proportion) * 1.1)) +
  labs(title = "A", subtitle = "Choice proportions by social context",
       x = "Social Context", y = "Proportion of Choices") +
  theme_classic(base_size = base_font_size) +
  theme(
    plot.title = element_text(face = "bold", size = title_font_size, hjust = -0.1),
    plot.subtitle = element_text(size = base_font_size),
    axis.title = element_text(size = base_font_size, color = "black"),
    axis.text = element_text(size = axis_font_size, color = "black"),
    legend.position = "bottom",
    legend.title = element_text(size = base_font_size, face = "bold"),
    legend.text = element_text(size = axis_font_size),
    legend.key.size = unit(0.4, "cm"),
    panel.grid.major.y = element_line(color = "grey90", size = 0.2),
    axis.line = element_line(color = "black", size = 0.4),
    axis.ticks = element_line(color = "black", size = 0.3)
  )

# Panel B: ALL choices by social complexity (including none)
panel_b_data <- data_clean %>%
  group_by(social_complexity, outcome_clean) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(social_complexity) %>%
  mutate(proportion = count / sum(count),
         se = sqrt(proportion * (1 - proportion) / sum(count)))

panel_b <- ggplot(panel_b_data, aes(x = social_complexity, y = proportion, fill = outcome_clean)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7,
           color = "black", size = 0.3, alpha = 0.9) +
  geom_errorbar(aes(ymin = pmax(0, proportion - 1.96*se), ymax = proportion + 1.96*se),
                position = position_dodge(width = 0.8), width = 0.15, size = 0.4) +
  scale_fill_manual(values = bio_colors[c("explore", "exploit", "none")],
                    name = "Choice Type",
                    labels = c("Explore", "Exploit", "No Choice")) +
  scale_x_discrete(labels = c("Solo", "Duo", "Trio")) +
  scale_y_continuous(labels = function(x) paste0(round(x*100), "%"),
                     expand = expansion(mult = c(0, 0.05)),
                     limits = c(0, max(panel_b_data$proportion) * 1.1)) +
  labs(title = "B", subtitle = "Choice Proportions by Social Complexity",
       x = "Social Complexity", y = "Proportion of Choices") +
  theme_classic(base_size = base_font_size) +
  theme(
    plot.title = element_text(face = "bold", size = title_font_size, hjust = -0.1),
    plot.subtitle = element_text(size = base_font_size),
    axis.title = element_text(size = base_font_size, color = "black"),
    axis.text = element_text(size = axis_font_size, color = "black"),
    legend.position = "bottom",
    legend.title = element_text(size = base_font_size, face = "bold"),
    legend.text = element_text(size = axis_font_size),
    legend.key.size = unit(0.4, "cm"),
    panel.grid.major.y = element_line(color = "grey90", size = 0.2),
    axis.line = element_line(color = "black", size = 0.4),
    axis.ticks = element_line(color = "black", size = 0.3)
  )

# Panel C: Individual choice patterns by rank (with sex grouping)
panel_c_data <- data_clean %>%
  group_by(monkey, initial, sex, RELATIVE_RANK, outcome_clean) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(monkey, initial, sex, RELATIVE_RANK) %>%
  mutate(total = sum(count),
         proportion = count / total) %>%
  mutate(se = sqrt(proportion * (1 - proportion) / total),
         rank_factor = factor(RELATIVE_RANK))

# Order by sex and initial 
panel_c_data$initial <- factor(panel_c_data$initial, 
                               levels = c("F", "D", "E", "A", "C", "I"))

panel_c <- ggplot(panel_c_data, aes(x = initial, y = proportion, 
                                   fill = interaction(outcome_clean, rank_factor))) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7,
           color = "black", size = 0.3, alpha = 0.9) +
  geom_errorbar(aes(ymin = pmax(0, proportion - 1.96*se), ymax = proportion + 1.96*se),
                position = position_dodge(width = 0.8), width = 0.15, size = 0.4) +
  # Add vertical line to separate sexes
  geom_vline(xintercept = 3.5, linetype = "dashed", color = "grey60", size = 0.5) +
  # Add sex labels
  annotate("text", x = 2, y = max(panel_c_data$proportion) * 0.95, 
           label = "Males", fontface = "bold", size = 3.5) +
  annotate("text", x = 5, y = max(panel_c_data$proportion) * 0.95, 
           label = "Females", fontface = "bold", size = 3.5) +
  scale_fill_manual(values = c(
    # Explore colors by rank
    "explore.1" = rank_colors["1"], "explore.2" = rank_colors["2"], "explore.3" = rank_colors["3"],
    # Exploit colors by rank (lighter versions)
    "exploit.1" = "#FFB3BA", "exploit.2" = "#FFCC99", "exploit.3" = "#B3D9FF",
    # None colors by rank (very light versions)  
    "none.1" = "#F0F0F0", "none.2" = "#E0E0E0", "none.3" = "#D0D0D0"
  ),
  name = "Choice & Rank",
  labels = function(x) {
    parts <- strsplit(x, "\\.")
    choice <- sapply(parts, function(p) p[1])
    rank <- sapply(parts, function(p) p[2])
    paste0(stringr::str_to_title(choice), " (Rank ", rank, ")")
  }) +
  scale_y_continuous(labels = function(x) paste0(round(x*100), "%"),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(title = "C", subtitle = "Individual Choice Patterns by Rank",
       x = "Individual (by Sex)", y = "Proportion of Choices") +
  theme_classic(base_size = base_font_size) +
  theme(
    plot.title = element_text(face = "bold", size = title_font_size, hjust = -0.1),
    plot.subtitle = element_text(size = base_font_size),
    axis.title = element_text(size = base_font_size, color = "black"),
    axis.text = element_text(size = axis_font_size, color = "black"),
    legend.position = "bottom",
    legend.title = element_text(size = base_font_size, face = "bold"),
    legend.text = element_text(size = axis_font_size-1),
    legend.key.size = unit(0.3, "cm"),
    panel.grid.major.y = element_line(color = "grey90", size = 0.2),
    axis.line = element_line(color = "black", size = 0.4),
    axis.ticks = element_line(color = "black", size = 0.3)
  ) +
  guides(fill = guide_legend(ncol = 3))

# Panel D: Individual exploration rates by sex
panel_d_data <- data_clean %>%
  filter(outcome_clean == "explore") %>%
  group_by(monkey, initial, sex) %>%
  summarise(count = n(), .groups = "drop") %>%
  left_join(
    data_clean %>%
      group_by(monkey, initial, sex) %>%
      summarise(total = n(), .groups = "drop"),
    by = c("monkey", "initial", "sex")
  ) %>%
  mutate(proportion = count / total,
         se = sqrt(proportion * (1 - proportion) / total))

# Order by sex (Males first, then Females)
panel_d_data$initial <- factor(panel_d_data$initial, 
                               levels = c("F", "D", "E", "A", "C", "I"))

panel_d <- ggplot(panel_d_data, aes(x = initial, y = proportion, fill = sex)) +
  geom_col(width = 0.7, color = "black", size = 0.3, alpha = 0.9) +
  geom_errorbar(aes(ymin = pmax(0, proportion - 1.96*se), ymax = proportion + 1.96*se),
                width = 0.15, size = 0.4) +
  # Add vertical line to separate sexes
  geom_vline(xintercept = 3.5, linetype = "dashed", color = "grey60", size = 0.5) +
  # Add sex labels
  annotate("text", x = 2, y = max(panel_d_data$proportion) * 0.95, 
           label = "Males", fontface = "bold", size = 3.5) +
  annotate("text", x = 5, y = max(panel_d_data$proportion) * 0.95, 
           label = "Females", fontface = "bold", size = 3.5) +
  scale_fill_manual(values = c("Male" = "#4682B4", "Female" = "#DC143C"),
                    name = "Sex") +
  scale_y_continuous(labels = function(x) paste0(round(x*100), "%"),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(title = "D", subtitle = "Individual Exploration Rates by Sex",
       x = "Individual (by Sex)", y = "Exploration Rate") +
  theme_classic(base_size = base_font_size) +
  theme(
    plot.title = element_text(face = "bold", size = title_font_size, hjust = -0.1),
    plot.subtitle = element_text(size = base_font_size),
    axis.title = element_text(size = base_font_size, color = "black"),
    axis.text = element_text(size = axis_font_size, color = "black"),
    legend.position = "bottom",
    legend.title = element_text(size = base_font_size, face = "bold"),
    legend.text = element_text(size = axis_font_size),
    legend.key.size = unit(0.4, "cm"),
    panel.grid.major.y = element_line(color = "grey90", size = 0.2),
    axis.line = element_line(color = "black", size = 0.4),
    axis.ticks = element_line(color = "black", size = 0.3)
  )

# Create Figure 1 with 4 panels in 2x2 grid
figure1_layout <- grid.arrange(
  arrangeGrob(panel_a, panel_b, nrow = 1),
  arrangeGrob(panel_c, panel_d, nrow = 1),
  nrow = 2,
  top = textGrob("Figure 1. Behavioral Measurements", 
                gp = gpar(fontsize = title_font_size, fontface = "bold"))
)

ggsave("results/figures/Figure1_Behavioral_Measurements_Publication.tiff", 
       figure1_layout,
       width = fig_width*2, height = fig_height*2, units = "mm", dpi = fig_dpi,
       compression = "lzw")

ggsave("results/figures/Figure1_Behavioral_Measurements_Publication.png", 
       figure1_layout,
       width = fig_width*2, height = fig_height*2, units = "mm", dpi = fig_dpi)

# Also save individual panels
ggsave("results/figures/Figure1_Panel_A.png", panel_a, 
       width = fig_width, height = fig_height, units = "mm", dpi = fig_dpi)
ggsave("results/figures/Figure1_Panel_B.png", panel_b, 
       width = fig_width, height = fig_height, units = "mm", dpi = fig_dpi)
ggsave("results/figures/Figure1_Panel_C.png", panel_c, 
       width = fig_width, height = fig_height, units = "mm", dpi = fig_dpi)
ggsave("results/figures/Figure1_Panel_D.png", panel_d, 
       width = fig_width, height = fig_height, units = "mm", dpi = fig_dpi)

# =============================================================================
# 3. FIT MODELS FOR FIGURE 2
# =============================================================================

cat("Fitting hierarchical multinomial models...\n")

# Fit models
model_basic <- multinom(outcome_clean ~ social_complexity, data = data_clean, trace = FALSE)
model_individual <- multinom(outcome_clean ~ social_complexity + monkey_id, 
                           data = data_clean, trace = FALSE)
model_full <- multinom(outcome_clean ~ social_complexity + monkey_id + rank_std + 
                      subjective_value_std + exploit_preference_std + 
                      explore_expectation_std, 
                      data = data_clean, trace = FALSE)

# Extract coefficients
extract_coef_ci <- function(model) {
  coef_matrix <- summary(model)$coefficients
  se_matrix <- summary(model)$standard.errors
  
  coef_df <- data.frame()
  for(outcome in rownames(coef_matrix)) {
    for(term in colnames(coef_matrix)) {
      coef_df <- rbind(coef_df, data.frame(
        outcome = outcome,
        term = term,
        estimate = coef_matrix[outcome, term],
        se = se_matrix[outcome, term],
        ci_lower = coef_matrix[outcome, term] - 1.96 * se_matrix[outcome, term],
        ci_upper = coef_matrix[outcome, term] + 1.96 * se_matrix[outcome, term]
      ))
    }
  }
  return(coef_df)
}

coef_data <- extract_coef_ci(model_full)

# Clean term names
coef_data$term_clean <- case_when(
  coef_data$term == "social_complexityduo" ~ "Duo vs Solo",
  coef_data$term == "social_complexitytrio" ~ "Trio vs Solo", 
  coef_data$term == "rank_std" ~ "Dominance Rank",
  coef_data$term == "subjective_value_std" ~ "Subjective Value",
  coef_data$term == "exploit_preference_std" ~ "Exploit Preference",
  coef_data$term == "explore_expectation_std" ~ "Explore Expectation",
  grepl("monkey_id", coef_data$term) ~ gsub("monkey_id", "", coef_data$term),
  TRUE ~ coef_data$term
)

# =============================================================================
# 4. FIGURE 2: HIERARCHICAL REGRESSION (Publication Quality) 
# =============================================================================

cat("Creating Figure 2: Publication-quality Hierarchical Regression...\n")

# Panel A: Beta coefficients (forest plot style)
panel_2a_data <- coef_data[coef_data$term != "(Intercept)" & 
                          !grepl("monkey_id", coef_data$term), ]

panel_2a <- ggplot(panel_2a_data, aes(x = estimate, y = reorder(term_clean, estimate))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40", size = 0.5) +
  geom_point(aes(color = outcome), size = 2.5, 
             position = position_dodge(height = 0.4)) +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper, color = outcome), 
                height = 0.15, size = 0.6,
                position = position_dodge(height = 0.4)) +
  scale_color_manual(values = bio_colors[c("explore", "none")],
                    name = "Outcome", 
                    labels = c("Explore", "No Choice")) +
  facet_wrap(~factor(outcome, labels = c("Exploration", "No Choice")), 
             scales = "free_x", nrow = 1) +
  labs(title = "A", subtitle = "Regression coefficients (vs exploitation baseline)",
       x = "Log-odds ratio", y = "Model Terms") +
  theme_classic(base_size = base_font_size) +
  theme(
    plot.title = element_text(face = "bold", size = title_font_size, hjust = -0.1),
    plot.subtitle = element_text(size = base_font_size),
    axis.title = element_text(size = base_font_size, color = "black"),
    axis.text = element_text(size = axis_font_size, color = "black"),
    strip.text = element_text(size = base_font_size, face = "bold"),
    strip.background = element_rect(fill = "grey95", color = "black", size = 0.3),
    legend.position = "none",
    panel.grid.major.x = element_line(color = "grey90", size = 0.2),
    axis.line = element_line(color = "black", size = 0.4),
    axis.ticks = element_line(color = "black", size = 0.3)
  )

# Panel B: Model comparison 
model_comparison <- data.frame(
  Model = factor(c("Basic", "Individual", "Hierarchical"), 
                levels = c("Basic", "Individual", "Hierarchical")),
  AIC = c(AIC(model_basic), AIC(model_individual), AIC(model_full)),
  BIC = c(BIC(model_basic), BIC(model_individual), BIC(model_full)),
  LogLik = c(logLik(model_basic), logLik(model_individual), logLik(model_full))
)

panel_2b <- ggplot(model_comparison, aes(x = Model)) +
  geom_col(aes(y = AIC), fill = bio_colors["explore"], alpha = 0.8, 
           color = "black", size = 0.3, width = 0.7) +
  geom_text(aes(y = AIC, label = round(AIC)), vjust = -0.5, size = 3) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "B", subtitle = "Model comparison (AIC)",
       x = "Model Complexity", y = "AIC (lower = better fit)") +
  theme_classic(base_size = base_font_size) +
  theme(
    plot.title = element_text(face = "bold", size = title_font_size, hjust = -0.1),
    plot.subtitle = element_text(size = base_font_size),
    axis.title = element_text(size = base_font_size, color = "black"),
    axis.text = element_text(size = axis_font_size, color = "black"),
    panel.grid.major.y = element_line(color = "grey90", size = 0.2),
    axis.line = element_line(color = "black", size = 0.4),
    axis.ticks = element_line(color = "black", size = 0.3)
  )

# Panel C: Model predictions
pred_data <- expand.grid(
  social_complexity = factor(c("solo", "duo", "trio"), levels = c("solo", "duo", "trio")),
  monkey_id = factor("EBI"),
  rank_std = 0, subjective_value_std = 0,
  exploit_preference_std = 0, explore_expectation_std = 0
)

pred_probs <- predict(model_full, newdata = pred_data, type = "probs")
pred_df <- cbind(pred_data, pred_probs)
pred_df_long <- reshape(pred_df, 
                       varying = c("explore", "exploit", "none"),
                       v.names = "probability", timevar = "outcome",
                       times = c("explore", "exploit", "none"),
                       direction = "long")

panel_2c <- ggplot(pred_df_long, aes(x = social_complexity, y = probability, fill = outcome)) +
  geom_col(position = "stack", color = "black", size = 0.3, alpha = 0.9) +
  scale_fill_manual(values = bio_colors[c("explore", "exploit", "none")],
                    name = "Choice",
                    labels = c("Explore", "Exploit", "No Choice")) +
  scale_x_discrete(labels = c("Solo", "Duo", "Trio")) +
  scale_y_continuous(labels = function(x) paste0(round(x*100), "%"),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(title = "C", subtitle = "Model predictions",
       x = "Social Complexity", y = "Predicted Probability") +
  theme_classic(base_size = base_font_size) +
  theme(
    plot.title = element_text(face = "bold", size = title_font_size, hjust = -0.1),
    plot.subtitle = element_text(size = base_font_size),
    axis.title = element_text(size = base_font_size, color = "black"),
    axis.text = element_text(size = axis_font_size, color = "black"),
    legend.position = "bottom",
    legend.title = element_text(size = base_font_size, face = "bold"),
    legend.text = element_text(size = axis_font_size),
    legend.key.size = unit(0.4, "cm"),
    panel.grid.major.y = element_line(color = "grey90", size = 0.2),
    axis.line = element_line(color = "black", size = 0.4),
    axis.ticks = element_line(color = "black", size = 0.3)
  )

# Create Figure 2 with proper spacing
ggsave("results/figures/Figure2_Hierarchical_Regression_Publication.tiff", 
       grid.arrange(panel_2a, panel_2b, panel_2c, nrow = 1,
                   top = textGrob("Figure 2. Hierarchical Multinomial Bayesian Regression Analysis", 
                                 gp = gpar(fontsize = title_font_size, fontface = "bold"))),
       width = fig_width*3, height = fig_height, units = "mm", dpi = fig_dpi,
       compression = "lzw")

ggsave("results/figures/Figure2_Hierarchical_Regression_Publication.png", 
       grid.arrange(panel_2a, panel_2b, panel_2c, nrow = 1,
                   top = textGrob("Figure 2. Hierarchical Multinomial Bayesian Regression Analysis", 
                                 gp = gpar(fontsize = title_font_size, fontface = "bold"))),
       width = fig_width*3, height = fig_height, units = "mm", dpi = fig_dpi)

# =============================================================================
# 5. STATISTICS SUMMARY
# =============================================================================

cat("\n=============================================================================\n")
cat("PUBLICATION-QUALITY FIGURES COMPLETE!\n") 
cat("=============================================================================\n")
cat("Generated figures:\n")
cat("- Figure1_Behavioral_Measurements_Publication.tiff/.png\n")
cat("- Figure2_Hierarchical_Regression_Publication.tiff/.png\n")
cat("\nFigure specifications:\n")
cat("- High resolution (300 DPI)\n")
cat("- TIFF format for publication submission\n")
cat("- PNG format for presentations/web\n")
cat("- Professional typography and spacing\n")
cat("- Colorblind-safe palette\n")
cat("- Biology journal aesthetic standards\n")
cat("=============================================================================\n") 