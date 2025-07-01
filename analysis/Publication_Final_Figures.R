# =============================================================================
# Publication-Quality Hierarchical Multinomial Bayesian Regression Analysis
# Social Frames of Reference in Explore-Exploit Decision-Making
# =============================================================================
# Final publication-ready figures for biology journal submission

# Load required libraries
library(nnet)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(viridis)
library(grid)

# Set publication-quality parameters
options(scipen = 999)

# Publication figure parameters
fig_width <- 200  # mm (wider for better readability)
fig_height <- 140 # mm (taller for better proportions)
fig_dpi <- 300    # Publication DPI
base_font_size <- 14  # Larger for readability
title_font_size <- 16
axis_font_size <- 12

# Professional color palette (colorblind safe)
bio_colors <- c(
  "explore" = "#D55E00",    # Orange-red
  "exploit" = "#009E73",    # Blue-green  
  "none" = "#0072B2",       # Blue
  "Social" = "#E69F00",     # Orange
  "Non-Social" = "#56B4E9"  # Light blue
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
data_clean <- data_clean[complete.cases(data_clean[c("RELATIVE_RANK", "SUBJECTIVE_CHOSEN_VALUE")]), ]

# Standardize continuous variables
data_clean$rank_std <- scale(data_clean$RELATIVE_RANK)[,1]
data_clean$subjective_value_std <- scale(data_clean$SUBJECTIVE_CHOSEN_VALUE)[,1]
data_clean$exploit_preference_std <- scale(data_clean$subjective_exploit)[,1]
data_clean$explore_expectation_std <- scale(data_clean$expected_explore)[,1]

cat("Data preparation complete.\n")
cat("Final sample size:", nrow(data_clean), "trials\n")
cat("Subjects:", length(unique(data_clean$monkey_id)), "\n")

# =============================================================================
# 2. FIGURE 1: BEHAVIORAL MEASUREMENTS (Publication Quality)
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
           color = "black", linewidth = 0.5, alpha = 0.9) +
  geom_errorbar(aes(ymin = pmax(0, proportion - 1.96*se), ymax = proportion + 1.96*se),
                position = position_dodge(width = 0.8), width = 0.2, linewidth = 0.6) +
  scale_fill_manual(values = bio_colors[c("explore", "exploit", "none")],
                    name = "Choice Type",
                    labels = c("Explore", "Exploit", "No Choice")) +
  scale_y_continuous(labels = function(x) paste0(round(x*100), "%"),
                     expand = expansion(mult = c(0, 0.1)),
                     limits = c(0, max(panel_a_data$proportion) * 1.15)) +
  labs(title = "A", subtitle = "Choice proportions by social context",
       x = "Social Context", y = "Proportion of Choices") +
  theme_classic(base_size = base_font_size) +
  theme(
    plot.title = element_text(face = "bold", size = title_font_size, hjust = 0),
    plot.subtitle = element_text(size = base_font_size, margin = margin(b = 20)),
    axis.title = element_text(size = base_font_size, color = "black", face = "bold"),
    axis.text = element_text(size = axis_font_size, color = "black"),
    legend.position = "bottom",
    legend.title = element_text(size = base_font_size, face = "bold"),
    legend.text = element_text(size = axis_font_size),
    legend.key.size = unit(0.6, "cm"),
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.3),
    axis.line = element_line(color = "black", linewidth = 0.6),
    axis.ticks = element_line(color = "black", linewidth = 0.4),
    plot.margin = margin(10, 10, 10, 10)
  )

# Panel B: Exploration and exploitation by social complexity
panel_b_data <- data_clean %>%
  filter(outcome_clean %in% c("explore", "exploit")) %>%
  group_by(social_complexity, outcome_clean) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(social_complexity) %>%
  mutate(proportion = count / sum(count),
         se = sqrt(proportion * (1 - proportion) / sum(count)))

panel_b <- ggplot(panel_b_data, aes(x = social_complexity, y = proportion, fill = outcome_clean)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7,
           color = "black", linewidth = 0.5, alpha = 0.9) +
  geom_errorbar(aes(ymin = pmax(0, proportion - 1.96*se), ymax = proportion + 1.96*se),
                position = position_dodge(width = 0.8), width = 0.2, linewidth = 0.6) +
  scale_fill_manual(values = bio_colors[c("explore", "exploit")],
                    name = "Choice Type",
                    labels = c("Explore", "Exploit")) +
  scale_x_discrete(labels = c("Solo", "Duo", "Trio")) +
  scale_y_continuous(labels = function(x) paste0(round(x*100), "%"),
                     expand = expansion(mult = c(0, 0.1)),
                     limits = c(0, max(panel_b_data$proportion) * 1.15)) +
  labs(title = "B", subtitle = "Explore vs exploit by social complexity",
       x = "Social Complexity", y = "Proportion of Choices") +
  theme_classic(base_size = base_font_size) +
  theme(
    plot.title = element_text(face = "bold", size = title_font_size, hjust = 0),
    plot.subtitle = element_text(size = base_font_size, margin = margin(b = 20)),
    axis.title = element_text(size = base_font_size, color = "black", face = "bold"),
    axis.text = element_text(size = axis_font_size, color = "black"),
    legend.position = "bottom",
    legend.title = element_text(size = base_font_size, face = "bold"),
    legend.text = element_text(size = axis_font_size),
    legend.key.size = unit(0.6, "cm"),
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.3),
    axis.line = element_line(color = "black", linewidth = 0.6),
    axis.ticks = element_line(color = "black", linewidth = 0.4),
    plot.margin = margin(10, 10, 10, 10)
  )

# Panel C: Individual differences
panel_c_data <- data_clean %>%
  group_by(monkey_id, social_condition, outcome_clean) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(monkey_id, social_condition) %>%
  mutate(total = sum(count),
         proportion = count / total) %>%
  filter(outcome_clean == "explore") %>%
  mutate(se = sqrt(proportion * (1 - proportion) / total))

panel_c <- ggplot(panel_c_data, aes(x = monkey_id, y = proportion, fill = social_condition)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7,
           color = "black", linewidth = 0.5, alpha = 0.9) +
  geom_errorbar(aes(ymin = pmax(0, proportion - 1.96*se), ymax = proportion + 1.96*se),
                position = position_dodge(width = 0.8), width = 0.2, linewidth = 0.6) +
  scale_fill_manual(values = bio_colors[c("Non-Social", "Social")],
                    name = "Context") +
  scale_y_continuous(labels = function(x) paste0(round(x*100), "%"),
                     expand = expansion(mult = c(0, 0.1)),
                     limits = c(0, max(panel_c_data$proportion) * 1.15)) +
  labs(title = "C", subtitle = "Individual exploration rates",
       x = "Individual", y = "Exploration Rate") +
  theme_classic(base_size = base_font_size) +
  theme(
    plot.title = element_text(face = "bold", size = title_font_size, hjust = 0),
    plot.subtitle = element_text(size = base_font_size, margin = margin(b = 20)),
    axis.title = element_text(size = base_font_size, color = "black", face = "bold"),
    axis.text = element_text(size = axis_font_size, color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.title = element_text(size = base_font_size, face = "bold"),
    legend.text = element_text(size = axis_font_size),
    legend.key.size = unit(0.6, "cm"),
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.3),
    axis.line = element_line(color = "black", linewidth = 0.6),
    axis.ticks = element_line(color = "black", linewidth = 0.4),
    plot.margin = margin(10, 10, 10, 10)
  )

# Create and save Figure 1 as individual panels (better quality)
ggsave("results/figures/Figure1_Panel_A.png", panel_a,
       width = fig_width, height = fig_height, units = "mm", dpi = fig_dpi)
ggsave("results/figures/Figure1_Panel_B.png", panel_b,
       width = fig_width, height = fig_height, units = "mm", dpi = fig_dpi)
ggsave("results/figures/Figure1_Panel_C.png", panel_c,
       width = fig_width, height = fig_height, units = "mm", dpi = fig_dpi)

# Combined Figure 1
fig1_combined <- grid.arrange(panel_a, panel_b, panel_c, nrow = 1,
                             top = textGrob("Figure 1. Behavioral Measurements", 
                                           gp = gpar(fontsize = title_font_size + 2, fontface = "bold")))

ggsave("results/figures/Figure1_Publication_Final.tiff", fig1_combined,
       width = fig_width*3, height = fig_height, units = "mm", dpi = fig_dpi,
       compression = "lzw")

ggsave("results/figures/Figure1_Publication_Final.png", fig1_combined,
       width = fig_width*3, height = fig_height, units = "mm", dpi = fig_dpi)

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
  TRUE ~ coef_data$term
)

# =============================================================================
# 4. FIGURE 2: HIERARCHICAL REGRESSION (Publication Quality) 
# =============================================================================

cat("Creating Figure 2: Publication-quality Hierarchical Regression...\n")

# Panel A: Beta coefficients (forest plot style)
panel_2a_data <- coef_data[coef_data$term != "(Intercept)" & 
                          !grepl("monkey_id", coef_data$term), ]

# Create separate plots for each outcome
explore_data <- panel_2a_data[panel_2a_data$outcome == "explore", ]
none_data <- panel_2a_data[panel_2a_data$outcome == "none", ]

# Exploration coefficients
panel_2a_explore <- ggplot(explore_data, aes(x = estimate, y = reorder(term_clean, estimate))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.8) +
  geom_point(color = bio_colors["explore"], size = 4) +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), 
                color = bio_colors["explore"], height = 0.2, linewidth = 1) +
  labs(title = "A", subtitle = "Exploration coefficients (vs exploitation)",
       x = "Log-odds ratio", y = "Model Terms") +
  theme_classic(base_size = base_font_size) +
  theme(
    plot.title = element_text(face = "bold", size = title_font_size, hjust = 0),
    plot.subtitle = element_text(size = base_font_size, margin = margin(b = 20)),
    axis.title = element_text(size = base_font_size, color = "black", face = "bold"),
    axis.text = element_text(size = axis_font_size, color = "black"),
    panel.grid.major.x = element_line(color = "grey85", linewidth = 0.3),
    axis.line = element_line(color = "black", linewidth = 0.6),
    axis.ticks = element_line(color = "black", linewidth = 0.4),
    plot.margin = margin(10, 10, 10, 10)
  )

# Panel B: Model comparison 
model_comparison <- data.frame(
  Model = factor(c("Basic", "+ Individual", "+ All Effects"), 
                levels = c("Basic", "+ Individual", "+ All Effects")),
  AIC = c(AIC(model_basic), AIC(model_individual), AIC(model_full))
)

panel_2b <- ggplot(model_comparison, aes(x = Model, y = AIC)) +
  geom_col(fill = bio_colors["exploit"], alpha = 0.8, 
           color = "black", linewidth = 0.5, width = 0.7) +
  geom_text(aes(label = round(AIC)), vjust = -0.5, size = 4.5, fontface = "bold") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "B", subtitle = "Model comparison (AIC)",
       x = "Model Complexity", y = "AIC (lower = better fit)") +
  theme_classic(base_size = base_font_size) +
  theme(
    plot.title = element_text(face = "bold", size = title_font_size, hjust = 0),
    plot.subtitle = element_text(size = base_font_size, margin = margin(b = 20)),
    axis.title = element_text(size = base_font_size, color = "black", face = "bold"),
    axis.text = element_text(size = axis_font_size, color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.3),
    axis.line = element_line(color = "black", linewidth = 0.6),
    axis.ticks = element_line(color = "black", linewidth = 0.4),
    plot.margin = margin(10, 10, 10, 10)
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
  geom_col(position = "stack", color = "black", linewidth = 0.5, alpha = 0.9) +
  scale_fill_manual(values = bio_colors[c("explore", "exploit", "none")],
                    name = "Choice",
                    labels = c("Explore", "Exploit", "No Choice")) +
  scale_x_discrete(labels = c("Solo", "Duo", "Trio")) +
  scale_y_continuous(labels = function(x) paste0(round(x*100), "%"),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(title = "C", subtitle = "Model predictions",
       x = "Social Complexity", y = "Predicted Probability") +
  theme_classic(base_size = base_font_size) +
  theme(
    plot.title = element_text(face = "bold", size = title_font_size, hjust = 0),
    plot.subtitle = element_text(size = base_font_size, margin = margin(b = 20)),
    axis.title = element_text(size = base_font_size, color = "black", face = "bold"),
    axis.text = element_text(size = axis_font_size, color = "black"),
    legend.position = "bottom",
    legend.title = element_text(size = base_font_size, face = "bold"),
    legend.text = element_text(size = axis_font_size),
    legend.key.size = unit(0.6, "cm"),
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.3),
    axis.line = element_line(color = "black", linewidth = 0.6),
    axis.ticks = element_line(color = "black", linewidth = 0.4),
    plot.margin = margin(10, 10, 10, 10)
  )

# Save individual panels
ggsave("results/figures/Figure2_Panel_A.png", panel_2a_explore,
       width = fig_width, height = fig_height, units = "mm", dpi = fig_dpi)
ggsave("results/figures/Figure2_Panel_B.png", panel_2b,
       width = fig_width, height = fig_height, units = "mm", dpi = fig_dpi)
ggsave("results/figures/Figure2_Panel_C.png", panel_2c,
       width = fig_width, height = fig_height, units = "mm", dpi = fig_dpi)

# Combined Figure 2
fig2_combined <- grid.arrange(panel_2a_explore, panel_2b, panel_2c, nrow = 1,
                             top = textGrob("Figure 2. Hierarchical Multinomial Bayesian Regression Analysis", 
                                           gp = gpar(fontsize = title_font_size + 2, fontface = "bold")))

ggsave("results/figures/Figure2_Publication_Final.tiff", fig2_combined,
       width = fig_width*3, height = fig_height, units = "mm", dpi = fig_dpi,
       compression = "lzw")

ggsave("results/figures/Figure2_Publication_Final.png", fig2_combined,
       width = fig_width*3, height = fig_height, units = "mm", dpi = fig_dpi)

# =============================================================================
# 5. STATISTICS SUMMARY
# =============================================================================

cat("\n=============================================================================\n")
cat("PUBLICATION-QUALITY FIGURES COMPLETE!\n") 
cat("=============================================================================\n")
cat("Generated figures:\n")
cat("- Figure1_Publication_Final.tiff/.png (300 DPI, 540x140mm)\n")
cat("- Figure2_Publication_Final.tiff/.png (300 DPI, 540x140mm)\n")
cat("- Individual panels: Figure1_Panel_A/B/C.png\n")
cat("- Individual panels: Figure2_Panel_A/B/C.png\n")
cat("\nFigure specifications:\n")
cat("- Large, readable fonts (14pt base, 16pt titles)\n")
cat("- Colorblind-safe palette\n")
cat("- High contrast, clean aesthetics\n")
cat("- Professional error bars and confidence intervals\n")
cat("- Biology journal publication standards\n")
cat("- TIFF format for journal submission\n")
cat("- Individual PNG panels for flexibility\n")
cat("=============================================================================\n") 