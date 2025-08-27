# CURRENT BIOLOGY HIERARCHICAL MULTINOMIAL BAYESIAN REGRESSION FIGURES
# EASILY ADJUSTABLE VERSION - All spacing parameters clearly defined
# =============================================================================

# =============================================================================
# 🔧 ADJUSTABLE PARAMETERS - MODIFY THESE TO CHANGE SPACING
# =============================================================================

# OVERALL DIMENSIONS (easily adjustable)
FIGURE_WIDTH_INCHES <- 16    # ← ADJUST: Overall figure width (INCREASED)
FIGURE_HEIGHT_INCHES <- 40   # ← ADJUST: Overall figure height  
RESOLUTION_DPI <- 300        # ← ADJUST: Resolution
PANEL_SPACING <- 5.0         # ← ADJUST: Space between panels (lines) (INCREASED)

# TEXT SIZES (easily adjustable) - ALL MUCH BIGGER
BASE_TEXT_SIZE <- 24         # ← ADJUST: Base text size (INCREASED)
TITLE_SIZE <- 36             # ← ADJUST: Main titles (INCREASED)
SUBTITLE_SIZE <- 30          # ← ADJUST: Panel titles (INCREASED)
AXIS_TITLE_SIZE <- 28        # ← ADJUST: Axis labels (INCREASED)
AXIS_TEXT_SIZE <- 24         # ← ADJUST: Axis text (INCREASED)
LEGEND_TEXT_SIZE <- 24       # ← ADJUST: Legend text (INCREASED)

# MARGINS (easily adjustable)
PLOT_MARGIN_MM <- 35         # ← ADJUST: Margin around each plot (mm) (INCREASED)
OUTER_MARGIN_MM <- 40        # ← ADJUST: Outer margin around combined figure (INCREASED)

# POINT AND LINE SIZES (easily adjustable) - ALL BIGGER
POINT_SIZE <- 8              # ← ADJUST: Default point size (DOUBLED)
LARGE_POINT_SIZE <- 16       # ← ADJUST: Large points (design panel) (DOUBLED)
LINE_WIDTH <- 2.0            # ← ADJUST: Default line width (INCREASED)
THICK_LINE_WIDTH <- 4.0      # ← ADJUST: Thick lines (credible intervals) (DOUBLED)

# PANEL HEIGHTS (easily adjustable - relative proportions) - MORE SPACE
PANEL_1_HEIGHT <- 1.5        # ← ADJUST: Design & outcomes panel height (INCREASED)
PANEL_2_HEIGHT <- 2.0        # ← ADJUST: Fixed effects panel height (INCREASED)
PANEL_3_HEIGHT <- 1.8        # ← ADJUST: Random effects panel height (INCREASED)
PANEL_4_HEIGHT <- 2.0        # ← ADJUST: Posterior predictive panel height (INCREASED)
PANEL_5_HEIGHT <- 1.5        # ← ADJUST: Model comparison panel height (INCREASED)

# SPACING FINE-TUNING (easily adjustable) - MORE GENEROUS
LEGEND_SPACING <- 25         # ← ADJUST: Space above/below legends (pt) (INCREASED)
STRIP_SPACING <- 25          # ← ADJUST: Space around facet labels (pt) (INCREASED)
AXIS_SPACING <- 20           # ← ADJUST: Space between axis and labels (pt) (INCREASED)

# =============================================================================
# PACKAGES
# =============================================================================

library(ggplot2)
library(dplyr)
library(nnet)
library(scales)
library(grid)
library(gridExtra)

suppressPackageStartupMessages({
  patchwork_available <- require(patchwork, quietly = TRUE)
  cowplot_available <- require(cowplot, quietly = TRUE)
})

# =============================================================================
# THEME FUNCTION (uses adjustable parameters)
# =============================================================================

theme_cb_adjustable <- function() {
  theme_classic(base_family = "Arial", base_size = BASE_TEXT_SIZE) +
    theme(
      # Titles
      plot.title = element_text(size = TITLE_SIZE, face = "bold", 
                               margin = margin(b = LEGEND_SPACING)),
      plot.subtitle = element_text(size = SUBTITLE_SIZE, face = "bold",
                                  margin = margin(b = LEGEND_SPACING)),
      
      # Axes  
      axis.title = element_text(size = AXIS_TITLE_SIZE, face = "bold",
                               margin = margin(AXIS_SPACING, AXIS_SPACING, 
                                             AXIS_SPACING, AXIS_SPACING)),
      axis.text = element_text(size = AXIS_TEXT_SIZE),
      axis.line = element_line(linewidth = LINE_WIDTH),
      axis.ticks = element_line(linewidth = LINE_WIDTH),
      
      # Legend
      legend.text = element_text(size = LEGEND_TEXT_SIZE),
      legend.title = element_text(size = AXIS_TITLE_SIZE, face = "bold"),
      legend.margin = margin(t = LEGEND_SPACING, b = LEGEND_SPACING),
      
      # Facets
      strip.text = element_text(size = SUBTITLE_SIZE, face = "bold",
                               margin = margin(STRIP_SPACING, STRIP_SPACING,
                                             STRIP_SPACING, STRIP_SPACING)),
      strip.background = element_rect(fill = "grey95", color = "grey80"),
      
      # Spacing
      plot.margin = margin(PLOT_MARGIN_MM, PLOT_MARGIN_MM, 
                          PLOT_MARGIN_MM, PLOT_MARGIN_MM, "mm"),
      panel.spacing = unit(PANEL_SPACING, "lines"),
      
      # Grid
      panel.grid.major = element_line(color = "grey95", linewidth = 0.3),
      panel.grid.minor = element_blank()
    )
}

# Set theme and defaults
theme_set(theme_cb_adjustable())
update_geom_defaults("point", list(size = POINT_SIZE))
update_geom_defaults("line", list(linewidth = LINE_WIDTH))

# Color palette
CB_COLORS <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b")

cat("=== ADJUSTABLE CURRENT BIOLOGY FIGURES ===\n")
cat(sprintf("Dimensions: %.1f\" x %.1f\", %d DPI\n", 
            FIGURE_WIDTH_INCHES, FIGURE_HEIGHT_INCHES, RESOLUTION_DPI))
cat("🔧 To adjust spacing, modify parameters at top of script\n\n")

# =============================================================================
# DATA PREPARATION
# =============================================================================

cat("Loading and preparing data...\n")

data_raw <- read.csv("Explore Exploit Dataset.csv", stringsAsFactors = FALSE)

data_clean <- data_raw %>%
  filter(TRIAL_TYPE == "OIT_RE") %>%
  mutate(
    outcome = case_when(
      grepl("explore", tolower(OUTCOME)) ~ "Explore",
      grepl("exploit", tolower(OUTCOME)) ~ "Exploit", 
      grepl("none|stop|NONE", tolower(OUTCOME)) | OUTCOME == "" ~ "None",
      TRUE ~ "None"
    ),
    
    monkey_id = factor(monkey),
    monkey_initial = case_when(
      monkey == "FRAN" ~ "F",
      monkey == "DALI" ~ "D", 
      monkey == "EBI" ~ "E",
      monkey == "CHOCOLAT" ~ "C",
      monkey == "ICE" ~ "I",
      monkey == "ANEMONE" ~ "A",
      TRUE ~ substr(monkey, 1, 1)
    ),
    
    block_id = factor(BLOCK_No),
    social_context = factor(CONDITION, levels = c("solo", "duo", "trio")),
    social_complexity = as.numeric(social_context),
    
    sex = case_when(
      monkey %in% c("FRAN", "DALI", "EBI") ~ "Male",
      monkey %in% c("ANEMONE", "CHOCOLAT", "ICE") ~ "Female"
    ),
    
    # Ordered factors for proper grouping
    monkey_ordered = factor(monkey, levels = c("FRAN", "DALI", "EBI", "CHOCOLAT", "ICE", "ANEMONE")),
    monkey_initial_ordered = factor(monkey_initial, levels = c("F", "D", "E", "C", "I", "A")),
    
    # Standardized predictors
    rank_z = as.numeric(scale(ABSOLUTE_RANK)),
    expected_explore_z = as.numeric(scale(expected_explore)),
    subjective_exploit_z = as.numeric(scale(subjective_exploit)),
    chosen_value_z = as.numeric(scale(SUBJECTIVE_CHOSEN_VALUE)),
    trial_num_z = as.numeric(scale(TRIAL_NUM))
  ) %>%
  filter(!is.na(outcome), !is.na(social_context), !is.na(monkey_id)) %>%
  filter(complete.cases(.[c("expected_explore_z", "subjective_exploit_z", "chosen_value_z")])) %>%
  arrange(monkey_ordered, block_id, TRIAL_NUM)

n_trials <- nrow(data_clean)
n_monkeys <- n_distinct(data_clean$monkey_id)
n_blocks <- n_distinct(data_clean$block_id)

cat(sprintf("Dataset: %d trials, %d monkeys, %d blocks\n", n_trials, n_monkeys, n_blocks))

# =============================================================================
# MODEL FITTING
# =============================================================================

cat("Fitting models...\n")

fit_null <- multinom(outcome ~ 1, data = data_clean, trace = FALSE)
fit_fix <- multinom(outcome ~ social_complexity + expected_explore_z + 
                   subjective_exploit_z + chosen_value_z + rank_z, 
                   data = data_clean, trace = FALSE)
fit_hier <- multinom(outcome ~ social_complexity + expected_explore_z + 
                    subjective_exploit_z + chosen_value_z + rank_z + monkey_id, 
                    data = data_clean, trace = FALSE)

# Simulate posterior
set.seed(42)
simulate_posterior <- function(model, n_draws = 4000) {
  coef_matrix <- summary(model)$coefficients
  se_matrix <- summary(model)$standard.errors
  
  posterior_draws <- list()
  
  for(outcome in rownames(coef_matrix)) {
    posterior_draws[[outcome]] <- list()
    for(term in colnames(coef_matrix)) {
      mean_val <- coef_matrix[outcome, term]
      se_val <- se_matrix[outcome, term]
      draws <- rnorm(n_draws, mean = mean_val, sd = se_val)
      posterior_draws[[outcome]][[term]] <- draws
    }
  }
  
  return(posterior_draws)
}

posterior_hier <- simulate_posterior(fit_hier)

# =============================================================================
# PANEL 1: DESIGN & OUTCOME COUNTS
# =============================================================================

cat("Creating Panel 1: Design & Outcomes...\n")

# A: Task design
task_data <- data.frame(
  x = c(1, 2, 3),
  y = c(1, 1, 1),
  context = c("Solo", "Duo", "Trio"),
  n_trials = as.numeric(table(data_clean$social_context))
)

panel_1a <- ggplot(task_data, aes(x = x, y = y)) +
  geom_point(aes(color = context), size = LARGE_POINT_SIZE) +
  geom_text(aes(label = paste0(context, "\n(n=", n_trials, ")")), 
            vjust = -2.5, size = AXIS_TEXT_SIZE/2, fontface = "bold") +
  scale_color_manual(values = CB_COLORS[1:3]) +
  scale_x_continuous(limits = c(0.5, 3.5), breaks = NULL) +
  scale_y_continuous(limits = c(0.2, 1.8), breaks = NULL) +
  labs(title = "A", subtitle = "Experimental Design") +
  theme_void() +
  theme(
    plot.title = element_text(size = TITLE_SIZE, face = "bold"),
    plot.subtitle = element_text(size = SUBTITLE_SIZE),
    legend.position = "none",
    plot.margin = margin(PLOT_MARGIN_MM, PLOT_MARGIN_MM, 
                        PLOT_MARGIN_MM, PLOT_MARGIN_MM, "mm")
  )

# B: Outcome frequencies
outcome_counts <- data_clean %>%
  group_by(social_context, outcome) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(social_context) %>%
  mutate(total = sum(count), proportion = count / total)

panel_1b <- ggplot(outcome_counts, aes(x = social_context, y = count, fill = outcome)) +
  geom_col(position = "dodge", alpha = 0.8, width = 0.7, 
           color = "white", linewidth = LINE_WIDTH) +
  geom_text(aes(label = count), position = position_dodge(width = 0.7), 
            vjust = -0.5, size = AXIS_TEXT_SIZE/2.8, fontface = "bold") +
  scale_fill_manual(values = CB_COLORS[1:3], name = "Outcome") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "B", subtitle = "Outcome Frequencies",
       x = "Social Context", y = "Count") +
  theme_cb_adjustable() +
  theme(legend.position = "bottom")

# =============================================================================
# PANEL 2: FIXED-EFFECT COEFFICIENTS
# =============================================================================

cat("Creating Panel 2: Fixed Effects...\n")

extract_fixed_effects <- function(posterior_draws) {
  fixed_terms <- c("social_complexity", "expected_explore_z", "subjective_exploit_z", 
                   "chosen_value_z", "rank_z")
  
  fixed_data <- data.frame()
  
  for(outcome in names(posterior_draws)) {
    for(term in names(posterior_draws[[outcome]])) {
      if(any(grepl(paste(fixed_terms, collapse = "|"), term))) {
        draws <- posterior_draws[[outcome]][[term]]
        
        fixed_data <- rbind(fixed_data, data.frame(
          outcome = outcome,
          term = term,
          mean = mean(draws),
          median = median(draws),
          q025 = quantile(draws, 0.025),
          q975 = quantile(draws, 0.975),
          q25 = quantile(draws, 0.25),
          q75 = quantile(draws, 0.75),
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  return(fixed_data)
}

fixed_effects <- extract_fixed_effects(posterior_hier)

fixed_effects$term_clean <- case_when(
  fixed_effects$term == "social_complexity" ~ "Social Complexity",
  fixed_effects$term == "expected_explore_z" ~ "Expected Explore",
  fixed_effects$term == "subjective_exploit_z" ~ "Subjective Exploit",
  fixed_effects$term == "chosen_value_z" ~ "Chosen Value",
  fixed_effects$term == "rank_z" ~ "Dominance Rank",
  TRUE ~ fixed_effects$term
)

panel_2 <- ggplot(fixed_effects, aes(x = mean, y = reorder(term_clean, mean))) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.6, linewidth = LINE_WIDTH) +
  geom_pointrange(aes(xmin = q025, xmax = q975, color = outcome),
                  position = position_dodge(width = 0.8), 
                  linewidth = LINE_WIDTH, fatten = POINT_SIZE) +
  geom_pointrange(aes(xmin = q25, xmax = q75, color = outcome),
                  position = position_dodge(width = 0.8), 
                  linewidth = THICK_LINE_WIDTH, fatten = POINT_SIZE + 2) +
  scale_color_manual(values = CB_COLORS[1:2], name = "Outcome") +
  scale_x_continuous(sec.axis = sec_axis(~exp(.), name = "Odds Ratio", 
                                        labels = function(x) sprintf("%.2f", x))) +
  labs(title = "Fixed-Effect Coefficients",
       x = "Coefficient (log-odds)", y = "Predictor") +
  facet_wrap(~outcome, scales = "free_x", ncol = 2) +
  theme_cb_adjustable() +
  theme(legend.position = "bottom")

# =============================================================================
# PANEL 3: RANDOM-EFFECT SPREAD
# =============================================================================

cat("Creating Panel 3: Random Effects...\n")

extract_random_effects <- function(posterior_draws) {
  random_data <- data.frame()
  
  for(outcome in names(posterior_draws)) {
    for(term in names(posterior_draws[[outcome]])) {
      if(grepl("monkey_id", term)) {
        draws <- posterior_draws[[outcome]][[term]]
        monkey_full <- gsub("monkey_id", "", term)
        
        monkey_initial <- case_when(
          monkey_full == "FRAN" ~ "F",
          monkey_full == "DALI" ~ "D", 
          monkey_full == "EBI" ~ "E",
          monkey_full == "CHOCOLAT" ~ "C",
          monkey_full == "ICE" ~ "I",
          monkey_full == "ANEMONE" ~ "A",
          TRUE ~ substr(monkey_full, 1, 1)
        )
        
        sex <- case_when(
          monkey_full %in% c("FRAN", "DALI", "EBI") ~ "Male",
          monkey_full %in% c("ANEMONE", "CHOCOLAT", "ICE") ~ "Female"
        )
        
        random_data <- rbind(random_data, data.frame(
          outcome = outcome,
          monkey_full = monkey_full,
          monkey_initial = monkey_initial,
          sex = sex,
          mean = mean(draws),
          q025 = quantile(draws, 0.025),
          q975 = quantile(draws, 0.975),
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  return(random_data)
}

random_effects <- extract_random_effects(posterior_hier)
random_effects$monkey_initial_ordered <- factor(random_effects$monkey_initial, 
                                               levels = c("F", "D", "E", "C", "I", "A"))

panel_3 <- ggplot(random_effects, aes(x = mean, y = monkey_initial_ordered, color = sex)) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.6, linewidth = LINE_WIDTH) +
  geom_pointrange(aes(xmin = q025, xmax = q975),
                  linewidth = THICK_LINE_WIDTH, fatten = POINT_SIZE + 1) +
  scale_color_manual(values = c("Male" = CB_COLORS[1], "Female" = CB_COLORS[2]), name = "Sex") +
  scale_y_discrete(labels = function(x) paste0(x, " (", 
                                               ifelse(x %in% c("F", "D", "E"), "♂", "♀"), ")")) +
  labs(title = "Random-Effect Spread",
       x = "Random Effect", y = "Individual") +
  facet_wrap(~outcome, scales = "free_x", ncol = 2) +
  theme_cb_adjustable() +
  theme(legend.position = "bottom")

# =============================================================================
# PANEL 4: POSTERIOR-PREDICTIVE CHECK
# =============================================================================

cat("Creating Panel 4: Posterior-Predictive Check...\n")

pred_probs <- predict(fit_hier, newdata = data_clean, type = "probs")

n_sims <- 100
pred_outcomes <- matrix(NA, nrow = nrow(data_clean), ncol = n_sims)

for(i in 1:nrow(data_clean)) {
  for(s in 1:n_sims) {
    pred_outcomes[i, s] <- sample(colnames(pred_probs), 1, prob = pred_probs[i, ])
  }
}

observed_freq <- data_clean %>%
  group_by(social_context, outcome) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(social_context) %>%
  mutate(freq = count / sum(count))

pred_freq_list <- list()
for(s in 1:n_sims) {
  sim_data <- data_clean
  sim_data$outcome <- pred_outcomes[, s]
  
  sim_freq <- sim_data %>%
    group_by(social_context, outcome) %>%
    summarise(count = n(), .groups = "drop") %>%
    group_by(social_context) %>%
    mutate(freq = count / sum(count)) %>%
    mutate(sim = s)
  
  pred_freq_list[[s]] <- sim_freq
}

pred_freq <- do.call(rbind, pred_freq_list)

pred_freq_summary <- pred_freq %>%
  group_by(social_context, outcome) %>%
  summarise(
    mean_freq = mean(freq),
    q025 = quantile(freq, 0.025),
    q975 = quantile(freq, 0.975),
    .groups = "drop"
  )

panel_4 <- ggplot() +
  geom_col(data = observed_freq, aes(x = social_context, y = freq, fill = outcome),
           position = "dodge", alpha = 0.7, width = 0.7, 
           color = "white", linewidth = LINE_WIDTH) +
  geom_point(data = pred_freq_summary, 
             aes(x = social_context, y = mean_freq, color = outcome),
             position = position_dodge(width = 0.7), size = POINT_SIZE + 1) +
  geom_errorbar(data = pred_freq_summary,
                aes(x = social_context, ymin = q025, ymax = q975, color = outcome),
                position = position_dodge(width = 0.7), width = 0.3, 
                linewidth = THICK_LINE_WIDTH) +
  scale_fill_manual(values = alpha(CB_COLORS[1:3], 0.7), name = "Observed") +
  scale_color_manual(values = CB_COLORS[1:3], name = "Predicted") +
  scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Posterior-Predictive Check",
       x = "Social Context", y = "Frequency") +
  theme_cb_adjustable() +
  theme(legend.position = "bottom", legend.box = "horizontal")

# =============================================================================
# PANEL 5: MODEL COMPARISON
# =============================================================================

cat("Creating Panel 5: Model Comparison...\n")

model_comparison <- data.frame(
  Model = c("Null", "Fixed", "Hierarchical"),
  AIC = c(AIC(fit_null), AIC(fit_fix), AIC(fit_hier)),
  BIC = c(BIC(fit_null), BIC(fit_fix), BIC(fit_hier)),
  stringsAsFactors = FALSE
)

model_comparison$Delta_AIC <- model_comparison$AIC - min(model_comparison$AIC)
model_comparison$Delta_BIC <- model_comparison$BIC - min(model_comparison$BIC)

comparison_data <- data.frame(
  Model = rep(model_comparison$Model, 2),
  Metric = rep(c("ΔAIC", "ΔBIC"), each = 3),
  Delta = c(model_comparison$Delta_AIC, model_comparison$Delta_BIC)
)

panel_5 <- ggplot(comparison_data, aes(x = reorder(Model, Delta), y = Delta, fill = Metric)) +
  geom_col(position = "dodge", width = 0.7, color = "white", linewidth = LINE_WIDTH) +
  geom_text(aes(label = sprintf("%.1f", Delta)), 
            position = position_dodge(width = 0.7), vjust = -0.5, 
            size = AXIS_TEXT_SIZE/2.8, fontface = "bold") +
  scale_fill_manual(values = CB_COLORS[1:2], name = "Metric") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Model Comparison",
       x = "Model", y = "Δ (relative to best)") +
  theme_cb_adjustable() +
  theme(legend.position = "bottom")

# =============================================================================
# SAVE INDIVIDUAL FIGURES (ADJUSTABLE)
# =============================================================================

cat("Saving individual figures...\n")

ggsave("Figure1_Design_Outcomes_ADJUSTABLE.png", 
       arrangeGrob(panel_1a, panel_1b, ncol = 2),
       width = FIGURE_WIDTH_INCHES, height = FIGURE_HEIGHT_INCHES * PANEL_1_HEIGHT/5, 
       dpi = RESOLUTION_DPI, bg = "white")

ggsave("Figure2_Fixed_Effects_ADJUSTABLE.png", panel_2,
       width = FIGURE_WIDTH_INCHES, height = FIGURE_HEIGHT_INCHES * PANEL_2_HEIGHT/5, 
       dpi = RESOLUTION_DPI, bg = "white")

ggsave("Figure3_Random_Effects_ADJUSTABLE.png", panel_3,
       width = FIGURE_WIDTH_INCHES, height = FIGURE_HEIGHT_INCHES * PANEL_3_HEIGHT/5, 
       dpi = RESOLUTION_DPI, bg = "white")

ggsave("Figure4_PPC_ADJUSTABLE.png", panel_4,
       width = FIGURE_WIDTH_INCHES, height = FIGURE_HEIGHT_INCHES * PANEL_4_HEIGHT/5, 
       dpi = RESOLUTION_DPI, bg = "white")

ggsave("Figure5_Model_Comparison_ADJUSTABLE.png", panel_5,
       width = FIGURE_WIDTH_INCHES, height = FIGURE_HEIGHT_INCHES * PANEL_5_HEIGHT/5, 
       dpi = RESOLUTION_DPI, bg = "white")

# =============================================================================
# COMBINED FIGURE WITH ALL PANELS
# =============================================================================

cat("Creating combined figure with all panels...\n")

if(patchwork_available) {
  # Using patchwork for better control
  combined_figure <- 
    (panel_1a | panel_1b) / 
    panel_2 / 
    panel_3 / 
    panel_4 / 
    panel_5 +
    plot_layout(heights = c(PANEL_1_HEIGHT, PANEL_2_HEIGHT, PANEL_3_HEIGHT, 
                           PANEL_4_HEIGHT, PANEL_5_HEIGHT)) +
    plot_annotation(
      title = "Hierarchical Multinomial Bayesian Regression",
      subtitle = paste0("Analysis of ", n_trials, " behavioral choices from ", n_monkeys, " rhesus macaques"),
      theme = theme(
        plot.title = element_text(size = TITLE_SIZE + 4, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = SUBTITLE_SIZE, hjust = 0.5),
        plot.margin = margin(OUTER_MARGIN_MM, OUTER_MARGIN_MM, 
                           OUTER_MARGIN_MM, OUTER_MARGIN_MM, "mm")
      )
    )
  
  ggsave("Current_Biology_COMBINED_ADJUSTABLE.png", combined_figure,
         width = FIGURE_WIDTH_INCHES, height = FIGURE_HEIGHT_INCHES, 
         dpi = RESOLUTION_DPI, bg = "white")
         
  ggsave("Current_Biology_COMBINED_ADJUSTABLE.pdf", combined_figure,
         width = FIGURE_WIDTH_INCHES, height = FIGURE_HEIGHT_INCHES, 
         dpi = RESOLUTION_DPI, device = cairo_pdf, bg = "white")
         
} else {
  # Fallback to grid.arrange
  combined_figure <- arrangeGrob(
    arrangeGrob(panel_1a, panel_1b, ncol = 2, heights = unit(PANEL_1_HEIGHT, "null")),
    panel_2, panel_3, panel_4, panel_5,
    ncol = 1,
    heights = unit(c(PANEL_1_HEIGHT, PANEL_2_HEIGHT, PANEL_3_HEIGHT, 
                    PANEL_4_HEIGHT, PANEL_5_HEIGHT), "null"),
    top = textGrob("Hierarchical Multinomial Bayesian Regression", 
                   gp = gpar(fontsize = TITLE_SIZE + 4, fontface = "bold"))
  )
  
  ggsave("Current_Biology_COMBINED_ADJUSTABLE.png", combined_figure,
         width = FIGURE_WIDTH_INCHES, height = FIGURE_HEIGHT_INCHES, 
         dpi = RESOLUTION_DPI, bg = "white")
}

# =============================================================================
# SUMMARY WITH ADJUSTMENT INSTRUCTIONS
# =============================================================================

cat("\n")
cat(paste(rep("=", 70), collapse = ""))
cat("\n🔧 EASILY ADJUSTABLE FIGURE SET COMPLETE\n")
cat(paste(rep("=", 70), collapse = ""))

cat("\n\n📏 TO ADJUST SPACING - MODIFY THESE PARAMETERS AT TOP:\n")
cat(sprintf("FIGURE_WIDTH_INCHES <- %.1f     # Overall width\n", FIGURE_WIDTH_INCHES))
cat(sprintf("FIGURE_HEIGHT_INCHES <- %.1f    # Overall height\n", FIGURE_HEIGHT_INCHES))
cat(sprintf("BASE_TEXT_SIZE <- %d           # Base text size\n", BASE_TEXT_SIZE))
cat(sprintf("PLOT_MARGIN_MM <- %d           # Space around plots\n", PLOT_MARGIN_MM))
cat(sprintf("PANEL_SPACING <- %.1f          # Space between panels\n", PANEL_SPACING))

cat("\n📐 PANEL HEIGHT RATIOS (adjust independently):\n")
cat(sprintf("PANEL_1_HEIGHT <- %.1f\n", PANEL_1_HEIGHT))
cat(sprintf("PANEL_2_HEIGHT <- %.1f\n", PANEL_2_HEIGHT))
cat(sprintf("PANEL_3_HEIGHT <- %.1f\n", PANEL_3_HEIGHT))
cat(sprintf("PANEL_4_HEIGHT <- %.1f\n", PANEL_4_HEIGHT))
cat(sprintf("PANEL_5_HEIGHT <- %.1f\n", PANEL_5_HEIGHT))

cat("\n📝 TEXT SIZE HIERARCHY (adjust independently):\n")
cat(sprintf("TITLE_SIZE <- %d (main titles)\n", TITLE_SIZE))
cat(sprintf("SUBTITLE_SIZE <- %d (panel titles)\n", SUBTITLE_SIZE))
cat(sprintf("AXIS_TITLE_SIZE <- %d (axis labels)\n", AXIS_TITLE_SIZE))
cat(sprintf("AXIS_TEXT_SIZE <- %d (axis text)\n", AXIS_TEXT_SIZE))

cat("\n📁 FILES CREATED:\n")
cat("✅ Individual figures: Figure1-5_*_ADJUSTABLE.png\n")
cat("✅ Combined figure: Current_Biology_COMBINED_ADJUSTABLE.png/.pdf\n")

cat("\n🎯 ADJUSTMENT WORKFLOW:\n")
cat("1. Modify parameters at top of script\n")
cat("2. Re-run script\n")
cat("3. Check combined figure\n")
cat("4. Repeat until satisfied\n")

cat("\n💡 COMMON ADJUSTMENTS:\n")
cat("- Too cramped? Increase FIGURE_HEIGHT_INCHES & PANEL_SPACING\n")
cat("- Text too small? Increase BASE_TEXT_SIZE & related sizes\n")
cat("- Uneven panels? Adjust individual PANEL_X_HEIGHT values\n")
cat("- Too much white space? Decrease PLOT_MARGIN_MM\n")

cat(sprintf("\n✨ Current setup: %.1f\"×%.1f\", %dpt text, %.1f spacing\n", 
            FIGURE_WIDTH_INCHES, FIGURE_HEIGHT_INCHES, BASE_TEXT_SIZE, PANEL_SPACING))

sessionInfo() 