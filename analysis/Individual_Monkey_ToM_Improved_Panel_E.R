# =============================================================================
# IMPROVED PANEL E: INDIVIDUAL COGNITIVE HEATMAP
# Replace the useless text panel with a comprehensive heatmap
# =============================================================================

# Load required libraries
library(nnet)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(viridis)
library(grid)
library(reshape2)

# Set parameters
set.seed(42)
options(scipen = 999)

# Professional settings
fig_width <- 250  # mm (larger for 5 panels)
fig_height <- 180 # mm 
fig_dpi <- 300
base_font_size <- 12
title_font_size <- 14
subtitle_font_size <- 10

# Color palette for sex groups
sex_colors <- c("Male" = "#D55E00", "Female" = "#009E73")

# Individual monkey colors within sex groups
monkey_colors <- c(
  "F" = "#D55E00",      # FRAN (Male)
  "D" = "#E69F00",      # DALI (Male) 
  "E" = "#CC79A7",      # EBI (Male)
  "A" = "#009E73",      # ANEMONE (Female)
  "C" = "#0072B2",      # CHOCOLAT (Female)
  "I" = "#56B4E9"       # ICE (Female)
)

cat("=============================================================================\n")
cat("IMPROVED INDIVIDUAL MONKEY ANALYSIS WITH BETTER PANEL E\n")
cat("=============================================================================\n\n")

# =============================================================================
# 1. RELOAD AND RECOMPUTE ALL DATA
# =============================================================================

cat("1. RELOADING DATA AND COMPUTING ALL METRICS...\n")

# Load the dataset
data_raw <- read.csv("data/Explore Exploit Dataset.csv", stringsAsFactors = FALSE)
data_clean <- data_raw[data_raw$TRIAL_TYPE == "OIT_RE", ]

# Clean outcomes
outcome_clean <- character(nrow(data_clean))
outcome_clean[grepl("explore", tolower(data_clean$OUTCOME))] <- "explore"
outcome_clean[grepl("exploit", tolower(data_clean$OUTCOME))] <- "exploit"
outcome_clean[grepl("none|stop|NONE", tolower(data_clean$OUTCOME))] <- "none"

data_clean$outcome_clean <- outcome_clean
data_clean <- data_clean[data_clean$outcome_clean != "" & !is.na(data_clean$outcome_clean), ]

# Add social and individual information with initials
data_clean$is_social <- ifelse(data_clean$CONDITION == "solo", 0, 1)
data_clean$sex <- case_when(
  data_clean$monkey %in% c("FRAN", "DALI", "EBI") ~ "Male",
  data_clean$monkey %in% c("ANEMONE", "CHOCOLAT", "ICE") ~ "Female",
  TRUE ~ "Unknown"
)

# Add initials
data_clean$initial <- case_when(
  data_clean$monkey == "FRAN" ~ "F",
  data_clean$monkey == "DALI" ~ "D", 
  data_clean$monkey == "EBI" ~ "E",
  data_clean$monkey == "ANEMONE" ~ "A",
  data_clean$monkey == "CHOCOLAT" ~ "C",
  data_clean$monkey == "ICE" ~ "I"
)

# Create ordered factor for proper sex grouping
data_clean$sex_order <- factor(data_clean$sex, levels = c("Male", "Female"))
data_clean$initial_ordered <- factor(data_clean$initial, 
                                   levels = c("F", "D", "E", "A", "C", "I"))

cat("Data prepared successfully.\n")

# =============================================================================
# 2. COMPUTE ALL INDIVIDUAL METRICS FOR HEATMAP
# =============================================================================

cat("2. COMPUTING COMPREHENSIVE INDIVIDUAL METRICS...\n")

# Initialize comprehensive results dataframe
comprehensive_metrics <- data.frame()

for(monkey in unique(data_clean$monkey)) {
  monkey_data <- data_clean %>% filter(monkey == !!monkey)
  
  # Basic exploration metrics
  solo_exploration <- monkey_data %>% 
    filter(CONDITION == "solo" & !is.na(outcome_clean)) %>%
    summarise(rate = mean(outcome_clean == "explore", na.rm = TRUE)) %>%
    pull(rate)
  
  social_exploration <- monkey_data %>% 
    filter(CONDITION %in% c("duo", "trio") & !is.na(outcome_clean)) %>%
    summarise(rate = mean(outcome_clean == "explore", na.rm = TRUE)) %>%
    pull(rate)
  
  # Social influence (simple difference)
  social_influence <- solo_exploration - social_exploration
  
  # Activity rate (explore + exploit vs none)
  activity_rate <- monkey_data %>%
    filter(!is.na(outcome_clean)) %>%
    summarise(rate = mean(outcome_clean %in% c("explore", "exploit"), na.rm = TRUE)) %>%
    pull(rate)
  
  # Behavioral consistency (inverse of switching rate)
  behavioral_consistency <- monkey_data %>%
    filter(!is.na(outcome_clean)) %>%
    arrange(BLOCK_No, TRIAL_NUM) %>%
    mutate(switch = ifelse(outcome_clean != lag(outcome_clean, default = outcome_clean[1]), 1, 0)) %>%
    summarise(consistency = 1 - mean(switch, na.rm = TRUE)) %>%
    pull(consistency)
  
  # Context sensitivity (variance in behavior across conditions)
  context_sensitivity <- monkey_data %>%
    filter(!is.na(outcome_clean)) %>%
    group_by(CONDITION) %>%
    summarise(explore_rate = mean(outcome_clean == "explore", na.rm = TRUE), .groups = "drop") %>%
    summarise(sensitivity = sd(explore_rate, na.rm = TRUE)) %>%
    pull(sensitivity)
  
  # Rank responsiveness (correlation with relative rank)
  rank_correlation <- tryCatch({
    rank_data <- monkey_data %>% 
      filter(!is.na(outcome_clean) & !is.na(RELATIVE_RANK) & outcome_clean != "none")
    if(nrow(rank_data) > 10) {
      cor(rank_data$RELATIVE_RANK, 
          as.numeric(rank_data$outcome_clean == "explore"), 
          use = "complete.obs")
    } else { 0 }
  }, error = function(e) { 0 })
  
  # Get sex and initial
  sex <- unique(monkey_data$sex)[1]
  initial <- unique(monkey_data$initial)[1]
  
  comprehensive_metrics <- rbind(comprehensive_metrics, data.frame(
    monkey = monkey,
    initial = initial,
    sex = sex,
    solo_exploration = ifelse(length(solo_exploration) == 0, 0, solo_exploration),
    social_exploration = ifelse(length(social_exploration) == 0, 0, social_exploration),
    social_influence = social_influence,
    activity_rate = activity_rate,
    behavioral_consistency = behavioral_consistency,
    context_sensitivity = ifelse(is.na(context_sensitivity), 0, context_sensitivity),
    rank_responsiveness = ifelse(is.na(rank_correlation), 0, rank_correlation)
  ))
}

# Add ToM results (simplified computation)
tom_results <- data.frame()

for(monkey in unique(data_clean$monkey)) {
  monkey_data <- data_clean %>%
    filter(monkey == !!monkey) %>%
    arrange(BLOCK_No, TRIAL_NUM)
  
  if(nrow(monkey_data) >= 20) {
    # Simplified ToM calculation - test only best learning rate for speed
    best_correlation <- -Inf
    best_tom_level <- 0
    
    # Quick ToM assessment
    for(tom_level in 0:2) {
      # Use fixed learning rate for speed
      lr <- 0.1
      
      # Simplified ToM simulation
      n_trials <- nrow(monkey_data)
      predictions <- numeric(n_trials)
      current_belief <- 0.5
      
      for(i in 1:n_trials) {
        if(monkey_data$is_social[i] == 1 && i > 1) {
          last_choice <- ifelse(monkey_data$outcome_clean[i-1] == "explore", 1, 0)
          
          if(tom_level == 0) {
            current_belief <- 0.95 * current_belief + lr * last_choice
            predictions[i] <- current_belief
          } else if(tom_level == 1) {
            other_belief <- 0.95 * current_belief + lr * last_choice
            predictions[i] <- other_belief + ifelse(other_belief > 0.5, -0.2, 0.2)
          } else {
            other_belief <- 0.95 * current_belief + lr * last_choice
            predictions[i] <- other_belief + 0.1 * (other_belief - 0.5)
          }
          predictions[i] <- max(0, min(1, predictions[i]))
        } else {
          predictions[i] <- current_belief
        }
      }
      
      # Calculate correlation for social trials
      actual_choices <- ifelse(monkey_data$outcome_clean == "explore", 1, 
                             ifelse(monkey_data$outcome_clean == "exploit", 0, 0.5))
      social_indices <- which(monkey_data$is_social == 1)
      
      if(length(social_indices) > 5) {
        correlation <- cor(predictions[social_indices], 
                          actual_choices[social_indices], 
                          use = "complete.obs")
        
        if(!is.na(correlation) && correlation > best_correlation) {
          best_correlation <- correlation
          best_tom_level <- tom_level
        }
      }
    }
    
    tom_results <- rbind(tom_results, data.frame(
      monkey = monkey,
      tom_level = best_tom_level,
      tom_correlation = best_correlation
    ))
  }
}

# Merge with comprehensive metrics
comprehensive_metrics <- comprehensive_metrics %>%
  left_join(tom_results, by = "monkey") %>%
  mutate(
    tom_level = ifelse(is.na(tom_level), 0, tom_level),
    tom_correlation = ifelse(is.na(tom_correlation), 0, tom_correlation)
  )

cat("Comprehensive metrics computed for", nrow(comprehensive_metrics), "animals\n")

# =============================================================================
# 3. CREATE IMPROVED 5-PANEL FIGURE
# =============================================================================

cat("\n3. CREATING IMPROVED 5-PANEL FIGURE WITH BETTER PANEL E...\n")

# Panels A-D remain the same as before (simplified versions)

# Panel A: Social Influence
p1 <- ggplot(comprehensive_metrics, aes(x = reorder(initial, match(sex, c("Male", "Female"))), 
                                      y = social_influence, fill = sex)) +
  geom_col(alpha = 0.8, color = "black", linewidth = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_text(data = data.frame(x = c(2, 5), y = max(comprehensive_metrics$social_influence) * 1.1, 
                             label = c("Males", "Females")),
           aes(x = x, y = y, label = label), inherit.aes = FALSE, 
           fontface = "bold", size = 4) +
  geom_vline(xintercept = 3.5, linetype = "dashed", color = "gray70", alpha = 0.8) +
  scale_fill_manual(values = sex_colors, name = "Sex") +
  scale_x_discrete(labels = c("F", "D", "E", "A", "C", "I")) +
  labs(title = "A. Social Influence on Exploration",
       subtitle = "Solo exploration rate - Social exploration rate",
       x = "Individual (grouped by sex)", y = "Social Influence (Δ Exploration Rate)") +
  theme_classic(base_size = base_font_size) +
  theme(
    plot.title = element_text(face = "bold", size = title_font_size),
    plot.subtitle = element_text(size = subtitle_font_size),
    legend.position = "none"
  )

# Panel B: Theory of Mind Performance
p2 <- ggplot(comprehensive_metrics, aes(x = reorder(initial, match(sex, c("Male", "Female"))), 
                                      y = tom_correlation, fill = factor(tom_level))) +
  geom_col(alpha = 0.8, color = "black", linewidth = 0.3) +
  geom_text(aes(label = paste("ToM", tom_level)), vjust = -0.5, size = 2.5, fontface = "bold") +
  geom_text(data = data.frame(x = c(2, 5), y = max(comprehensive_metrics$tom_correlation) * 1.2, 
                             label = c("Males", "Females")),
           aes(x = x, y = y, label = label), inherit.aes = FALSE, 
           fontface = "bold", size = 4) +
  geom_vline(xintercept = 3.5, linetype = "dashed", color = "gray70", alpha = 0.8) +
  scale_fill_viridis_d(name = "ToM Level") +
  scale_x_discrete(labels = c("F", "D", "E", "A", "C", "I")) +
  labs(title = "B. Theory of Mind Sophistication",
       subtitle = "Model-behavior correlation by ToM level",
       x = "Individual (grouped by sex)", y = "ToM Model Correlation") +
  theme_classic(base_size = base_font_size) +
  theme(
    plot.title = element_text(face = "bold", size = title_font_size),
    plot.subtitle = element_text(size = subtitle_font_size),
    legend.position = "none"
  )

# Panel C: Activity vs Consistency
p3 <- ggplot(comprehensive_metrics, aes(x = activity_rate, y = behavioral_consistency, color = sex)) +
  geom_point(size = 4, alpha = 0.8) +
  geom_text(aes(label = initial), vjust = -1, fontface = "bold", size = 3) +
  scale_color_manual(values = sex_colors, name = "Sex") +
  labs(title = "C. Activity vs Behavioral Consistency",
       subtitle = "Trade-off between engagement and stability",
       x = "Activity Rate (Explore + Exploit)", y = "Behavioral Consistency") +
  theme_classic(base_size = base_font_size) +
  theme(
    plot.title = element_text(face = "bold", size = title_font_size),
    plot.subtitle = element_text(size = subtitle_font_size),
    legend.position = "none"
  )

# Panel D: Context Sensitivity vs Rank Responsiveness
p4 <- ggplot(comprehensive_metrics, aes(x = context_sensitivity, y = rank_responsiveness, color = sex)) +
  geom_point(size = 4, alpha = 0.8) +
  geom_text(aes(label = initial), vjust = -1, fontface = "bold", size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", alpha = 0.7) +
  scale_color_manual(values = sex_colors, name = "Sex") +
  labs(title = "D. Context Sensitivity vs Rank Responsiveness",
       subtitle = "Social adaptability dimensions",
       x = "Context Sensitivity (Behavior Variance)", y = "Rank Responsiveness (Correlation)") +
  theme_classic(base_size = base_font_size) +
  theme(
    plot.title = element_text(face = "bold", size = title_font_size),
    plot.subtitle = element_text(size = subtitle_font_size),
    legend.position = "none"
  )

# Panel E: COMPREHENSIVE COGNITIVE HEATMAP (NEW!)
# Prepare data for heatmap
heatmap_data <- comprehensive_metrics %>%
  select(initial, sex, social_influence, tom_correlation, activity_rate, 
         behavioral_consistency, context_sensitivity, rank_responsiveness) %>%
  # Normalize all metrics to 0-1 scale for comparison
  mutate(
    social_influence_norm = (social_influence - min(social_influence)) / (max(social_influence) - min(social_influence)),
    tom_correlation_norm = (tom_correlation - min(tom_correlation)) / (max(tom_correlation) - min(tom_correlation)),
    activity_rate_norm = activity_rate,  # Already 0-1
    consistency_norm = behavioral_consistency,  # Already 0-1
    context_sens_norm = (context_sensitivity - min(context_sensitivity)) / (max(context_sensitivity) - min(context_sensitivity)),
    rank_resp_norm = (rank_responsiveness - min(rank_responsiveness)) / (max(rank_responsiveness) - min(rank_responsiveness))
  ) %>%
  select(initial, sex, social_influence_norm, tom_correlation_norm, activity_rate_norm, 
         consistency_norm, context_sens_norm, rank_resp_norm) %>%
  # Reshape for heatmap
  melt(id.vars = c("initial", "sex"), variable.name = "metric", value.name = "value") %>%
  mutate(
    metric_label = case_when(
      metric == "social_influence_norm" ~ "Social\nInfluence",
      metric == "tom_correlation_norm" ~ "ToM\nPerformance", 
      metric == "activity_rate_norm" ~ "Activity\nRate",
      metric == "consistency_norm" ~ "Behavioral\nConsistency",
      metric == "context_sens_norm" ~ "Context\nSensitivity",
      metric == "rank_resp_norm" ~ "Rank\nResponsiveness"
    ),
    initial_ordered = factor(initial, levels = c("F", "D", "E", "A", "C", "I"))
  )

p5 <- ggplot(heatmap_data, aes(x = metric_label, y = initial_ordered, fill = value)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.2f", value)), size = 2.5, fontface = "bold", color = "white") +
  # Add sex group labels on the right
  geom_text(data = data.frame(x = 6.7, y = c(2, 5), label = c("Males", "Females")),
           aes(x = x, y = y, label = label), inherit.aes = FALSE, 
           fontface = "bold", size = 4, angle = 90) +
  # Add horizontal separator
  geom_hline(yintercept = 3.5, color = "gray30", linewidth = 1) +
  scale_fill_viridis_c(name = "Normalized\nScore", option = "plasma") +
  scale_y_discrete(labels = c("F", "D", "E", "A", "C", "I")) +
  labs(title = "E. Comprehensive Cognitive Profile Heatmap",
       subtitle = "Normalized scores across all behavioral dimensions",
       x = "Cognitive/Behavioral Metrics", y = "Individual") +
  theme_minimal(base_size = base_font_size) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = title_font_size),
    plot.subtitle = element_text(hjust = 0.5, size = subtitle_font_size),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    legend.key.size = unit(0.5, "cm")
  )

# Create shared legend for sex groups
legend_data <- data.frame(
  initial = c("F", "D", "E", "A", "C", "I"),
  full_name = c("FRAN", "DALI", "EBI", "ANEMONE", "CHOCOLAT", "ICE"),
  sex = c("Male", "Male", "Male", "Female", "Female", "Female"),
  y = 1
)

legend_plot <- ggplot(legend_data, aes(x = initial, y = y, fill = sex)) +
  geom_tile(color = "white", linewidth = 1, alpha = 0.7) +
  geom_text(aes(label = paste(initial, "\n", full_name)), size = 2.5, fontface = "bold") +
  geom_text(data = data.frame(x = c(2, 5), y = 1.5, label = c("Males", "Females")),
           aes(x = x, y = y, label = label), inherit.aes = FALSE, 
           fontface = "bold", size = 3.5) +
  geom_vline(xintercept = 3.5, linetype = "dashed", color = "gray70", alpha = 0.8) +
  scale_fill_manual(values = sex_colors) +
  labs(title = "Individual Key") +
  theme_void(base_size = base_font_size - 2) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = base_font_size),
    legend.position = "none"
  )

# Combine all panels
combined_plot <- grid.arrange(
  p1, p2,
  p3, p4, 
  p5,
  nrow = 3, ncol = 2,
  layout_matrix = rbind(c(1, 2), c(3, 4), c(5, 5)),
  top = textGrob("Individual Monkey Cognitive Analysis (Sex-Grouped)", 
                gp = gpar(fontsize = title_font_size + 4, fontface = "bold")),
  heights = c(2, 2, 2.5)
)

# Save the improved figure
ggsave("results/figures/Individual_Monkey_ToM_Improved_5Panel.png", combined_plot,
       width = fig_width*1.5, height = fig_height*1.5, units = "mm", dpi = fig_dpi)

ggsave("results/figures/Individual_Monkey_ToM_Improved_5Panel.tiff", combined_plot,
       width = fig_width*1.5, height = fig_height*1.5, units = "mm", dpi = fig_dpi)

# =============================================================================
# 4. SUMMARY OF IMPROVEMENTS
# =============================================================================

cat("\n=============================================================================\n")
cat("IMPROVED 5-PANEL FIGURE WITH INFORMATIVE PANEL E\n")
cat("=============================================================================\n\n")

cat("PANEL IMPROVEMENTS:\n")
cat("Panel A: Social Influence (difference score)\n")
cat("Panel B: Theory of Mind with correlation strength\n") 
cat("Panel C: Activity vs Consistency scatter plot\n")
cat("Panel D: Context Sensitivity vs Rank Responsiveness\n")
cat("Panel E: COMPREHENSIVE COGNITIVE HEATMAP (NEW!)\n\n")

cat("NEW PANEL E SHOWS:\n")
cat("- Social Influence: How much social presence affects behavior\n")
cat("- ToM Performance: Theory of mind model correlation\n")
cat("- Activity Rate: Proportion of trials with active choices\n")
cat("- Behavioral Consistency: Stability of choice patterns\n")
cat("- Context Sensitivity: Variance in behavior across conditions\n")
cat("- Rank Responsiveness: Correlation with dominance hierarchy\n\n")

cat("HEATMAP INTERPRETATION:\n")
print(comprehensive_metrics[, c("initial", "sex", "social_influence", "tom_correlation", 
                               "activity_rate", "behavioral_consistency", 
                               "context_sensitivity", "rank_responsiveness")])

cat("\n=============================================================================\n")
cat("IMPROVED ANALYSIS COMPLETE - Much more informative Panel E!\n")
cat("=============================================================================\n") 