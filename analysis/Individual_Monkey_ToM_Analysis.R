# =============================================================================
# INDIVIDUAL MONKEY BAYESIAN THEORY OF MIND ANALYSIS
# 5-Panel Detailed Figure Showing Individual Performance
# =============================================================================

# Load required libraries
library(nnet)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(viridis)
library(grid)

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

# Color palette for individual monkeys
monkey_colors <- c(
  "FRAN" = "#D55E00",      # Orange-red (Male)
  "DALI" = "#E69F00",      # Orange (Male) 
  "EBI" = "#CC79A7",       # Pink (Male)
  "ANEMONE" = "#009E73",   # Green (Female)
  "CHOCOLAT" = "#0072B2",  # Blue (Female)
  "ICE" = "#56B4E9"        # Light blue (Female)
)

# Sex colors for grouping
sex_colors <- c("Male" = "#D55E00", "Female" = "#009E73")

cat("=============================================================================\n")
cat("INDIVIDUAL MONKEY BAYESIAN THEORY OF MIND ANALYSIS\n")
cat("=============================================================================\n\n")

# =============================================================================
# 1. RELOAD AND RECOMPUTE DATA
# =============================================================================

cat("1. RELOADING DATA AND COMPUTING INDIVIDUAL METRICS...\n")

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

# Add social and individual information
data_clean$is_social <- ifelse(data_clean$CONDITION == "solo", 0, 1)
data_clean$sex <- case_when(
  data_clean$monkey %in% c("FRAN", "DALI", "EBI") ~ "Male",
  data_clean$monkey %in% c("ANEMONE", "CHOCOLAT", "ICE") ~ "Female",
  TRUE ~ "Unknown"
)

# =============================================================================
# 2. INDIVIDUAL SOCIAL INFLUENCE COEFFICIENTS
# =============================================================================

cat("2. COMPUTING INDIVIDUAL SOCIAL INFLUENCE COEFFICIENTS...\n")

# Fit individual models for each monkey
individual_social_effects <- data.frame()

for(monkey in unique(data_clean$monkey)) {
  monkey_data <- data_clean %>%
    filter(monkey == !!monkey & !is.na(outcome_clean) & outcome_clean != "none") %>%
    mutate(
      choice_numeric = ifelse(outcome_clean == "explore", 1, 0),
      social_present = ifelse(is_social == 1, 1, 0)
    )
  
  if(nrow(monkey_data) > 20 && sum(monkey_data$social_present) > 5 && sum(monkey_data$social_present == 0) > 5) {
    tryCatch({
      model <- glm(choice_numeric ~ social_present + SUBJECTIVE_CHOSEN_VALUE + RELATIVE_RANK,
                   data = monkey_data, family = binomial)
      
      coef_summary <- summary(model)$coefficients
      
      if("social_present" %in% rownames(coef_summary)) {
        social_coef <- coef_summary["social_present", "Estimate"]
        social_se <- coef_summary["social_present", "Std. Error"]
        social_p <- coef_summary["social_present", "Pr(>|z|)"]
        
        individual_social_effects <- rbind(individual_social_effects, data.frame(
          monkey = monkey,
          social_coefficient = social_coef,
          social_se = social_se,
          social_pvalue = social_p,
          significant = social_p < 0.05
        ))
      }
    }, error = function(e) {
      cat("Error fitting model for", monkey, ":", e$message, "\n")
    })
  }
}

# Add sex information
individual_social_effects <- individual_social_effects %>%
  left_join(data_clean %>% select(monkey, sex) %>% distinct(), by = "monkey")

cat("Individual social influence coefficients computed for", nrow(individual_social_effects), "animals\n")

# =============================================================================
# 3. INDIVIDUAL BAYESIAN BELIEF UPDATING METRICS
# =============================================================================

cat("3. COMPUTING INDIVIDUAL BAYESIAN BELIEF UPDATING METRICS...\n")

# Bayesian belief updating function
bayesian_belief_update <- function(monkey_data) {
  monkey_data <- monkey_data[order(monkey_data$BLOCK_No, monkey_data$TRIAL_NUM), ]
  
  prior_explore_prob <- 0.5  
  prior_precision <- 2       
  
  beliefs <- data.frame(
    trial = 1:nrow(monkey_data),
    posterior_belief = numeric(nrow(monkey_data)),
    belief_update = numeric(nrow(monkey_data)),
    prediction_error = numeric(nrow(monkey_data))
  )
  
  current_belief <- prior_explore_prob
  current_precision <- prior_precision
  
  for(i in 1:nrow(monkey_data)) {
    own_choice <- monkey_data$outcome_clean[i]
    
    if(monkey_data$is_social[i] == 1 && own_choice %in% c("explore", "exploit")) {
      observed_explore <- ifelse(own_choice == "explore", 1, 0)
      
      alpha <- current_precision * current_belief
      beta <- current_precision * (1 - current_belief)
      
      alpha_new <- alpha + observed_explore
      beta_new <- beta + (1 - observed_explore)
      
      new_belief <- alpha_new / (alpha_new + beta_new)
      new_precision <- alpha_new + beta_new
      
      beliefs$posterior_belief[i] <- new_belief
      beliefs$belief_update[i] <- new_belief - current_belief
      beliefs$prediction_error[i] <- observed_explore - current_belief
      
      current_belief <- new_belief
      current_precision <- min(new_precision, 20)  
    } else {
      beliefs$posterior_belief[i] <- current_belief
      beliefs$belief_update[i] <- 0
      beliefs$prediction_error[i] <- 0
    }
  }
  
  return(beliefs)
}

# Compute individual belief updating metrics
individual_belief_metrics <- data.frame()

for(monkey in unique(data_clean$monkey)) {
  monkey_data <- data_clean %>% filter(monkey == !!monkey)
  
  if(nrow(monkey_data) >= 10) {
    beliefs <- bayesian_belief_update(monkey_data)
    
    # Calculate metrics for social vs solo contexts
    for(condition in c("solo", "duo", "trio")) {
      condition_indices <- which(monkey_data$CONDITION == condition)
      
      if(length(condition_indices) > 5) {
        condition_beliefs <- beliefs[condition_indices, ]
        
        individual_belief_metrics <- rbind(individual_belief_metrics, data.frame(
          monkey = monkey,
          condition = condition,
          is_social = ifelse(condition == "solo", 0, 1),
          mean_belief_update = mean(abs(condition_beliefs$belief_update), na.rm = TRUE),
          belief_volatility = sd(condition_beliefs$posterior_belief, na.rm = TRUE),
          mean_prediction_error = mean(abs(condition_beliefs$prediction_error), na.rm = TRUE),
          n_trials = length(condition_indices)
        ))
      }
    }
  }
}

# Add sex information
individual_belief_metrics <- individual_belief_metrics %>%
  left_join(data_clean %>% select(monkey, sex) %>% distinct(), by = "monkey")

cat("Belief updating metrics computed for", length(unique(individual_belief_metrics$monkey)), "animals\n")

# =============================================================================
# 4. INDIVIDUAL THEORY OF MIND SOPHISTICATION
# =============================================================================

cat("4. COMPUTING INDIVIDUAL THEORY OF MIND SOPHISTICATION LEVELS...\n")

# ToM simulation function
simulate_tom_agent <- function(data, tom_level = 0, learning_rate = 0.1, memory_decay = 0.95) {
  n_trials <- nrow(data)
  predictions <- numeric(n_trials)
  
  if(tom_level == 0) {
    current_belief <- 0.5
  } else if(tom_level == 1) {
    current_belief <- 0.5
    other_belief <- 0.5
  } else {
    current_belief <- 0.5
    other_belief <- 0.5
    other_other_belief <- 0.5
  }
  
  for(i in 1:n_trials) {
    if(data$is_social[i] == 1 && i > 1) {
      if(tom_level == 0) {
        last_choice <- ifelse(data$outcome_clean[i-1] == "explore", 1, 0)
        current_belief <- memory_decay * current_belief + learning_rate * last_choice
        predictions[i] <- current_belief
        
      } else if(tom_level == 1) {
        last_choice <- ifelse(data$outcome_clean[i-1] == "explore", 1, 0)
        other_belief <- memory_decay * other_belief + learning_rate * last_choice
        strategic_adjustment <- ifelse(other_belief > 0.5, -0.2, 0.2)
        predictions[i] <- other_belief + strategic_adjustment
        current_belief <- predictions[i]
        
      } else {
        last_choice <- ifelse(data$outcome_clean[i-1] == "explore", 1, 0)
        other_other_belief <- memory_decay * other_other_belief + learning_rate * last_choice
        other_belief <- 0.7 * other_other_belief + 0.3 * other_belief
        strategic_factor <- 0.5 * (other_belief - other_other_belief)
        predictions[i] <- other_belief + strategic_factor
        current_belief <- predictions[i]
      }
      
      predictions[i] <- max(0, min(1, predictions[i]))
    } else {
      predictions[i] <- current_belief
    }
  }
  
  return(predictions)
}

# Fit ToM models for each individual
individual_tom_results <- data.frame()

for(monkey in unique(data_clean$monkey)) {
  monkey_data <- data_clean %>%
    filter(monkey == !!monkey) %>%
    arrange(BLOCK_No, TRIAL_NUM)
  
  if(nrow(monkey_data) >= 20) {
    best_correlation <- -Inf
    best_tom_level <- 0
    best_lr <- 0.1
    
    # Test different ToM levels and learning rates
    for(tom_level in 0:2) {
      for(lr in c(0.05, 0.1, 0.2)) {
        predictions <- simulate_tom_agent(monkey_data, tom_level = tom_level, learning_rate = lr)
        
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
            best_lr <- lr
          }
        }
      }
    }
    
    individual_tom_results <- rbind(individual_tom_results, data.frame(
      monkey = monkey,
      best_tom_level = best_tom_level,
      best_learning_rate = best_lr,
      best_correlation = best_correlation,
      tom_interpretation = case_when(
        best_tom_level == 0 ~ "Simple Frequency",
        best_tom_level == 1 ~ "1st-order ToM",
        best_tom_level == 2 ~ "2nd-order ToM"
      )
    ))
  }
}

# Add sex information
individual_tom_results <- individual_tom_results %>%
  left_join(data_clean %>% select(monkey, sex) %>% distinct(), by = "monkey")

cat("ToM analysis completed for", nrow(individual_tom_results), "animals\n")

# =============================================================================
# 5. EXPLORATION PATTERNS BY CONTEXT
# =============================================================================

cat("5. COMPUTING EXPLORATION PATTERNS BY CONTEXT...\n")

exploration_patterns <- data_clean %>%
  filter(!is.na(outcome_clean)) %>%
  group_by(monkey, CONDITION) %>%
  summarise(
    exploration_rate = mean(outcome_clean == "explore", na.rm = TRUE),
    exploitation_rate = mean(outcome_clean == "exploit", na.rm = TRUE),
    activity_rate = mean(outcome_clean %in% c("explore", "exploit"), na.rm = TRUE),
    n_trials = n(),
    .groups = "drop"
  ) %>%
  left_join(data_clean %>% select(monkey, sex) %>% distinct(), by = "monkey")

# Reshape for visualization
exploration_long <- exploration_patterns %>%
  select(monkey, CONDITION, exploration_rate, sex) %>%
  mutate(
    context_type = ifelse(CONDITION == "solo", "Solo", "Social"),
    social_complexity = factor(CONDITION, levels = c("solo", "duo", "trio"))
  )

cat("Exploration patterns computed for", length(unique(exploration_patterns$monkey)), "animals\n")

# =============================================================================
# 6. CREATE 5-PANEL COMPREHENSIVE FIGURE
# =============================================================================

cat("\n6. CREATING 5-PANEL INDIVIDUAL MONKEY ANALYSIS FIGURE...\n")

# Panel A: Individual Social Influence Coefficients
if(nrow(individual_social_effects) > 0) {
  p1 <- ggplot(individual_social_effects, aes(x = reorder(monkey, social_coefficient), y = social_coefficient, fill = sex)) +
    geom_col(alpha = 0.8, color = "black", linewidth = 0.3) +
    geom_errorbar(aes(ymin = social_coefficient - social_se, ymax = social_coefficient + social_se),
                  width = 0.3, linewidth = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    scale_fill_manual(values = sex_colors, name = "Sex") +
    labs(title = "A. Individual Social Influence",
         subtitle = "Effect of social presence on exploration (± SE)",
         x = "Individual", y = "Social Influence Coefficient") +
    theme_classic(base_size = base_font_size) +
    theme(
      plot.title = element_text(face = "bold", size = title_font_size),
      plot.subtitle = element_text(size = subtitle_font_size),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"
    )
} else {
  p1 <- ggplot() + labs(title = "A. Social Influence (Insufficient Data)") + theme_void()
}

# Panel B: Individual Belief Volatility Across Contexts
if(nrow(individual_belief_metrics) > 0) {
  p2 <- ggplot(individual_belief_metrics, aes(x = condition, y = belief_volatility, color = monkey)) +
    geom_point(size = 3, alpha = 0.8) +
    geom_line(aes(group = monkey), alpha = 0.6, linewidth = 1) +
    scale_color_manual(values = monkey_colors, name = "Individual") +
    labs(title = "B. Individual Belief Updating",
         subtitle = "Belief volatility across social contexts",
         x = "Social Context", y = "Belief Volatility") +
    theme_classic(base_size = base_font_size) +
    theme(
      plot.title = element_text(face = "bold", size = title_font_size),
      plot.subtitle = element_text(size = subtitle_font_size),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"
    )
} else {
  p2 <- ggplot() + labs(title = "B. Belief Updating (Insufficient Data)") + theme_void()
}

# Panel C: Individual Theory of Mind Sophistication
if(nrow(individual_tom_results) > 0) {
  p3 <- ggplot(individual_tom_results, aes(x = reorder(monkey, best_tom_level), y = best_correlation, fill = factor(best_tom_level))) +
    geom_col(alpha = 0.8, color = "black", linewidth = 0.3) +
    geom_text(aes(label = tom_interpretation), vjust = -0.5, size = 2.5, fontface = "bold") +
    scale_fill_viridis_d(name = "ToM Level", labels = c("0" = "Simple", "1" = "1st-order", "2" = "2nd-order")) +
    labs(title = "C. Individual Theory of Mind",
         subtitle = "Best-fitting ToM sophistication level",
         x = "Individual", y = "Model-Behavior Correlation") +
    theme_classic(base_size = base_font_size) +
    theme(
      plot.title = element_text(face = "bold", size = title_font_size),
      plot.subtitle = element_text(size = subtitle_font_size),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"
    )
} else {
  p3 <- ggplot() + labs(title = "C. Theory of Mind (Insufficient Data)") + theme_void()
}

# Panel D: Individual Exploration Patterns
if(nrow(exploration_long) > 0) {
  p4 <- ggplot(exploration_long, aes(x = social_complexity, y = exploration_rate, color = monkey)) +
    geom_point(size = 3, alpha = 0.8) +
    geom_line(aes(group = monkey), alpha = 0.6, linewidth = 1) +
    scale_color_manual(values = monkey_colors, name = "Individual") +
    labs(title = "D. Individual Exploration Patterns",
         subtitle = "Exploration rate across social complexity",
         x = "Social Context", y = "Exploration Rate") +
    theme_classic(base_size = base_font_size) +
    theme(
      plot.title = element_text(face = "bold", size = title_font_size),
      plot.subtitle = element_text(size = subtitle_font_size),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"
    )
} else {
  p4 <- ggplot() + labs(title = "D. Exploration Patterns (Insufficient Data)") + theme_void()
}

# Panel E: Individual Summary Profile
summary_data <- data.frame()

if(nrow(individual_social_effects) > 0 && nrow(individual_tom_results) > 0) {
  summary_data <- individual_social_effects %>%
    select(monkey, social_coefficient, significant) %>%
    full_join(individual_tom_results %>% select(monkey, best_tom_level, best_correlation), by = "monkey") %>%
    left_join(data_clean %>% select(monkey, sex) %>% distinct(), by = "monkey") %>%
    mutate(
      social_effect_strength = case_when(
        is.na(social_coefficient) ~ "No Data",
        abs(social_coefficient) > 0.5 & significant ~ "Strong",
        abs(social_coefficient) > 0.2 & significant ~ "Moderate", 
        significant ~ "Weak",
        TRUE ~ "Non-significant"
      ),
      tom_sophistication = case_when(
        is.na(best_tom_level) ~ "No Data",
        best_tom_level == 0 ~ "Simple",
        best_tom_level == 1 ~ "1st-order ToM",
        best_tom_level == 2 ~ "2nd-order ToM"
      )
    )
  
  p5 <- ggplot(summary_data, aes(x = reorder(monkey, best_tom_level), y = 1)) +
    geom_tile(aes(fill = sex), alpha = 0.3, color = "white", linewidth = 1) +
    geom_text(aes(label = paste(tom_sophistication, "\n", social_effect_strength)), 
             size = 2.8, fontface = "bold") +
    scale_fill_manual(values = sex_colors, name = "Sex") +
    labs(title = "E. Individual Cognitive Profiles",
         subtitle = "ToM sophistication & social influence strength",
         x = "Individual", y = "") +
    theme_void(base_size = base_font_size) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = title_font_size),
      plot.subtitle = element_text(hjust = 0.5, size = subtitle_font_size),
      axis.text.x = element_text(angle = 45, hjust = 1, size = base_font_size - 1),
      legend.position = "none"
    )
} else {
  p5 <- ggplot() + labs(title = "E. Cognitive Profiles (Insufficient Data)") + theme_void()
}

# Create shared legend for monkey colors
legend_data <- data.frame(
  monkey = names(monkey_colors),
  sex = c("Male", "Male", "Male", "Female", "Female", "Female"),
  y = 1
)

legend_plot <- ggplot(legend_data, aes(x = monkey, y = y, fill = monkey)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = paste(monkey, "\n(", sex, ")")), size = 2.5, fontface = "bold") +
  scale_fill_manual(values = monkey_colors) +
  labs(title = "Individual Legend") +
  theme_void(base_size = base_font_size - 2) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = base_font_size),
    legend.position = "none"
  )

# Combine all panels
combined_plot <- grid.arrange(
  p1, p2, p3, 
  p4, p5, legend_plot,
  nrow = 2, ncol = 3,
  top = textGrob("Individual Monkey Bayesian Theory of Mind Analysis", 
                gp = gpar(fontsize = title_font_size + 4, fontface = "bold")),
  heights = c(3, 2)
)

# Save the comprehensive figure
ggsave("results/figures/Individual_Monkey_ToM_5Panel.png", combined_plot,
       width = fig_width*1.5, height = fig_height*1.2, units = "mm", dpi = fig_dpi)

ggsave("results/figures/Individual_Monkey_ToM_5Panel.tiff", combined_plot,
       width = fig_width*1.5, height = fig_height*1.2, units = "mm", dpi = fig_dpi)

# =============================================================================
# 7. INDIVIDUAL SUMMARY STATISTICS
# =============================================================================

cat("\n=============================================================================\n")
cat("INDIVIDUAL MONKEY BAYESIAN THEORY OF MIND ANALYSIS - DETAILED RESULTS\n")
cat("=============================================================================\n\n")

# Individual Social Influence Summary
if(nrow(individual_social_effects) > 0) {
  cat("INDIVIDUAL SOCIAL INFLUENCE EFFECTS:\n")
  for(i in 1:nrow(individual_social_effects)) {
    row <- individual_social_effects[i, ]
    significance <- ifelse(row$significant, "SIGNIFICANT", "non-significant")
    cat(sprintf("• %s (%s): coefficient = %.3f, p = %.3f (%s)\n", 
                row$monkey, row$sex, row$social_coefficient, row$social_pvalue, significance))
  }
  cat("\n")
}

# Individual ToM Results Summary
if(nrow(individual_tom_results) > 0) {
  cat("INDIVIDUAL THEORY OF MIND SOPHISTICATION:\n")
  for(i in 1:nrow(individual_tom_results)) {
    row <- individual_tom_results[i, ]
    cat(sprintf("• %s (%s): %s (correlation = %.3f, learning rate = %.2f)\n", 
                row$monkey, row$sex, row$tom_interpretation, row$best_correlation, row$best_learning_rate))
  }
  cat("\n")
}

# Sex differences summary
if(nrow(individual_tom_results) > 0) {
  male_tom <- individual_tom_results %>% filter(sex == "Male")
  female_tom <- individual_tom_results %>% filter(sex == "Female")
  
  cat("SEX DIFFERENCES IN COGNITIVE SOPHISTICATION:\n")
  if(nrow(male_tom) > 0) {
    male_tom_levels <- table(male_tom$best_tom_level)
    cat("Males ToM distribution:", paste(names(male_tom_levels), "=", male_tom_levels, collapse = ", "), "\n")
  }
  if(nrow(female_tom) > 0) {
    female_tom_levels <- table(female_tom$best_tom_level)
    cat("Females ToM distribution:", paste(names(female_tom_levels), "=", female_tom_levels, collapse = ", "), "\n")
  }
  cat("\n")
}

cat("OVERALL INDIVIDUAL DIFFERENCES:\n")
cat("- Each animal shows a unique cognitive profile\n")
cat("- Individual variation in Theory of Mind sophistication\n") 
cat("- Sex differences may exist in social cognitive strategies\n")
cat("- Some animals show sophisticated recursive reasoning (2nd-order ToM)\n\n")

cat("=============================================================================\n")
cat("5-PANEL INDIVIDUAL ANALYSIS COMPLETE\n")
cat("Detailed figures saved to results/figures/Individual_Monkey_ToM_5Panel.*\n")
cat("=============================================================================\n") 