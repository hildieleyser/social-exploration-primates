# Advanced Insight Plots for Social Decision-Making Analysis
# Creates publication-quality plots with deep analytical insights

library(graphics)

# Load and prepare data
data <- read.csv("Explore Exploit Dataset.csv", stringsAsFactors = FALSE)
main_trials <- data[data$TRIAL_TYPE == "OIT_RE" & !is.na(data$OUTCOME), ]

# Create variables
main_trials$explore_choice <- ifelse(main_trials$OUTCOME == "explore", 1, 0)
main_trials$social_complexity <- factor(ifelse(main_trials$CONDITION == "solo", "Individual",
                                              ifelse(main_trials$CONDITION == "duo", "Dyadic", "Triadic")),
                                       levels = c("Individual", "Dyadic", "Triadic"))
main_trials$social_complexity_numeric <- as.numeric(main_trials$social_complexity) - 1
main_trials$rank <- main_trials$RELATIVE_RANK
main_trials$rank[is.na(main_trials$rank)] <- 0
main_trials$monkey_id <- factor(main_trials$monkey)
main_trials$expectation <- main_trials$expected_explore
main_trials$known_value <- main_trials$SUBJECTIVE_CHOSEN_VALUE

# Clean data
main_trials <- main_trials[complete.cases(main_trials[c("explore_choice", "social_complexity", 
                                                       "rank", "monkey_id", 
                                                       "expectation", "known_value")]), ]

# Get monkey names
monkey_names <- c("ANEMONE", "CHOCOLAT", "DALI", "EBI", "FRAN", "ICE")

# ================================================================================
# FIGURE 1: COMPREHENSIVE DATA INSIGHT DASHBOARD
# ================================================================================

create_comprehensive_dashboard <- function() {
  
  png("Comprehensive_Data_Insight_Dashboard.png", width = 7200, height = 4800, res = 600)
  
  # Set up 2x3 layout
  par(mfrow = c(2, 3), mar = c(4, 4, 3, 2), family = "sans")
  
  # Panel A: Social Complexity Main Effect
  complexity_means <- tapply(main_trials$explore_choice, main_trials$social_complexity, mean)
  complexity_se <- tapply(main_trials$explore_choice, main_trials$social_complexity, 
                         function(x) sd(x)/sqrt(length(x)))
  
  bp <- barplot(complexity_means, ylim = c(0, 0.2), 
                col = c("#3498db", "#e74c3c", "#f39c12"),
                names.arg = c("Individual", "Dyadic", "Triadic"),
                main = "A. Social Complexity Effect",
                ylab = "Exploration Rate",
                cex.main = 1.2, cex.lab = 1.1)
  
  # Add error bars
  arrows(bp, complexity_means - complexity_se, 
         bp, complexity_means + complexity_se,
         length = 0.05, angle = 90, code = 3, lwd = 2)
  
  # Add sample sizes
  complexity_n <- table(main_trials$social_complexity)
  for(i in 1:3) {
    text(bp[i], complexity_means[i] + complexity_se[i] + 0.01, 
         paste("n =", complexity_n[i]), cex = 0.8)
  }
  
  # Panel B: Individual Differences
  monkey_means <- tapply(main_trials$explore_choice, main_trials$monkey_id, mean)
  monkey_n <- table(main_trials$monkey_id)
  
  bp2 <- barplot(monkey_means, ylim = c(0, 0.8),
                 col = rainbow(6, alpha = 0.7),
                 names.arg = monkey_names,
                 main = "B. Individual Differences",
                 ylab = "Exploration Rate",
                 cex.main = 1.2, cex.lab = 1.1, las = 2)
  
  # Add sample sizes
  for(i in 1:6) {
    text(bp2[i], monkey_means[i] + 0.05, 
         paste("n =", monkey_n[i]), cex = 0.7, srt = 90)
  }
  
  # Panel C: Interaction Plot
  interaction_data <- with(main_trials, 
                          tapply(explore_choice, list(monkey_id, social_complexity), mean))
  
  plot(1:3, interaction_data[1,], type = "l", col = rainbow(6)[1], lwd = 2,
       ylim = c(0, 0.8), xlab = "Social Complexity", ylab = "Exploration Rate",
       main = "C. Monkey × Social Context",
       xaxt = "n", cex.main = 1.2, cex.lab = 1.1)
  
  axis(1, at = 1:3, labels = c("Individual", "Dyadic", "Triadic"))
  
  for(i in 2:6) {
    lines(1:3, interaction_data[i,], col = rainbow(6)[i], lwd = 2)
  }
  
  legend("topright", legend = monkey_names, col = rainbow(6), 
         lwd = 2, cex = 0.8, ncol = 2)
  
  # Panel D: Rank Effects in Social Contexts
  social_data <- main_trials[main_trials$social_complexity != "Individual", ]
  rank_means <- tapply(social_data$explore_choice, social_data$rank, mean)
  rank_se <- tapply(social_data$explore_choice, social_data$rank, 
                   function(x) sd(x)/sqrt(length(x)))
  
  bp3 <- barplot(rank_means, ylim = c(0, 0.2),
                 col = c("#27ae60", "#f39c12", "#e74c3c"),
                 names.arg = c("Dominant", "Middle", "Subordinate"),
                 main = "D. Dominance Rank Effect",
                 ylab = "Exploration Rate",
                 cex.main = 1.2, cex.lab = 1.1)
  
  arrows(bp3, rank_means - rank_se, 
         bp3, rank_means + rank_se,
         length = 0.05, angle = 90, code = 3, lwd = 2)
  
  # Panel E: Expectation vs. Behavior
  exp_bins <- cut(main_trials$expectation, breaks = 5, labels = c("Very Low", "Low", "Medium", "High", "Very High"))
  exp_means <- tapply(main_trials$explore_choice, exp_bins, mean, na.rm = TRUE)
  
  bp4 <- barplot(exp_means, ylim = c(0, 0.3),
                 col = colorRampPalette(c("#3498db", "#e74c3c"))(5),
                 main = "E. Running Expectation Effect",
                 ylab = "Exploration Rate",
                 cex.main = 1.2, cex.lab = 1.1, las = 2)
  
  # Panel F: Model Predictions vs. Observed
  model <- glm(explore_choice ~ social_complexity_numeric + monkey_id, 
               data = main_trials, family = binomial())
  
  predicted <- predict(model, type = "response")
  
  # Binned plot
  bins <- cut(predicted, breaks = 10)
  bin_means_pred <- tapply(predicted, bins, mean)
  bin_means_obs <- tapply(main_trials$explore_choice, bins, mean)
  
  plot(bin_means_pred, bin_means_obs, pch = 19, col = "#2c3e50",
       xlab = "Model Predictions", ylab = "Observed Rate",
       main = "F. Model Fit (Predicted vs. Observed)",
       cex.main = 1.2, cex.lab = 1.1)
  
  # Add perfect prediction line
  abline(0, 1, col = "#e74c3c", lwd = 2, lty = 2)
  
  # Add correlation
  cor_val <- cor(bin_means_pred, bin_means_obs, use = "complete.obs")
  text(0.05, 0.25, paste("r =", round(cor_val, 3)), cex = 1.1)
  
  dev.off()
  
  cat("Comprehensive dashboard created: Comprehensive_Data_Insight_Dashboard.png\n")
}

# ================================================================================
# FIGURE 2: HIERARCHICAL STRUCTURE VISUALIZATION
# ================================================================================

create_hierarchical_structure_viz <- function() {
  
  png("Hierarchical_Structure_Visualization.png", width = 6000, height = 4000, res = 600)
  
  par(mfrow = c(2, 2), mar = c(4, 4, 3, 2), family = "sans")
  
  # Panel A: Trial-level variation
  trial_summary <- aggregate(list(explore_rate = main_trials$explore_choice),
                            list(block = main_trials$BLOCK_No), mean)
  
  hist(trial_summary$explore_rate, breaks = 20, col = "#3498db", alpha = 0.7,
       main = "A. Block-Level Variation", xlab = "Block Exploration Rate",
       ylab = "Frequency", cex.main = 1.2)
  
  # Panel B: Monkey-level consistency
  monkey_consistency <- by(main_trials, main_trials$monkey_id, function(x) {
    tapply(x$explore_choice, x$social_complexity, mean)
  })
  
  boxplot(monkey_consistency, main = "B. Individual Consistency Across Contexts",
          ylab = "Exploration Rate", xlab = "Social Complexity",
          col = c("#e8f5e8", "#fff3cd", "#ffeaa7"), cex.main = 1.2)
  
  # Panel C: Sample size distribution
  sample_sizes <- table(main_trials$monkey_id, main_trials$social_complexity)
  
  barplot(t(sample_sizes), beside = TRUE, col = c("#3498db", "#e74c3c", "#f39c12"),
          main = "C. Sample Size Distribution", 
          ylab = "Number of Trials", xlab = "Monkey",
          cex.main = 1.2, las = 2,
          names.arg = monkey_names)
  
  legend("topright", legend = c("Individual", "Dyadic", "Triadic"),
         fill = c("#3498db", "#e74c3c", "#f39c12"), cex = 0.9)
  
  # Panel D: Temporal patterns
  if("date" %in% names(main_trials)) {
    main_trials$date_numeric <- as.numeric(as.Date(main_trials$date))
    if(length(unique(main_trials$date_numeric)) > 1) {
      daily_rates <- aggregate(list(explore_rate = main_trials$explore_choice),
                              list(date = main_trials$date_numeric), mean)
      
      plot(daily_rates$date, daily_rates$explore_rate, type = "l", lwd = 2,
           main = "D. Temporal Patterns", xlab = "Date", ylab = "Daily Exploration Rate",
           cex.main = 1.2, col = "#2c3e50")
      
      # Add trend line
      trend_model <- lm(explore_rate ~ date, data = daily_rates)
      abline(trend_model, col = "#e74c3c", lwd = 2, lty = 2)
    } else {
      plot(1, 1, type = "n", main = "D. No Temporal Variation")
      text(1, 1, "All trials from same date", cex = 1.2)
    }
  }
  
  dev.off()
  
  cat("Hierarchical structure visualization created: Hierarchical_Structure_Visualization.png\n")
}

# ================================================================================
# FIGURE 3: MECHANISTIC INSIGHTS
# ================================================================================

create_mechanistic_insights <- function() {
  
  png("Mechanistic_Insights_Analysis.png", width = 7200, height = 4800, res = 600)
  
  par(mfrow = c(2, 3), mar = c(4, 4, 3, 2), family = "sans")
  
  # Panel A: Cognitive Load Hypothesis
  cognitive_load <- main_trials$social_complexity_numeric
  explore_rate <- main_trials$explore_choice
  
  # Smooth regression
  smooth_model <- loess(explore_rate ~ cognitive_load)
  load_seq <- seq(0, 2, length.out = 100)
  smooth_pred <- predict(smooth_model, newdata = data.frame(cognitive_load = load_seq))
  
  plot(cognitive_load + runif(length(cognitive_load), -0.1, 0.1), explore_rate,
       pch = 19, col = rgb(44/255, 62/255, 80/255, 0.3), cex = 0.8,
       xlab = "Social Complexity (Cognitive Load)", ylab = "Exploration Choice",
       main = "A. Cognitive Load Hypothesis", cex.main = 1.2)
  
  lines(load_seq, smooth_pred, col = "#e74c3c", lwd = 3)
  
  # Panel B: Information vs. Competition
  # Separate social contexts
  social_data <- main_trials[main_trials$social_complexity != "Individual", ]
  
  if(nrow(social_data) > 0) {
    info_effect <- tapply(social_data$explore_choice, 
                         list(social_data$social_complexity, social_data$rank), mean)
    
    barplot(info_effect, beside = TRUE, col = c("#3498db", "#e74c3c"),
            main = "B. Information vs. Competition", 
            ylab = "Exploration Rate", xlab = "Dominance Rank",
            cex.main = 1.2, names.arg = c("Dom", "Mid", "Sub"))
    
    legend("topright", legend = c("Dyadic", "Triadic"), 
           fill = c("#3498db", "#e74c3c"), cex = 0.9)
  }
  
  # Panel C: Expectation Updating
  exp_vals <- seq(0, 1, length.out = 50)
  exp_effect <- sapply(exp_vals, function(e) {
    subset_data <- main_trials[abs(main_trials$expectation - e) < 0.1, ]
    if(nrow(subset_data) > 5) mean(subset_data$explore_choice) else NA
  })
  
  plot(exp_vals, exp_effect, type = "l", lwd = 3, col = "#27ae60",
       xlab = "Running Expectation", ylab = "Exploration Rate",
       main = "C. Expectation-Behavior Relationship", cex.main = 1.2)
  
  # Panel D: Value Sensitivity
  val_vals <- seq(0, 1, length.out = 50)
  val_effect <- sapply(val_vals, function(v) {
    subset_data <- main_trials[abs(main_trials$known_value - v) < 0.1, ]
    if(nrow(subset_data) > 5) mean(subset_data$explore_choice) else NA
  })
  
  plot(val_vals, val_effect, type = "l", lwd = 3, col = "#f39c12",
       xlab = "Known Option Value", ylab = "Exploration Rate",
       main = "D. Value Sensitivity", cex.main = 1.2)
  
  # Panel E: Social Context Transitions
  # Look at how behavior changes within sessions
  context_transitions <- by(main_trials, main_trials$monkey_id, function(monkey_data) {
    if(nrow(monkey_data) > 20) {
      early <- monkey_data[1:(nrow(monkey_data)/2), ]
      late <- monkey_data[(nrow(monkey_data)/2 + 1):nrow(monkey_data), ]
      c(early = mean(early$explore_choice), late = mean(late$explore_choice))
    } else {
      c(early = NA, late = NA)
    }
  })
  
  transitions_matrix <- do.call(rbind, context_transitions)
  
  plot(transitions_matrix[, "early"], transitions_matrix[, "late"],
       pch = 19, cex = 2, col = rainbow(6),
       xlab = "Early Session Exploration", ylab = "Late Session Exploration",
       main = "E. Within-Session Learning", cex.main = 1.2)
  
  abline(0, 1, lty = 2, col = "gray")
  text(transitions_matrix[, "early"], transitions_matrix[, "late"],
       monkey_names, pos = 3, cex = 0.8)
  
  # Panel F: Model Mechanisms
  # Show coefficient effects
  full_model <- glm(explore_choice ~ social_complexity_numeric + rank + 
                   expectation + known_value + monkey_id, 
                   data = main_trials, family = binomial())
  
  coefs <- summary(full_model)$coefficients[2:5, ]
  effect_names <- c("Social\nComplexity", "Rank", "Expectation", "Known\nValue")
  
  bp <- barplot(coefs[, 1], ylim = c(-1.5, 1), 
                col = c("#e74c3c", "#f39c12", "#27ae60", "#3498db"),
                names.arg = effect_names,
                main = "F. Model Mechanisms", ylab = "Log-Odds Effect",
                cex.main = 1.2, las = 2)
  
  # Add error bars
  arrows(bp, coefs[, 1] - coefs[, 2], bp, coefs[, 1] + coefs[, 2],
         length = 0.05, angle = 90, code = 3, lwd = 2)
  
  # Add significance stars
  p_vals <- coefs[, 4]
  stars <- ifelse(p_vals < 0.001, "***", 
                 ifelse(p_vals < 0.01, "**",
                       ifelse(p_vals < 0.05, "*", "")))
  
  text(bp, coefs[, 1] + coefs[, 2] + 0.1, stars, cex = 1.2)
  
  abline(h = 0, lty = 2, col = "gray")
  
  dev.off()
  
  cat("Mechanistic insights analysis created: Mechanistic_Insights_Analysis.png\n")
}

# Create all figures
create_comprehensive_dashboard()
create_hierarchical_structure_viz()
create_mechanistic_insights()

cat("\nAdvanced insight visualizations completed!\n")
cat("These figures provide deep understanding of:\n")
cat("1. Overall patterns and individual differences\n")
cat("2. Hierarchical data structure\n")  
cat("3. Mechanistic insights into decision-making processes\n") 