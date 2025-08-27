# =============================================================================
# PREFERENCE ATTRIBUTION FIGURES: How monkeys model what others want
# =============================================================================

library(dplyr)

# Load results
load("results/preference_attribution_results.RData")

# Sex information
sex_info <- data.frame(
  monkey = c("FRAN", "DALI", "EBI", "ANEMONE", "CHOCOLAT", "ICE"),
  sex = c("Male", "Male", "Male", "Female", "Female", "Female"),
  initial = c("F", "D", "E", "A", "C", "I"),
  stringsAsFactors = FALSE
)

strategic_summary <- merge(strategic_summary, sex_info, by = "monkey", all.x = TRUE)
mentalizing_df <- merge(mentalizing_df, sex_info, by = "monkey", all.x = TRUE)

# Professional settings
output_width <- 14
output_height <- 10
dpi <- 300
colors <- c("Male" = "#2166AC", "Female" = "#D6604D")

cat("Creating preference attribution figures...\n")

# =============================================================================
# FIGURE: PREFERENCE INFERENCE AND STRATEGIC BEHAVIOR
# =============================================================================

tiff("results/Figure_Preference_Attribution.tiff", 
     width = output_width, height = output_height, units = "in", res = dpi)

par(mfrow = c(2, 3), mar = c(5, 5, 3, 2), oma = c(3, 3, 3, 1))

# Panel A: Individual preference inference trajectories
monkey_names <- c("F", "D", "E", "A", "C", "I")
for(i in 1:6) {
  monkey_initial <- monkey_names[i]
  full_name <- sex_info$monkey[sex_info$initial == monkey_initial]
  monkey_sex <- sex_info$sex[sex_info$initial == monkey_initial]
  
  if(full_name %in% names(preference_models)) {
    beliefs <- preference_models[[full_name]]$beliefs
    
    # Plot preference inference for first other monkey (if exists)
    if(length(beliefs) > 0) {
      first_other <- beliefs[[1]]
      
      plot(first_other$trial, first_other$inferred_explore_preference * 100,
           type = "l", lwd = 3, col = colors[monkey_sex],
           xlab = "Trial Number", 
           ylab = "Inferred Explore Preference (%)",
           main = paste(monkey_initial, ": Modeling Others' Wants"),
           ylim = c(0, 100),
           cex.main = 1.3, cex.lab = 1.2, cex.axis = 1.1)
      
      # Add confidence band
      confidence_upper <- pmin(100, first_other$inferred_explore_preference * 100 + 
                              first_other$confidence * 20)
      confidence_lower <- pmax(0, first_other$inferred_explore_preference * 100 - 
                              first_other$confidence * 20)
      
      polygon(c(first_other$trial, rev(first_other$trial)),
              c(confidence_upper, rev(confidence_lower)),
              col = paste0(colors[monkey_sex], "30"), border = NA)
      
      # Add reference lines
      abline(h = 50, lty = 3, col = "gray60", lwd = 1)
      
      # Show final inference
      final_pref <- tail(first_other$inferred_explore_preference, 1) * 100
      text(max(first_other$trial) * 0.7, final_pref + 10,
           paste("Final:", round(final_pref, 1), "%"), 
           cex = 1.1, font = 2, col = colors[monkey_sex])
    } else {
      plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "", 
           main = paste(monkey_initial, ": No Data"))
    }
  } else {
    plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "", 
         main = paste(monkey_initial, ": No Data"))
  }
}

mtext("How Monkeys Infer What Others Want", outer = TRUE, cex = 1.6, font = 2, side = 3, line = 1)
mtext("Individual differences in preference attribution over time", 
      outer = TRUE, cex = 1.2, side = 1, line = 1)

dev.off()

# =============================================================================
# FIGURE: STRATEGIC BEHAVIOR AND MENTALIZING ABILITIES
# =============================================================================

tiff("results/Figure_Strategic_Mentalizing.tiff", 
     width = output_width, height = output_height, units = "in", res = dpi)

par(mfrow = c(2, 2), mar = c(5, 5, 3, 2), oma = c(3, 3, 3, 1))

# Panel A: Strategic behavior types
strategy_counts <- table(strategic_summary$best_strategy)
strategy_colors <- c("conformity" = "#1B9E77", "contrast" = "#D95F02", "conditional" = "#7570B3")

barplot(strategy_counts,
        col = strategy_colors[names(strategy_counts)],
        main = "A) Strategic Behavior Types",
        ylab = "Number of Monkeys",
        xlab = "Strategy Type",
        cex.main = 1.4, cex.lab = 1.3, cex.names = 1.2)

# Add strategy descriptions
text(0.7, strategy_counts[1]/2, "Copy\nOthers", cex = 1.1, font = 2)
if(length(strategy_counts) > 1) {
  text(1.9, strategy_counts[2]/2, "Contrast\nOthers", cex = 1.1, font = 2)
}
if(length(strategy_counts) > 2) {
  text(3.1, strategy_counts[3]/2, "Conditional", cex = 1.1, font = 2)
}

# Panel B: Preference coefficient vs mentalizing ability
mentalizing_strategic <- merge(mentalizing_df, strategic_summary, by = "monkey", all = TRUE)

plot(mentalizing_strategic$mentalizing_score, mentalizing_strategic$preference_coefficient,
     xlim = c(0, max(mentalizing_strategic$mentalizing_score, na.rm = TRUE) * 1.1),
     ylim = c(-3, 3),
     xlab = "Mentalizing Ability Score", 
     ylab = "Strategic Preference Coefficient",
     main = "B) Mentalizing vs Strategic Behavior",
     pch = 19, cex = 2.5, col = colors[mentalizing_strategic$sex],
     cex.main = 1.4, cex.lab = 1.3, cex.axis = 1.2)

text(mentalizing_strategic$mentalizing_score + 0.01, 
     mentalizing_strategic$preference_coefficient + 0.1,
     mentalizing_strategic$initial, font = 2, cex = 1.2, 
     col = colors[mentalizing_strategic$sex])

abline(h = 0, lty = 2, col = "gray50", lwd = 2)

# Panel C: Accuracy vs Confidence
plot(mentalizing_df$confidence_level, mentalizing_df$preference_accuracy,
     xlim = c(0, 1), ylim = c(0, 1),
     xlab = "Confidence in Inferences", 
     ylab = "Accuracy of Preference Attribution",
     main = "C) Confidence vs Accuracy",
     pch = 19, cex = 2.5, col = colors[mentalizing_df$sex],
     cex.main = 1.4, cex.lab = 1.3, cex.axis = 1.2)

text(mentalizing_df$confidence_level + 0.02, mentalizing_df$preference_accuracy + 0.02,
     mentalizing_df$initial, font = 2, cex = 1.2, col = colors[mentalizing_df$sex])

# Add diagonal line (perfect calibration)
abline(a = 0, b = 1, lty = 2, col = "gray50", lwd = 2)

# Panel D: Sex differences in mentalizing
sex_mental_summary <- mentalizing_df %>%
  group_by(sex) %>%
  summarise(
    mean_accuracy = mean(preference_accuracy, na.rm = TRUE),
    se_accuracy = sd(preference_accuracy, na.rm = TRUE) / sqrt(n()),
    mean_confidence = mean(confidence_level, na.rm = TRUE),
    se_confidence = sd(confidence_level, na.rm = TRUE) / sqrt(n()),
    mean_score = mean(mentalizing_score, na.rm = TRUE),
    se_score = sd(mentalizing_score, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

x_pos <- c(1, 2)
barplot(sex_mental_summary$mean_score,
        names.arg = sex_mental_summary$sex,
        col = colors[sex_mental_summary$sex],
        main = "D) Sex Differences in Mentalizing",
        ylab = "Mentalizing Ability Score",
        xlab = "Sex",
        cex.main = 1.4, cex.lab = 1.3, cex.names = 1.2)

# Add error bars
x_centers <- c(0.7, 1.9)
segments(x_centers, sex_mental_summary$mean_score - sex_mental_summary$se_score,
         x_centers, sex_mental_summary$mean_score + sex_mental_summary$se_score,
         lwd = 2, col = "black")

mtext("Strategic Decision-Making Based on Inferred Preferences", 
      outer = TRUE, cex = 1.6, font = 2, side = 3, line = 1)

legend("topright", legend = c("Male", "Female"), 
       pch = 19, col = colors, cex = 1.2, 
       inset = c(-0.1, -0.1), xpd = TRUE)

dev.off()

# =============================================================================
# FIGURE: DETAILED PREFERENCE INFERENCE MODELS
# =============================================================================

tiff("results/Figure_Preference_Models.tiff", 
     width = 16, height = 10, units = "in", res = dpi)

par(mfrow = c(2, 3), mar = c(4, 4, 3, 1), oma = c(3, 3, 3, 1))

for(monkey_name in names(preference_models)[1:6]) {
  monkey_initial <- sex_info$initial[sex_info$monkey == monkey_name]
  monkey_sex <- sex_info$sex[sex_info$monkey == monkey_name]
  
  beliefs <- preference_models[[monkey_name]]$beliefs
  
  if(length(beliefs) > 0) {
    # Create a complex multi-line plot showing inferences about all others
    plot(1, 1, type = "n", 
         xlim = c(1, nrow(preference_models[[monkey_name]]$trials)),
         ylim = c(0, 1),
         xlab = "Trial Number", 
         ylab = "Inferred Explore Preference",
         main = paste(monkey_initial, ": Models of All Others"),
         cex.main = 1.3, cex.lab = 1.2, cex.axis = 1.1)
    
    # Plot inference for each other monkey
    other_colors <- rainbow(length(beliefs), alpha = 0.8)
    for(i in 1:length(beliefs)) {
      other_belief <- beliefs[[i]]
      lines(other_belief$trial, other_belief$inferred_explore_preference,
            lwd = 2, col = other_colors[i])
      
      # Add final preference label
      final_pref <- tail(other_belief$inferred_explore_preference, 1)
      text(max(other_belief$trial), final_pref,
           paste("O", i, sep = ""), cex = 0.9, font = 2, col = other_colors[i])
    }
    
    abline(h = 0.5, lty = 3, col = "gray60", lwd = 1)
    
    # Add overall mentalizing score
    mental_score <- mentalizing_df$mentalizing_score[mentalizing_df$monkey == monkey_name]
    text(max(other_belief$trial) * 0.2, 0.9,
         paste("Score:", round(mental_score, 3)), 
         cex = 1.1, font = 2, col = colors[monkey_sex])
  } else {
    plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "", 
         main = paste(monkey_initial, ": No Data"))
  }
}

mtext("Individual Mental Models: How Each Monkey Tracks Others' Preferences", 
      outer = TRUE, cex = 1.6, font = 2, side = 3, line = 1)
mtext("O1, O2, etc. = Different other monkeys being modeled", 
      outer = TRUE, cex = 1.1, side = 1, line = 1)

dev.off()

cat("Preference attribution figures created:\n")
cat("- Figure_Preference_Attribution.tiff: Individual inference trajectories\n")
cat("- Figure_Strategic_Mentalizing.tiff: Strategic behavior and abilities\n")
cat("- Figure_Preference_Models.tiff: Detailed mental models\n\n")

cat("INTERPRETATION:\n")
cat("===============\n")
cat("These figures reveal:\n")
cat("1. Monkeys actively infer what others want/prefer\n")
cat("2. Individual differences in mentalizing accuracy\n")
cat("3. Strategic use of preference inferences\n")
cat("4. Different behavioral strategies (conform vs contrast)\n")
cat("5. Sex differences in social cognitive abilities\n") 