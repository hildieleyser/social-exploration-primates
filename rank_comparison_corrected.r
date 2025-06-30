# CORRECTED ABSOLUTE vs RELATIVE RANK COMPARISON + MATHEMATICAL MODEL
# Absolute rank: Two groups of 1-3, not one group of 1-6

library(nnet)

# Load data
data_raw <- read.csv("Explore Exploit Dataset.csv")

# Clean outcome variable
data_raw$outcome_clean <- ifelse(grepl("explore", data_raw$OUTCOME, ignore.case = TRUE), "explore",
                               ifelse(grepl("exploit", data_raw$OUTCOME, ignore.case = TRUE), "exploit", 
                                     ifelse(data_raw$OUTCOME %in% c("NONE", "none", "stop"), "none", "other")))

data_valid <- data_raw[!is.na(data_raw$outcome_clean) & data_raw$outcome_clean != "other", ]

# Add monkey info with CORRECTED ABSOLUTE HIERARCHY
# Two groups of 1-3 based on hierarchy levels
monkey_info <- data.frame(
  monkey = c("FRAN", "CHOCOLAT", "DALI", "ICE", "EBI", "ANEMONE"),
  sex = c("Male", "Female", "Male", "Female", "Male", "Female"),
  hierarchy = c("Dominant", "Dominant", "Intermediate", "Intermediate", "Subordinate", "Subordinate"),
  absolute_rank_group = c("Group1", "Group1", "Group2", "Group2", "Group3", "Group3"),  # Two groups structure
  absolute_rank_within_group = c(1, 2, 1, 2, 1, 2),  # Rank within each pair
  absolute_rank_hierarchy = c(1, 1, 2, 2, 3, 3)  # Hierarchy level (1=Dominant, 2=Intermediate, 3=Subordinate)
)

data_analysis <- merge(data_valid, monkey_info, by = "monkey", all.x = TRUE)

# Filter complete data
model_data <- data_analysis[
  !is.na(data_analysis$CONDITION) &
  !is.na(data_analysis$PAIRED_WITH) &
  !is.na(data_analysis$RELATIVE_RANK) &
  !is.na(data_analysis$SUBJECTIVE_CHOSEN_VALUE) &
  !is.na(data_analysis$subjective_exploit) &
  !is.na(data_analysis$expected_explore), ]

# Calculate exploration rates
safe_rate <- function(subset_data, outcome) {
  if(nrow(subset_data) == 0) return(0)
  sum(subset_data$outcome_clean == outcome, na.rm = TRUE) / nrow(subset_data) * 100
}

# CORRECTED ABSOLUTE RANK ANALYSIS - Two groups of 1-3
absolute_rank_effects <- data.frame()
for(hier_level in 1:3) {  # Hierarchy levels: 1=Dominant, 2=Intermediate, 3=Subordinate
  subset_data <- model_data[model_data$absolute_rank_hierarchy == hier_level, ]
  if(nrow(subset_data) > 0) {
    hierarchy_name <- c("Dominant", "Intermediate", "Subordinate")[hier_level]
    monkeys_in_level <- unique(subset_data$monkey)
    
    absolute_rank_effects <- rbind(absolute_rank_effects, data.frame(
      Absolute_Rank = hier_level,
      Hierarchy_Level = hierarchy_name,
      Monkeys = paste(monkeys_in_level, collapse = ", "),
      Explore = safe_rate(subset_data, "explore"),
      Exploit = safe_rate(subset_data, "exploit"),
      None = safe_rate(subset_data, "none"),
      N_Trials = nrow(subset_data),
      N_Monkeys = length(monkeys_in_level)
    ))
  }
}

# RELATIVE RANK ANALYSIS (context-dependent) - unchanged
relative_rank_effects <- data.frame()
for(rank in 1:3) {
  subset_data <- model_data[model_data$RELATIVE_RANK == rank, ]
  if(nrow(subset_data) > 0) {
    relative_rank_effects <- rbind(relative_rank_effects, data.frame(
      Relative_Rank = rank,
      Explore = safe_rate(subset_data, "explore"),
      Exploit = safe_rate(subset_data, "exploit"),
      None = safe_rate(subset_data, "none"),
      N_Trials = nrow(subset_data)
    ))
  }
}

# WITHIN-GROUP ABSOLUTE RANK ANALYSIS
# Look at rank 1 vs rank 2 within each hierarchy level
within_group_effects <- data.frame()
for(hier_level in 1:3) {
  for(within_rank in 1:2) {
    subset_data <- model_data[model_data$absolute_rank_hierarchy == hier_level & 
                             model_data$absolute_rank_within_group == within_rank, ]
    if(nrow(subset_data) > 0) {
      hierarchy_name <- c("Dominant", "Intermediate", "Subordinate")[hier_level]
      monkey_name <- unique(subset_data$monkey)
      
      within_group_effects <- rbind(within_group_effects, data.frame(
        Hierarchy_Level = hierarchy_name,
        Within_Group_Rank = within_rank,
        Monkey = monkey_name,
        Explore = safe_rate(subset_data, "explore"),
        N_Trials = nrow(subset_data)
      ))
    }
  }
}

# CONTEXT-SPECIFIC RELATIVE RANK ANALYSIS - unchanged
context_relative_rank <- data.frame()
for(context in c("solo", "duo", "trio")) {
  for(rank in 1:3) {
    subset_data <- model_data[model_data$CONDITION == context & model_data$RELATIVE_RANK == rank, ]
    if(nrow(subset_data) > 0) {
      context_relative_rank <- rbind(context_relative_rank, data.frame(
        Context = context,
        Relative_Rank = rank,
        Explore = safe_rate(subset_data, "explore"),
        N_Trials = nrow(subset_data)
      ))
    }
  }
}

# MATHEMATICAL MODEL COMPONENTS - unchanged
baseline_exploration <- 32.1
social_complexity_cost <- -10.5
hierarchy_advantage <- 7.5
sex_strategy_bonus <- 11.1
individual_variation_sd <- 10.7
subjective_value_coeff <- 88.47
exploit_value_coeff <- 0.22
explore_expect_coeff <- -0.12

# Create corrected visualization
pdf("CORRECTED_RANK_COMPARISON_AND_MODEL.pdf", width = 20, height = 16)
par(mfrow = c(3, 3), mar = c(5, 5, 4, 2))

# PLOT 1: Corrected Absolute Rank Effects (Hierarchy Levels)
plot(absolute_rank_effects$Absolute_Rank, absolute_rank_effects$Explore,
     type = "b", pch = 16, cex = 2, lwd = 3, col = "darkred",
     main = "ABSOLUTE RANK:\nHierarchy Levels (Two Groups of 1-3)", 
     xlab = "Hierarchy Level (1=Dominant, 2=Intermediate, 3=Subordinate)", 
     ylab = "Exploration Rate (%)",
     ylim = c(15, 50), cex.main = 1.4, cex.lab = 1.2)
text(absolute_rank_effects$Absolute_Rank, absolute_rank_effects$Explore + 2, 
     absolute_rank_effects$Hierarchy_Level, cex = 1.0, font = 2)
text(absolute_rank_effects$Absolute_Rank, absolute_rank_effects$Explore - 2, 
     paste0(round(absolute_rank_effects$Explore, 1), "%"), cex = 1.0, font = 2)
text(absolute_rank_effects$Absolute_Rank, absolute_rank_effects$Explore - 4, 
     paste0("n=", absolute_rank_effects$N_Monkeys), cex = 0.9, col = "gray")
# Add trend line
lm_abs <- lm(absolute_rank_effects$Explore ~ absolute_rank_effects$Absolute_Rank)
abline(lm_abs, col = "red", lwd = 2, lty = 2)
r_squared_abs <- summary(lm_abs)$r.squared
text(2, 45, paste("R² =", round(r_squared_abs, 3)), cex = 1.2, col = "red", font = 2)

# PLOT 2: Relative Rank Effects - unchanged
plot(relative_rank_effects$Relative_Rank, relative_rank_effects$Explore,
     type = "b", pch = 16, cex = 2, lwd = 3, col = "darkblue",
     main = "RELATIVE RANK:\nContext-Dependent Position", 
     xlab = "Relative Rank Position (1=Highest in Context)", ylab = "Exploration Rate (%)",
     ylim = c(10, 40), cex.main = 1.4, cex.lab = 1.2)
text(relative_rank_effects$Relative_Rank, relative_rank_effects$Explore + 2, 
     paste0(round(relative_rank_effects$Explore, 1), "%"), cex = 1.2, font = 2)
lm_rel <- lm(relative_rank_effects$Explore ~ relative_rank_effects$Relative_Rank)
abline(lm_rel, col = "blue", lwd = 2, lty = 2)
r_squared_rel <- summary(lm_rel)$r.squared
text(2, 35, paste("R² =", round(r_squared_rel, 3)), cex = 1.2, col = "blue", font = 2)

# PLOT 3: Corrected Direct Comparison
plot(1:3, relative_rank_effects$Explore, type = "b", pch = 16, cex = 2, lwd = 3, col = "blue",
     main = "CORRECTED COMPARISON:\nAbsolute (Hierarchy) vs Relative Rank", 
     xlab = "Rank Position", ylab = "Exploration Rate (%)",
     ylim = c(10, 50), cex.main = 1.4, cex.lab = 1.2)
lines(1:3, absolute_rank_effects$Explore, type = "b", pch = 17, cex = 2, lwd = 3, col = "red")
legend("topright", c("Relative Rank (1-3)", "Absolute Hierarchy (1-3)"), 
       col = c("blue", "red"), pch = c(16, 17), lwd = 3, cex = 1.1)
text(2.5, 40, paste("Relative: R² =", round(r_squared_rel, 3)), col = "blue", cex = 1.1)
text(2.5, 37, paste("Absolute: R² =", round(r_squared_abs, 3)), col = "red", cex = 1.1)

# PLOT 4: Within-Group Rank Effects
# Show rank 1 vs rank 2 within each hierarchy level
dominant_data <- within_group_effects[within_group_effects$Hierarchy_Level == "Dominant", ]
intermediate_data <- within_group_effects[within_group_effects$Hierarchy_Level == "Intermediate", ]
subordinate_data <- within_group_effects[within_group_effects$Hierarchy_Level == "Subordinate", ]

plot(1, 1, type = "n", xlim = c(0.5, 2.5), ylim = c(15, 60),
     main = "WITHIN-GROUP ABSOLUTE RANK:\nRank 1 vs 2 in Each Hierarchy Level",
     xlab = "Within-Group Rank", ylab = "Exploration Rate (%)",
     cex.main = 1.4, cex.lab = 1.2)

if(nrow(dominant_data) > 0) {
  lines(dominant_data$Within_Group_Rank, dominant_data$Explore, col = "gold", lwd = 3)
  points(dominant_data$Within_Group_Rank, dominant_data$Explore, col = "gold", pch = 16, cex = 2)
}
if(nrow(intermediate_data) > 0) {
  lines(intermediate_data$Within_Group_Rank, intermediate_data$Explore, col = "orange", lwd = 3)
  points(intermediate_data$Within_Group_Rank, intermediate_data$Explore, col = "orange", pch = 16, cex = 2)
}
if(nrow(subordinate_data) > 0) {
  lines(subordinate_data$Within_Group_Rank, subordinate_data$Explore, col = "brown", lwd = 3)
  points(subordinate_data$Within_Group_Rank, subordinate_data$Explore, col = "brown", pch = 16, cex = 2)
}

legend("topright", c("Dominant", "Intermediate", "Subordinate"), 
       col = c("gold", "orange", "brown"), lwd = 3, pch = 16, cex = 1.1)

# Add monkey names
if(nrow(dominant_data) > 0) {
  text(dominant_data$Within_Group_Rank, dominant_data$Explore + 2, 
       dominant_data$Monkey, cex = 0.8, col = "gold")
}
if(nrow(intermediate_data) > 0) {
  text(intermediate_data$Within_Group_Rank, intermediate_data$Explore + 2, 
       intermediate_data$Monkey, cex = 0.8, col = "orange")
}
if(nrow(subordinate_data) > 0) {
  text(subordinate_data$Within_Group_Rank, subordinate_data$Explore + 2, 
       subordinate_data$Monkey, cex = 0.8, col = "brown")
}

# PLOT 5: Context-Dependent Relative Rank - unchanged
context_colors <- c("lightblue", "orange", "red")
plot(1, 1, type = "n", xlim = c(0.5, 3.5), ylim = c(0, 50),
     main = "RELATIVE RANK BY CONTEXT:\nSame Rank, Different Meaning",
     xlab = "Relative Rank Position", ylab = "Exploration Rate (%)",
     cex.main = 1.4, cex.lab = 1.2)
for(i in 1:3) {
  context <- c("solo", "duo", "trio")[i]
  subset_data <- context_relative_rank[context_relative_rank$Context == context, ]
  lines(subset_data$Relative_Rank, subset_data$Explore, col = context_colors[i], lwd = 3)
  points(subset_data$Relative_Rank, subset_data$Explore, col = context_colors[i], pch = 16, cex = 2)
}
legend("topright", c("Solo", "Duo", "Trio"), col = context_colors, lwd = 3, pch = 16, cex = 1.1)

# PLOT 6: Mathematical Model Structure - unchanged
plot(0, 0, type = "n", xlim = c(0, 10), ylim = c(0, 10), axes = FALSE,
     main = "MATHEMATICAL MODEL STRUCTURE", cex.main = 1.4)
text(5, 9, "EXPLORATION RATE = f(Social Context, Identity, Values)", cex = 1.3, font = 2)
text(2, 7.5, "BASELINE", cex = 1.1, font = 2)
text(2, 7, "32.1%", cex = 1.1, col = "blue")
text(5, 7.5, "SOCIAL EFFECTS", cex = 1.1, font = 2)
text(5, 7, "-10.5% × Partners", cex = 1.0, col = "red")
text(5, 6.5, "+7.5% × Rank", cex = 1.0, col = "green")
text(8, 7.5, "IDENTITY EFFECTS", cex = 1.1, font = 2)
text(8, 7, "+11.1% × Male", cex = 1.0, col = "purple")
text(8, 6.5, "±10.7% Individual", cex = 1.0, col = "orange")
text(5, 5, "VALUE INTEGRATION", cex = 1.1, font = 2)
text(5, 4.5, "+88.47 × Chosen Value", cex = 1.0, col = "darkgreen")
text(5, 4, "+0.22 × Exploit Value", cex = 1.0, col = "brown")
text(5, 3.5, "-0.12 × Explore Expect", cex = 1.0, col = "gray")

# PLOT 7: Corrected Effect Size Comparison
effect_sizes <- c(
  abs(diff(range(absolute_rank_effects$Explore))),  # Corrected absolute rank range
  abs(diff(range(relative_rank_effects$Explore))),  # Relative rank range
  abs(social_complexity_cost * 2),  # Solo to trio effect
  sex_strategy_bonus,  # Sex effect
  individual_variation_sd * 2  # Individual variation range
)
effect_names <- c("Absolute\nHierarchy", "Relative\nRank", "Social\nComplexity", "Sex\nDifference", "Individual\nVariation")

barplot(effect_sizes, names.arg = effect_names,
        main = "CORRECTED EFFECT SIZE COMPARISON:\nMagnitude of Different Factors",
        ylab = "Effect Size (% points)", 
        col = c("red", "blue", "orange", "green", "purple"),
        ylim = c(0, max(effect_sizes) * 1.2), cex.main = 1.4, cex.lab = 1.2)
text(c(0.7, 1.9, 3.1, 4.3, 5.5), effect_sizes + 1, 
     paste0(round(effect_sizes, 1), "%"), cex = 1.1, font = 2)

# PLOT 8: Model Components - unchanged
components <- c("Baseline", "Social Partners", "Hierarchy", "Sex", "Individual", "Values")
coefficients <- c(baseline_exploration, social_complexity_cost, hierarchy_advantage, 
                 sex_strategy_bonus, individual_variation_sd, subjective_value_coeff/10)
colors <- c("gray", "red", "gold", "blue", "purple", "green")

barplot(abs(coefficients), names.arg = components,
        main = "MODEL COMPONENTS:\nRelative Importance",
        ylab = "Coefficient Magnitude", 
        col = colors, las = 2, cex.main = 1.4, cex.lab = 1.2)
text(c(0.7, 1.9, 3.1, 4.3, 5.5, 6.7), abs(coefficients) + 2, 
     round(coefficients, 1), cex = 1.0, font = 2)

# PLOT 9: Corrected Rank Interpretation Guide
plot(0, 0, type = "n", xlim = c(0, 10), ylim = c(0, 10), axes = FALSE,
     main = "CORRECTED RANK INTERPRETATION GUIDE", cex.main = 1.4)

# Absolute rank explanation
text(2.5, 8.5, "ABSOLUTE RANK", cex = 1.3, font = 2, col = "red")
text(2.5, 8, "Hierarchy levels (two groups of 1-3)", cex = 1.1)
text(2.5, 7.5, "1=Dominant (FRAN, CHOCOLAT)", cex = 1.0)
text(2.5, 7, "2=Intermediate (DALI, ICE)", cex = 1.0)
text(2.5, 6.5, "3=Subordinate (EBI, ANEMONE)", cex = 1.0)
text(2.5, 6, "Fixed hierarchy identity", cex = 1.1, font = 2)
text(2.5, 5.5, paste("Effect size:", round(abs(diff(range(absolute_rank_effects$Explore))), 1), "%"), cex = 1.0, col = "red")

# Relative rank explanation
text(7.5, 8.5, "RELATIVE RANK", cex = 1.3, font = 2, col = "blue")
text(7.5, 8, "Context-dependent position", cex = 1.1)
text(7.5, 7.5, "1=Highest in current context", cex = 1.0)
text(7.5, 7, "2=Middle, 3=Lowest", cex = 1.0)
text(7.5, 6.5, "Changes with social context", cex = 1.0)
text(7.5, 6, "Situational identity", cex = 1.1, font = 2)
text(7.5, 5.5, paste("Effect size:", round(abs(diff(range(relative_rank_effects$Explore))), 1), "%"), cex = 1.0, col = "blue")

# Key insight
text(5, 4, "KEY INSIGHT:", cex = 1.3, font = 2)
text(5, 3.5, "Both show clear rank effects", cex = 1.2)
text(5, 3, "Absolute: Fixed hierarchy matters", cex = 1.2)
text(5, 2.5, "Relative: Context position matters", cex = 1.2)
text(5, 2, paste("Absolute R² =", round(r_squared_abs, 3), "vs Relative R² =", round(r_squared_rel, 3)), cex = 1.1)

dev.off()

# Create corrected summary comparison table
corrected_rank_comparison <- data.frame(
  Measure = c("Effect Size (% range)", "R-squared", "Behavioral Prediction", "Conceptual Meaning", "Structure"),
  Absolute_Rank = c(
    paste0(round(abs(diff(range(absolute_rank_effects$Explore))), 1), "%"),
    round(r_squared_abs, 3),
    "Fixed hierarchy identity",
    "Dominant > Intermediate > Subordinate",
    "Two groups of 1-3"
  ),
  Relative_Rank = c(
    paste0(round(abs(diff(range(relative_rank_effects$Explore))), 1), "%"),
    round(r_squared_rel, 3),
    "Situational identity", 
    "Context-dependent position",
    "Single ranking 1-3"
  ),
  Comparison = c(
    ifelse(abs(diff(range(absolute_rank_effects$Explore))) > abs(diff(range(relative_rank_effects$Explore))), "Absolute larger", "Relative larger"),
    ifelse(r_squared_abs > r_squared_rel, "Absolute better", "Relative better"),
    "Different concepts",
    "Different concepts", 
    "Different structures"
  )
)

write.csv(corrected_rank_comparison, "corrected_rank_comparison.csv", row.names = FALSE)

# Individual monkey breakdown
monkey_breakdown <- data.frame()
for(monkey in unique(model_data$monkey)) {
  monkey_data <- model_data[model_data$monkey == monkey, ]
  monkey_breakdown <- rbind(monkey_breakdown, data.frame(
    Monkey = monkey,
    Sex = unique(monkey_data$sex),
    Hierarchy = unique(monkey_data$hierarchy),
    Absolute_Rank = unique(monkey_data$absolute_rank_hierarchy),
    Within_Group_Rank = unique(monkey_data$absolute_rank_within_group),
    Avg_Relative_Rank = round(mean(monkey_data$RELATIVE_RANK), 2),
    Exploration_Rate = round(safe_rate(monkey_data, "explore"), 1),
    N_Trials = nrow(monkey_data)
  ))
}

write.csv(monkey_breakdown, "monkey_rank_breakdown.csv", row.names = FALSE)

cat("=== CORRECTED RANK COMPARISON COMPLETE ===\n")
cat("Generated files:\n")
cat("✓ CORRECTED_RANK_COMPARISON_AND_MODEL.pdf (9 plots)\n")
cat("✓ corrected_rank_comparison.csv\n")
cat("✓ monkey_rank_breakdown.csv\n")
cat("\nCORRECTED FINDINGS:\n")
cat("ABSOLUTE RANK (Hierarchy Levels 1-3):\n")
cat("- Dominant (FRAN, CHOCOLAT):", round(absolute_rank_effects$Explore[1], 1), "%\n")
cat("- Intermediate (DALI, ICE):", round(absolute_rank_effects$Explore[2], 1), "%\n") 
cat("- Subordinate (EBI, ANEMONE):", round(absolute_rank_effects$Explore[3], 1), "%\n")
cat("- Effect size:", round(abs(diff(range(absolute_rank_effects$Explore))), 1), "%\n")
cat("- R²:", round(r_squared_abs, 3), "\n")
cat("\nRELATIVE RANK (Context-dependent 1-3):\n")
cat("- Rank 1:", round(relative_rank_effects$Explore[1], 1), "%\n")
cat("- Rank 2:", round(relative_rank_effects$Explore[2], 1), "%\n")
cat("- Rank 3:", round(relative_rank_effects$Explore[3], 1), "%\n")
cat("- Effect size:", round(abs(diff(range(relative_rank_effects$Explore))), 1), "%\n")
cat("- R²:", round(r_squared_rel, 3), "\n") 