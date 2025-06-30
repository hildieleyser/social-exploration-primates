# =============================================================================
# Simple Statistical Analysis: Social Frames of Reference (Base R)
# =============================================================================
# This script performs the core statistical analysis using only base R
# and built-in packages to avoid dependency issues.

# Load required libraries (built-in packages only)
library(stats)
library(graphics)
library(grDevices)

# Set up plotting parameters
png_width <- 800
png_height <- 600
png_res <- 150

# =============================================================================
# 1. DATA LOADING AND PREPARATION
# =============================================================================

cat("Loading and preparing data...\n")

# Load dataset
data_raw <- read.csv("data/Explore Exploit Dataset.csv", stringsAsFactors = FALSE)

# Basic data exploration
cat("Raw data dimensions:", dim(data_raw), "\n")
cat("Column names:", names(data_raw), "\n")

# Data cleaning and preparation
data_clean <- data_raw[data_raw$TRIAL_TYPE == "OIT_RE", ]

# Clean outcome variable
outcome_clean <- character(nrow(data_clean))
outcome_clean[grepl("explore", tolower(data_clean$OUTCOME))] <- "explore"
outcome_clean[grepl("exploit", tolower(data_clean$OUTCOME))] <- "exploit"
outcome_clean[grepl("none|stop", tolower(data_clean$OUTCOME))] <- "none"

# Add cleaned variables to dataset
data_clean$outcome_clean <- outcome_clean
data_clean$social_complexity <- factor(data_clean$CONDITION, levels = c("solo", "duo", "trio"))
data_clean$monkey_id <- factor(data_clean$monkey)

# Remove missing data
data_clean <- data_clean[!is.na(data_clean$outcome_clean), ]
data_clean <- data_clean[complete.cases(data_clean[c("RELATIVE_RANK", "SUBJECTIVE_CHOSEN_VALUE")]), ]

cat("Data preparation complete.\n")
cat("Final sample size:", nrow(data_clean), "trials\n")
cat("Subjects:", length(unique(data_clean$monkey_id)), "\n")

# =============================================================================
# 2. DESCRIPTIVE STATISTICS
# =============================================================================

cat("\nGenerating descriptive statistics...\n")

# Overall outcome distribution
outcome_table <- table(data_clean$outcome_clean)
outcome_prop <- prop.table(outcome_table)

cat("Overall Outcome Distribution:\n")
print(outcome_table)
print(round(outcome_prop, 3))

# Outcome by social complexity
complexity_table <- table(data_clean$social_complexity, data_clean$outcome_clean)
complexity_prop <- prop.table(complexity_table, margin = 1)

cat("\nOutcome by Social Complexity:\n")
print(complexity_table)
print(round(complexity_prop, 3))

# =============================================================================
# 3. STATISTICAL TESTS
# =============================================================================

cat("\nPerforming statistical tests...\n")

# Chi-square test for independence
chi_test <- chisq.test(complexity_table)
cat("Chi-square test for independence:\n")
cat("X-squared =", chi_test$statistic, ", df =", chi_test$parameter, ", p-value =", chi_test$p.value, "\n")

# Effect size (Cramér's V)
n <- sum(complexity_table)
cramers_v <- sqrt(chi_test$statistic / (n * (min(nrow(complexity_table), ncol(complexity_table)) - 1)))
cat("Cramér's V =", cramers_v, "\n")

# =============================================================================
# 4. FIGURE 1: MAIN EFFECTS
# =============================================================================

cat("\nGenerating Figure 1: Main Effects...\n")

# Calculate exploration rates by condition
conditions <- c("solo", "duo", "trio")
explore_rates <- numeric(3)
explore_se <- numeric(3)

for(i in 1:3) {
  condition_data <- data_clean[data_clean$social_complexity == conditions[i], ]
  n_total <- nrow(condition_data)
  n_explore <- sum(condition_data$outcome_clean == "explore")
  explore_rates[i] <- n_explore / n_total
  explore_se[i] <- sqrt(explore_rates[i] * (1 - explore_rates[i]) / n_total)
}

# Create figure
png("results/figures/figure1_main_effects.png", width = png_width, height = png_height, res = png_res)
par(mar = c(5, 5, 4, 2))

# Create bar plot
bp <- barplot(explore_rates, 
              names.arg = c("Solo", "Duo", "Trio"),
              ylim = c(0, max(explore_rates + 1.96 * explore_se) * 1.1),
              ylab = "Exploration Rate",
              xlab = "Social Context",
              main = "Social Complexity Effects on Exploration",
              col = c("#440154", "#31688E", "#FDE725"),
              border = "black")

# Add error bars
arrows(bp, explore_rates - 1.96 * explore_se, 
       bp, explore_rates + 1.96 * explore_se, 
       length = 0.05, angle = 90, code = 3, lwd = 2)

# Add percentage labels
text(bp, explore_rates + 1.96 * explore_se + 0.01, 
     paste0(round(explore_rates * 100, 1), "%"), 
     pos = 3, font = 2)

dev.off()

# =============================================================================
# 5. FIGURE 2: INDIVIDUAL DIFFERENCES
# =============================================================================

cat("Generating Figure 2: Individual Differences...\n")

# Calculate individual exploration rates
individuals <- unique(data_clean$monkey_id)
ind_rates <- numeric(length(individuals))
ind_se <- numeric(length(individuals))

for(i in 1:length(individuals)) {
  ind_data <- data_clean[data_clean$monkey_id == individuals[i], ]
  n_total <- nrow(ind_data)
  n_explore <- sum(ind_data$outcome_clean == "explore")
  ind_rates[i] <- n_explore / n_total
  ind_se[i] <- sqrt(ind_rates[i] * (1 - ind_rates[i]) / n_total)
}

# Sort by exploration rate
order_idx <- order(ind_rates, decreasing = TRUE)
individuals_sorted <- individuals[order_idx]
ind_rates_sorted <- ind_rates[order_idx]
ind_se_sorted <- ind_se[order_idx]

# Create figure
png("results/figures/figure2_individual_differences.png", width = png_width, height = png_height, res = png_res)
par(mar = c(5, 8, 4, 2))

# Create horizontal bar plot
bp <- barplot(ind_rates_sorted, 
              names.arg = individuals_sorted,
              horiz = TRUE,
              xlim = c(0, max(ind_rates_sorted + 1.96 * ind_se_sorted) * 1.1),
              xlab = "Exploration Rate",
              ylab = "Individual",
              main = "Individual Differences in Exploration",
              col = rainbow(length(individuals)),
              border = "black")

# Add error bars
arrows(ind_rates_sorted - 1.96 * ind_se_sorted, bp,
       ind_rates_sorted + 1.96 * ind_se_sorted, bp,
       length = 0.05, angle = 90, code = 3, lwd = 2)

# Add percentage labels
text(ind_rates_sorted + 1.96 * ind_se_sorted + 0.02, bp,
     paste0(round(ind_rates_sorted * 100, 1), "%"),
     pos = 4, font = 2)

dev.off()

# =============================================================================
# 6. FIGURE 3: INTERACTION EFFECTS
# =============================================================================

cat("Generating Figure 3: Interaction Effects...\n")

# Create rank categories
data_clean$rank_category <- cut(data_clean$RELATIVE_RANK, 
                               breaks = c(0, 1.5, 2.5, 3), 
                               labels = c("Dominant", "Middle", "Subordinate"))

# Calculate exploration by social complexity and rank
conditions <- c("solo", "duo", "trio")
ranks <- c("Dominant", "Middle", "Subordinate")
interaction_rates <- matrix(NA, nrow = 3, ncol = 3)
interaction_se <- matrix(NA, nrow = 3, ncol = 3)

for(i in 1:3) {
  for(j in 1:3) {
    subset_data <- data_clean[data_clean$social_complexity == conditions[i] & 
                             data_clean$rank_category == ranks[j] & 
                             !is.na(data_clean$rank_category), ]
    if(nrow(subset_data) > 0) {
      n_total <- nrow(subset_data)
      n_explore <- sum(subset_data$outcome_clean == "explore")
      interaction_rates[i, j] <- n_explore / n_total
      interaction_se[i, j] <- sqrt(interaction_rates[i, j] * (1 - interaction_rates[i, j]) / n_total)
    }
  }
}

# Create figure
png("results/figures/figure3_interaction_effects.png", width = png_width, height = png_height, res = png_res)
par(mar = c(5, 5, 4, 2))

# Create line plot
plot(1:3, interaction_rates[, 1], 
     type = "b", pch = 16, lwd = 2, col = "#440154",
     xlim = c(0.8, 3.2), ylim = c(0, max(interaction_rates, na.rm = TRUE) * 1.2),
     xlab = "Social Context", ylab = "Exploration Rate",
     main = "Interaction: Social Complexity × Dominance Rank",
     xaxt = "n")

lines(1:3, interaction_rates[, 2], type = "b", pch = 17, lwd = 2, col = "#31688E")
lines(1:3, interaction_rates[, 3], type = "b", pch = 18, lwd = 2, col = "#FDE725")

# Add axis labels
axis(1, at = 1:3, labels = c("Solo", "Duo", "Trio"))

# Add legend
legend("topright", legend = ranks, 
       col = c("#440154", "#31688E", "#FDE725"), 
       pch = 16:18, lwd = 2, bty = "n")

dev.off()

# =============================================================================
# 7. FIGURE 4: OUTCOME DISTRIBUTION
# =============================================================================

cat("Generating Figure 4: Outcome Distribution...\n")

# Create stacked bar plot by condition
outcome_counts <- table(data_clean$social_complexity, data_clean$outcome_clean)
outcome_props <- prop.table(outcome_counts, margin = 1)

# Create figure
png("results/figures/figure4_outcome_distribution.png", width = png_width, height = png_height, res = png_res)
par(mar = c(5, 5, 4, 2))

# Create stacked bar plot
bp <- barplot(t(outcome_props), 
              names.arg = c("Solo", "Duo", "Trio"),
              ylab = "Proportion of Choices",
              xlab = "Social Context",
              main = "Choice Distribution by Social Context",
              col = c("#E31A1C", "#33A02C", "#1F78B4"),
              border = "black",
              legend.text = c("Explore", "Exploit", "None"),
              args.legend = list(x = "topright", bty = "n"))

dev.off()

# =============================================================================
# 8. SIMPLE MULTINOMIAL ANALYSIS
# =============================================================================

cat("Performing multinomial logistic regression...\n")

# Install and load nnet if available
if("nnet" %in% rownames(installed.packages())) {
  library(nnet)
  
  # Fit multinomial model
  model <- multinom(outcome_clean ~ social_complexity + monkey_id, 
                   data = data_clean, trace = FALSE)
  
  # Model summary
  cat("Multinomial logistic regression summary:\n")
  print(summary(model))
  
  # Calculate AIC
  cat("Model AIC:", AIC(model), "\n")
  
} else {
  cat("nnet package not available - skipping multinomial regression\n")
}

# =============================================================================
# 9. SAVE SUMMARY STATISTICS
# =============================================================================

cat("Saving summary statistics...\n")

# Create summary data frame
summary_stats <- data.frame(
  Condition = c("Solo", "Duo", "Trio"),
  N_Total = as.numeric(table(data_clean$social_complexity)),
  N_Explore = as.numeric(table(data_clean$social_complexity, data_clean$outcome_clean)[, "explore"]),
  Exploration_Rate = explore_rates,
  Standard_Error = explore_se,
  CI_Lower = explore_rates - 1.96 * explore_se,
  CI_Upper = explore_rates + 1.96 * explore_se
)

# Save to CSV
write.csv(summary_stats, "results/summary_statistics.csv", row.names = FALSE)

# Individual statistics
individual_stats <- data.frame(
  Individual = individuals,
  N_Total = as.numeric(table(data_clean$monkey_id)),
  N_Explore = as.numeric(table(data_clean$monkey_id, data_clean$outcome_clean)[, "explore"]),
  Exploration_Rate = ind_rates,
  Standard_Error = ind_se
)

write.csv(individual_stats, "results/individual_statistics.csv", row.names = FALSE)

# =============================================================================
# 10. ANALYSIS SUMMARY
# =============================================================================

cat("\n=============================================================================\n")
cat("ANALYSIS COMPLETE!\n")
cat("=============================================================================\n")
cat("Generated figures:\n")
cat("- Figure 1: Main Effects (figure1_main_effects.png)\n")
cat("- Figure 2: Individual Differences (figure2_individual_differences.png)\n") 
cat("- Figure 3: Interaction Effects (figure3_interaction_effects.png)\n")
cat("- Figure 4: Outcome Distribution (figure4_outcome_distribution.png)\n")
cat("\nSummary statistics:\n")
cat("- Chi-square =", round(chi_test$statistic, 2), ", p =", format(chi_test$p.value, scientific = TRUE), "\n")
cat("- Cramér's V =", round(cramers_v, 3), "\n")
cat("- Solo exploration:", round(explore_rates[1] * 100, 1), "%\n")
cat("- Duo exploration:", round(explore_rates[2] * 100, 1), "%\n")
cat("- Trio exploration:", round(explore_rates[3] * 100, 1), "%\n")
cat("\nResults saved to results/ directory\n")
cat("=============================================================================\n") 