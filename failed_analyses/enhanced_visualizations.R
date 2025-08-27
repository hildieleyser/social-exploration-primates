# =============================================================================
# Enhanced Data Visualizations for Explore/Exploit Analysis
# =============================================================================

# Set CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org/"))

cat("Creating enhanced visualizations...\n")

# =============================================================================
# 1. LOAD AND PREPARE DATA
# =============================================================================

# Load results if available, otherwise run analysis
if(file.exists("simple_analysis_results.RData")) {
  load("simple_analysis_results.RData")
  cat("Loaded existing results\n")
} else {
  cat("Running analysis first...\n")
  source("simple_analysis.R")
}

# Ensure we have the clean data
if(!exists("clean_data")) {
  data <- read.csv("Explore Exploit Dataset.csv", stringsAsFactors = FALSE)
  
  # Recreate clean data
  data$decision_type <- ifelse(
    grepl("explore", tolower(data$OUTCOME)), "explore",
    ifelse(grepl("exploit", tolower(data$OUTCOME)), "exploit", "other")
  )
  
  data$social_context <- ifelse(
    data$CONDITION == "solo", "solo",
    ifelse(data$CONDITION == "duo", "duo",
      ifelse(data$CONDITION == "trio", "trio", "other"))
  )
  
  clean_data <- data[
    !is.na(data$monkey) & 
    !is.na(data$CONDITION) & 
    !is.na(data$OUTCOME) & 
    !is.na(data$expected_explore) &
    data$decision_type %in% c("explore", "exploit") &
    data$social_context != "other", 
  ]
  
  clean_data$explore_binary <- as.numeric(clean_data$decision_type == "explore")
  clean_data$expected_explore_z <- scale(clean_data$expected_explore)[,1]
}

# =============================================================================
# 2. CREATE MULTIPLE VISUALIZATIONS
# =============================================================================

# Set up for multiple plots
par(mfrow = c(1, 1))  # Reset to single plot

# Color schemes
context_colors <- c("lightblue", "lightgreen", "lightcoral")
monkey_colors <- rainbow(length(unique(clean_data$monkey)))

cat("Creating Plot 1: Basic exploration rates...\n")

# =============================================================================
# PLOT 1: Enhanced Exploration Rates by Social Context
# =============================================================================

png("plot1_exploration_by_context.png", width = 800, height = 600)

# Calculate summary stats
summary_stats <- aggregate(
  list(explore_rate = clean_data$explore_binary),
  by = list(social_context = clean_data$social_context),
  FUN = function(x) c(mean = mean(x), 
                     se = sd(x)/sqrt(length(x)),
                     n = length(x))
)

# Extract values
means <- summary_stats$explore_rate[,1]
ses <- summary_stats$explore_rate[,2]
contexts <- summary_stats$social_context

# Create barplot with error bars
barplot_result <- barplot(
  means,
  names.arg = contexts,
  main = "Exploration Rate by Social Context\n(with Standard Errors)",
  ylab = "Proportion of Explore Decisions",
  xlab = "Social Context",
  col = context_colors,
  ylim = c(0, max(means + ses) * 1.2),
  border = "black",
  lwd = 2
)

# Add error bars
arrows(barplot_result, means - ses, barplot_result, means + ses,
       angle = 90, code = 3, length = 0.1, lwd = 2)

# Add sample sizes
text(barplot_result, means + ses + 0.05, 
     paste("n =", summary_stats$explore_rate[,3]), 
     cex = 0.9, font = 2)

# Add percentage labels on bars
text(barplot_result, means/2, 
     paste0(round(means * 100, 1), "%"), 
     cex = 1.2, font = 2, col = "darkblue")

dev.off()

cat("Creating Plot 2: Individual monkey patterns...\n")

# =============================================================================
# PLOT 2: Individual Monkey Patterns
# =============================================================================

png("plot2_individual_monkeys.png", width = 1000, height = 600)

# Calculate individual monkey stats
monkey_stats <- aggregate(
  list(explore_rate = clean_data$explore_binary),
  by = list(monkey = clean_data$monkey, social_context = clean_data$social_context),
  FUN = mean
)

# Create matrix for plotting
monkeys <- unique(monkey_stats$monkey)
contexts <- c("duo", "solo", "trio")
plot_matrix <- matrix(NA, nrow = length(monkeys), ncol = length(contexts))
rownames(plot_matrix) <- monkeys
colnames(plot_matrix) <- contexts

for(i in 1:nrow(monkey_stats)) {
  monkey <- monkey_stats$monkey[i]
  context <- monkey_stats$social_context[i]
  rate <- monkey_stats$explore_rate[i]
  plot_matrix[monkey, context] <- rate
}

# Create grouped barplot
barplot(
  t(plot_matrix),
  beside = TRUE,
  main = "Individual Monkey Exploration Rates by Social Context",
  ylab = "Proportion of Explore Decisions",
  xlab = "Monkey",
  col = context_colors,
  legend = contexts,
  ylim = c(0, 1),
  border = "black",
  args.legend = list(x = "topright", bty = "n")
)

# Add grid
grid(nx = NULL, ny = NULL, col = "gray", lty = "dotted")

dev.off()

cat("Creating Plot 3: Expectation vs exploration relationship...\n")

# =============================================================================
# PLOT 3: Expectation vs Exploration Scatter Plot
# =============================================================================

png("plot3_expectation_exploration.png", width = 800, height = 600)

# Create scatter plot with different symbols for contexts
plot(clean_data$expected_explore, clean_data$explore_binary,
     main = "Relationship between Expectation and Exploration",
     xlab = "Expected Explore Probability",
     ylab = "Exploration Decision (0 = Exploit, 1 = Explore)",
     pch = 16, col = "lightgray", cex = 0.8)

# Add context-specific points
contexts <- c("duo", "solo", "trio")
symbols <- c(16, 17, 18)  # circle, triangle, diamond

for(i in 1:length(contexts)) {
  context_data <- clean_data[clean_data$social_context == contexts[i], ]
  points(context_data$expected_explore, context_data$explore_binary,
         pch = symbols[i], col = context_colors[i], cex = 1.2)
}

# Add smooth trend lines for each context
for(i in 1:length(contexts)) {
  context_data <- clean_data[clean_data$social_context == contexts[i], ]
  if(nrow(context_data) > 10) {
    smooth_fit <- loess(explore_binary ~ expected_explore, data = context_data)
    x_seq <- seq(min(context_data$expected_explore), max(context_data$expected_explore), length.out = 50)
    y_pred <- predict(smooth_fit, newdata = data.frame(expected_explore = x_seq))
    lines(x_seq, y_pred, col = context_colors[i], lwd = 3)
  }
}

# Add legend
legend("topleft", 
       legend = contexts,
       pch = symbols,
       col = context_colors,
       bty = "n",
       cex = 1.2)

# Add grid
grid(col = "gray", lty = "dotted")

dev.off()

cat("Creating Plot 4: Binned expectation analysis...\n")

# =============================================================================
# PLOT 4: Binned Expectation Analysis
# =============================================================================

png("plot4_expectation_bins.png", width = 800, height = 600)

# Create expectation bins
clean_data$expect_bin <- cut(clean_data$expected_explore, 
                            breaks = 5, 
                            labels = c("Very Low", "Low", "Medium", "High", "Very High"))

# Calculate stats by bin and context
bin_stats <- aggregate(
  list(explore_rate = clean_data$explore_binary),
  by = list(expect_bin = clean_data$expect_bin, 
           social_context = clean_data$social_context),
  FUN = function(x) c(mean = mean(x), n = length(x))
)

# Filter out bins with few observations
bin_stats <- bin_stats[bin_stats$explore_rate[,2] >= 5, ]  # At least 5 observations

# Create interaction plot
interaction.plot(
  x.factor = bin_stats$expect_bin,
  trace.factor = bin_stats$social_context,
  response = bin_stats$explore_rate[,1],
  type = "b",
  main = "Exploration Rate by Expectation Level and Social Context",
  xlab = "Expectation Level",
  ylab = "Exploration Rate",
  col = context_colors[1:length(unique(bin_stats$social_context))],
  lwd = 3,
  pch = 16,
  cex = 1.5,
  legend = TRUE
)

# Add grid
grid(col = "gray", lty = "dotted")

dev.off()

cat("Creating Plot 5: Model predictions...\n")

# =============================================================================
# PLOT 5: Model Predictions and Residuals
# =============================================================================

png("plot5_model_diagnostics.png", width = 1200, height = 800)

# Set up 2x2 plot layout
par(mfrow = c(2, 2))

# Load model if available
if(exists("results") && "model2" %in% names(results)) {
  model <- results$model2
} else {
  # Refit model
  model <- glm(explore_binary ~ social_context * expected_explore_z,
               data = clean_data, family = binomial())
}

# Plot 1: Residuals vs Fitted
plot(model, which = 1, main = "Residuals vs Fitted Values")

# Plot 2: Q-Q plot
plot(model, which = 2, main = "Normal Q-Q Plot")

# Plot 3: Predicted probabilities by context
predicted_probs <- predict(model, type = "response")
boxplot(predicted_probs ~ clean_data$social_context,
        main = "Predicted Exploration Probabilities",
        xlab = "Social Context",
        ylab = "Predicted Probability",
        col = context_colors)

# Plot 4: Residuals vs Expectation
plot(clean_data$expected_explore, residuals(model, type = "pearson"),
     main = "Residuals vs Expectation",
     xlab = "Expected Explore Probability",
     ylab = "Pearson Residuals",
     pch = 16, col = "darkblue")
abline(h = 0, col = "red", lwd = 2)

# Reset plot layout
par(mfrow = c(1, 1))

dev.off()

cat("Creating Plot 6: Data distributions...\n")

# =============================================================================
# PLOT 6: Data Distributions
# =============================================================================

png("plot6_data_distributions.png", width = 1200, height = 800)

# Set up 2x2 plot layout
par(mfrow = c(2, 2))

# Plot 1: Histogram of expectation by context
hist(clean_data$expected_explore[clean_data$social_context == "solo"],
     main = "Distribution of Expectation by Context",
     xlab = "Expected Explore Probability",
     col = "lightblue", alpha = 0.7, breaks = 20)
hist(clean_data$expected_explore[clean_data$social_context == "duo"],
     col = "lightgreen", alpha = 0.7, breaks = 20, add = TRUE)
hist(clean_data$expected_explore[clean_data$social_context == "trio"],
     col = "lightcoral", alpha = 0.7, breaks = 20, add = TRUE)
legend("topright", legend = c("Solo", "Duo", "Trio"), 
       fill = c("lightblue", "lightgreen", "lightcoral"), bty = "n")

# Plot 2: Boxplot of expectation by context
boxplot(expected_explore ~ social_context, data = clean_data,
        main = "Expectation Distribution by Context",
        xlab = "Social Context",
        ylab = "Expected Explore Probability",
        col = context_colors)

# Plot 3: Trial distribution by monkey
barplot(table(clean_data$monkey),
        main = "Number of Trials per Monkey",
        xlab = "Monkey",
        ylab = "Number of Trials",
        col = monkey_colors,
        las = 2)

# Plot 4: Decisions by trial type
if("TRIAL_TYPE" %in% names(clean_data)) {
  trial_table <- table(clean_data$TRIAL_TYPE, clean_data$decision_type)
  barplot(trial_table,
          main = "Decisions by Trial Type",
          xlab = "Decision Type",
          ylab = "Count",
          col = c("lightblue", "lightcoral"),
          legend = rownames(trial_table),
          beside = TRUE)
} else {
  plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
  text(1, 1, "Trial type data not available", cex = 1.5)
}

# Reset plot layout
par(mfrow = c(1, 1))

dev.off()

cat("Creating Plot 7: Time series analysis...\n")

# =============================================================================
# PLOT 7: Time Series and Block Effects
# =============================================================================

png("plot7_temporal_patterns.png", width = 1200, height = 600)

# Set up 1x2 plot layout
par(mfrow = c(1, 2))

# Plot 1: Exploration rate by trial number
if("TRIAL_NUM" %in% names(clean_data)) {
  # Calculate running average
  trial_stats <- aggregate(
    list(explore_rate = clean_data$explore_binary),
    by = list(trial_num = clean_data$TRIAL_NUM),
    FUN = mean
  )
  
  plot(trial_stats$trial_num, trial_stats$explore_rate,
       type = "b",
       main = "Exploration Rate by Trial Number",
       xlab = "Trial Number",
       ylab = "Exploration Rate",
       pch = 16, col = "darkblue", lwd = 2)
  
  # Add smooth trend line
  if(nrow(trial_stats) > 3) {
    smooth_fit <- loess(explore_rate ~ trial_num, data = trial_stats)
    lines(trial_stats$trial_num, fitted(smooth_fit), col = "red", lwd = 3)
  }
}

# Plot 2: Block effects
if("BLOCK_No" %in% names(clean_data)) {
  block_stats <- aggregate(
    list(explore_rate = clean_data$explore_binary),
    by = list(block = clean_data$BLOCK_No),
    FUN = function(x) c(mean = mean(x), n = length(x))
  )
  
  # Filter blocks with enough data
  block_stats <- block_stats[block_stats$explore_rate[,2] >= 5, ]
  
  plot(as.numeric(gsub("BLOCK_", "", block_stats$block)), 
       block_stats$explore_rate[,1],
       type = "b",
       main = "Exploration Rate by Block",
       xlab = "Block Number",
       ylab = "Exploration Rate",
       pch = 16, col = "darkgreen", lwd = 2)
  
  # Add confidence intervals
  y_vals <- block_stats$explore_rate[,1]
  n_vals <- block_stats$explore_rate[,2]
  se_vals <- sqrt(y_vals * (1 - y_vals) / n_vals)
  
  arrows(as.numeric(gsub("BLOCK_", "", block_stats$block)), 
         y_vals - 1.96 * se_vals,
         as.numeric(gsub("BLOCK_", "", block_stats$block)), 
         y_vals + 1.96 * se_vals,
         angle = 90, code = 3, length = 0.05, col = "darkgreen")
}

# Reset plot layout
par(mfrow = c(1, 1))

dev.off()

cat("Creating Plot 8: Summary dashboard...\n")

# =============================================================================
# PLOT 8: Summary Dashboard
# =============================================================================

png("plot8_summary_dashboard.png", width = 1400, height = 1000)

# Set up 3x2 plot layout
par(mfrow = c(3, 2), mar = c(4, 4, 3, 2))

# Panel 1: Overall rates
overall_stats <- aggregate(
  list(explore_rate = clean_data$explore_binary),
  by = list(social_context = clean_data$social_context),
  FUN = mean
)

barplot(overall_stats$explore_rate,
        names.arg = overall_stats$social_context,
        main = "Overall Exploration Rates",
        col = context_colors,
        ylim = c(0, 0.7))

# Panel 2: Effect sizes (odds ratios)
if(exists("model")) {
  coefs <- coef(model)
  ors <- exp(coefs[-1])  # Exclude intercept
  
  barplot(ors,
          main = "Effect Sizes (Odds Ratios)",
          las = 2,
          col = "steelblue",
          ylab = "Odds Ratio")
  abline(h = 1, col = "red", lwd = 2, lty = 2)
}

# Panel 3: Sample sizes
sample_sizes <- table(clean_data$social_context)
pie(sample_sizes,
    main = "Sample Sizes by Context",
    col = context_colors,
    labels = paste(names(sample_sizes), "\n(n=", sample_sizes, ")", sep=""))

# Panel 4: Monkey contributions
monkey_contrib <- table(clean_data$monkey)
barplot(monkey_contrib,
        main = "Trials per Monkey",
        las = 2,
        col = monkey_colors,
        cex.names = 0.8)

# Panel 5: Expectation summary
boxplot(expected_explore ~ social_context, data = clean_data,
        main = "Expectation by Context",
        col = context_colors,
        cex.axis = 0.8)

# Panel 6: Key statistics text
plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", 
     xlim = c(0, 1), ylim = c(0, 1))

# Add text summary
if(exists("model")) {
  model_summary <- summary(model)
  text(0.1, 0.9, "KEY FINDINGS:", font = 2, cex = 1.2)
  text(0.1, 0.8, paste("N =", nrow(clean_data), "trials"), cex = 1)
  text(0.1, 0.7, paste("Monkeys =", length(unique(clean_data$monkey))), cex = 1)
  text(0.1, 0.6, paste("AIC =", round(AIC(model), 1)), cex = 1)
  
  # Extract p-values
  pvals <- model_summary$coefficients[, "Pr(>|z|)"]
  text(0.1, 0.5, "Significant effects:", font = 2, cex = 1)
  sig_effects <- names(pvals)[pvals < 0.05]
  if(length(sig_effects) > 0) {
    for(i in 1:min(3, length(sig_effects))) {
      text(0.1, 0.4 - i*0.08, paste("•", sig_effects[i]), cex = 0.9)
    }
  }
}

# Reset plot parameters
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2))

dev.off()

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n=============================================================================\n")
cat("ENHANCED VISUALIZATIONS COMPLETE\n")
cat("=============================================================================\n")

plot_files <- c(
  "plot1_exploration_by_context.png",
  "plot2_individual_monkeys.png", 
  "plot3_expectation_exploration.png",
  "plot4_expectation_bins.png",
  "plot5_model_diagnostics.png",
  "plot6_data_distributions.png",
  "plot7_temporal_patterns.png",
  "plot8_summary_dashboard.png"
)

cat("Created", length(plot_files), "visualization files:\n")
for(i in 1:length(plot_files)) {
  cat(paste0("  ", i, ". ", plot_files[i], "\n"))
}

cat("\nPlot descriptions:\n")
cat("1. Basic exploration rates with error bars\n")
cat("2. Individual monkey patterns by context\n") 
cat("3. Scatter plot of expectation vs exploration\n")
cat("4. Binned expectation analysis\n")
cat("5. Model diagnostic plots\n")
cat("6. Data distribution summaries\n")
cat("7. Temporal patterns and block effects\n")
cat("8. Summary dashboard with key findings\n")

cat("\nTo view plots:\n")
cat("  • On Mac: open plot1_exploration_by_context.png\n")
cat("  • On Windows: start plot1_exploration_by_context.png\n")
cat("  • Or drag files to your image viewer\n")

cat("\nAll visualizations saved to current directory!\n") 