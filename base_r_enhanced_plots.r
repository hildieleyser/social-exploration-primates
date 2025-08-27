# Enhanced Analysis with Advanced Plotting - Base R Version
# Compatible with older R versions

print("Loading data...")

# 1. Load and prepare the data (using base R only)
data <- read.csv("Explore Exploit Dataset.csv", stringsAsFactors = FALSE)

# Remove missing values
data <- data[!is.na(data$OUTCOME) & !is.na(data$CONDITION) & !is.na(data$expected_explore), ]

# Create outcome categories
data$OUTCOME_CAT <- ifelse(data$OUTCOME == 0, "none",
                          ifelse(data$OUTCOME == 1, "explore", "exploit"))

# Convert to factors
data$CONDITION <- factor(data$CONDITION, levels = c("Solo", "Duo", "Trio"))
data$OUTCOME_CAT <- factor(data$OUTCOME_CAT, levels = c("explore", "exploit", "none"))
data$monkey <- factor(data$monkey)
data$BLOCK_No <- factor(data$BLOCK_No)

print(paste("Data loaded. Rows:", nrow(data)))
print("OUTCOME distribution:")
print(table(data$OUTCOME_CAT))
print("CONDITION distribution:")
print(table(data$CONDITION))

# 2. Fit multinomial logistic regression
if (!require(nnet, quietly = TRUE)) {
  install.packages("nnet")
  library(nnet)
}

print("Fitting multinomial logistic regression model...")

# Fit the model
fit <- multinom(OUTCOME_CAT ~ CONDITION * expected_explore + 
                as.numeric(monkey) + as.numeric(BLOCK_No), 
                data = data)

print("Model fitted successfully!")

# 3. Generate predictions for plotting
print("Generating predictions...")

# Create prediction grid
conditions <- levels(data$CONDITION)
expectation_range <- seq(min(data$expected_explore, na.rm = TRUE),
                        max(data$expected_explore, na.rm = TRUE),
                        length.out = 20)

# Initialize results data frame
all_preds <- data.frame()

for (cond in conditions) {
  for (exp_val in expectation_range) {
    # Create newdata for prediction
    newdata <- data.frame(
      CONDITION = factor(cond, levels = levels(data$CONDITION)),
      expected_explore = exp_val,
      monkey = median(as.numeric(data$monkey), na.rm = TRUE),
      BLOCK_No = median(as.numeric(data$BLOCK_No), na.rm = TRUE)
    )
    
    # Get predictions
    probs <- predict(fit, newdata = newdata, type = "probs")
    
    # Handle single prediction case
    if (is.vector(probs)) {
      prob_df <- data.frame(
        CONDITION = cond,
        expected_explore = exp_val,
        OUTCOME_CAT = names(probs),
        estimate = probs
      )
    } else {
      prob_df <- data.frame(
        CONDITION = cond,
        expected_explore = exp_val,
        OUTCOME_CAT = colnames(probs),
        estimate = as.vector(probs)
      )
    }
    
    all_preds <- rbind(all_preds, prob_df)
  }
}

# Add approximate confidence intervals
se_multiplier <- 1.96  # 95% CI
all_preds$conf.low <- pmax(0, all_preds$estimate - se_multiplier * sqrt(all_preds$estimate * (1 - all_preds$estimate) / nrow(data)))
all_preds$conf.high <- pmin(1, all_preds$estimate + se_multiplier * sqrt(all_preds$estimate * (1 - all_preds$estimate) / nrow(data)))

print("Creating enhanced plots...")

# Color scheme
colors <- c("Solo" = "red", "Duo" = "blue", "Trio" = "darkgreen")

# Plot 1: Line plots by outcome type (mimicking facet_wrap)
outcomes <- levels(data$OUTCOME_CAT)

# Set up plotting area for 3 subplots
par(mfrow = c(1, 3), mar = c(5, 4, 4, 2) + 0.1)

for (outcome in outcomes) {
  subset_data <- all_preds[all_preds$OUTCOME_CAT == outcome, ]
  
  # Plot setup
  plot(subset_data$expected_explore[subset_data$CONDITION == "Solo"], 
       subset_data$estimate[subset_data$CONDITION == "Solo"],
       type = "n",
       xlim = range(subset_data$expected_explore),
       ylim = c(0, max(subset_data$estimate) * 1.1),
       xlab = "Expectation (expected_explore)",
       ylab = "Predicted Probability",
       main = paste("Outcome:", outcome))
  
  # Add lines for each condition
  for (cond in conditions) {
    cond_data <- subset_data[subset_data$CONDITION == cond, ]
    lines(cond_data$expected_explore, cond_data$estimate, 
          col = colors[cond], lwd = 2)
  }
  
  # Add legend only to first plot
  if (outcome == outcomes[1]) {
    legend("topright", legend = names(colors), col = colors, lwd = 2, cex = 0.8)
  }
}

# Reset plotting parameters
par(mfrow = c(1, 1))

print("Plot 1 completed: Basic predicted probabilities by condition and expectation")

# Plot 2: Enhanced plot with confidence intervals (focusing on explore outcome)
print("Creating Plot 2: Enhanced plot with confidence intervals...")

# Focus on 'explore' outcome for detailed CI plot
explore_data <- all_preds[all_preds$OUTCOME_CAT == "explore", ]

# Create the plot
plot(range(explore_data$expected_explore), range(c(explore_data$conf.low, explore_data$conf.high)),
     type = "n",
     xlab = "Expectation (expected_explore)",
     ylab = "Predicted Probability",
     main = "Predicted Probability of Exploration (with 95% CI)")

# Add confidence ribbons and lines for each condition
for (cond in conditions) {
  cond_data <- explore_data[explore_data$CONDITION == cond, ]
  
  # Sort by expectation for proper polygon drawing
  cond_data <- cond_data[order(cond_data$expected_explore), ]
  
  # Create polygon for confidence interval
  x_vals <- c(cond_data$expected_explore, rev(cond_data$expected_explore))
  y_vals <- c(cond_data$conf.low, rev(cond_data$conf.high))
  
  # Add semi-transparent confidence interval
  polygon(x_vals, y_vals, col = adjustcolor(colors[cond], alpha.f = 0.3), border = NA)
  
  # Add line
  lines(cond_data$expected_explore, cond_data$estimate, 
        col = colors[cond], lwd = 3)
}

# Add legend
legend("topright", legend = names(colors), col = colors, lwd = 3)

print("Plot 2 completed: Enhanced plot with confidence intervals")

# Plot 3: Comparison at mean expectation level
print("Creating Plot 3: Comparison at mean expectation level...")

mean_expect <- mean(data$expected_explore, na.rm = TRUE)

# Get predictions at mean expectation
mean_preds <- data.frame()

for (cond in conditions) {
  newdata <- data.frame(
    CONDITION = factor(cond, levels = levels(data$CONDITION)),
    expected_explore = mean_expect,
    monkey = median(as.numeric(data$monkey), na.rm = TRUE),
    BLOCK_No = median(as.numeric(data$BLOCK_No), na.rm = TRUE)
  )
  
  probs <- predict(fit, newdata = newdata, type = "probs")
  
  if (is.vector(probs)) {
    prob_df <- data.frame(
      CONDITION = cond,
      OUTCOME_CAT = names(probs),
      estimate = probs
    )
  } else {
    prob_df <- data.frame(
      CONDITION = cond,
      OUTCOME_CAT = colnames(probs),
      estimate = as.vector(probs)
    )
  }
  
  mean_preds <- rbind(mean_preds, prob_df)
}

# Add confidence intervals
mean_preds$conf.low <- pmax(0, mean_preds$estimate - se_multiplier * sqrt(mean_preds$estimate * (1 - mean_preds$estimate) / nrow(data)))
mean_preds$conf.high <- pmin(1, mean_preds$estimate + se_multiplier * sqrt(mean_preds$estimate * (1 - mean_preds$estimate) / nrow(data)))

# Create grouped barplot-style visualization
outcome_colors <- c("explore" = "red", "exploit" = "blue", "none" = "gray")

# Set up the plot
par(mar = c(8, 5, 4, 2))

# Create positions for each group
n_cond <- length(conditions)
n_outcome <- length(outcomes)
bar_width <- 0.8 / n_outcome
x_positions <- matrix(nrow = n_cond, ncol = n_outcome)

for (i in 1:n_cond) {
  center <- i
  start <- center - (n_outcome - 1) * bar_width / 2
  x_positions[i, ] <- start + (0:(n_outcome-1)) * bar_width
}

# Initialize plot
plot(c(0.5, n_cond + 0.5), c(0, 1),
     type = "n",
     xlab = "",
     ylab = "Predicted Probability",
     main = paste("Predicted Probabilities by Condition and Outcome\n(at Mean Expectation =", round(mean_expect, 3), ")"),
     xaxt = "n")

# Add bars and error bars
for (i in 1:n_cond) {
  cond <- conditions[i]
  for (j in 1:n_outcome) {
    outcome <- outcomes[j]
    
    # Find the corresponding data point
    data_point <- mean_preds[mean_preds$CONDITION == cond & mean_preds$OUTCOME_CAT == outcome, ]
    
    if (nrow(data_point) > 0) {
      # Draw bar
      rect(x_positions[i, j] - bar_width/2, 0,
           x_positions[i, j] + bar_width/2, data_point$estimate,
           col = outcome_colors[outcome], border = "black")
      
      # Draw error bar
      arrows(x_positions[i, j], data_point$conf.low,
             x_positions[i, j], data_point$conf.high,
             angle = 90, code = 3, length = 0.05)
    }
  }
}

# Add x-axis labels
axis(1, at = 1:n_cond, labels = conditions, las = 2)

# Add legend
legend("topright", legend = names(outcome_colors), fill = outcome_colors, cex = 0.8)

print("Plot 3 completed: Comparison plot at mean expectation level")

# 4. Summary statistics
print("\n=== SUMMARY STATISTICS ===")
cat("\nTotal observations:", nrow(data))
cat("\nNumber of monkeys:", length(unique(data$monkey)))
cat("\nNumber of blocks:", length(unique(data$BLOCK_No)))

print("\nExploration rates by condition:")
for (cond in levels(data$CONDITION)) {
  subset_data <- data[data$CONDITION == cond, ]
  cat("\n", cond, ":")
  cat("\n  n =", nrow(subset_data))
  cat("\n  explore rate =", round(mean(subset_data$OUTCOME_CAT == "explore"), 3))
  cat("\n  exploit rate =", round(mean(subset_data$OUTCOME_CAT == "exploit"), 3))
  cat("\n  none rate =", round(mean(subset_data$OUTCOME_CAT == "none"), 3))
}

print("\n\nModel summary:")
print(summary(fit))

print("\nAnalysis completed successfully!")
print("Three enhanced plots have been generated:")
print("1. Basic predicted probabilities by condition and expectation (faceted)")
print("2. Enhanced plot with confidence intervals for exploration outcome") 
print("3. Comparison barplot at mean expectation level") 