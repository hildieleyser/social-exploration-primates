# Proper Trinomial Analysis for Explore-Exploit Dataset
# Base R analysis of choice behavior

# Load required libraries
library(graphics)
library(stats)

# Load and examine data
cat("Loading Explore-Exploit dataset...\n")
data <- read.csv("Explore Exploit Dataset.csv", stringsAsFactors = FALSE)
cat("Data loaded successfully. Dimensions:", dim(data), "\n")

# Examine the OUTCOME variable (actual choices)
cat("\nUnique outcomes in OUTCOME column:\n")
print(table(data$OUTCOME))

# Create trinomial choice variable
# 0 = exploit_pink, 1 = exploit_blue, 2 = explore
choice_mapping <- function(outcome) {
  case_when <- function(condition, value, default = NA) {
    ifelse(condition, value, default)
  }
  
  result <- rep(NA, length(outcome))
  result[outcome == "exploit_pink"] <- 0
  result[outcome == "exploit_blue"] <- 1  
  result[outcome == "explore"] <- 2
  return(result)
}

# Apply choice mapping
data$choice_trinomial <- choice_mapping(data$OUTCOME)
cat("\nTrinomial choice mapping:\n")
cat("0 = exploit_pink, 1 = exploit_blue, 2 = explore\n")
print(table(data$choice_trinomial, useNA = "always"))

# Remove any missing choices
complete_data <- data[!is.na(data$choice_trinomial), ]
cat("Complete cases:", nrow(complete_data), "out of", nrow(data), "\n")

# Extract variables for analysis
choices <- complete_data$choice_trinomial
condition <- as.factor(complete_data$CONDITION)
monkey <- as.factor(complete_data$monkey)
trial_type <- as.factor(complete_data$TRIAL_TYPE)
block <- as.factor(complete_data$BLOCK_No)

# Basic descriptive statistics
cat("\n=== DESCRIPTIVE STATISTICS ===\n")
choice_table <- table(choices)
names(choice_table) <- c("Exploit Pink", "Exploit Blue", "Explore")
cat("Choice frequencies:\n")
print(choice_table)

choice_props <- prop.table(choice_table)
cat("\nChoice proportions:\n")
print(round(choice_props, 3))

# Analysis by condition
cat("\n=== ANALYSIS BY CONDITION ===\n")
condition_table <- table(condition, choices)
colnames(condition_table) <- c("Exploit Pink", "Exploit Blue", "Explore")
cat("Choices by condition:\n")
print(condition_table)

condition_props <- prop.table(condition_table, margin = 1)
cat("\nChoice proportions by condition:\n")
print(round(condition_props, 3))

# Analysis by monkey
cat("\n=== ANALYSIS BY MONKEY ===\n")
monkey_table <- table(monkey, choices)
colnames(monkey_table) <- c("Exploit Pink", "Exploit Blue", "Explore")
cat("Choices by monkey:\n")
print(monkey_table)

monkey_props <- prop.table(monkey_table, margin = 1)
cat("\nChoice proportions by monkey:\n")
print(round(monkey_props, 3))

# Statistical tests
cat("\n=== STATISTICAL TESTS ===\n")

# Chi-square test for condition effect
if (length(levels(condition)) > 1) {
  chi_condition <- chisq.test(condition_table)
  cat("Chi-square test for condition effect:\n")
  cat("Chi-square =", round(chi_condition$statistic, 3), "\n")
  cat("df =", chi_condition$parameter, "\n")
  cat("p-value =", formatC(chi_condition$p.value, format = "e", digits = 3), "\n")
  
  if (chi_condition$p.value < 0.05) {
    cat("*** Significant condition effect (p < 0.05) ***\n")
  } else {
    cat("No significant condition effect (p >= 0.05)\n")
  }
}

# Chi-square test for monkey effect
if (length(levels(monkey)) > 1) {
  chi_monkey <- chisq.test(monkey_table)
  cat("\nChi-square test for monkey effect:\n")
  cat("Chi-square =", round(chi_monkey$statistic, 3), "\n")
  cat("df =", chi_monkey$parameter, "\n")
  cat("p-value =", formatC(chi_monkey$p.value, format = "e", digits = 3), "\n")
  
  if (chi_monkey$p.value < 0.05) {
    cat("*** Significant monkey effect (p < 0.05) ***\n")
  } else {
    cat("No significant monkey effect (p >= 0.05)\n")
  }
}

# Multinomial logistic regression using individual binomial models
cat("\n=== MULTINOMIAL LOGISTIC REGRESSION ===\n")

# Model 1: Exploit Blue vs Exploit Pink
blue_vs_pink <- ifelse(choices == 1, 1, ifelse(choices == 0, 0, NA))
valid_bp <- !is.na(blue_vs_pink)

if (sum(valid_bp) > 0 && length(levels(condition)) > 1) {
  model_bp <- glm(blue_vs_pink[valid_bp] ~ condition[valid_bp] + monkey[valid_bp], 
                  family = binomial(link = "logit"))
  cat("Model: Exploit Blue vs Exploit Pink\n")
  cat("Coefficients:\n")
  print(summary(model_bp)$coefficients)
  cat("AIC:", round(AIC(model_bp), 2), "\n\n")
}

# Model 2: Explore vs Exploit Pink
explore_vs_pink <- ifelse(choices == 2, 1, ifelse(choices == 0, 0, NA))
valid_ep <- !is.na(explore_vs_pink)

if (sum(valid_ep) > 0 && length(levels(condition)) > 1) {
  model_ep <- glm(explore_vs_pink[valid_ep] ~ condition[valid_ep] + monkey[valid_ep], 
                  family = binomial(link = "logit"))
  cat("Model: Explore vs Exploit Pink\n")
  cat("Coefficients:\n")
  print(summary(model_ep)$coefficients)
  cat("AIC:", round(AIC(model_ep), 2), "\n\n")
}

# Model 3: Explore vs Exploit Blue
explore_vs_blue <- ifelse(choices == 2, 1, ifelse(choices == 1, 0, NA))
valid_eb <- !is.na(explore_vs_blue)

if (sum(valid_eb) > 0 && length(levels(condition)) > 1) {
  model_eb <- glm(explore_vs_blue[valid_eb] ~ condition[valid_eb] + monkey[valid_eb], 
                  family = binomial(link = "logit"))
  cat("Model: Explore vs Exploit Blue\n")
  cat("Coefficients:\n")
  print(summary(model_eb)$coefficients)
  cat("AIC:", round(AIC(model_eb), 2), "\n\n")
}

# Explore vs Any Exploit
explore_vs_exploit <- ifelse(choices == 2, 1, 0)
if (length(levels(condition)) > 1) {
  model_exp_vs_all <- glm(explore_vs_exploit ~ condition + monkey, 
                          family = binomial(link = "logit"))
  cat("Model: Explore vs Any Exploit\n")
  cat("Coefficients:\n")
  print(summary(model_exp_vs_all)$coefficients)
  cat("AIC:", round(AIC(model_exp_vs_all), 2), "\n\n")
}

# Visualizations
cat("=== GENERATING VISUALIZATIONS ===\n")

png("explore_exploit_trinomial_analysis.png", width = 1200, height = 800)
par(mfrow = c(2, 3), mar = c(4, 4, 3, 2))

# Plot 1: Overall choice distribution
colors <- c("lightcoral", "lightblue", "lightgreen")
barplot(choice_table, 
        main = "Overall Choice Distribution",
        xlab = "Choice Type", ylab = "Frequency",
        col = colors,
        names.arg = c("Exploit\nPink", "Exploit\nBlue", "Explore"))

# Plot 2: Choice proportions
barplot(choice_props,
        main = "Choice Proportions", 
        xlab = "Choice Type", ylab = "Proportion",
        col = colors,
        ylim = c(0, max(choice_props) * 1.1),
        names.arg = c("Exploit\nPink", "Exploit\nBlue", "Explore"))

# Plot 3: Choices by condition
if (length(levels(condition)) > 1) {
  barplot(t(condition_props),
          main = "Choice Proportions by Condition",
          xlab = "Condition", ylab = "Proportion",
          col = colors,
          beside = TRUE,
          legend = c("Exploit Pink", "Exploit Blue", "Explore"),
          args.legend = list(x = "topright"))
} else {
  plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
  text(1, 1, "Single condition analysis", cex = 1.2, adj = 0.5)
}

# Plot 4: Choices by monkey
if (length(levels(monkey)) > 1) {
  barplot(t(monkey_props),
          main = "Choice Proportions by Monkey",
          xlab = "Monkey", ylab = "Proportion",
          col = colors,
          beside = TRUE,
          legend = c("Exploit Pink", "Exploit Blue", "Explore"),
          args.legend = list(x = "topright"))
} else {
  plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
  text(1, 1, "Single monkey analysis", cex = 1.2, adj = 0.5)
}

# Plot 5: Exploration rate over time (by trial)
if ("TRIAL_NUM" %in% names(complete_data)) {
  explore_by_trial <- aggregate(explore_vs_exploit, 
                               by = list(trial = complete_data$TRIAL_NUM), 
                               FUN = mean)
  plot(explore_by_trial$trial, explore_by_trial$x,
       type = "b", pch = 16,
       main = "Exploration Rate by Trial",
       xlab = "Trial Number", ylab = "Proportion of Explore Choices",
       ylim = c(0, 1))
  abline(h = mean(explore_vs_exploit), col = "red", lty = 2)
}

# Plot 6: Summary statistics
plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
summary_text <- paste(
  "Dataset Summary:",
  paste("Total trials:", nrow(complete_data)),
  paste("Monkeys:", length(unique(monkey))),
  paste("Conditions:", length(unique(condition))),
  paste("Blocks:", length(unique(block))),
  "",
  "Choice Frequencies:",
  paste("Exploit Pink:", choice_table[1]),
  paste("Exploit Blue:", choice_table[2]),
  paste("Explore:", choice_table[3]),
  "",
  paste("Exploration Rate:", round(mean(explore_vs_exploit), 3)),
  sep = "\n"
)
text(1, 1, summary_text, cex = 0.9, adj = 0.5)

dev.off()

cat("Comprehensive visualization saved as 'explore_exploit_trinomial_analysis.png'\n")

# Summary report
cat("\n=== ANALYSIS SUMMARY ===\n")
cat("Trinomial Analysis of Explore-Exploit Behavior\n")
cat("=============================================\n")
cat("Total trials analyzed:", nrow(complete_data), "\n")
cat("Overall exploration rate:", round(mean(explore_vs_exploit), 3), "\n")
cat("Exploit pink preference:", round(choice_props[1], 3), "\n")
cat("Exploit blue preference:", round(choice_props[2], 3), "\n")
cat("Explore preference:", round(choice_props[3], 3), "\n")

if (length(levels(condition)) > 1) {
  cat("\nCondition effects detected:", any(apply(condition_props, 2, var) > 0.01), "\n")
}

if (length(levels(monkey)) > 1) {
  cat("Individual differences detected:", any(apply(monkey_props, 2, var) > 0.01), "\n")
}

cat("\nThis analysis provides a comprehensive examination of trinomial choice behavior")
cat("\nusing standard statistical methods available in base R.\n")
cat("\nAnalysis complete!\n") 