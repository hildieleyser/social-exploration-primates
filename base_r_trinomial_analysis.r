# Base R Trinomial Analysis for AMY Dataset
# An alternative approach without brms dependencies

# Load required libraries (basic R packages)
library(graphics)
library(stats)

# Set working directory and load data
cat("Loading AMY dataset...\n")

# Check if data file exists
if (file.exists("Explore Exploit Dataset.csv")) {
  data_file <- "Explore Exploit Dataset.csv"
} else if (file.exists("AMY_DATA.csv")) {
  data_file <- "AMY_DATA.csv"
} else {
  stop("Data file not found. Please ensure 'Explore Exploit Dataset.csv' or 'AMY_DATA.csv' is in the working directory.")
}

# Load data
data <- read.csv(data_file, stringsAsFactors = FALSE)
cat("Data loaded successfully. Dimensions:", dim(data), "\n")
cat("Column names:", paste(names(data), collapse = ", "), "\n")

# Data preprocessing
cat("\nPreparing data for trinomial analysis...\n")

# Assume the data has columns for choices (0, 1, 2) and conditions
# You may need to adjust these column names based on your actual data structure
if ("choice" %in% names(data)) {
  choices <- data$choice
} else if ("response" %in% names(data)) {
  choices <- data$response
} else {
  # Use first numeric column as choices
  numeric_cols <- sapply(data, is.numeric)
  if (any(numeric_cols)) {
    choices <- data[, which(numeric_cols)[1]]
    cat("Using column '", names(data)[which(numeric_cols)[1]], "' as choices\n")
  } else {
    stop("No suitable choice column found")
  }
}

# Convert to trinomial format (0, 1, 2)
if (length(unique(choices)) == 3) {
  unique_choices <- sort(unique(choices))
  choices <- match(choices, unique_choices) - 1
  cat("Choices recoded to 0, 1, 2 format\n")
}

# Create condition variable if it doesn't exist
if ("condition" %in% names(data)) {
  condition <- as.factor(data$condition)
} else if ("group" %in% names(data)) {
  condition <- as.factor(data$group)
} else {
  # Create a single condition
  condition <- factor(rep("all", nrow(data)))
  cat("No condition variable found, treating all data as one group\n")
}

# Participant ID
if ("participant" %in% names(data) || "subject" %in% names(data) || "id" %in% names(data)) {
  participant_col <- names(data)[names(data) %in% c("participant", "subject", "id")][1]
  participant <- as.factor(data[[participant_col]])
} else {
  participant <- factor(1:nrow(data))
  cat("No participant ID found, creating sequential IDs\n")
}

# Basic descriptive statistics
cat("\n=== DESCRIPTIVE STATISTICS ===\n")
choice_table <- table(choices)
cat("Choice frequencies:\n")
print(choice_table)

choice_props <- prop.table(choice_table)
cat("\nChoice proportions:\n")
print(round(choice_props, 3))

# Cross-tabulation by condition
cat("\nChoices by condition:\n")
condition_table <- table(condition, choices)
print(condition_table)

cat("\nChoice proportions by condition:\n")
condition_props <- prop.table(condition_table, margin = 1)
print(round(condition_props, 3))

# Statistical tests
cat("\n=== STATISTICAL TESTS ===\n")

# Chi-square test for association between condition and choice
if (length(levels(condition)) > 1) {
  chi_test <- chisq.test(condition_table)
  cat("Chi-square test for independence:\n")
  cat("Chi-square =", round(chi_test$statistic, 3), "\n")
  cat("df =", chi_test$parameter, "\n")
  cat("p-value =", round(chi_test$p.value, 4), "\n")
  
  if (chi_test$p.value < 0.05) {
    cat("*** Significant association between condition and choice (p < 0.05) ***\n")
  } else {
    cat("No significant association between condition and choice (p >= 0.05)\n")
  }
}

# Multinomial logistic regression (simple version)
cat("\n=== MULTINOMIAL ANALYSIS ===\n")

# Using base R's multinom-like approach with individual binomial models
# Compare choice 1 vs 0, and choice 2 vs 0

# Model 1: Choice 1 vs Choice 0
choice_1v0 <- ifelse(choices == 1, 1, ifelse(choices == 0, 0, NA))
valid_1v0 <- !is.na(choice_1v0)

if (sum(valid_1v0) > 0 && length(levels(condition)) > 1) {
  model_1v0 <- glm(choice_1v0[valid_1v0] ~ condition[valid_1v0], 
                   family = binomial(link = "logit"))
  cat("Model: Choice 1 vs Choice 0\n")
  print(summary(model_1v0))
}

# Model 2: Choice 2 vs Choice 0  
choice_2v0 <- ifelse(choices == 2, 1, ifelse(choices == 0, 0, NA))
valid_2v0 <- !is.na(choice_2v0)

if (sum(valid_2v0) > 0 && length(levels(condition)) > 1) {
  model_2v0 <- glm(choice_2v0[valid_2v0] ~ condition[valid_2v0], 
                   family = binomial(link = "logit"))
  cat("\nModel: Choice 2 vs Choice 0\n")
  print(summary(model_2v0))
}

# Visualizations
cat("\n=== GENERATING VISUALIZATIONS ===\n")

# Create a simple bar plot
png("trinomial_analysis_baseR.png", width = 800, height = 600)
par(mfrow = c(2, 2))

# Plot 1: Overall choice distribution
barplot(choice_table, 
        main = "Overall Choice Distribution",
        xlab = "Choice", ylab = "Frequency",
        col = c("lightblue", "lightgreen", "lightcoral"))

# Plot 2: Choice proportions
barplot(choice_props,
        main = "Choice Proportions", 
        xlab = "Choice", ylab = "Proportion",
        col = c("lightblue", "lightgreen", "lightcoral"),
        ylim = c(0, 1))

# Plot 3: Choices by condition (if multiple conditions)
if (length(levels(condition)) > 1) {
  barplot(t(condition_props),
          main = "Choice Proportions by Condition",
          xlab = "Condition", ylab = "Proportion",
          col = c("lightblue", "lightgreen", "lightcoral"),
          beside = TRUE,
          legend = c("Choice 0", "Choice 1", "Choice 2"))
} else {
  plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
  text(1, 1, "Single condition:\nNo between-group\ncomparison available", 
       cex = 1.2, adj = 0.5)
}

# Plot 4: Summary statistics text
plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
text(1, 1, paste("Total observations:", nrow(data),
                 "\nUnique participants:", length(unique(participant)),
                 "\nConditions:", length(levels(condition)),
                 "\nChoice 0:", choice_table[1],
                 "\nChoice 1:", choice_table[2], 
                 "\nChoice 2:", choice_table[3]),
     cex = 1.2, adj = 0.5)

dev.off()

cat("Visualization saved as 'trinomial_analysis_baseR.png'\n")

# Summary report
cat("\n=== ANALYSIS SUMMARY ===\n")
cat("This analysis provides a basic trinomial choice analysis using base R functions.\n")
cat("While not as sophisticated as Bayesian methods, it provides:\n")
cat("- Descriptive statistics for choice distributions\n")
cat("- Chi-square tests for independence\n") 
cat("- Basic logistic regression models\n")
cat("- Visual summaries of the data\n")
cat("\nFor more advanced Bayesian analysis, consider:\n")
cat("- Updating R to a newer version\n")
cat("- Installing compilation tools (Xcode, gfortran)\n")
cat("- Using online Bayesian analysis platforms\n")

cat("\nAnalysis complete!\n") 