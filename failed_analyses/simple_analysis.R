# =============================================================================
# Simple Explore/Exploit Analysis (Compatible with Older R)
# =============================================================================

# Set CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org/"))

cat("Starting simple analysis with base R...\n")

# =============================================================================
# 1. LOAD DATA WITH BASE R
# =============================================================================

data <- read.csv("Explore Exploit Dataset.csv", stringsAsFactors = FALSE)

cat("Original data dimensions:", dim(data), "\n")

# =============================================================================
# 2. CLEAN DATA WITH BASE R
# =============================================================================

# Create explore/exploit classification
data$decision_type <- ifelse(
  grepl("explore", tolower(data$OUTCOME)), "explore",
  ifelse(grepl("exploit", tolower(data$OUTCOME)), "exploit", "other")
)

# Clean social context
data$social_context <- ifelse(
  data$CONDITION == "solo", "solo",
  ifelse(data$CONDITION == "duo", "duo",
    ifelse(data$CONDITION == "trio", "trio", "other"))
)

# Filter to complete cases
clean_data <- data[
  !is.na(data$monkey) & 
  !is.na(data$CONDITION) & 
  !is.na(data$OUTCOME) & 
  !is.na(data$expected_explore) &
  data$decision_type %in% c("explore", "exploit") &
  data$social_context != "other", 
]

cat("Cleaned data dimensions:", dim(clean_data), "\n")
cat("Unique monkeys:", length(unique(clean_data$monkey)), "\n")

# =============================================================================
# 3. DESCRIPTIVE STATISTICS
# =============================================================================

# Summary by social context
summary_table <- aggregate(
  list(explore_rate = (clean_data$decision_type == "explore")),
  by = list(social_context = clean_data$social_context),
  FUN = mean
)

cat("\nExploration rates by social context:\n")
print(summary_table)

# Summary by monkey and context
monkey_summary <- aggregate(
  list(
    explore_rate = (clean_data$decision_type == "explore"),
    mean_expectation = clean_data$expected_explore,
    n_trials = rep(1, nrow(clean_data))
  ),
  by = list(
    monkey = clean_data$monkey,
    social_context = clean_data$social_context
  ),
  FUN = function(x) if(length(x) == 1) x else c(mean(x), length(x))[1]
)

cat("\nSummary by monkey and context:\n")
print(head(monkey_summary, 10))

# =============================================================================
# 4. SIMPLE STATISTICAL ANALYSIS
# =============================================================================

# Convert to binary for logistic regression
clean_data$explore_binary <- as.numeric(clean_data$decision_type == "explore")

# Standardize expectation
clean_data$expected_explore_z <- scale(clean_data$expected_explore)[,1]

# Simple logistic regression (no random effects)
cat("\nFitting logistic regression...\n")

# Model 1: Main effects only
model1 <- glm(
  explore_binary ~ social_context + expected_explore_z,
  data = clean_data,
  family = binomial()
)

cat("Model 1 Summary:\n")
print(summary(model1))

# Model 2: With interaction
model2 <- glm(
  explore_binary ~ social_context * expected_explore_z,
  data = clean_data,
  family = binomial()
)

cat("\nModel 2 (with interaction) Summary:\n")
print(summary(model2))

# =============================================================================
# 5. INTERPRET RESULTS
# =============================================================================

# Extract coefficients
coef1 <- summary(model1)$coefficients
coef2 <- summary(model2)$coefficients

cat("\n=============================================================================\n")
cat("RESULTS INTERPRETATION\n")
cat("=============================================================================\n")

# Main effects from Model 1
cat("Main Effects (Model 1):\n")
if("social_contextduo" %in% rownames(coef1)) {
  cat("Duo vs Solo effect:", round(coef1["social_contextduo", "Estimate"], 3), 
      " (p =", round(coef1["social_contextduo", "Pr(>|z|)"], 3), ")\n")
}
cat("Solo vs Duo effect:", round(coef1["social_contextsolo", "Estimate"], 3), 
    " (p =", round(coef1["social_contextsolo", "Pr(>|z|)"], 3), ")\n")
cat("Trio vs Duo effect:", round(coef1["social_contexttrio", "Estimate"], 3), 
    " (p =", round(coef1["social_contexttrio", "Pr(>|z|)"], 3), ")\n")
cat("Expectation effect:", round(coef1["expected_explore_z", "Estimate"], 3), 
    " (p =", round(coef1["expected_explore_z", "Pr(>|z|)"], 3), ")\n")

# Convert to odds ratios
cat("\nOdds Ratios (easier to interpret):\n")
cat("Solo vs Duo OR:", round(exp(coef1["social_contextsolo", "Estimate"]), 3), "\n")
cat("Trio vs Duo OR:", round(exp(coef1["social_contexttrio", "Estimate"]), 3), "\n")
cat("Expectation OR:", round(exp(coef1["expected_explore_z", "Estimate"]), 3), "\n")

# Model comparison
aic1 <- AIC(model1)
aic2 <- AIC(model2)

cat("\nModel Comparison (AIC - lower is better):\n")
cat("Model 1 (main effects):", round(aic1, 1), "\n")
cat("Model 2 (with interaction):", round(aic2, 1), "\n")
cat("Best model:", if(aic2 < aic1) "Model 2 (interaction)" else "Model 1 (main effects)", "\n")

# =============================================================================
# 6. SIMPLE VISUALIZATION (BASE R)
# =============================================================================

# Create simple bar plot
png("exploration_by_context.png", width = 600, height = 400)
barplot(
  summary_table$explore_rate,
  names.arg = summary_table$social_context,
  main = "Exploration Rate by Social Context",
  ylab = "Proportion of Explore Decisions",
  xlab = "Social Context",
  col = c("lightblue", "lightgreen", "lightcoral"),
  ylim = c(0, max(summary_table$explore_rate) * 1.1)
)
dev.off()

cat("\nPlot saved as: exploration_by_context.png\n")

# =============================================================================
# 7. SAVE RESULTS
# =============================================================================

results <- list(
  data_summary = summary_table,
  monkey_summary = monkey_summary,
  model1 = model1,
  model2 = model2,
  model_comparison = data.frame(
    model = c("Main Effects", "With Interaction"),
    AIC = c(aic1, aic2)
  )
)

save(results, clean_data, file = "simple_analysis_results.RData")

cat("\n=============================================================================\n")
cat("SIMPLE ANALYSIS COMPLETE\n")
cat("=============================================================================\n")
cat("Results saved to: simple_analysis_results.RData\n")
cat("Plot saved to: exploration_by_context.png\n")
cat("\nThis gives you the basic answer to your research question!\n")
cat("For more sophisticated Bayesian analysis, consider updating R to version 4.x\n") 