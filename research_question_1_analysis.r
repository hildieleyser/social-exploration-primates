# RESEARCH QUESTION 1: SOCIAL REFERENCE FRAMES AND IDENTITY MODELS
# Comprehensive analysis of rank vs gender vs individual differences
# Plus relative vs absolute rank model comparison

library(ggplot2)
library(nnet)
library(dplyr)

cat("=== RESEARCH QUESTION 1 ANALYSIS ===\n")
cat("Social reference frames and identity models in primate decision-making\n\n")

# Load and prepare data
data_raw <- read.csv("Explore Exploit Dataset.csv", stringsAsFactors = FALSE)
data_exp <- data_raw[data_raw$TRIAL_TYPE == "OIT_RE", ]
data_exp$outcome_clean <- ifelse(grepl("explore", data_exp$OUTCOME, ignore.case = TRUE), "explore",
                                ifelse(grepl("exploit", data_exp$OUTCOME, ignore.case = TRUE), "exploit", 
                                      ifelse(grepl("none|NONE", data_exp$OUTCOME, ignore.case = TRUE), "none", NA)))

# Clean data
data_clean <- data_exp[!is.na(data_exp$outcome_clean), ]
data_clean$outcome_clean <- factor(data_clean$outcome_clean, levels = c("exploit", "explore", "none"))

# Add proper variables
data_clean$MONKEY <- data_clean$SUBJECT
data_clean$CONDITION_CLEAN <- factor(data_clean$CONDITION, levels = c("solo", "duo", "trio"))
data_clean$SEX <- ifelse(data_clean$MONKEY %in% c("FRAN", "DALI", "EBI"), "Male", "Female")
data_clean$SEX <- factor(data_clean$SEX, levels = c("Male", "Female"))

# Create absolute rank (fixed hierarchy within each group)
# Group 1 (Females): CHOCOLAT=1, ICE=2, ANEMONE=3  
# Group 2 (Males): FRAN=1, DALI=2, EBI=3
data_clean$ABSOLUTE_RANK <- ifelse(data_clean$MONKEY == "CHOCOLAT", 1,
                                  ifelse(data_clean$MONKEY == "ICE", 2,
                                        ifelse(data_clean$MONKEY == "ANEMONE", 3,
                                              ifelse(data_clean$MONKEY == "FRAN", 1,
                                                    ifelse(data_clean$MONKEY == "DALI", 2,
                                                          ifelse(data_clean$MONKEY == "EBI", 3, NA))))))

# Relative rank is already in RELATIVE_RANK column
data_clean$RELATIVE_RANK_CLEAN <- factor(data_clean$RELATIVE_RANK, levels = c(1, 2, 3))
data_clean$ABSOLUTE_RANK_CLEAN <- factor(data_clean$ABSOLUTE_RANK, levels = c(1, 2, 3))

cat("Data prepared. Sample size:", nrow(data_clean), "trials\n")

# ================================================================================
# ANALYSIS 1: RANK VS GENDER IMPORTANCE
# ================================================================================

cat("\n=== ANALYSIS 1: RANK VS GENDER IMPORTANCE ===\n")

# Calculate exploration rates by rank and gender
rank_gender_summary <- data_clean %>%
  group_by(RELATIVE_RANK_CLEAN, SEX) %>%
  summarise(
    total_trials = n(),
    explore_trials = sum(outcome_clean == "explore"),
    exploration_rate = explore_trials / total_trials * 100,
    .groups = "drop"
  )

print(rank_gender_summary)

# Overall effects
rank_effect <- data_clean %>%
  group_by(RELATIVE_RANK_CLEAN) %>%
  summarise(exploration_rate = mean(outcome_clean == "explore") * 100, .groups = "drop")

gender_effect <- data_clean %>%
  group_by(SEX) %>%
  summarise(exploration_rate = mean(outcome_clean == "explore") * 100, .groups = "drop")

cat("\nRank Effect Range:", max(rank_effect$exploration_rate) - min(rank_effect$exploration_rate), "%\n")
cat("Gender Effect Range:", max(gender_effect$exploration_rate) - min(gender_effect$exploration_rate), "%\n")

# ================================================================================
# ANALYSIS 2: INDIVIDUAL DIFFERENCES
# ================================================================================

cat("\n=== ANALYSIS 2: INDIVIDUAL DIFFERENCES ===\n")

individual_summary <- data_clean %>%
  group_by(MONKEY, SEX, ABSOLUTE_RANK_CLEAN) %>%
  summarise(
    total_trials = n(),
    exploration_rate = mean(outcome_clean == "explore") * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(exploration_rate))

print(individual_summary)

individual_range <- max(individual_summary$exploration_rate) - min(individual_summary$exploration_rate)
cat("\nIndividual Differences Range:", individual_range, "%\n")

# ================================================================================
# ANALYSIS 3: MODEL COMPARISON - RELATIVE vs ABSOLUTE RANK
# ================================================================================

cat("\n=== ANALYSIS 3: RELATIVE vs ABSOLUTE RANK MODEL COMPARISON ===\n")

# Prepare predictors (using available columns)
data_model <- data_clean[complete.cases(data_clean[c("outcome_clean", "CONDITION_CLEAN", "RELATIVE_RANK_CLEAN", "ABSOLUTE_RANK_CLEAN", "SEX")]), ]

cat("Model data size:", nrow(data_model), "trials\n")

# Model 1: Relative Rank
model_relative <- multinom(outcome_clean ~ CONDITION_CLEAN + RELATIVE_RANK_CLEAN + SEX, 
                          data = data_model, trace = FALSE)

# Model 2: Absolute Rank  
model_absolute <- multinom(outcome_clean ~ CONDITION_CLEAN + ABSOLUTE_RANK_CLEAN + SEX, 
                          data = data_model, trace = FALSE)

# Model comparison
aic_relative <- AIC(model_relative)
aic_absolute <- AIC(model_absolute)

cat("\nModel Comparison:\n")
cat("Relative Rank Model AIC:", round(aic_relative, 2), "\n")
cat("Absolute Rank Model AIC:", round(aic_absolute, 2), "\n")
cat("Best Model:", ifelse(aic_relative < aic_absolute, "Relative Rank", "Absolute Rank"), "\n")
cat("AIC Difference:", round(abs(aic_relative - aic_absolute), 2), "\n")

# Prediction accuracy
pred_relative <- predict(model_relative, data_model)
pred_absolute <- predict(model_absolute, data_model)

acc_relative <- mean(pred_relative == data_model$outcome_clean) * 100
acc_absolute <- mean(pred_absolute == data_model$outcome_clean) * 100

cat("\nPrediction Accuracy:\n")
cat("Relative Rank Model:", round(acc_relative, 1), "%\n")
cat("Absolute Rank Model:", round(acc_absolute, 1), "%\n")

# ================================================================================
# CREATE VISUALIZATIONS
# ================================================================================

pdf("RESEARCH_QUESTION_1_RESULTS.pdf", width = 16, height = 12)

# Layout for 6 panels
layout(matrix(c(1,2,3,4,5,6), nrow = 2, ncol = 3, byrow = TRUE))
par(mar = c(4, 4, 3, 2))

# ================================================================================
# FIGURE 1: RANK vs GENDER EFFECTS
# ================================================================================

# Panel A: Rank Effect
rank_data <- data_clean %>%
  group_by(RELATIVE_RANK_CLEAN) %>%
  summarise(
    exploration_rate = mean(outcome_clean == "explore") * 100,
    se = sqrt(exploration_rate * (100 - exploration_rate) / n()),
    .groups = "drop"
  )

barplot(rank_data$exploration_rate, 
        names.arg = paste("Rank", rank_data$RELATIVE_RANK_CLEAN),
        col = c("#E8F4FD", "#B3E5FC", "#81D4FA"),
        border = "black",
        ylim = c(0, 50),
        main = "A. Relative Rank Effect on Exploration",
        ylab = "Exploration Rate (%)",
        cex.main = 1.2, font.main = 2)

# Add error bars
arrows(c(0.7, 1.9, 3.1), rank_data$exploration_rate - rank_data$se,
       c(0.7, 1.9, 3.1), rank_data$exploration_rate + rank_data$se,
       angle = 90, code = 3, length = 0.1, lwd = 2)

text(2, 45, paste("Range:", round(max(rank_data$exploration_rate) - min(rank_data$exploration_rate), 1), "%"),
     cex = 1.2, font = 2, col = "darkblue")

# Panel B: Gender Effect
gender_data <- data_clean %>%
  group_by(SEX) %>%
  summarise(
    exploration_rate = mean(outcome_clean == "explore") * 100,
    se = sqrt(exploration_rate * (100 - exploration_rate) / n()),
    .groups = "drop"
  )

barplot(gender_data$exploration_rate,
        names.arg = gender_data$SEX,
        col = c("#FFF3E0", "#FFE0B2"),
        border = "black",
        ylim = c(0, 50),
        main = "B. Gender Effect on Exploration", 
        ylab = "Exploration Rate (%)",
        cex.main = 1.2, font.main = 2)

arrows(c(0.7, 1.9), gender_data$exploration_rate - gender_data$se,
       c(0.7, 1.9), gender_data$exploration_rate + gender_data$se,
       angle = 90, code = 3, length = 0.1, lwd = 2)

text(1.3, 45, paste("Range:", round(max(gender_data$exploration_rate) - min(gender_data$exploration_rate), 1), "%"),
     cex = 1.2, font = 2, col = "darkorange")

# ================================================================================
# FIGURE 2: INDIVIDUAL DIFFERENCES
# ================================================================================

# Panel C: Individual Exploration Rates
individual_plot <- individual_summary[order(individual_summary$exploration_rate, decreasing = TRUE), ]
colors_ind <- c("#E8F5E8", "#C8E6C9", "#A5D6A7", "#81C784", "#66BB6A", "#4CAF50")

barplot(individual_plot$exploration_rate,
        names.arg = individual_plot$MONKEY,
        col = colors_ind,
        border = "black",
        ylim = c(0, 60),
        main = "C. Individual Differences in Exploration",
        ylab = "Exploration Rate (%)",
        cex.main = 1.2, font.main = 2,
        las = 2)

text(3.5, 55, paste("Range:", round(individual_range, 1), "%"),
     cex = 1.2, font = 2, col = "darkgreen")

# Panel D: Rank vs Gender vs Individual Comparison
effect_sizes <- c(
  max(rank_data$exploration_rate) - min(rank_data$exploration_rate),
  max(gender_data$exploration_rate) - min(gender_data$exploration_rate),
  individual_range
)

barplot(effect_sizes,
        names.arg = c("Rank", "Gender", "Individual"),
        col = c("#E3F2FD", "#FFF3E0", "#E8F5E8"),
        border = "black",
        ylim = c(0, max(effect_sizes) * 1.2),
        main = "D. Effect Size Comparison",
        ylab = "Effect Size (% Range)",
        cex.main = 1.2, font.main = 2)

# Add values on bars
text(c(0.7, 1.9, 3.1), effect_sizes + 1, 
     paste(round(effect_sizes, 1), "%"),
     cex = 1.1, font = 2)

# ================================================================================
# FIGURE 3: RELATIVE vs ABSOLUTE RANK COMPARISON
# ================================================================================

# Panel E: Model Performance Comparison
model_metrics <- data.frame(
  Model = c("Relative Rank", "Absolute Rank"),
  AIC = c(aic_relative, aic_absolute),
  Accuracy = c(acc_relative, acc_absolute)
)

# AIC comparison (lower is better)
barplot(model_metrics$AIC,
        names.arg = model_metrics$Model,
        col = c("#F3E5F5", "#E1BEE7"),
        border = "black",
        main = "E. Model Fit Comparison (AIC)",
        ylab = "AIC (lower = better)",
        cex.main = 1.2, font.main = 2)

text(1.3, max(model_metrics$AIC) * 0.9, 
     paste("Best:", ifelse(aic_relative < aic_absolute, "Relative", "Absolute")),
     cex = 1.2, font = 2, col = "purple")

# Panel F: Prediction Accuracy
barplot(model_metrics$Accuracy,
        names.arg = model_metrics$Model,
        col = c("#F3E5F5", "#E1BEE7"),
        border = "black",
        ylim = c(0, 100),
        main = "F. Prediction Accuracy",
        ylab = "Accuracy (%)",
        cex.main = 1.2, font.main = 2)

text(c(0.7, 1.9), model_metrics$Accuracy + 3,
     paste(round(model_metrics$Accuracy, 1), "%"),
     cex = 1.1, font = 2)

dev.off()

# ================================================================================
# SUMMARY RESULTS
# ================================================================================

cat("\n=== RESEARCH QUESTION 1 SUMMARY ===\n")
cat("1. RANK vs GENDER IMPORTANCE:\n")
cat("   - Rank effect range:", round(max(rank_data$exploration_rate) - min(rank_data$exploration_rate), 1), "%\n")
cat("   - Gender effect range:", round(max(gender_data$exploration_rate) - min(gender_data$exploration_rate), 1), "%\n")
cat("   - RANK IS", ifelse(effect_sizes[1] > effect_sizes[2], "MORE", "LESS"), "IMPORTANT THAN GENDER\n")

cat("\n2. RANK vs INDIVIDUAL DIFFERENCES:\n")
cat("   - Rank effect range:", round(effect_sizes[1], 1), "%\n")
cat("   - Individual differences range:", round(individual_range, 1), "%\n")
cat("   - INDIVIDUAL DIFFERENCES ARE", ifelse(individual_range > effect_sizes[1], "MORE", "LESS"), "IMPORTANT THAN RANK\n")

cat("\n3. RELATIVE vs ABSOLUTE RANK:\n")
cat("   - Relative Rank Model: AIC =", round(aic_relative, 2), ", Accuracy =", round(acc_relative, 1), "%\n")
cat("   - Absolute Rank Model: AIC =", round(aic_absolute, 2), ", Accuracy =", round(acc_absolute, 1), "%\n")
cat("   -", ifelse(aic_relative < aic_absolute, "RELATIVE RANK", "ABSOLUTE RANK"), "MODEL FITS BETTER\n")

cat("\nCreated RESEARCH_QUESTION_1_RESULTS.pdf with 6 comprehensive analysis panels!\n") 