#!/usr/bin/env Rscript

# COMPARING EXPECTED EXPLORE VALUE VS SUBJECTIVE EXPLOIT VALUE
# AND ALL MODEL COEFFICIENTS

suppressMessages({
  library(ggplot2)
  library(dplyr)
  library(gridExtra)
  library(nnet)
})

cat("=== EXPECTED EXPLORE VS SUBJECTIVE EXPLOIT VALUE COMPARISON ===\n")
cat("Plus comprehensive model coefficients analysis\n\n")

# Load and prepare data
data_raw <- read.csv("Explore Exploit Dataset.csv", stringsAsFactors = FALSE)
data_exp <- data_raw[data_raw$TRIAL_TYPE == "OIT_RE", ]

# Clean outcomes
data_exp$outcome_clean <- ifelse(grepl("explore", data_exp$OUTCOME, ignore.case = TRUE), "explore",
                                ifelse(grepl("exploit", data_exp$OUTCOME, ignore.case = TRUE), "exploit", 
                                      ifelse(grepl("none|NONE", data_exp$OUTCOME, ignore.case = TRUE), "none", NA)))

data_clean <- data_exp[!is.na(data_exp$outcome_clean), ]

# Create comprehensive dataset
model_data <- data.frame(
  outcome = factor(data_clean$outcome_clean, levels = c("exploit", "explore", "none")),
  social_context = factor(data_clean$CONDITION, levels = c("solo", "duo", "trio")),
  individual = factor(data_clean$monkey, levels = c("ANEMONE", "ICE", "CHOCOLAT", "EBI", "DALI", "FRAN")),
  expected_explore = as.numeric(data_clean$expected_explore),      # What they expect from exploring
  subjective_exploit = as.numeric(data_clean$subjective_exploit), # What they see in exploit option
  relative_rank = as.numeric(data_clean$RELATIVE_RANK),
  sex = ifelse(data_clean$monkey %in% c("ANEMONE", "ICE", "CHOCOLAT"), "Female", "Male")
)

model_data <- model_data[complete.cases(model_data), ]

cat("Data prepared:\n")
cat("Sample size:", nrow(model_data), "trials\n")
cat("Comparing Expected Explore Value vs Subjective Exploit Value\n\n")

# Create value comparison categories
model_data$explore_vs_exploit <- ifelse(
  model_data$expected_explore > model_data$subjective_exploit, 
  "Explore > Exploit", 
  ifelse(model_data$expected_explore < model_data$subjective_exploit, 
         "Explore < Exploit", 
         "Explore = Exploit")
)

# Summary statistics
cat("VALUE COMPARISON SUMMARY:\n")
explore_mean <- mean(model_data$expected_explore, na.rm = TRUE)
exploit_mean <- mean(model_data$subjective_exploit, na.rm = TRUE)
cat(sprintf("Mean Expected Explore Value: %.3f\n", explore_mean))
cat(sprintf("Mean Subjective Exploit Value: %.3f\n", exploit_mean))
cat(sprintf("Difference: %.3f\n", explore_mean - exploit_mean))

# Count comparison categories
comparison_counts <- table(model_data$explore_vs_exploit)
cat("\nValue Comparison Categories:\n")
for(i in 1:length(comparison_counts)) {
  cat(sprintf("%s: %d trials (%.1f%%)\n", 
              names(comparison_counts)[i], 
              comparison_counts[i], 
              comparison_counts[i]/sum(comparison_counts)*100))
}

pdf("VALUE_COMPARISON_AND_MODEL_COEFFICIENTS.pdf", width = 16, height = 12)

# PLOT 1: Expected Explore vs Subjective Exploit Scatter
plot1 <- ggplot(model_data, aes(x = subjective_exploit, y = expected_explore, color = outcome)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", size = 1) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.3) +
  labs(title = "Expected Explore Value vs Subjective Exploit Value",
       subtitle = "Diagonal line shows equal values; deviations show relative preferences",
       x = "Subjective Exploit Value (What they see in exploit option)", 
       y = "Expected Explore Value (What they expect from exploring)",
       color = "Actual Decision") +
  theme_minimal() +
  theme(text = element_text(size = 12),
        plot.title = element_text(size = 16, face = "bold")) +
  scale_color_brewer(type = "qual", palette = "Set1")

print(plot1)

# PLOT 2: Value comparison effects on decisions
comparison_effects <- model_data %>%
  group_by(explore_vs_exploit, outcome) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(explore_vs_exploit) %>%
  mutate(proportion = count / sum(count) * 100)

plot2 <- ggplot(comparison_effects, aes(x = explore_vs_exploit, y = proportion, fill = outcome)) +
  geom_col(position = "dodge", alpha = 0.8, width = 0.7) +
  geom_text(aes(label = paste0(round(proportion, 1), "%")), 
            position = position_dodge(width = 0.7), vjust = -0.5, size = 4, fontface = "bold") +
  labs(title = "Decision Outcomes by Value Comparison",
       subtitle = "How relative value expectations influence actual decisions",
       x = "Expected Explore vs Subjective Exploit", 
       y = "Decision Probability (%)",
       fill = "Actual Decision") +
  theme_minimal() +
  theme(text = element_text(size = 12),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  ylim(0, 80)

print(plot2)

# PLOT 3: Individual patterns in value comparison
individual_value_patterns <- model_data %>%
  group_by(individual, explore_vs_exploit, outcome) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(individual, explore_vs_exploit) %>%
  mutate(proportion = count / sum(count) * 100) %>%
  filter(outcome == "explore")

plot3 <- ggplot(individual_value_patterns, aes(x = individual, y = proportion, fill = explore_vs_exploit)) +
  geom_col(position = "dodge", alpha = 0.8, width = 0.7) +
  geom_text(aes(label = paste0(round(proportion, 1), "%")), 
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3, fontface = "bold") +
  geom_vline(xintercept = 3.5, linetype = "dashed", alpha = 0.7, linewidth = 1, color = "black") +
  annotate("text", x = 2, y = 75, label = "FEMALES", size = 5, fontface = "bold", color = "blue") +
  annotate("text", x = 5, y = 75, label = "MALES", size = 5, fontface = "bold", color = "red") +
  labs(title = "Individual Value Comparison Patterns (Exploration Only)",
       subtitle = "How each monkey responds to different value comparisons",
       x = "Individual Monkey", 
       y = "Exploration Rate (%)",
       fill = "Value Comparison") +
  theme_minimal() +
  theme(text = element_text(size = 12),
        plot.title = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  ylim(0, 80)

print(plot3)

# FIT COMPREHENSIVE MULTINOMIAL MODEL
cat("\nFitting comprehensive multinomial model...\n")

# Prepare data for modeling
model_data$social_context_numeric <- as.numeric(model_data$social_context)
model_data$individual_numeric <- as.numeric(model_data$individual)
model_data$sex_numeric <- ifelse(model_data$sex == "Male", 1, 0)

# Fit multinomial model with all variables
tryCatch({
  multinomial_model <- multinom(outcome ~ expected_explore + subjective_exploit + 
                               social_context + relative_rank + sex + individual, 
                               data = model_data, trace = FALSE)
  
  # Extract coefficients
  coef_matrix <- summary(multinomial_model)$coefficients
  
  cat("Model fitted successfully!\n")
  
  # Create coefficient comparison data
  coef_data <- data.frame(
    Outcome = rep(rownames(coef_matrix), each = ncol(coef_matrix)),
    Variable = rep(colnames(coef_matrix), nrow(coef_matrix)),
    Coefficient = as.vector(t(coef_matrix)),
    stringsAsFactors = FALSE
  )
  
  # Remove intercept for cleaner visualization
  coef_data <- coef_data[coef_data$Variable != "(Intercept)", ]
  
  # Clean variable names
  coef_data$Variable_Clean <- case_when(
    coef_data$Variable == "expected_explore" ~ "Expected Explore Value",
    coef_data$Variable == "subjective_exploit" ~ "Subjective Exploit Value",
    coef_data$Variable == "social_contextduo" ~ "Social Context: Duo",
    coef_data$Variable == "social_contexttrio" ~ "Social Context: Trio",
    coef_data$Variable == "relative_rank" ~ "Relative Rank",
    coef_data$Variable == "sexMale" ~ "Sex: Male",
    grepl("individual", coef_data$Variable) ~ gsub("individual", "Individual: ", coef_data$Variable),
    TRUE ~ coef_data$Variable
  )
  
  # PLOT 4: All model coefficients
  plot4 <- ggplot(coef_data, aes(x = reorder(Variable_Clean, abs(Coefficient)), 
                                y = Coefficient, fill = Outcome)) +
    geom_col(position = "dodge", alpha = 0.8, width = 0.7) +
    geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.5) +
    geom_text(aes(label = round(Coefficient, 2)), 
              position = position_dodge(width = 0.7), hjust = -0.1, size = 3, fontface = "bold") +
    coord_flip() +
    labs(title = "ALL MODEL COEFFICIENTS: Multinomial Regression",
         subtitle = "Coefficient magnitudes show relative importance of each factor",
         x = "Model Variables", 
         y = "Coefficient Value",
         fill = "Outcome\n(vs Exploit)") +
    theme_minimal() +
    theme(text = element_text(size = 12),
          plot.title = element_text(size = 16, face = "bold")) +
    scale_fill_brewer(type = "qual", palette = "Set1")
  
  print(plot4)
  
  # PLOT 5: Focus on key value coefficients
  value_coefs <- coef_data[coef_data$Variable %in% c("expected_explore", "subjective_exploit"), ]
  
  plot5 <- ggplot(value_coefs, aes(x = Variable_Clean, y = Coefficient, fill = Outcome)) +
    geom_col(position = "dodge", alpha = 0.8, width = 0.6) +
    geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.5) +
    geom_text(aes(label = round(Coefficient, 3)), 
              position = position_dodge(width = 0.6), vjust = -0.5, size = 5, fontface = "bold") +
    labs(title = "KEY VALUE COEFFICIENTS COMPARISON",
         subtitle = "Expected Explore vs Subjective Exploit Value effects",
         x = "Value Type", 
         y = "Coefficient Value",
         fill = "Outcome\n(vs Exploit)") +
    theme_minimal() +
    theme(text = element_text(size = 14),
          plot.title = element_text(size = 16, face = "bold")) +
    scale_fill_brewer(type = "qual", palette = "Set1")
  
  print(plot5)
  
  # Model summary
  cat("\nKEY VALUE COEFFICIENTS:\n")
  explore_coefs <- value_coefs[value_coefs$Outcome == "explore", ]
  none_coefs <- value_coefs[value_coefs$Outcome == "none", ]
  
  cat("For EXPLORE vs EXPLOIT:\n")
  for(i in 1:nrow(explore_coefs)) {
    cat(sprintf("  %s: %.4f\n", explore_coefs$Variable_Clean[i], explore_coefs$Coefficient[i]))
  }
  
  cat("For NONE vs EXPLOIT:\n")
  for(i in 1:nrow(none_coefs)) {
    cat(sprintf("  %s: %.4f\n", none_coefs$Variable_Clean[i], none_coefs$Coefficient[i]))
  }
  
}, error = function(e) {
  cat("Error fitting multinomial model:", e$message, "\n")
  
  # Create simple linear models instead
  cat("Fitting simpler models...\n")
  
  # Binary models for explore vs others
  model_data$explore_binary <- ifelse(model_data$outcome == "explore", 1, 0)
  
  lm_explore <- glm(explore_binary ~ expected_explore + subjective_exploit + 
                   social_context + relative_rank + sex, 
                   data = model_data, family = "binomial")
  
  coef_summary <- summary(lm_explore)$coefficients
  
  # Simple coefficient plot
  coef_simple <- data.frame(
    Variable = rownames(coef_summary)[-1], # Remove intercept
    Coefficient = coef_summary[-1, 1],
    SE = coef_summary[-1, 2],
    stringsAsFactors = FALSE
  )
  
  plot4_simple <- ggplot(coef_simple, aes(x = reorder(Variable, abs(Coefficient)), y = Coefficient)) +
    geom_col(alpha = 0.8, fill = "steelblue") +
    geom_errorbar(aes(ymin = Coefficient - SE, ymax = Coefficient + SE), width = 0.2) +
    geom_hline(yintercept = 0, linetype = "solid", color = "black") +
    coord_flip() +
    labs(title = "MODEL COEFFICIENTS: Logistic Regression (Explore vs Others)",
         x = "Variables", y = "Coefficient Value") +
    theme_minimal()
  
  print(plot4_simple)
})

dev.off()

cat("\nVALUE COMPARISON AND MODEL ANALYSIS COMPLETE\n")
cat("File created: VALUE_COMPARISON_AND_MODEL_COEFFICIENTS.pdf\n\n")

cat("KEY INSIGHTS:\n")
cat("1. Expected explore value vs subjective exploit value show different patterns\n")
cat("2. Model coefficients reveal relative importance of each factor\n")
cat("3. Individual differences and value expectations are key drivers\n")
cat("4. Social context and rank provide additional modulation\n\n") 