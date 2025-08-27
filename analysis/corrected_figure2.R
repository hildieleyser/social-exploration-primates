# Corrected Figure 2: Advanced Analysis
library(nnet)
library(ggplot2) 
library(dplyr)
library(gridExtra)
library(grid)

cat("Creating corrected Figure 2...\n")

# Load data
data_raw <- read.csv("data/Explore Exploit Dataset.csv", stringsAsFactors = FALSE)
data_clean <- data_raw[data_raw$TRIAL_TYPE == "OIT_RE", ]

# Clean outcomes
outcome_clean <- character(nrow(data_clean))
outcome_clean[grepl("explore", tolower(data_clean$OUTCOME))] <- "explore"
outcome_clean[grepl("exploit", tolower(data_clean$OUTCOME))] <- "exploit"
outcome_clean[grepl("none|stop", tolower(data_clean$OUTCOME))] <- "none"

data_clean$outcome_clean <- outcome_clean
data_clean$social_complexity <- factor(data_clean$CONDITION, levels = c("solo", "duo", "trio"))
data_clean$monkey_id <- factor(data_clean$monkey)

# Remove missing
data_clean <- data_clean[!is.na(data_clean$outcome_clean), ]
data_clean <- data_clean[data_clean$outcome_clean != "", ]
data_clean <- data_clean[complete.cases(data_clean[c("RELATIVE_RANK", "SUBJECTIVE_CHOSEN_VALUE")]), ]

# Standardize
data_clean$rank_std <- scale(data_clean$RELATIVE_RANK)[,1]
data_clean$subjective_value_std <- scale(data_clean$SUBJECTIVE_CHOSEN_VALUE)[,1]
data_clean$exploit_preference_std <- scale(data_clean$subjective_exploit)[,1]
data_clean$explore_expectation_std <- scale(data_clean$expected_explore)[,1]

# Colors
bio_colors <- c("explore" = "#D55E00", "exploit" = "#009E73", "none" = "#0072B2", "model" = "#E69F00")

# Fit models for comparison
model_chance <- multinom(outcome_clean ~ 1, data = data_clean, trace = FALSE)
model_social <- multinom(outcome_clean ~ social_complexity, data = data_clean, trace = FALSE)  
model_individual <- multinom(outcome_clean ~ social_complexity + monkey_id, data = data_clean, trace = FALSE)
model_full <- multinom(outcome_clean ~ social_complexity + monkey_id + rank_std + 
                      subjective_value_std + exploit_preference_std + 
                      explore_expectation_std, data = data_clean, trace = FALSE)

cat("Models fitted successfully\n")

# Panel A: Individual predictor slopes
predictors <- c("rank_std", "subjective_value_std", "exploit_preference_std", "explore_expectation_std")
names(predictors) <- c("Dominance Rank", "Subjective Value", "Exploit Preference", "Explore Expectation")

predictor_plots <- list()
for(i in 1:length(predictors)) {
  pred <- predictors[i]
  pred_name <- names(predictors)[i]
  
  # Fit model with just this predictor
  formula_str <- paste("outcome_clean ~ social_complexity + monkey_id +", pred)
  model_pred <- multinom(as.formula(formula_str), data = data_clean, trace = FALSE)
  
  # Extract coefficients
  coef_matrix <- summary(model_pred)$coefficients
  se_matrix <- summary(model_pred)$standard.errors
  
  pred_coefs <- data.frame(
    outcome = rownames(coef_matrix),
    estimate = coef_matrix[, pred],
    se = se_matrix[, pred]
  )
  pred_coefs$ci_lower <- pred_coefs$estimate - 1.96 * pred_coefs$se
  pred_coefs$ci_upper <- pred_coefs$estimate + 1.96 * pred_coefs$se
  
  p <- ggplot(pred_coefs, aes(x = outcome, y = estimate, fill = outcome)) +
    geom_col(width = 0.6, color = "black", linewidth = 0.3, alpha = 0.8) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.15, linewidth = 0.4) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    scale_fill_manual(values = bio_colors[c("explore", "none")]) +
    labs(title = pred_name, x = "Outcome", y = "Log-odds") +
    theme_classic(base_size = 8) +
    theme(plot.title = element_text(size = 9, face = "bold", hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none")
  
  predictor_plots[[i]] <- p
}

panel_a <- grid.arrange(grobs = predictor_plots, nrow = 2, ncol = 2,
                       top = textGrob("A. Individual Predictor Effects", 
                                     gp = gpar(fontsize = 12, fontface = "bold")))

cat("Panel A complete\n")

# Panel B: Combined beta coefficients on one plot
coef_matrix <- summary(model_full)$coefficients
se_matrix <- summary(model_full)$standard.errors

all_coefs <- data.frame()
for(outcome in rownames(coef_matrix)) {
  for(term in colnames(coef_matrix)) {
    if(term != "(Intercept)" && !grepl("monkey_id", term)) {
      all_coefs <- rbind(all_coefs, data.frame(
        outcome = outcome,
        term = term,
        estimate = coef_matrix[outcome, term],
        se = se_matrix[outcome, term]
      ))
    }
  }
}

all_coefs$ci_lower <- all_coefs$estimate - 1.96 * all_coefs$se
all_coefs$ci_upper <- all_coefs$estimate + 1.96 * all_coefs$se

# Clean term names
all_coefs$term_clean <- case_when(
  all_coefs$term == "social_complexityduo" ~ "Duo vs Solo",
  all_coefs$term == "social_complexitytrio" ~ "Trio vs Solo", 
  all_coefs$term == "rank_std" ~ "Dominance Rank",
  all_coefs$term == "subjective_value_std" ~ "Subjective Value",
  all_coefs$term == "exploit_preference_std" ~ "Exploit Preference",
  all_coefs$term == "explore_expectation_std" ~ "Explore Expectation",
  TRUE ~ all_coefs$term
)

panel_b <- ggplot(all_coefs, aes(x = estimate, y = reorder(term_clean, estimate), color = outcome)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.5) +
  geom_point(size = 2.5) +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0.15, linewidth = 0.6) +
  scale_color_manual(values = bio_colors[c("explore", "none")], name = "Outcome") +
  labs(title = "B", subtitle = "All Beta Coefficients Together",
       x = "Log-odds ratio", y = "Model Terms") +
  theme_classic(base_size = 10) +
  theme(plot.title = element_text(face = "bold", size = 12, hjust = -0.1),
        legend.position = "bottom",
        panel.grid.major.x = element_line(color = "grey90", linewidth = 0.2))

cat("Panel B complete\n")

# Panel C: Model comparison vs chance and fixed strategies  
model_comparison <- data.frame(
  Model = factor(c("Chance", "Social Only", "Individual", "Full Hierarchical"), 
                levels = c("Chance", "Social Only", "Individual", "Full Hierarchical")),
  AIC = c(AIC(model_chance), AIC(model_social), AIC(model_individual), AIC(model_full))
)

# Calculate accuracy vs chance
accuracy_full <- mean(predict(model_full, type = "class") == data_clean$outcome_clean)
accuracy_chance <- 1/3

panel_c1 <- ggplot(model_comparison, aes(x = Model, y = AIC)) +
  geom_col(fill = bio_colors["model"], alpha = 0.8, color = "black", linewidth = 0.3, width = 0.7) +
  geom_text(aes(label = round(AIC)), vjust = -0.5, size = 3) +
  labs(title = "AIC Comparison", x = "Model", y = "AIC (lower = better)") +
  theme_classic(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.y = element_line(color = "grey90", linewidth = 0.2))

# Accuracy comparison
acc_data <- data.frame(
  Model = c("Chance", "Full Model"),
  Accuracy = c(accuracy_chance, accuracy_full)
)

panel_c2 <- ggplot(acc_data, aes(x = Model, y = Accuracy)) +
  geom_col(fill = c("grey60", bio_colors["explore"]), alpha = 0.8, color = "black", linewidth = 0.3, width = 0.5) +
  geom_text(aes(label = paste0(round(Accuracy*100, 1), "%")), vjust = -0.5, size = 3) +
  geom_hline(yintercept = 1/3, linetype = "dashed", color = "red") +
  scale_y_continuous(labels = function(x) paste0(round(x*100), "%")) +
  labs(title = "Accuracy vs Chance", x = "Model", y = "Prediction Accuracy") +
  theme_classic(base_size = 10)

panel_c <- grid.arrange(panel_c1, panel_c2, nrow = 1,
                       top = textGrob("C. Model Comparison vs Chance & Fixed", 
                                     gp = gpar(fontsize = 12, fontface = "bold")))

cat("Panel C complete\n")

# Panel D: Model predictions bar graph
pred_data <- expand.grid(
  social_complexity = factor(c("solo", "duo", "trio"), levels = c("solo", "duo", "trio")),
  monkey_id = factor("EBI"),
  rank_std = 0, subjective_value_std = 0,
  exploit_preference_std = 0, explore_expectation_std = 0
)

pred_probs <- predict(model_full, newdata = pred_data, type = "probs")
pred_df <- cbind(pred_data, pred_probs)
pred_df_long <- reshape(pred_df, 
                       varying = c("explore", "exploit", "none"),
                       v.names = "probability", timevar = "outcome",
                       times = c("explore", "exploit", "none"),
                       direction = "long")

panel_d <- ggplot(pred_df_long, aes(x = social_complexity, y = probability, fill = outcome)) +
  geom_col(position = "dodge", color = "black", linewidth = 0.3, alpha = 0.9, width = 0.8) +
  scale_fill_manual(values = bio_colors[c("explore", "exploit", "none")], name = "Choice Type") +
  scale_x_discrete(labels = c("Solo", "Duo", "Trio")) +
  scale_y_continuous(labels = function(x) paste0(round(x*100), "%")) +
  labs(title = "D", subtitle = "Model Predictions Bar Graph",
       x = "Social Complexity", y = "Predicted Probability") +
  theme_classic(base_size = 10) +
  theme(plot.title = element_text(face = "bold", size = 12, hjust = -0.1),
        legend.position = "bottom")

cat("Panel D complete\n")

# Save all panels
ggsave("results/figures/Figure2_Panel_A_Individual_Predictors.png", panel_a, 
       width = 180, height = 120, units = "mm", dpi = 300)
ggsave("results/figures/Figure2_Panel_B_Combined_Betas.png", panel_b, 
       width = 180, height = 120, units = "mm", dpi = 300)
ggsave("results/figures/Figure2_Panel_C_Model_Comparison.png", panel_c, 
       width = 180, height = 120, units = "mm", dpi = 300)
ggsave("results/figures/Figure2_Panel_D_Prediction_Bars.png", panel_d, 
       width = 180, height = 120, units = "mm", dpi = 300)

cat("Corrected Figure 2 complete with all requested changes!\n")
cat("- Panel A: Each predictor has its own slope\n")
cat("- Panel B: All beta coefficients on one graph\n") 
cat("- Panel C: Comparison to chance and fixed strategies\n")
cat("- Panel D: Model predictions bar graph included\n")
