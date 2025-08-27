# =============================================================================
# MODEL DIAGNOSTICS AND INTERPRETATION PLOTS
# =============================================================================

library(ggplot2)
library(dplyr)
library(nnet)
library(broom)
library(patchwork)
library(cowplot)
library(gridExtra)
library(reshape2)
library(scales)

cat("=== GENERATING MODEL DIAGNOSTICS PLOTS ===\n")

# Load data
raw_data <- read.csv("Explore Exploit Dataset.csv", stringsAsFactors = FALSE)

data_clean <- raw_data %>%
  filter(TRIAL_TYPE == "OIT_RE") %>%
  mutate(
    outcome = case_when(
      grepl("explore", tolower(OUTCOME)) ~ "Explore",
      grepl("exploit", tolower(OUTCOME)) ~ "Exploit", 
      grepl("none|stop|NONE", tolower(OUTCOME)) | OUTCOME == "" ~ "None",
      TRUE ~ "None"
    ),
    social_context = factor(CONDITION, levels = c("solo", "duo", "trio")),
    monkey_id = factor(monkey),
    social_complexity = as.numeric(social_context),
    rank_z = as.numeric(scale(ABSOLUTE_RANK)),
    expected_explore_z = as.numeric(scale(expected_explore)),
    subjective_exploit_z = as.numeric(scale(subjective_exploit))
  ) %>%
  filter(!is.na(outcome), !is.na(social_context), !is.na(monkey_id)) %>%
  filter(complete.cases(.[c("expected_explore_z", "subjective_exploit_z")]))

# Fit models
fit_null <- multinom(outcome ~ 1, data = data_clean, trace = FALSE)
fit_fix <- multinom(outcome ~ social_complexity + expected_explore_z + 
                   subjective_exploit_z + rank_z, 
                   data = data_clean, trace = FALSE)
fit_hier <- multinom(outcome ~ social_complexity + expected_explore_z + 
                    subjective_exploit_z + rank_z + monkey_id, 
                    data = data_clean, trace = FALSE)

# 1. AIC/BIC Model Comparison Plot
model_comparison <- data.frame(
  Model = c("Null", "Fixed Effects", "Hierarchical"),
  AIC = c(AIC(fit_null), AIC(fit_fix), AIC(fit_hier)),
  BIC = c(BIC(fit_null), BIC(fit_fix), BIC(fit_hier))
)
model_comparison_long <- melt(model_comparison, id.vars = "Model")
plot_aic_bic <- ggplot(model_comparison_long, aes(x = Model, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  labs(title = "AIC/BIC Model Comparison", y = "Value", x = "Model", fill = "Criterion") +
  theme_minimal()
ggsave("Plot_AIC_BIC.png", plot_aic_bic, width = 6, height = 4, dpi = 300)
ggsave("Plot_AIC_BIC.pdf", plot_aic_bic, width = 6, height = 4)

# 2. Coefficient Forest Plot
coef_df <- tidy(fit_hier, conf.int = TRUE)
coef_df <- coef_df %>% filter(term != "(Intercept)")
plot_coef <- ggplot(coef_df, aes(x = estimate, y = term, color = outcome)) +
  geom_point(position = position_dodge(width = 0.7), size = 2) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, position = position_dodge(width = 0.7)) +
  facet_wrap(~outcome, scales = "free_y") +
  labs(title = "Model Coefficient Forest Plot", x = "Estimate (log-odds)", y = "Predictor") +
  theme_minimal()
ggsave("Plot_Coefficient_Forest.png", plot_coef, width = 8, height = 5, dpi = 300)
ggsave("Plot_Coefficient_Forest.pdf", plot_coef, width = 8, height = 5)

# 3. Predictor Importance Plot (absolute standardized coefficients)
coef_df$abs_estimate <- abs(coef_df$estimate)
predictor_importance <- coef_df %>% group_by(term) %>% summarize(importance = mean(abs_estimate))
plot_importance <- ggplot(predictor_importance, aes(x = reorder(term, importance), y = importance)) +
  geom_col(fill = "#0073C2FF") +
  coord_flip() +
  labs(title = "Predictor Importance (Mean |Estimate|)", x = "Predictor", y = "Mean |Estimate| (log-odds)") +
  theme_minimal()
ggsave("Plot_Predictor_Importance.png", plot_importance, width = 6, height = 4, dpi = 300)
ggsave("Plot_Predictor_Importance.pdf", plot_importance, width = 6, height = 4)

# 4. Predicted Probability Curves (for expected_explore_z)
pred_grid <- expand.grid(
  expected_explore_z = seq(-2, 2, length.out = 100),
  social_complexity = 2,
  subjective_exploit_z = 0,
  rank_z = 0,
  monkey_id = levels(data_clean$monkey_id)[1]
)
pred_probs <- as.data.frame(predict(fit_hier, pred_grid, type = "probs"))
pred_grid$Exploit <- pred_probs$Exploit
pred_grid$Explore <- pred_probs$Explore
pred_grid$None <- pred_probs$None
plot_prob_curve <- ggplot(pred_grid, aes(x = expected_explore_z)) +
  geom_line(aes(y = Exploit, color = "Exploit"), size = 1) +
  geom_line(aes(y = Explore, color = "Explore"), size = 1) +
  geom_line(aes(y = None, color = "None"), size = 1) +
  labs(title = "Predicted Probability Curves (Expected Explore)", x = "Expected Explore (z)", y = "Predicted Probability", color = "Outcome") +
  scale_color_manual(values = c("Exploit" = "#0073C2FF", "Explore" = "#EFC000FF", "None" = "#868686FF")) +
  theme_minimal()
ggsave("Plot_Probability_Curves.png", plot_prob_curve, width = 7, height = 4, dpi = 300)
ggsave("Plot_Probability_Curves.pdf", plot_prob_curve, width = 7, height = 4)

# 5. Residual Diagnostics
resid_pearson <- residuals(fit_hier, type = "pearson")
resid_deviance <- residuals(fit_hier, type = "deviance")
plot_resid_hist <- ggplot(data.frame(resid_pearson), aes(x = resid_pearson)) +
  geom_histogram(bins = 30, fill = "#0073C2FF", color = "white") +
  labs(title = "Histogram of Pearson Residuals", x = "Pearson Residual", y = "Count") +
  theme_minimal()
ggsave("Plot_Residual_Histogram.png", plot_resid_hist, width = 6, height = 4, dpi = 300)
ggsave("Plot_Residual_Histogram.pdf", plot_resid_hist, width = 6, height = 4)

plot_resid_qq <- ggplot(data.frame(resid_pearson), aes(sample = resid_pearson)) +
  stat_qq() + stat_qq_line() +
  labs(title = "QQ Plot of Pearson Residuals", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()
ggsave("Plot_Residual_QQ.png", plot_resid_qq, width = 6, height = 4, dpi = 300)
ggsave("Plot_Residual_QQ.pdf", plot_resid_qq, width = 6, height = 4)

fitted_vals <- rowSums(predict(fit_hier, type = "probs") * (1:3))
plot_resid_fitted <- ggplot(data.frame(fitted = fitted_vals, resid = resid_pearson), aes(x = fitted, y = resid)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, color = "#EFC000FF") +
  labs(title = "Pearson Residuals vs Fitted Values", x = "Fitted Value", y = "Pearson Residual") +
  theme_minimal()
ggsave("Plot_Residuals_vs_Fitted.png", plot_resid_fitted, width = 6, height = 4, dpi = 300)
ggsave("Plot_Residuals_vs_Fitted.pdf", plot_resid_fitted, width = 6, height = 4)

# 6. Calibration Plot
pred_probs_all <- as.data.frame(predict(fit_hier, type = "probs"))
pred_probs_all$Observed <- as.numeric(data_clean$outcome == "Exploit")
pred_probs_all$Predicted <- pred_probs_all$Exploit
pred_probs_all$bin <- cut(pred_probs_all$Predicted, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE)
calib <- pred_probs_all %>% group_by(bin) %>% summarize(
  mean_pred = mean(Predicted),
  mean_obs = mean(Observed),
  n = n()
)
plot_calib <- ggplot(calib, aes(x = mean_pred, y = mean_obs)) +
  geom_point(size = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  labs(title = "Calibration Plot (Exploit)", x = "Mean Predicted Probability", y = "Observed Proportion") +
  theme_minimal()
ggsave("Plot_Calibration.png", plot_calib, width = 6, height = 4, dpi = 300)
ggsave("Plot_Calibration.pdf", plot_calib, width = 6, height = 4)

# 7. Cross-Validation Accuracy Plot
set.seed(123)
cv_results <- data.frame()
for(i in 1:5) {
  test_idx <- sample(1:nrow(data_clean), size = round(nrow(data_clean) * 0.2))
  train_data <- data_clean[-test_idx, ]
  test_data <- data_clean[test_idx, ]
  cv_fit <- multinom(outcome ~ social_complexity + expected_explore_z + subjective_exploit_z + rank_z + monkey_id, data = train_data, trace = FALSE)
  cv_pred <- predict(cv_fit, test_data, type = "class")
  acc <- mean(cv_pred == test_data$outcome)
  cv_results <- rbind(cv_results, data.frame(Fold = i, Accuracy = acc))
}
plot_cv <- ggplot(cv_results, aes(x = factor(Fold), y = Accuracy)) +
  geom_bar(stat = "identity", fill = "#0073C2FF", width = 0.6) +
  geom_hline(yintercept = mean(cv_results$Accuracy), linetype = "dashed", color = "red") +
  labs(title = "Cross-Validation Accuracy by Fold", x = "Fold", y = "Accuracy") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme_minimal()
ggsave("Plot_CV_Accuracy.png", plot_cv, width = 6, height = 4, dpi = 300)
ggsave("Plot_CV_Accuracy.pdf", plot_cv, width = 6, height = 4)

cat("All diagnostic plots saved as PNG and PDF.\n") 