# =============================================================================
# POSTER KEY FINDINGS PLOTS: Clean Classic Style
# =============================================================================

library(ggplot2)
library(dplyr)
library(nnet)
library(scales)
library(reshape2)

# Color palette
cb_colors <- c("Exploit" = "#0073C2FF", "Explore" = "#EFC000FF", "None" = "#868686FF")

# Theme for poster
theme_poster <- function() {
  theme_classic(base_size = 14) +
    theme(
      axis.title = element_text(face = "bold", size = 16),
      axis.text = element_text(size = 14),
      plot.title = element_text(face = "bold", size = 18, hjust = 0),
      legend.title = element_text(face = "bold", size = 14),
      legend.text = element_text(size = 13),
      legend.position = "right"
    )
}

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

# 1. Model Comparison (AIC/BIC)
model_comparison <- data.frame(
  Model = c("Null", "Fixed Effects", "Hierarchical"),
  AIC = c(AIC(fit_null), AIC(fit_fix), AIC(fit_hier)),
  BIC = c(BIC(fit_null), BIC(fit_fix), BIC(fit_hier))
)
model_comparison_long <- melt(model_comparison, id.vars = "Model")
plot_aic_bic <- ggplot(model_comparison_long, aes(x = Model, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  labs(title = "Model Comparison: AIC/BIC", y = "Value", x = "Model", fill = "Criterion") +
  theme_poster()
ggsave("Poster_AIC_BIC.png", plot_aic_bic, width = 6, height = 4, dpi = 300)
ggsave("Poster_AIC_BIC.pdf", plot_aic_bic, width = 6, height = 4)

# 2. Key Predictors’ Effects (Coefficient Plot)
coef_matrix <- summary(fit_hier)$coefficients
se_matrix <- summary(fit_hier)$standard.errors
coef_df <- data.frame()
for (outcome in rownames(coef_matrix)) {
  for (term in colnames(coef_matrix)) {
    if (term != "(Intercept)") {
      coef_val <- coef_matrix[outcome, term]
      se_val <- se_matrix[outcome, term]
      coef_df <- rbind(coef_df, data.frame(
        Outcome = outcome,
        Predictor = term,
        Estimate = coef_val,
        SE = se_val,
        Lower = coef_val - 1.96 * se_val,
        Upper = coef_val + 1.96 * se_val
      ))
    }
  }
}
plot_coef <- ggplot(coef_df, aes(x = Estimate, y = Predictor, color = Outcome)) +
  geom_point(position = position_dodge(width = 0.7), size = 3) +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper), height = 0.2, position = position_dodge(width = 0.7)) +
  facet_wrap(~Outcome, scales = "free_y") +
  labs(title = "Key Predictors’ Effects (log-odds)", x = "Estimate (log-odds)", y = "Predictor") +
  scale_color_manual(values = cb_colors) +
  theme_poster()
ggsave("Poster_Coefficient_Plot.png", plot_coef, width = 8, height = 5, dpi = 300)
ggsave("Poster_Coefficient_Plot.pdf", plot_coef, width = 8, height = 5)

# 3. Predicted Probability Curves (Expected Explore)
pred_grid <- expand.grid(
  expected_explore_z = seq(-2, 2, length.out = 100),
  social_complexity = 2,  # must match model
  subjective_exploit_z = 0,
  rank_z = 0,
  monkey_id = levels(data_clean$monkey_id)[1]
)
pred_probs <- as.data.frame(predict(fit_hier, pred_grid, type = "probs"))
pred_grid$Exploit <- pred_probs$Exploit
pred_grid$Explore <- pred_probs$Explore
pred_grid$None <- pred_probs$None
plot_prob_curve <- ggplot(pred_grid, aes(x = expected_explore_z)) +
  geom_line(aes(y = Exploit, color = "Exploit"), size = 1.2) +
  geom_line(aes(y = Explore, color = "Explore"), size = 1.2) +
  geom_line(aes(y = None, color = "None"), size = 1.2) +
  labs(title = "Predicted Probability by Expected Explore Value", x = "Expected Explore (z)", y = "Predicted Probability", color = "Outcome") +
  scale_color_manual(values = cb_colors) +
  theme_poster()
ggsave("Poster_Probability_Curves.png", plot_prob_curve, width = 7, height = 4, dpi = 300)
ggsave("Poster_Probability_Curves.pdf", plot_prob_curve, width = 7, height = 4)

# 4. Observed vs Predicted by Social Context
grouped <- data_clean %>%
  group_by(social_context, outcome) %>%
  summarise(Observed = n(), .groups = "drop") %>%
  group_by(social_context) %>%
  mutate(Observed = Observed / sum(Observed))
pred_grid2 <- expand.grid(
  social_complexity = c(1, 2, 3),
  expected_explore_z = 0,
  subjective_exploit_z = 0,
  rank_z = 0,
  monkey_id = levels(data_clean$monkey_id)[1]
)
pred_grid2$social_context <- factor(c("solo", "duo", "trio"), levels = c("solo", "duo", "trio"))[pred_grid2$social_complexity]
pred_probs2 <- as.data.frame(predict(fit_hier, pred_grid2, type = "probs"))
pred_long <- melt(pred_probs2)
pred_long$social_context <- rep(pred_grid2$social_context, each = 3)
pred_long$outcome <- rep(c("Exploit", "Explore", "None"), times = nrow(pred_grid2))
pred_long <- pred_long %>% filter(variable == outcome)
pred_long <- pred_long %>% select(social_context, outcome, Predicted = value)
plot_data <- left_join(grouped, pred_long, by = c("social_context", "outcome"))
plot_data_long <- melt(plot_data, id.vars = c("social_context", "outcome"), variable.name = "Type", value.name = "Proportion")
plot_obs_pred <- ggplot(plot_data_long, aes(x = social_context, y = Proportion, fill = outcome, alpha = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = cb_colors) +
  scale_alpha_manual(values = c(Observed = 1, Predicted = 0.5)) +
  labs(title = "Observed vs Predicted by Social Context", x = "Social Context", y = "Proportion", fill = "Outcome", alpha = "Type") +
  theme_poster()
ggsave("Poster_ObsPred_Context.png", plot_obs_pred, width = 7, height = 4, dpi = 300)
ggsave("Poster_ObsPred_Context.pdf", plot_obs_pred, width = 7, height = 4)

# 5. Individual Differences (Predicted by Monkey)
pred_grid3 <- expand.grid(
  social_complexity = 1,  # solo
  expected_explore_z = 0,
  subjective_exploit_z = 0,
  rank_z = 0,
  monkey_id = levels(data_clean$monkey_id)
)
pred_grid3$social_context <- factor("solo", levels = c("solo", "duo", "trio"))
pred_probs3 <- as.data.frame(predict(fit_hier, pred_grid3, type = "probs"))
pred_long3 <- melt(pred_probs3)
pred_long3$monkey_id <- rep(pred_grid3$monkey_id, each = 3)
pred_long3$outcome <- rep(c("Exploit", "Explore", "None"), times = nrow(pred_grid3))
pred_long3 <- pred_long3 %>% filter(variable == outcome)
pred_long3 <- pred_long3 %>% select(monkey_id, outcome, Predicted = value)
plot_indiv <- ggplot(pred_long3, aes(x = monkey_id, y = Predicted, fill = outcome)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = cb_colors) +
  labs(title = "Individual Differences (Predicted by Monkey)", x = "Monkey", y = "Predicted Proportion", fill = "Outcome") +
  theme_poster()
ggsave("Poster_Individual_Differences.png", plot_indiv, width = 7, height = 4, dpi = 300)
ggsave("Poster_Individual_Differences.pdf", plot_indiv, width = 7, height = 4)

cat("All poster key findings plots saved as PNG and PDF.\n") 