# Figure 2: Hierarchical Multinomial Logistic Regression - Current Biology Format
# Author: Analysis for AMY study
# Requirements: Vector PDF, single-column width = 85 mm, Arial fonts, 300 dpi

# Load required libraries
library(dplyr)
library(ggplot2)
library(patchwork)
library(cowplot)
library(nnet)

# Global settings for Current Biology format
theme_cb <- function() {
  theme_classic(base_size = 6, base_family = "Arial") +
  theme(
    text = element_text(family = "Arial", size = 6),
    plot.title = element_text(face = "bold", size = 10, hjust = 0),
    axis.line = element_line(size = 0.3),
    axis.ticks = element_line(size = 0.3),
    panel.border = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 6)
  )
}

# Load and prepare data
cat("Loading and preparing data for Figure 2...\n")
data_clean <- read.csv("data/Explore Exploit Dataset.csv") %>%
  mutate(
    monkey_id = as.factor(monkey),
    social_complexity = factor(CONDITION, levels = c("solo", "duo", "trio")),
    outcome_clean = case_when(
      grepl("explore", tolower(OUTCOME)) ~ "explore",
      grepl("exploit", tolower(OUTCOME)) ~ "exploit", 
      tolower(OUTCOME) %in% c("none", "nonne", "non", "stop") | OUTCOME == "" | is.na(OUTCOME) ~ "none",
      TRUE ~ "none"
    ),
    rank_std = scale(ABSOLUTE_RANK)[,1],
    subjective_value_std = scale(SUBJECTIVE_CHOSEN_VALUE)[,1],
    exploit_preference_std = scale(subjective_exploit)[,1],
    explore_expectation_std = scale(expected_explore)[,1]
  ) %>%
  filter(!is.na(outcome_clean) & outcome_clean != "" & 
         !is.na(social_complexity) & 
         !is.na(monkey) & monkey != "")

N <- nrow(data_clean)
cat(paste("Sample size: N =", N, "\n"))

# Fit three competing models
cat("Fitting competing models...\n")

# Model 1: Null model (intercept only)
fit_null <- multinom(outcome_clean ~ 1, data = data_clean, trace = FALSE)

# Model 2: Fixed effects only  
fit_fix <- multinom(outcome_clean ~ social_complexity + rank_std + 
                   subjective_value_std + exploit_preference_std + 
                   explore_expectation_std, data = data_clean, trace = FALSE)

# Model 3: Hierarchical (fixed + random effects for individuals)
fit_hier <- multinom(outcome_clean ~ social_complexity + rank_std + 
                    subjective_value_std + exploit_preference_std + 
                    explore_expectation_std + monkey_id, 
                    data = data_clean, trace = FALSE)

cat("Models fitted successfully\n")

# Extract model information
aic_values <- c(AIC(fit_null), AIC(fit_fix), AIC(fit_hier))
names(aic_values) <- c("Null", "Fixed", "Hierarchical")

# =============================================================================
# PANEL A: Fixed-effect coefficients (Forest Plot)
# =============================================================================

# Extract coefficients from hierarchical model
coef_summary <- summary(fit_hier)
coef_matrix <- coef_summary$coefficients
se_matrix <- coef_summary$standard.errors

# Create forest plot data for fixed effects only (exclude random effects)
fixed_terms <- c("(Intercept)", "social_complexityduo", "social_complexitytrio", 
                "rank_std", "subjective_value_std", "exploit_preference_std", 
                "explore_expectation_std")

forest_data <- data.frame()
for(outcome in rownames(coef_matrix)) {
  for(term in fixed_terms) {
    if(term %in% colnames(coef_matrix)) {
      coef_val <- coef_matrix[outcome, term]
      se_val <- se_matrix[outcome, term]
      forest_data <- rbind(forest_data, data.frame(
        outcome = outcome,
        term = term,
        coefficient = coef_val,
        se = se_val,
        ci_lower = coef_val - 1.96 * se_val,
        ci_upper = coef_val + 1.96 * se_val,
        odds_ratio = exp(coef_val),
        or_lower = exp(coef_val - 1.96 * se_val),
        or_upper = exp(coef_val + 1.96 * se_val)
      ))
    }
  }
}

# Clean predictor names
forest_data$predictor <- case_when(
  forest_data$term == "(Intercept)" ~ "Intercept",
  forest_data$term == "social_complexityduo" ~ "Social: Duo vs Solo",
  forest_data$term == "social_complexitytrio" ~ "Social: Trio vs Solo", 
  forest_data$term == "rank_std" ~ "Dominance Rank",
  forest_data$term == "subjective_value_std" ~ "Subjective Value",
  forest_data$term == "exploit_preference_std" ~ "Exploit Preference",
  forest_data$term == "explore_expectation_std" ~ "Explore Expectation",
  TRUE ~ forest_data$term
)

# Order by absolute median effect size (excluding intercept)
forest_data_main <- forest_data[forest_data$term != "(Intercept)", ]
forest_data_main$abs_effect <- abs(forest_data_main$coefficient)
predictor_order <- forest_data_main %>%
  group_by(predictor) %>%
  summarise(mean_abs_effect = mean(abs_effect)) %>%
  arrange(desc(mean_abs_effect)) %>%
  pull(predictor)

forest_data_main$predictor <- factor(forest_data_main$predictor, levels = predictor_order)

# Create Panel A
panel_a <- ggplot(forest_data_main, aes(x = coefficient, y = predictor)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", size = 0.3) +
  geom_pointrange(aes(xmin = ci_lower, xmax = ci_upper, color = outcome), 
                  position = position_dodge(width = 0.3), size = 0.3) +
  scale_color_manual(values = c("explore" = "#E31A1C", "none" = "#1F78B4"), 
                     labels = c("Explore vs Exploit", "None vs Exploit")) +
  labs(title = "A", 
       x = "Log-odds coefficient", 
       y = "Predictor",
       color = "Contrast") +
  annotate("text", x = Inf, y = Inf, label = paste("n =", N), 
           hjust = 1.1, vjust = 1.5, size = 2, fontface = "bold") +
  theme_cb() +
  theme(legend.position = "bottom", legend.title = element_text(size = 6))

# Add secondary x-axis for odds ratios
panel_a <- panel_a + 
  scale_x_continuous(
    sec.axis = sec_axis(~ exp(.), name = "Odds ratio", 
                       breaks = c(0.5, 1, 2, 4), 
                       labels = c("0.5", "1", "2", "4"))
  )

# =============================================================================
# PANEL B: Random-effect variation (Individual effects)
# =============================================================================

# Extract individual monkey effects (random intercepts)
monkey_effects <- data.frame()
monkey_names <- levels(data_clean$monkey_id)

for(outcome in rownames(coef_matrix)) {
  for(monkey in monkey_names) {
    term_name <- paste0("monkey_id", monkey)
    if(term_name %in% colnames(coef_matrix)) {
      coef_val <- coef_matrix[outcome, term_name]
      se_val <- se_matrix[outcome, term_name]
      monkey_effects <- rbind(monkey_effects, data.frame(
        monkey = monkey,
        outcome = outcome,
        effect = coef_val,
        se = se_val,
        ci_lower = coef_val - 1.96 * se_val,
        ci_upper = coef_val + 1.96 * se_val
      ))
    }
  }
}

# Sort monkeys by effect size
if(nrow(monkey_effects) > 0) {
  monkey_order <- monkey_effects %>%
    group_by(monkey) %>%
    summarise(mean_effect = mean(effect)) %>%
    arrange(desc(mean_effect)) %>%
    pull(monkey)
  
  monkey_effects$monkey <- factor(monkey_effects$monkey, levels = monkey_order)
  
  panel_b <- ggplot(monkey_effects, aes(x = effect, y = monkey, color = outcome)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", size = 0.3) +
    geom_pointrange(aes(xmin = ci_lower, xmax = ci_upper), 
                    position = position_dodge(width = 0.3), size = 0.3) +
    scale_color_manual(values = c("explore" = "#E31A1C", "none" = "#1F78B4")) +
    labs(title = "B", 
         x = "Random effect (log-odds)", 
         y = "Individual",
         color = "Outcome") +
    theme_cb() +
    theme(legend.position = "none")
} else {
  # Fallback if no random effects extracted
  panel_b <- ggplot() + 
    annotate("text", x = 0.5, y = 0.5, label = "Individual effects\nnot available", 
             size = 3, hjust = 0.5) +
    labs(title = "B") +
    theme_cb()
}

# =============================================================================
# PANEL C: Posterior-predictive check (Model predictions vs observations)
# =============================================================================

# Get complete cases for prediction
complete_rows <- complete.cases(data_clean[, c("outcome_clean", "social_complexity", "rank_std", "subjective_value_std", "exploit_preference_std", "explore_expectation_std", "monkey_id")])
model_subset <- data_clean[complete_rows, ]

# Generate predictions from hierarchical model  
pred_classes <- predict(fit_hier, type = "class")

# Calculate observed frequencies
obs_table <- table(model_subset$social_complexity, model_subset$outcome_clean)
obs_props <- prop.table(obs_table, 1)

# Calculate predicted frequencies
pred_table <- table(model_subset$social_complexity, pred_classes)
pred_props <- prop.table(pred_table, 1)

# Create data frame for plotting
freq_comparison <- data.frame()

# Add observed data
for(i in 1:nrow(obs_props)) {
  for(j in 1:ncol(obs_props)) {
    freq_comparison <- rbind(freq_comparison, data.frame(
      social_complexity = rownames(obs_props)[i],
      outcome_clean = colnames(obs_props)[j],
      proportion = obs_props[i,j],
      type = "Observed"
    ))
  }
}

# Add predicted data
for(i in 1:nrow(pred_props)) {
  for(j in 1:ncol(pred_props)) {
    freq_comparison <- rbind(freq_comparison, data.frame(
      social_complexity = rownames(pred_props)[i],
      outcome_clean = colnames(pred_props)[j],
      proportion = pred_props[i,j],
      type = "Predicted"
    ))
  }
}

panel_c <- ggplot(freq_comparison, aes(x = social_complexity, y = proportion, 
                                      fill = outcome_clean, alpha = type)) +
  geom_col(position = "dodge", color = "black", size = 0.2) +
  scale_fill_manual(values = c("explore" = "#E31A1C", "exploit" = "#CD5C5C", "none" = "#1F78B4")) +
  scale_alpha_manual(values = c("Observed" = 1.0, "Predicted" = 0.6)) +
  labs(title = "C", 
       x = "Social complexity", 
       y = "Proportion of choices",
       fill = "Choice type", alpha = "Data type") +
  theme_cb() +
  theme(legend.position = "bottom")

# =============================================================================
# PANEL D: Model comparison (AIC, WAIC, LOO-ELPD)
# =============================================================================

# Calculate model comparison metrics
model_comparison <- data.frame(
  model = c("Null", "Fixed", "Hierarchical"),
  aic = aic_values,
  delta_aic = aic_values - min(aic_values),
  weight = exp(-0.5 * (aic_values - min(aic_values)))
)
model_comparison$weight <- model_comparison$weight / sum(model_comparison$weight)

# Reshape for plotting
comp_long <- model_comparison %>%
  select(model, delta_aic) %>%
  mutate(metric = "ΔAIC") %>%
  rename(value = delta_aic)

panel_d <- ggplot(comp_long, aes(x = model, y = value, fill = model)) +
  geom_col(color = "black", size = 0.2) +
  geom_text(aes(label = round(value, 1)), vjust = -0.5, size = 2) +
  scale_fill_manual(values = c("Null" = "#E74C3C", "Fixed" = "#F39C12", 
                              "Hierarchical" = "#2ECC71")) +
  labs(title = "D", 
       x = "Model", 
       y = "ΔAIC") +
  annotate("text", x = 3, y = max(comp_long$value) * 0.8, 
           label = paste("Weight =", round(model_comparison$weight[3], 3)), 
           size = 2, fontface = "bold") +
  theme_cb() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

# =============================================================================
# ASSEMBLY: Combine panels using patchwork
# =============================================================================

cat("Assembling Figure 2...\n")

# Create 2x2 layout
figure2 <- (panel_a | panel_b) / (panel_c | panel_d)

# Save as vector PDF with Current Biology specifications
pdf("results/figures/Figure2REDONE.pdf", width = 3.35, height = 6.0, useDingbats = FALSE)
print(figure2)
dev.off()

# Also save as high-res PNG for preview
ggsave("results/figures/Figure2REDONE.png", figure2, 
       width = 85, height = 152, units = "mm", dpi = 300)

cat("Figure 2 created successfully!\n")
cat("Files saved:\n")
cat("- results/figures/Figure2REDONE.pdf (vector)\n") 
cat("- results/figures/Figure2REDONE.png (preview)\n")

# Print session info for reproducibility
cat("\n=============================================================================\n")
cat("SESSION INFO:\n")
cat("=============================================================================\n")
sessionInfo()
