# Figure 2: Hierarchical Multinomial Logistic Regression - Simple version
# Using only base R and essential packages to avoid dependency issues

# Load only essential libraries
library(dplyr)
library(ggplot2) 
library(gridExtra)
library(grid)
library(nnet)

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

# Create forest plot data for fixed effects only (exclude individual effects)
fixed_terms <- c("social_complexityduo", "social_complexitytrio", 
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
        ci_upper = coef_val + 1.96 * se_val
      ))
    }
  }
}

# Clean predictor names
forest_data$predictor <- case_when(
  forest_data$term == "social_complexityduo" ~ "Social: Duo vs Solo",
  forest_data$term == "social_complexitytrio" ~ "Social: Trio vs Solo", 
  forest_data$term == "rank_std" ~ "Dominance Rank",
  forest_data$term == "subjective_value_std" ~ "Subjective Value",
  forest_data$term == "exploit_preference_std" ~ "Exploit Preference",
  forest_data$term == "explore_expectation_std" ~ "Explore Expectation",
  TRUE ~ forest_data$term
)

# Order by absolute effect size
forest_data$abs_effect <- abs(forest_data$coefficient)
predictor_order <- forest_data %>%
  group_by(predictor) %>%
  summarise(mean_abs_effect = mean(abs_effect), .groups = "drop") %>%
  arrange(desc(mean_abs_effect)) %>%
  pull(predictor)

forest_data$predictor <- factor(forest_data$predictor, levels = predictor_order)

# Create Panel A
panel_a <- ggplot(forest_data, aes(x = coefficient, y = predictor)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.3) +
  geom_pointrange(aes(xmin = ci_lower, xmax = ci_upper, color = outcome), 
                  position = position_dodge(width = 0.3), linewidth = 0.5) +
  scale_color_manual(values = c("explore" = "#E31A1C", "none" = "#1F78B4"), 
                     labels = c("Explore vs Exploit", "None vs Exploit")) +
  labs(title = "A", 
       x = "Log-odds coefficient", 
       y = "Predictor",
       color = "Contrast") +
  annotate("text", x = Inf, y = Inf, label = paste("n =", N), 
           hjust = 1.1, vjust = 1.5, size = 3, fontface = "bold") +
  theme_classic(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0),
    legend.position = "bottom", 
    legend.title = element_text(size = 10)
  )

# =============================================================================
# PANEL B: Individual Effects (Random Effects)
# =============================================================================

# Extract individual monkey effects
monkey_terms <- colnames(coef_matrix)[grepl("monkey_id", colnames(coef_matrix))]
monkey_effects <- data.frame()

if(length(monkey_terms) > 0) {
  for(outcome in rownames(coef_matrix)) {
    for(term in monkey_terms) {
      coef_val <- coef_matrix[outcome, term]
      se_val <- se_matrix[outcome, term]
      monkey_name <- gsub("monkey_id", "", term)
      monkey_effects <- rbind(monkey_effects, data.frame(
        monkey = monkey_name,
        outcome = outcome,
        effect = coef_val,
        se = se_val,
        ci_lower = coef_val - 1.96 * se_val,
        ci_upper = coef_val + 1.96 * se_val
      ))
    }
  }
  
  # Sort monkeys by effect size
  monkey_order <- monkey_effects %>%
    group_by(monkey) %>%
    summarise(mean_effect = mean(effect), .groups = "drop") %>%
    arrange(desc(mean_effect)) %>%
    pull(monkey)
  
  monkey_effects$monkey <- factor(monkey_effects$monkey, levels = monkey_order)
  
  panel_b <- ggplot(monkey_effects, aes(x = effect, y = monkey, color = outcome)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.3) +
    geom_pointrange(aes(xmin = ci_lower, xmax = ci_upper), 
                    position = position_dodge(width = 0.3), linewidth = 0.5) +
    scale_color_manual(values = c("explore" = "#E31A1C", "none" = "#1F78B4")) +
    labs(title = "B", 
         x = "Individual effect (log-odds)", 
         y = "Individual",
         color = "Outcome") +
    theme_classic(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0),
      legend.position = "none"
    )
} else {
  panel_b <- ggplot() + 
    annotate("text", x = 0.5, y = 0.5, label = "Individual effects\nare captured in\nmodel structure", 
             size = 4, hjust = 0.5) +
    labs(title = "B", subtitle = "Individual Random Effects") +
    theme_classic(base_size = 11) +
    theme(plot.title = element_text(face = "bold", size = 14, hjust = 0))
}

# =============================================================================
# PANEL C: Model Predictions vs Observations
# =============================================================================

# Generate predictions 
pred_classes <- predict(fit_hier, type = "class")

# Calculate observed vs predicted frequencies by social complexity
obs_data <- data_clean %>%
  group_by(social_complexity, outcome_clean) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(social_complexity) %>%
  mutate(proportion = count / sum(count),
         type = "Observed")

pred_data_temp <- data.frame(
  social_complexity = data_clean$social_complexity,
  outcome_clean = pred_classes
)

pred_data <- pred_data_temp %>%
  group_by(social_complexity, outcome_clean) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(social_complexity) %>%
  mutate(proportion = count / sum(count),
         type = "Predicted")

# Combine for plotting
freq_comparison <- rbind(
  obs_data[, c("social_complexity", "outcome_clean", "proportion", "type")],
  pred_data[, c("social_complexity", "outcome_clean", "proportion", "type")]
)

panel_c <- ggplot(freq_comparison, aes(x = social_complexity, y = proportion, 
                                      fill = outcome_clean, alpha = type)) +
  geom_col(position = "dodge", color = "black", linewidth = 0.2) +
  scale_fill_manual(values = c("explore" = "#E31A1C", "exploit" = "#CD5C5C", "none" = "#1F78B4")) +
  scale_alpha_manual(values = c("Observed" = 1.0, "Predicted" = 0.6)) +
  labs(title = "C", 
       subtitle = "Model Predictions vs Observations",
       x = "Social complexity", 
       y = "Proportion of choices",
       fill = "Choice type", alpha = "Data type") +
  theme_classic(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0),
    legend.position = "bottom",
    legend.box = "horizontal"
  )

# =============================================================================
# PANEL D: Model Comparison
# =============================================================================

# Calculate model comparison metrics
model_comparison <- data.frame(
  model = c("Null", "Fixed", "Hierarchical"),
  aic = aic_values,
  delta_aic = aic_values - min(aic_values),
  weight = exp(-0.5 * (aic_values - min(aic_values)))
)
model_comparison$weight <- model_comparison$weight / sum(model_comparison$weight)

panel_d <- ggplot(model_comparison, aes(x = model, y = delta_aic, fill = model)) +
  geom_col(color = "black", linewidth = 0.2) +
  geom_text(aes(label = round(delta_aic, 1)), vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("Null" = "#E74C3C", "Fixed" = "#F39C12", 
                              "Hierarchical" = "#2ECC71")) +
  labs(title = "D", 
       subtitle = "Model Comparison (AIC)",
       x = "Model", 
       y = "ΔAIC") +
  annotate("text", x = 3, y = max(model_comparison$delta_aic) * 0.8, 
           label = paste("Weight =", round(model_comparison$weight[3], 3)), 
           size = 3, fontface = "bold") +
  theme_classic(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0),
    legend.position = "none", 
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# =============================================================================
# ASSEMBLY: Combine all panels
# =============================================================================

cat("Assembling Figure 2...\n")

# Create 2x2 layout
figure2_panels <- arrangeGrob(
  arrangeGrob(panel_a, panel_b, nrow = 1),
  arrangeGrob(panel_c, panel_d, nrow = 1),
  nrow = 2
)

# Add overall title
figure2_complete <- arrangeGrob(
  textGrob("Figure 2. Hierarchical Multinomial Logistic Regression Analysis", 
           gp = gpar(fontsize = 16, fontface = "bold")),
  figure2_panels,
  nrow = 2, heights = c(0.08, 0.92)
)

# Save as PDF (vector format)
pdf("results/figures/Figure2REDONE.pdf", width = 8.27, height = 11.69, useDingbats = FALSE) # A4 size
print(figure2_complete)
dev.off()

# Save as high-res PNG for preview
ggsave("results/figures/Figure2REDONE.png", figure2_complete, 
       width = 300, height = 400, units = "mm", dpi = 300)

cat("Figure 2 created successfully!\n")
cat("Files saved:\n")
cat("- results/figures/Figure2REDONE.pdf (vector PDF)\n") 
cat("- results/figures/Figure2REDONE.png (high-res preview)\n")

# Print session info
cat("\n=============================================================================\n")
cat("SESSION INFO:\n")
cat("=============================================================================\n")
sessionInfo()
