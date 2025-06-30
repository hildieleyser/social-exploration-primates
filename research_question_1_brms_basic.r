# RESEARCH QUESTION 1: BASIC BRMS VISUALIZATIONS
# Minimal version that definitely works

library(brms)
library(bayesplot)
library(ggplot2)

cat("=== BASIC BRMS ANALYSIS ===\n")

# Load and prepare data
data_raw <- read.csv("Explore Exploit Dataset.csv", stringsAsFactors = FALSE)
data_exp <- data_raw[data_raw$TRIAL_TYPE == "OIT_RE", ]
data_exp$outcome_clean <- ifelse(grepl("explore", data_exp$OUTCOME, ignore.case = TRUE), "explore",
                                ifelse(grepl("exploit", data_exp$OUTCOME, ignore.case = TRUE), "exploit", 
                                      ifelse(grepl("none|NONE", data_exp$OUTCOME, ignore.case = TRUE), "none", NA)))

data_clean <- data_exp[!is.na(data_exp$outcome_clean), ]
data_clean$outcome_clean <- factor(data_clean$outcome_clean, levels = c("exploit", "explore", "none"))
data_clean$monkey <- data_clean$monkey
data_clean$condition <- factor(data_clean$CONDITION, levels = c("solo", "duo", "trio"))
data_clean$sex <- ifelse(data_clean$monkey %in% c("FRAN", "DALI", "EBI"), "Male", "Female")
data_clean$sex <- factor(data_clean$sex, levels = c("Male", "Female"))
data_clean$relative_rank <- factor(data_clean$RELATIVE_RANK, levels = c(1, 2, 3))
data_clean$absolute_rank <- factor(data_clean$ABSOLUTE_RANK, levels = c(1, 2, 3))

model_data <- data_clean[complete.cases(data_clean[c("outcome_clean", "condition", "relative_rank", "absolute_rank", "sex", "monkey")]), ]

# Set up brms
options(mc.cores = parallel::detectCores())

priors_rel <- c(
  prior(normal(0, 1), class = Intercept, dpar = muexplore),
  prior(normal(0, 1), class = Intercept, dpar = munone),
  prior(normal(0, 0.5), class = b, dpar = muexplore),
  prior(normal(0, 0.5), class = b, dpar = munone),
  prior(exponential(1), class = sd, group = monkey, dpar = muexplore),
  prior(exponential(1), class = sd, group = monkey, dpar = munone)
)

cat("Fitting Bayesian models...\n")

bayesian_relative <- brm(outcome_clean ~ condition + relative_rank + sex + (1|monkey),
                        data = model_data, family = categorical(), prior = priors_rel,
                        iter = 1000, warmup = 500, chains = 2,
                        silent = 2, refresh = 0)

bayesian_absolute <- brm(outcome_clean ~ condition + absolute_rank + sex + (1|monkey),
                        data = model_data, family = categorical(), prior = priors_rel,
                        iter = 1000, warmup = 500, chains = 2,
                        silent = 2, refresh = 0)

cat("Models fitted successfully!\n")

# Create basic visualizations
pdf("RESEARCH_QUESTION_1_BRMS_RESULTS.pdf", width = 16, height = 12)

# 1. MCMC Traces
p1 <- mcmc_trace(bayesian_relative, pars = c("b_muexplore_relative_rank2", "b_muexplore_relative_rank3"))
print(p1 + ggtitle("A. MCMC Traces - Relative Rank Effects") + theme_minimal())

# 2. Posterior distributions comparison
p2 <- mcmc_areas(bayesian_relative, 
                pars = c("b_muexplore_relative_rank2", "b_muexplore_relative_rank3"))
print(p2 + ggtitle("B. Posterior Distributions - Relative Rank Effects") + theme_minimal())

p3 <- mcmc_areas(bayesian_absolute,
                pars = c("b_muexplore_absolute_rank2", "b_muexplore_absolute_rank3"))
print(p3 + ggtitle("C. Posterior Distributions - Absolute Rank Effects") + theme_minimal())

# 3. Conditional effects
ce_rel <- conditional_effects(bayesian_relative, effects = "relative_rank", categorical = TRUE)
p4 <- plot(ce_rel)[[1]] + 
  ggtitle("D. Conditional Effects - Relative Rank") + 
  theme_minimal()
print(p4)

ce_abs <- conditional_effects(bayesian_absolute, effects = "absolute_rank", categorical = TRUE)
p5 <- plot(ce_abs)[[1]] + 
  ggtitle("E. Conditional Effects - Absolute Rank") + 
  theme_minimal()
print(p5)

# 4. Posterior predictive checks
p6 <- pp_check(bayesian_relative, ndraws = 50) + 
  ggtitle("F. Posterior Predictive Check - Relative Rank Model") + 
  theme_minimal()
print(p6)

p7 <- pp_check(bayesian_absolute, ndraws = 50) + 
  ggtitle("G. Posterior Predictive Check - Absolute Rank Model") + 
  theme_minimal()
print(p7)

dev.off()

# Model comparison
cat("Computing LOO comparison...\n")
loo_rel <- loo(bayesian_relative)
loo_abs <- loo(bayesian_absolute)
loo_comp <- loo_compare(loo_rel, loo_abs)
model_weights_vals <- model_weights(bayesian_relative, bayesian_absolute, weights = "loo")

# Create simple comparison plot
pdf("BRMS_MODEL_COMPARISON_RESULTS.pdf", width = 10, height = 6)

loo_data <- data.frame(
  Model = c("Relative Rank", "Absolute Rank"),
  ELPD = c(loo_rel$estimates["elpd_loo", "Estimate"], 
           loo_abs$estimates["elpd_loo", "Estimate"]),
  SE = c(loo_rel$estimates["elpd_loo", "SE"], 
         loo_abs$estimates["elpd_loo", "SE"]),
  Weight = c(model_weights_vals[1], model_weights_vals[2])
)

p_comparison <- ggplot(loo_data, aes(x = Model, y = ELPD)) +
  geom_col(aes(fill = Weight), alpha = 0.8) +
  geom_errorbar(aes(ymin = ELPD - SE, ymax = ELPD + SE), width = 0.2, size = 1) +
  geom_text(aes(label = paste("Weight:", round(Weight, 3))), 
            vjust = -0.5, size = 5, fontface = "bold") +
  labs(title = "Bayesian Model Comparison - Research Question 1",
       subtitle = "Expected Log Predictive Density ± Standard Error",
       y = "ELPD (higher = better)",
       caption = "LOO Cross-Validation Results") +
  scale_fill_gradient(low = "#E3F2FD", high = "#1976D2", name = "Model Weight") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12, face = "bold")
  )

print(p_comparison)

dev.off()

# Results summary
cat("\n=== RESEARCH QUESTION 1 RESULTS ===\n")
cat("Files created:\n")
cat("- RESEARCH_QUESTION_1_BRMS_RESULTS.pdf\n")
cat("- BRMS_MODEL_COMPARISON_RESULTS.pdf\n")

cat("\n=== BAYESIAN MODEL COMPARISON ===\n")
print(loo_comp)

cat("\nModel Weights (LOO):\n")
cat("Relative Rank Model:", round(model_weights_vals[1], 4), "\n")
cat("Absolute Rank Model:", round(model_weights_vals[2], 4), "\n")

# Calculate effect sizes
posterior_rel <- as.data.frame(bayesian_relative)

cat("\n=== EFFECT SIZES (RELATIVE RANK MODEL) ===\n")
cat("Rank Effects (log-odds):\n")
cat("Rank 2 vs 1: Mean =", round(mean(posterior_rel$b_muexplore_relative_rank2), 3), 
    ", 95% CI = [", round(quantile(posterior_rel$b_muexplore_relative_rank2, 0.025), 3), ",", 
    round(quantile(posterior_rel$b_muexplore_relative_rank2, 0.975), 3), "]\n")
cat("Rank 3 vs 1: Mean =", round(mean(posterior_rel$b_muexplore_relative_rank3), 3), 
    ", 95% CI = [", round(quantile(posterior_rel$b_muexplore_relative_rank3, 0.025), 3), ",", 
    round(quantile(posterior_rel$b_muexplore_relative_rank3, 0.975), 3), "]\n")

cat("\nContext Effects (log-odds):\n")
cat("Duo vs Solo: Mean =", round(mean(posterior_rel$b_muexplore_conditionduo), 3), 
    ", 95% CI = [", round(quantile(posterior_rel$b_muexplore_conditionduo, 0.025), 3), ",", 
    round(quantile(posterior_rel$b_muexplore_conditionduo, 0.975), 3), "]\n")
cat("Trio vs Solo: Mean =", round(mean(posterior_rel$b_muexplore_conditiontrio), 3), 
    ", 95% CI = [", round(quantile(posterior_rel$b_muexplore_conditiontrio, 0.025), 3), ",", 
    round(quantile(posterior_rel$b_muexplore_conditiontrio, 0.975), 3), "]\n")

cat("\nSex Effect (log-odds):\n")
cat("Female vs Male: Mean =", round(mean(posterior_rel$b_muexplore_sexFemale), 3), 
    ", 95% CI = [", round(quantile(posterior_rel$b_muexplore_sexFemale, 0.025), 3), ",", 
    round(quantile(posterior_rel$b_muexplore_sexFemale, 0.975), 3), "]\n")

cat("\n=== RESEARCH QUESTIONS ANSWERED ===\n")
rank_effect_range <- abs(mean(posterior_rel$b_muexplore_relative_rank3) - 0)  # vs baseline
sex_effect <- abs(mean(posterior_rel$b_muexplore_sexFemale))

cat("1. Is rank more important than gender?\n")
if(rank_effect_range > sex_effect) {
  cat("   YES - Rank effect (", round(rank_effect_range, 3), ") > Sex effect (", round(sex_effect, 3), ")\n")
} else {
  cat("   NO - Sex effect (", round(sex_effect, 3), ") > Rank effect (", round(rank_effect_range, 3), ")\n")
}

cat("2. Relative vs Absolute rank models:\n")
if(model_weights_vals[1] > model_weights_vals[2]) {
  cat("   RELATIVE RANK WINS - Weight:", round(model_weights_vals[1], 3), "vs", round(model_weights_vals[2], 3), "\n")
} else {
  cat("   ABSOLUTE RANK WINS - Weight:", round(model_weights_vals[2], 3), "vs", round(model_weights_vals[1], 3), "\n")
}

cat("\nBRMS analysis complete!\n") 