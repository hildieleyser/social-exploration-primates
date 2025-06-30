#!/usr/bin/env Rscript

# NESTED HIERARCHICAL TRINOMIAL REGRESSION STRUCTURE
# Complete model diagram with equations and research question

suppressMessages({
  library(ggplot2)
  library(dplyr)
  library(gridExtra)
  library(grid)
})

cat("=== HIERARCHICAL TRINOMIAL REGRESSION STRUCTURE ===\n")
cat("Creating comprehensive model diagram with equations and research question\n\n")

# Load data for context
data_raw <- read.csv("Explore Exploit Dataset.csv", stringsAsFactors = FALSE)
data_exp <- data_raw[data_raw$TRIAL_TYPE == "OIT_RE", ]

data_exp$outcome_clean <- ifelse(grepl("explore", data_exp$OUTCOME, ignore.case = TRUE), "explore",
                                ifelse(grepl("exploit", data_exp$OUTCOME, ignore.case = TRUE), "exploit", 
                                      ifelse(grepl("none|NONE", data_exp$OUTCOME, ignore.case = TRUE), "none", NA)))

data_clean <- data_exp[!is.na(data_exp$outcome_clean), ]

# Create sample size info for the diagram
total_trials <- nrow(data_clean)
n_monkeys <- length(unique(data_clean$monkey))
n_conditions <- length(unique(data_clean$CONDITION))
n_blocks <- length(unique(data_clean$BLOCK))

cat("Model structure overview:\n")
cat("Total trials:", total_trials, "\n")
cat("Monkeys:", n_monkeys, "\n")
cat("Social conditions:", n_conditions, "\n")
cat("Blocks:", n_blocks, "\n\n")

pdf("HIERARCHICAL_TRINOMIAL_MODEL_STRUCTURE.pdf", width = 20, height = 16)

# Create the comprehensive model diagram
par(mfrow = c(1, 1), mar = c(1, 1, 1, 1))

# Set up plotting area
plot(1, 1, type = "n", xlim = c(0, 20), ylim = c(0, 16), 
     xlab = "", ylab = "", axes = FALSE, main = "")

# Title
text(10, 15.5, "NESTED HIERARCHICAL TRINOMIAL REGRESSION MODEL", 
     cex = 2.5, font = 2, col = "darkblue")
text(10, 14.8, "Primate Social Decision-Making: Reference Frames and Identity Models", 
     cex = 1.8, font = 1, col = "darkred")

# LEVEL 1: OBSERVATION LEVEL
rect(1, 12.5, 19, 13.8, col = "lightblue", border = "blue", lwd = 2)
text(10, 13.4, "LEVEL 1: OBSERVATION LEVEL (Trial i, Monkey j, Block k)", 
     cex = 1.4, font = 2)
text(10, 13.0, paste("N =", total_trials, "trials"), cex = 1.2)
text(10, 12.7, "Y_ijk ~ Multinomial(π_exploit, π_explore, π_none)", cex = 1.1, font = 3)

# LEVEL 2: LINEAR PREDICTORS
rect(1, 10.5, 19, 12.3, col = "lightgreen", border = "darkgreen", lwd = 2)
text(10, 12.0, "LEVEL 2: LINEAR PREDICTORS", cex = 1.4, font = 2)

# Explore equation
text(10, 11.6, "log(π_explore/π_exploit) = β₀ᵉ + β₁ᵉ(Expected_Explore) + β₂ᵉ(Subjective_Exploit) +", 
     cex = 1.0, font = 3)
text(10, 11.3, "β₃ᵉ(Social_Context) + β₄ᵉ(Relative_Rank) + β₅ᵉ(Sex) + α_jᵉ + γ_kᵉ", 
     cex = 1.0, font = 3)

# None equation
text(10, 10.9, "log(π_none/π_exploit) = β₀ⁿ + β₁ⁿ(Expected_Explore) + β₂ⁿ(Subjective_Exploit) +", 
     cex = 1.0, font = 3)
text(10, 10.6, "β₃ⁿ(Social_Context) + β₄ⁿ(Relative_Rank) + β₅ⁿ(Sex) + α_jⁿ + γ_kⁿ", 
     cex = 1.0, font = 3)

# LEVEL 3: RANDOM EFFECTS
rect(1, 8.5, 9.5, 10.3, col = "lightyellow", border = "orange", lwd = 2)
text(5.25, 10.0, "LEVEL 3A: MONKEY EFFECTS", cex = 1.2, font = 2)
text(5.25, 9.6, paste("J =", n_monkeys, "monkeys"), cex = 1.0)
text(5.25, 9.3, "α_jᵉ ~ Normal(0, σ²_monkey_explore)", cex = 0.9, font = 3)
text(5.25, 9.0, "α_jⁿ ~ Normal(0, σ²_monkey_none)", cex = 0.9, font = 3)
text(5.25, 8.7, "Individual Identity Models", cex = 0.9, font = 1, col = "darkred")

rect(10.5, 8.5, 19, 10.3, col = "lightpink", border = "purple", lwd = 2)
text(14.75, 10.0, "LEVEL 3B: BLOCK EFFECTS", cex = 1.2, font = 2)
text(14.75, 9.6, paste("K =", n_blocks, "blocks"), cex = 1.0)
text(14.75, 9.3, "γ_kᵉ ~ Normal(0, σ²_block_explore)", cex = 0.9, font = 3)
text(14.75, 9.0, "γ_kⁿ ~ Normal(0, σ²_block_none)", cex = 0.9, font = 3)
text(14.75, 8.7, "Temporal/Learning Effects", cex = 0.9, font = 1, col = "darkred")

# LEVEL 4: HYPERPRIORS
rect(1, 6.5, 19, 8.3, col = "lavender", border = "darkviolet", lwd = 2)
text(10, 8.0, "LEVEL 4: HYPERPRIORS", cex = 1.4, font = 2)
text(6, 7.6, "β ~ Normal(0, 2.5)", cex = 1.0, font = 3)
text(14, 7.6, "σ²_monkey ~ Half-Cauchy(0, 1)", cex = 1.0, font = 3)
text(6, 7.2, "Weakly informative priors", cex = 0.9, font = 1)
text(14, 7.2, "σ²_block ~ Half-Cauchy(0, 1)", cex = 1.0, font = 3)
text(10, 6.8, "Bayesian Uncertainty Quantification", cex = 1.0, font = 1, col = "darkred")

# RESEARCH QUESTION CONNECTION
rect(1, 4.0, 19, 6.3, col = "mistyrose", border = "darkred", lwd = 3)
text(10, 6.0, "RESEARCH QUESTION CONNECTION", cex = 1.4, font = 2, col = "darkred")
text(10, 5.6, "How do reference frames and models of identity determine decision-making?", 
     cex = 1.2, font = 2)

# Three key components
text(4, 5.2, "REFERENCE FRAMES:", cex = 1.0, font = 2)
text(4, 4.9, "• Social Context (solo/duo/trio)", cex = 0.9)
text(4, 4.6, "• Expected Explore Value", cex = 0.9)
text(4, 4.3, "• Subjective Exploit Value", cex = 0.9)

text(10, 5.2, "IDENTITY MODELS:", cex = 1.0, font = 2)
text(10, 4.9, "• Individual Effects (α_j)", cex = 0.9)
text(10, 4.6, "• Relative Rank", cex = 0.9)
text(10, 4.3, "• Sex Differences", cex = 0.9)

text(16, 5.2, "HIERARCHICAL STRUCTURE:", cex = 1.0, font = 2)
text(16, 4.9, "• Monkey-level variation", cex = 0.9)
text(16, 4.6, "• Block-level variation", cex = 0.9)
text(16, 4.3, "• Nested dependencies", cex = 0.9)

# MODEL INTERPRETATION
rect(1, 1.5, 19, 3.8, col = "lightcyan", border = "darkcyan", lwd = 2)
text(10, 3.5, "MODEL INTERPRETATION", cex = 1.4, font = 2, col = "darkcyan")

text(6.5, 3.1, "TRINOMIAL PROBABILITIES:", cex = 1.0, font = 2)
text(6.5, 2.8, "P(Explore) = exp(η_explore) / [1 + exp(η_explore) + exp(η_none)]", 
     cex = 0.8, font = 3)
text(6.5, 2.5, "P(None) = exp(η_none) / [1 + exp(η_explore) + exp(η_none)]", 
     cex = 0.8, font = 3)
text(6.5, 2.2, "P(Exploit) = 1 / [1 + exp(η_explore) + exp(η_none)]", 
     cex = 0.8, font = 3)
text(6.5, 1.9, "(Reference category)", cex = 0.8, font = 1, col = "gray")

text(13.5, 3.1, "COEFFICIENT INTERPRETATION:", cex = 1.0, font = 2)
text(13.5, 2.8, "β₁ᵉ > 0: Higher expected explore → More exploration", cex = 0.8)
text(13.5, 2.5, "β₂ᵉ < 0: Higher exploit value → Less exploration", cex = 0.8)
text(13.5, 2.2, "α_j: Individual 'exploration personality'", cex = 0.8)
text(13.5, 1.9, "γ_k: Learning/adaptation over time", cex = 0.8)

# Add arrows showing hierarchical structure
arrows(5, 8.3, 5, 10.3, lwd = 2, col = "red", length = 0.1)
arrows(15, 8.3, 15, 10.3, lwd = 2, col = "red", length = 0.1)
arrows(10, 10.3, 10, 12.3, lwd = 2, col = "red", length = 0.1)
arrows(10, 12.3, 10, 12.5, lwd = 2, col = "red", length = 0.1)

# Add model fit information
text(1, 0.8, "Model Information:", cex = 1.1, font = 2)
text(1, 0.5, paste("• Observations:", total_trials, "trials"), cex = 0.9)
text(1, 0.2, paste("• Groups: 6 monkeys ×", n_blocks, "blocks"), cex = 0.9)

text(10, 0.8, "Estimation Method:", cex = 1.1, font = 2)
text(10, 0.5, "• Bayesian MCMC (brms/Stan)", cex = 0.9)
text(10, 0.2, "• 4 chains × 2000 iterations", cex = 0.9)

text(17, 0.8, "Model Comparison:", cex = 1.1, font = 2)
text(17, 0.5, "• LOO-CV for validation", cex = 0.9)
text(17, 0.2, "• WAIC for selection", cex = 0.9)

dev.off()

# Create a second page with detailed equations
pdf("HIERARCHICAL_TRINOMIAL_EQUATIONS_DETAILED.pdf", width = 16, height = 12)

par(mfrow = c(1, 1), mar = c(2, 2, 2, 2))
plot(1, 1, type = "n", xlim = c(0, 16), ylim = c(0, 12), 
     xlab = "", ylab = "", axes = FALSE, main = "")

# Title
text(8, 11.5, "DETAILED MATHEMATICAL SPECIFICATION", 
     cex = 2.2, font = 2, col = "darkblue")
text(8, 11.0, "Hierarchical Trinomial Regression for Primate Decision-Making", 
     cex = 1.5, font = 1, col = "darkred")

# Level 1: Likelihood
rect(0.5, 9.5, 15.5, 10.8, col = "lightblue", border = "blue", lwd = 2)
text(8, 10.4, "LEVEL 1: LIKELIHOOD", cex = 1.3, font = 2)
text(8, 10.0, "Y_ijk | π_ijk ~ Multinomial(1, π_ijk)", cex = 1.1, font = 3)
text(8, 9.7, "where π_ijk = (π_exploit, π_explore, π_none) and Σπ = 1", 
     cex = 1.0, font = 3)

# Level 2: Linear predictors
rect(0.5, 7.0, 15.5, 9.3, col = "lightgreen", border = "darkgreen", lwd = 2)
text(8, 9.0, "LEVEL 2: LINEAR PREDICTORS", cex = 1.3, font = 2)

text(8, 8.6, "η_explore_ijk = β₀ᵉ + β₁ᵉ × Expected_Explore_ijk + β₂ᵉ × Subjective_Exploit_ijk +", 
     cex = 0.9, font = 3)
text(8, 8.3, "β₃ᵉ × Social_Duo_ijk + β₄ᵉ × Social_Trio_ijk + β₅ᵉ × Relative_Rank_ijk +", 
     cex = 0.9, font = 3)
text(8, 8.0, "β₆ᵉ × Sex_Male_j + α_jᵉ + γ_kᵉ", 
     cex = 0.9, font = 3)

text(8, 7.6, "η_none_ijk = β₀ⁿ + β₁ⁿ × Expected_Explore_ijk + β₂ⁿ × Subjective_Exploit_ijk +", 
     cex = 0.9, font = 3)
text(8, 7.3, "β₃ⁿ × Social_Duo_ijk + β₄ⁿ × Social_Trio_ijk + β₅ⁿ × Relative_Rank_ijk +", 
     cex = 0.9, font = 3)
text(8, 7.0, "β₆ⁿ × Sex_Male_j + α_jⁿ + γ_kⁿ", 
     cex = 0.9, font = 3)

# Level 3: Random effects
rect(0.5, 5.0, 15.5, 6.8, col = "lightyellow", border = "orange", lwd = 2)
text(8, 6.5, "LEVEL 3: RANDOM EFFECTS", cex = 1.3, font = 2)

text(5, 6.1, "MONKEY EFFECTS:", cex = 1.0, font = 2)
text(5, 5.8, "α_jᵉ ~ Normal(0, σ²_monkey_explore)", cex = 0.9, font = 3)
text(5, 5.5, "α_jⁿ ~ Normal(0, σ²_monkey_none)", cex = 0.9, font = 3)
text(5, 5.2, "j ∈ {ANEMONE, ICE, CHOCOLAT, EBI, DALI, FRAN}", cex = 0.8)

text(11, 6.1, "BLOCK EFFECTS:", cex = 1.0, font = 2)
text(11, 5.8, "γ_kᵉ ~ Normal(0, σ²_block_explore)", cex = 0.9, font = 3)
text(11, 5.5, "γ_kⁿ ~ Normal(0, σ²_block_none)", cex = 0.9, font = 3)
text(11, 5.2, paste("k ∈ {1, 2, ...,", n_blocks, "}"), cex = 0.8)

# Level 4: Priors
rect(0.5, 3.0, 15.5, 4.8, col = "lavender", border = "darkviolet", lwd = 2)
text(8, 4.5, "LEVEL 4: PRIOR DISTRIBUTIONS", cex = 1.3, font = 2)

text(5, 4.1, "FIXED EFFECTS:", cex = 1.0, font = 2)
text(5, 3.8, "β₀ᵉ, β₀ⁿ ~ Normal(0, 2.5)", cex = 0.9, font = 3)
text(5, 3.5, "β₁ᵉ...β₆ᵉ ~ Normal(0, 2.5)", cex = 0.9, font = 3)
text(5, 3.2, "β₁ⁿ...β₆ⁿ ~ Normal(0, 2.5)", cex = 0.9, font = 3)

text(11, 4.1, "VARIANCE COMPONENTS:", cex = 1.0, font = 2)
text(11, 3.8, "σ_monkey_explore ~ Half-Cauchy(0, 1)", cex = 0.9, font = 3)
text(11, 3.5, "σ_monkey_none ~ Half-Cauchy(0, 1)", cex = 0.9, font = 3)
text(11, 3.2, "σ_block_explore, σ_block_none ~ Half-Cauchy(0, 1)", cex = 0.9, font = 3)

# Transformation
rect(0.5, 1.0, 15.5, 2.8, col = "lightcyan", border = "darkcyan", lwd = 2)
text(8, 2.5, "INVERSE LOGIT TRANSFORMATION", cex = 1.3, font = 2)

text(8, 2.1, "π_explore_ijk = exp(η_explore_ijk) / [1 + exp(η_explore_ijk) + exp(η_none_ijk)]", 
     cex = 0.9, font = 3)
text(8, 1.8, "π_none_ijk = exp(η_none_ijk) / [1 + exp(η_explore_ijk) + exp(η_none_ijk)]", 
     cex = 0.9, font = 3)
text(8, 1.5, "π_exploit_ijk = 1 / [1 + exp(η_explore_ijk) + exp(η_none_ijk)]", 
     cex = 0.9, font = 3)
text(8, 1.2, "Multinomial logit with exploit as reference category", cex = 0.9, font = 1, col = "gray")

# Variable definitions
text(1, 0.6, "Variables: i=trial, j=monkey, k=block; Y∈{exploit=1, explore=2, none=3}", 
     cex = 0.8, font = 1)
text(1, 0.3, "Covariates: Expected_Explore, Subjective_Exploit ∈ [0,1]; Social ∈ {solo, duo, trio}; Rank ∈ {1,2,3}; Sex ∈ {F,M}", 
     cex = 0.8, font = 1)

dev.off()

cat("\nHIERARCHICAL TRINOMIAL MODEL STRUCTURE: COMPLETE\n")
cat("Files created:\n")
cat("• HIERARCHICAL_TRINOMIAL_MODEL_STRUCTURE.pdf - Visual model diagram\n")
cat("• HIERARCHICAL_TRINOMIAL_EQUATIONS_DETAILED.pdf - Detailed mathematical specification\n\n")

cat("MODEL SUMMARY:\n")
cat("• 4-level hierarchical structure\n")
cat("• Trinomial outcomes (exploit/explore/none)\n")
cat("• Individual and block random effects\n")
cat("• Bayesian estimation with weakly informative priors\n")
cat("• Direct connection to research question about reference frames and identity\n\n")

cat("RESEARCH QUESTION CONNECTION:\n")
cat("• Reference frames: Social context + value expectations\n")
cat("• Identity models: Individual + rank + sex effects\n")
cat("• Hierarchical structure captures nested dependencies\n")
cat("• Bayesian approach quantifies uncertainty in all parameters\n\n") 