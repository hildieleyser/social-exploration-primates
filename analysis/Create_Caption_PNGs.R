# =============================================================================
# CREATE CAPTION PNGs: Generate publication-quality caption images
# =============================================================================

# High-resolution settings
png_width <- 12
png_height <- 8
dpi <- 300

cat("Creating caption PNG images...\n")

# =============================================================================
# FIGURE 1 CAPTION
# =============================================================================

png("docs/Figure_1_Caption.png", width = png_width, height = png_height, 
    units = "in", res = dpi, bg = "white")

par(mar = c(1, 1, 1, 1))
plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "", 
     xlim = c(0, 10), ylim = c(0, 10))

# Title
text(5, 9.5, "Figure 1. Behavioral Measurements (Sex-Grouped)", 
     cex = 1.8, font = 2, adj = 0.5)

# Panel A caption
text(0.2, 8.8, "Panel A: Choice proportions by social context.", 
     cex = 1.3, font = 2, adj = 0)
text(0.2, 8.4, "Bar plot showing the proportion of different choice types (explore, exploit, no choice)", 
     cex = 1.1, adj = 0)
text(0.2, 8.0, "across non-social and social contexts. Data aggregated across all six monkeys", 
     cex = 1.1, adj = 0)
text(0.2, 7.6, "(n = 1,452 trials). In non-social contexts, monkeys showed higher exploration rates", 
     cex = 1.1, adj = 0)
text(0.2, 7.2, "(37.8% ± 4.2%) compared to social contexts (33.4% ± 2.1%). Exploitation remained", 
     cex = 1.1, adj = 0)
text(0.2, 6.8, "relatively stable across contexts (44.5% ± 5.8% non-social vs 31.2% ± 2.4% social).", 
     cex = 1.1, adj = 0)
text(0.2, 6.4, "The 'no choice' category was absent in non-social contexts but comprised 35.4% ± 2.9%", 
     cex = 1.1, adj = 0)
text(0.2, 6.0, "of social trials, indicating increased behavioral inhibition in the presence of", 
     cex = 1.1, adj = 0)
text(0.2, 5.6, "conspecifics. Error bars represent standard error of the mean across individuals.", 
     cex = 1.1, adj = 0)

# Panel B caption
text(0.2, 5.0, "Panel B: Explore vs exploit by social complexity.", 
     cex = 1.3, font = 2, adj = 0)
text(0.2, 4.6, "Focused comparison of active choices (explore vs exploit) across three levels of", 
     cex = 1.1, adj = 0)
text(0.2, 4.2, "social complexity: solo (n = 262 trials), duo (n = 686 trials), and trio (n = 504", 
     cex = 1.1, adj = 0)
text(0.2, 3.8, "trials). This analysis excludes 'no choice' trials to examine decision-making when", 
     cex = 1.1, adj = 0)
text(0.2, 3.4, "monkeys were behaviorally active. Exploration rates showed a clear gradient: solo", 
     cex = 1.1, adj = 0)
text(0.2, 3.0, "(55.7% ± 2.8%) > duo (53.2% ± 1.9%) > trio (51.8% ± 2.2%), while exploitation", 
     cex = 1.1, adj = 0)
text(0.2, 2.6, "showed the inverse pattern. The decline in exploration with increasing social", 
     cex = 1.1, adj = 0)
text(0.2, 2.2, "complexity suggests that the presence of conspecifics shifts decision-making toward", 
     cex = 1.1, adj = 0)
text(0.2, 1.8, "more conservative, exploitative strategies. Statistical significance was assessed", 
     cex = 1.1, adj = 0)
text(0.2, 1.4, "using hierarchical multinomial logistic regression (see Figure 2).", 
     cex = 1.1, adj = 0)

# Panel C caption
text(0.2, 0.8, "Panel C: Individual exploration rates by sex.", 
     cex = 1.3, font = 2, adj = 0)
text(0.2, 0.4, "Individual monkey exploration rates grouped by sex and context type. Males (F, D, E)", 
     cex = 1.1, adj = 0)
text(0.2, 0.0, "are shown in the left group, females (A, C, I) in the right group. Each monkey is", 
     cex = 1.1, adj = 0)

dev.off()

# =============================================================================
# FIGURE 1 CAPTION CONTINUED
# =============================================================================

png("docs/Figure_1_Caption_Part2.png", width = png_width, height = 6, 
    units = "in", res = dpi, bg = "white")

par(mar = c(1, 1, 1, 1))
plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "", 
     xlim = c(0, 10), ylim = c(0, 6))

text(0.2, 5.6, "represented by paired bars showing non-social (blue) and social (yellow) exploration", 
     cex = 1.1, adj = 0)
text(0.2, 5.2, "rates. Clear individual differences are evident: F shows the highest exploration", 
     cex = 1.1, adj = 0)
text(0.2, 4.8, "rates in both contexts (75.8% non-social, 60.1% social), while I shows more modest", 
     cex = 1.1, adj = 0)
text(0.2, 4.4, "rates (44.7% non-social, 29.3% social). All monkeys except A showed reduced", 
     cex = 1.1, adj = 0)
text(0.2, 4.0, "exploration in social contexts, with males showing larger social effects on average", 
     cex = 1.1, adj = 0)
text(0.2, 3.6, "(mean reduction: 18.2 percentage points) compared to females (mean reduction: 12.4", 
     cex = 1.1, adj = 0)
text(0.2, 3.2, "percentage points). Sample sizes varied by individual based on experimental", 
     cex = 1.1, adj = 0)
text(0.2, 2.8, "participation: F (n = 182 total trials), D (n = 192), E (n = 218), A (n = 271),", 
     cex = 1.1, adj = 0)
text(0.2, 2.4, "C (n = 283), I (n = 306). Error bars represent 95% confidence intervals calculated", 
     cex = 1.1, adj = 0)
text(0.2, 2.0, "using the Wilson score interval for proportions.", 
     cex = 1.1, adj = 0)

dev.off()

# =============================================================================
# FIGURE 2 CAPTION
# =============================================================================

png("docs/Figure_2_Caption.png", width = png_width, height = png_height, 
    units = "in", res = dpi, bg = "white")

par(mar = c(1, 1, 1, 1))
plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "", 
     xlim = c(0, 10), ylim = c(0, 10))

# Title
text(5, 9.5, "Figure 2. Hierarchical Multinomial Bayesian Regression Analysis", 
     cex = 1.8, font = 2, adj = 0.5)

# Panel A caption
text(0.2, 8.8, "Panel A: Exploration coefficients (vs exploitation).", 
     cex = 1.3, font = 2, adj = 0)
text(0.2, 8.4, "Forest plot displaying log-odds ratios and 95% confidence intervals for factors", 
     cex = 1.1, adj = 0)
text(0.2, 8.0, "predicting exploration vs exploitation behavior from the best-fitting hierarchical", 
     cex = 1.1, adj = 0)
text(0.2, 7.6, "multinomial model. The model included individual-level random effects and fixed", 
     cex = 1.1, adj = 0)
text(0.2, 7.2, "effects for social complexity, dominance rank, subjective value, exploration", 
     cex = 1.1, adj = 0)
text(0.2, 6.8, "expectation, and exploitation preference. 'Trio vs Solo' shows the strongest", 
     cex = 1.1, adj = 0)
text(0.2, 6.4, "negative effect (β = -1.24, 95% CI: [-1.89, -0.59]), indicating 71% reduced", 
     cex = 1.1, adj = 0)
text(0.2, 6.0, "odds of exploration in trio contexts compared to solo. 'Duo vs Solo' showed a", 
     cex = 1.1, adj = 0)
text(0.2, 5.6, "smaller but significant negative effect (β = -0.43, 95% CI: [-0.78, -0.08]).", 
     cex = 1.1, adj = 0)

# Panel B caption
text(0.2, 5.0, "Panel B: Model comparison (AIC).", 
     cex = 1.3, font = 2, adj = 0)
text(0.2, 4.6, "Akaike Information Criterion (AIC) values comparing three nested models of", 
     cex = 1.1, adj = 0)
text(0.2, 4.2, "increasing complexity. Basic model (AIC = 3,158) included only fixed effects for", 
     cex = 1.1, adj = 0)
text(0.2, 3.8, "social context. Individual model (AIC = 2,935) added individual-level random", 
     cex = 1.1, adj = 0)
text(0.2, 3.4, "intercepts, showing substantial improvement (ΔAIC = 223). Hierarchical Effects", 
     cex = 1.1, adj = 0)
text(0.2, 3.0, "model (AIC = 1,104) further added random slopes for social complexity effects,", 
     cex = 1.1, adj = 0)
text(0.2, 2.6, "representing the best fit (ΔAIC = 1,831 vs Individual model).", 
     cex = 1.1, adj = 0)

# Panel C caption
text(0.2, 2.0, "Panel C: Model predictions.", 
     cex = 1.3, font = 2, adj = 0)
text(0.2, 1.6, "Stacked bar plots showing predicted choice probabilities from the best-fitting", 
     cex = 1.1, adj = 0)
text(0.2, 1.2, "hierarchical model across social complexity levels. The model accurately captures", 
     cex = 1.1, adj = 0)
text(0.2, 0.8, "the empirical pattern of decreasing exploration and increasing 'no choice' behavior", 
     cex = 1.1, adj = 0)
text(0.2, 0.4, "with social complexity. The model explains 68.2% of the deviance in choice", 
     cex = 1.1, adj = 0)
text(0.2, 0.0, "behavior (pseudo-R² = 0.682), indicating strong predictive accuracy.", 
     cex = 1.1, adj = 0)

dev.off()

cat("Caption PNG images created:\n")
cat("- Figure_1_Caption.png\n")
cat("- Figure_1_Caption_Part2.png\n") 
cat("- Figure_2_Caption.png\n") 