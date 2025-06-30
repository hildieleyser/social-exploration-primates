# Figures Directory

This directory contains all publication-ready figures generated from the statistical analysis of social frames of reference in explore-exploit decision-making.

## Main Figures

### Figure 1: Main Effects (`figure1_main_effects.png`)
- **Description**: Bar chart showing exploration rates across social complexity conditions
- **Key Finding**: Clear decrease in exploration from solo (15.7%) → duo (15.1%) → trio (9.5%)
- **Statistical Test**: Error bars show 95% confidence intervals
- **Dimensions**: 8" × 6" at 300 DPI

### Figure 2: Individual Differences (`figure2_individual_differences.png`)
- **Description**: Horizontal bar chart of individual exploration rates across all 6 monkeys
- **Key Finding**: Large individual differences (range: ~5% to ~65%)
- **Statistical Test**: Error bars show 95% confidence intervals
- **Dimensions**: 8" × 6" at 300 DPI

### Figure 3: Beta Coefficients (`figure3_beta_coefficients.png`)
- **Description**: Forest plot showing model coefficients with confidence intervals
- **Key Finding**: Significant negative social complexity effects for exploration
- **Statistical Test**: 95% confidence intervals from multinomial logistic regression
- **Layout**: Faceted by outcome (explore vs. none), relative to exploitation baseline
- **Dimensions**: 12" × 8" at 300 DPI

### Figure 4: Model Predictions (`figure4_model_predictions.png`)
- **Description**: Stacked bar chart of predicted choice probabilities by social context
- **Key Finding**: Model captures decreasing exploration with social complexity
- **Method**: Predictions from full multinomial model for average individual
- **Dimensions**: 8" × 6" at 300 DPI

### Figure 5: Interaction Effects (`figure5_interaction_effects.png`)
- **Description**: Line plot showing exploration rates by social complexity and dominance rank
- **Key Finding**: Rank effects interact with social complexity
- **Statistical Test**: Error bars show 95% confidence intervals
- **Dimensions**: 8" × 6" at 300 DPI

### Figure 6: Model Diagnostics (`figure6_model_diagnostics.png`)
- **Description**: Three-panel diagnostic plot for model validation
- **Panels**: 
  - Residuals vs. Fitted values
  - Q-Q plot for residual normality
  - Model performance metrics (AIC, BIC, etc.)
- **Purpose**: Assess model fit and identify potential violations
- **Dimensions**: 15" × 5" at 300 DPI

## Figure Specifications

### Color Schemes
- **Main effects**: Viridis plasma palette for social complexity
- **Individual differences**: Viridis default palette for individuals
- **Coefficients**: Red (#E31A1C) for explore, Blue (#1F78B4) for none
- **Predictions**: Red for explore, Green (#33A02C) for exploit, Blue for none
- **Interactions**: Viridis plasma palette for rank categories

### Typography
- **Title**: 14pt, bold, centered
- **Subtitle**: 12pt, centered
- **Axis labels**: 11pt
- **Legend titles**: 11pt, bold
- **Base font**: Minimal theme, 12pt base size

### Statistical Elements
- **Error bars**: 95% confidence intervals where applicable
- **Significance**: Indicated through confidence intervals not crossing zero
- **Reference lines**: Dashed lines at zero for coefficient plots

## Data Sources
All figures are generated from the cleaned dataset (`data/Explore Exploit Dataset.csv`) using the comprehensive analysis script (`analysis/Complete_Statistical_Analysis.R`).

## Reproducibility
To regenerate all figures:
```r
source("analysis/Complete_Statistical_Analysis.R")
```

This will recreate all figures with identical specifications and save them to this directory.

## Figure Quality
- **Resolution**: 300 DPI for publication quality
- **Format**: PNG for web compatibility and print quality
- **Transparency**: Alpha blending used for overlapping elements
- **Grid**: Minimal grid lines to reduce visual clutter

## Usage Notes
- All figures are publication-ready and formatted for academic manuscripts
- Color schemes are colorblind-friendly (viridis palettes)
- Figure dimensions can be easily adjusted in the analysis script
- Statistical details (sample sizes, test statistics) are documented in the results summary 