# My Figures Directory

This directory contains all publication-ready figures I generated from my statistical analysis of social frames of reference in explore-exploit decision-making.

## My Main Figures

### Figure 1: Main Effects (`figure1_main_effects.png`)
- **Description**: Bar chart I created showing exploration rates across social complexity conditions
- **Key Finding**: Clear decrease in exploration I discovered from solo (15.7%) → duo (15.1%) → trio (9.5%)
- **Statistical Test**: Error bars I included show 95% confidence intervals
- **Dimensions**: 8" × 6" at 300 DPI

### Figure 2: Individual Differences (`figure2_individual_differences.png`)
- **Description**: Horizontal bar chart I made of individual exploration rates across all 6 monkeys I studied
- **Key Finding**: Large individual differences I observed (range: ~5% to ~65%)
- **Statistical Test**: Error bars I added show 95% confidence intervals
- **Dimensions**: 8" × 6" at 300 DPI

### Figure 3: Beta Coefficients (`figure3_beta_coefficients.png`)
- **Description**: Forest plot I generated showing my model coefficients with confidence intervals
- **Key Finding**: Significant negative social complexity effects for exploration I found
- **Statistical Test**: 95% confidence intervals from my multinomial logistic regression
- **Layout**: Faceted by outcome (explore vs. none), relative to exploitation baseline
- **Dimensions**: 12" × 8" at 300 DPI

### Figure 4: Model Predictions (`figure4_model_predictions.png`)
- **Description**: Stacked bar chart I created of predicted choice probabilities by social context
- **Key Finding**: My model captures decreasing exploration with social complexity
- **Method**: Predictions from my full multinomial model for average individual
- **Dimensions**: 8" × 6" at 300 DPI

### Figure 5: Interaction Effects (`figure5_interaction_effects.png`)
- **Description**: Line plot I designed showing exploration rates by social complexity and dominance rank
- **Key Finding**: Rank effects interact with social complexity as I hypothesized
- **Statistical Test**: Error bars I included show 95% confidence intervals
- **Dimensions**: 8" × 6" at 300 DPI

### Figure 6: Model Diagnostics (`figure6_model_diagnostics.png`)
- **Description**: Three-panel diagnostic plot I created for my model validation
- **Panels**: 
  - Residuals vs. Fitted values from my model
  - Q-Q plot for residual normality I checked
  - Model performance metrics (AIC, BIC, etc.) I calculated
- **Purpose**: Assess my model fit and identify potential violations
- **Dimensions**: 15" × 5" at 300 DPI

## My Figure Specifications

### Color Schemes I Used
- **Main effects**: Viridis plasma palette for social complexity
- **Individual differences**: Viridis default palette for individuals
- **Coefficients**: Red (#E31A1C) for explore, Blue (#1F78B4) for none
- **Predictions**: Red for explore, Green (#33A02C) for exploit, Blue for none
- **Interactions**: Viridis plasma palette for rank categories

### Typography I Applied
- **Title**: 14pt, bold, centered
- **Subtitle**: 12pt, centered
- **Axis labels**: 11pt
- **Legend titles**: 11pt, bold
- **Base font**: Minimal theme, 12pt base size

### Statistical Elements I Included
- **Error bars**: 95% confidence intervals where applicable
- **Significance**: Indicated through confidence intervals not crossing zero
- **Reference lines**: Dashed lines at zero for coefficient plots

## My Data Sources
All figures I generated come from my cleaned dataset (`data/Explore Exploit Dataset.csv`) using my comprehensive analysis script (`analysis/Complete_Statistical_Analysis.R`).

## Reproducibility of My Work
To regenerate all my figures:
```r
source("analysis/Complete_Statistical_Analysis.R")
```

This will recreate all my figures with identical specifications and save them to this directory.

## My Figure Quality Standards
- **Resolution**: 300 DPI for publication quality
- **Format**: PNG for web compatibility and print quality
- **Transparency**: Alpha blending I used for overlapping elements
- **Grid**: Minimal grid lines to reduce visual clutter

## Usage Notes for My Figures
- All my figures are publication-ready and formatted for academic manuscripts
- I used colorblind-friendly palettes (viridis schemes)
- Figure dimensions can be easily adjusted in my analysis script
- Statistical details (sample sizes, test statistics) are documented in my results summary 