# Bayesian Hierarchical Analysis of Explore/Exploit Decisions

This repository contains R scripts for analyzing how social context and expectation influence explore/exploit decisions in primates using Bayesian hierarchical modeling.

## Research Question

**Does social context and expectation influence the outcome of their decision to explore or exploit?**

## Files Overview

### Main Analysis Scripts

1. **`bayesian_explore_exploit_analysis.R`** - Main analysis script
   - Data preprocessing and cleaning
   - Bayesian hierarchical multinomial regression models
   - Model comparison using LOO cross-validation
   - Results visualization and interpretation
   - Hypothesis testing using Bayes factors

2. **`bayesian_robustness_checks.R`** - Robustness and sensitivity analysis
   - Alternative model specifications
   - Prior sensitivity analysis
   - Individual monkey analyses
   - Cross-validation and prediction accuracy
   - Effect size calculations

### Data Files

- **`Explore Exploit Dataset.csv`** - Raw dataset containing trial-by-trial decisions

## Required R Packages

The scripts will automatically install required packages, but ensure you have:

```r
# Core packages
install.packages(c("tidyverse", "brms", "bayesplot", "loo", "tidybayes"))

# Additional packages for visualization and diagnostics
install.packages(c("ggplot2", "gridExtra", "coda", "stringr", "data.table"))
```

## Data Structure

The dataset contains the following key variables:

- **CONDITION**: Social context (solo, duo, trio)
- **OUTCOME**: Decision outcome (explore, exploit, or specific choices)
- **expected_explore**: Expectation measure (continuous, 0-1)
- **subjective_exploit**: Subjective exploit value
- **monkey**: Individual monkey identifier
- **BLOCK_No**: Experimental block
- **TRIAL_TYPE**: Type of trial (OIT_RE, CONTROL)

## Analysis Approach

### 1. Data Preprocessing
- Clean and categorize explore/exploit decisions
- Create standardized continuous predictors
- Handle missing data and ensure adequate sample sizes

### 2. Model Specifications

**Model 1: Main Effects**
```
decision_type ~ social_context + expected_explore_z + controls + 
                (1 | monkey_id) + (1 | block_id)
```

**Model 2: Interaction Model**
```
decision_type ~ social_context * expected_explore_z + controls + 
                (1 + expected_explore_z | monkey_id) + (1 | block_id)
```

**Model 3: Full Hierarchical**
```
decision_type ~ social_context * expected_explore_z + controls + 
                (1 + social_context + expected_explore_z | monkey_id) + 
                (1 | block_id)
```

### 3. Bayesian Framework
- Uses `brms` package with Stan backend
- Weakly informative priors
- MCMC sampling with convergence diagnostics
- Model comparison via LOO cross-validation

## Running the Analysis

1. **Ensure data file is in working directory**:
   ```r
   # Check that "Explore Exploit Dataset.csv" is present
   file.exists("Explore Exploit Dataset.csv")
   ```

2. **Run main analysis**:
   ```r
   source("bayesian_explore_exploit_analysis.R")
   ```

3. **Run robustness checks** (optional):
   ```r
   source("bayesian_robustness_checks.R")
   ```

## Expected Runtime

- Main analysis: ~30-60 minutes (depending on hardware)
- Robustness checks: ~60-120 minutes
- Individual monkey models may take additional time

## Output Files

### Model Objects
- `bayesian_explore_exploit_models.RData` - Fitted Bayesian models
- `robustness_analysis_results.RData` - Alternative models and checks

### Data Files
- `cleaned_explore_exploit_data.csv` - Preprocessed dataset
- `analysis_results_summary.RData` - Summary statistics and key findings

### Visualizations
- Posterior distribution plots
- Conditional effects plots
- Individual monkey patterns
- Model diagnostics plots

## Key Research Hypotheses

1. **H1**: Social context influences explore/exploit decisions
   - Solo vs. Duo comparison
   - Solo vs. Trio comparison

2. **H2**: Expectation influences explore/exploit decisions
   - Higher expectation → more exploration

3. **H3**: Social context and expectation interact
   - Effects of expectation differ by social context

## Model Interpretation

### Probability Statements
The analysis provides probability statements such as:
- P(duo context increases exploration vs. solo) = ?
- P(expectation positively influences exploration) = ?

### Effect Sizes
Standardized effect sizes (Cohen's d equivalent) for logistic regression:
- Small effect: |d| ≈ 0.2
- Medium effect: |d| ≈ 0.5  
- Large effect: |d| ≈ 0.8

### Bayes Factors
Evidence ratios for hypotheses:
- BF > 3: Moderate evidence
- BF > 10: Strong evidence
- BF > 30: Very strong evidence

## Troubleshooting

### Common Issues

1. **Installation problems**:
   ```r
   # Install development version if needed
   install.packages("brms", repos = "https://cloud.r-project.org/")
   ```

2. **Memory issues**:
   - Reduce `chains` or `iter` parameters
   - Use `cores = 1` if parallel processing causes problems

3. **Convergence issues**:
   - Increase `adapt_delta` (e.g., 0.99)
   - Increase `max_treedepth`
   - Check for problematic predictors

### Model Diagnostics

Always check:
- R-hat values (should be < 1.01)
- Effective sample sizes (should be > 400)
- Trace plots for good mixing
- Posterior predictive checks

## Extensions

Potential additional analyses:
- Temporal dynamics using time series models
- Non-linear effects using splines
- Multinomial models for specific choice outcomes
- Hierarchical models with cross-classified random effects

## References

- Bürkner, P. C. (2017). brms: An R package for Bayesian multilevel models using Stan. *Journal of Statistical Software*, 80(1), 1-28.
- Gelman, A., et al. (2013). *Bayesian Data Analysis* (3rd ed.). CRC Press.
- McElreath, R. (2020). *Statistical Rethinking* (2nd ed.). CRC Press.

## Contact

For questions about the analysis or interpretation of results, please refer to the comments in the R scripts or consult the Bayesian modeling literature cited above. 