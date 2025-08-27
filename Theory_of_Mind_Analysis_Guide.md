# Theory of Mind Analysis for Monkey Explore-Exploit Data

## Overview

This analysis tests whether monkeys demonstrate **theory of mind** (ToM) capabilities during social explore-exploit decisions. Theory of mind refers to the ability to attribute mental states (beliefs, desires, intentions) to others and use this information to predict their behavior.

## Research Hypothesis

**Primary Hypothesis**: Subordinate monkeys will show theory of mind by considering dominant monkeys' preferences and behaviors, while dominant monkeys will not consider subordinates preferences.

**Secondary Hypothesis**: Intermediate-ranked monkeys will show context-dependent theory of mind - they will consider dominants preferences when subordinate, but not subordinates preferences when dominant.

## Mathematical Framework

### 1. Bayesian Hierarchical Models

We use Bayesian hierarchical models to account for:
- **Individual differences** between monkeys
- **Repeated measures** within monkeys
- **Uncertainty** in parameter estimates
- **Model comparison** using Leave-One-Out Cross-Validation (LOO-CV)

###2 of Mind Variables

```r
# Key variables created:
tom_context = case_when(
  rank_diff < 0 ~ "subordinate_to_partner,  # Key ToM test
  rank_diff > 0 ~dominant_to_partner",     # Control condition
  rank_diff == 0 ~ "same_rank"               # Baseline
)
```

### 3. Model Specifications

#### Model 1sic Theory of Mind
```r
choice_type ~ 1+ social_context + rank_position + tom_context + 
              expected_explore + subjective_chosen + 
              (1+ social_context + tom_context | monkey_id)
```

#### Model 2: Rank × ToM Interaction
```r
choice_type ~ 1+ social_context * rank_position + tom_context * rank_position + 
              expected_explore + subjective_chosen + 
              (1+ social_context + tom_context | monkey_id)
```

#### Model 3: Partner-Specific Effects
```r
choice_type ~ 1+ social_context + rank_position + tom_context + 
              rank_diff + expected_explore + subjective_chosen + 
              (1+ social_context + tom_context + rank_diff | monkey_id)
```

## Key Predictions

### 1. Subordinate Theory of Mind
- **Prediction**: Subordinates (rank 3) will show different exploit rates when paired with dominants vs. other subordinates
- **Test**: Compare exploit rates in `subordinate_to_partner` vs. `dominant_to_partner` contexts
- **Expected**: Higher exploit rates when subordinate to dominant (strategic behavior)

###2Dominant Ignorance
- **Prediction**: Dominants (rank 1) will show similar behavior regardless of partner rank
- **Test**: Compare exploit rates across different partner ranks for dominants
- **Expected**: No significant difference (no ToM consideration)

### 3. Intermediate Context-Dependency
- **Prediction**: Intermediates (rank 2) will show ToM when subordinate, but not when dominant
- **Test**: Compare behavior when rank_diff < 0 vs. rank_diff >0- **Expected**: Asymmetric ToM effects

## Statistical Validation

### 1. Model Comparison
- **LOO-CV**: Leave-One-Out Cross-Validation for model selection
- **WAIC**: Widely Applicable Information Criterion
- **R²**: Bayesian R-squared for model fit

###2 Effect Sizes
- **Odds Ratios**: For categorical outcomes
- **Credible Intervals**: 95% Bayesian credible intervals
- **Standardized Effects**: Cohensd equivalents

### 3. Diagnostics
- **Trace Plots**: MCMC convergence
- **Posterior Predictive Checks**: Model fit validation
- **Residual Analysis**: Model adequacy

## Visualization Strategy

###1ry of Mind Effects by Rank
- **X-axis**: Rank position (1=Dominant, 2ntermediate, 3=Subordinate)
- **Y-axis**: Exploit rate
- **Color**: ToM context (subordinate/dominant/same rank)
- **Expected Pattern**: Steep slope for subordinates, flat for dominants

### 2. Partner-Specific Effects
- **X-axis**: Rank difference with partner
- **Y-axis**: Exploit choice (0/1)
- **Expected Pattern**: Negative slope (more exploitation when subordinate)

### 3. Individual Differences
- **Bar plots**: Exploit rates by monkey and context
- **Expected Pattern**: Individual variation in ToM sensitivity

### 4. Social Condition Comparison
- **X-axis**: Social condition (solo/duo/trio)
- **Y-axis**: Exploit rate
- **Expected Pattern**: Higher rates in social conditions for subordinates

## Interpretation Guidelines

### Strong Theory of Mind Evidence
1. **Significant tom_context effects** in hierarchical models
2. **Rank × ToM interactions** showing asymmetric effects
3. **Individual differences** in ToM sensitivity
4. **Partner-specific effects** correlated with rank differences

### Weak Theory of Mind Evidence
1. **No significant tom_context effects**
2. **Symmetric effects** across rank positions
3vidual differences** in social sensitivity
4. **No partner-specific effects**

### Alternative Explanations to Consider
1. **Simple dominance avoidance**: Subordinates avoid conflict
2. **Resource competition**: Strategic resource allocation
3. **Social learning**: Imitation of dominant behavior
4. **Risk assessment**: Subordinates are more risk-averse

## Publication-Ready Analysis

### 1. Preregistered Hypotheses
- Clear, testable predictions
- Specific effect sizes
- Power analysis considerations

### 2. Robust Statistical Framework
- Bayesian hierarchical models
- Multiple model comparison
- Comprehensive diagnostics

### 3. Transparent Reporting
- All model specifications
- Effect sizes and uncertainties
- Model comparison results
- Diagnostic plots

### 4. Replicable Code
- Complete analysis scripts
- Data preprocessing steps
- Visualization code
- Results export

## Expected Outcomes

### If Theory of Mind is Present:
- Subordinates will exploit more when paired with dominants
- Dominants will show no partner-specific effects
- Intermediates will show context-dependent effects
- Individual differences in ToM sensitivity

### If Theory of Mind is Absent:
- No significant tom_context effects
- Similar behavior across all rank positions
- No partner-specific effects
- No individual differences in social sensitivity

## Next Steps

1. **Run the analysis scripts** on your dataset
2 model diagnostics** for convergence
3Compare model fits** using LOO-CV
4. **Interpret effect sizes** and credible intervals
5. **Create publication-ready figures**
6. **Write up results** with clear statistical reporting

## References

- **Bayesian Analysis**: Gelman et al. (213sian Data Analysis
- **Theory of Mind**: Premack & Woodruff (1978havioral and Brain Sciences
- **Social Cognition**: Call & Tomasello (2008) Trends in Cognitive Sciences
- **Hierarchical Models**: Gelman & Hill (26ata Analysis Using Regression and Multilevel/Hierarchical Models

---

*This analysis provides a rigorous, statistically validated approach to testing theory of mind capabilities in non-human primates using modern Bayesian methods.* 