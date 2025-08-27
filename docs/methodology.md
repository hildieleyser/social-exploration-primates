# Methodology

## Experimental Design

### Participants
- **Species**: Non-human primates (Macaca mulatta)
- **Sample Size**: 6 individuals (3 male, 3 female)
- **Age Range**: 4-8 years
- **Housing**: Social groups in enriched environments

### Experimental Paradigm

The study employed a computerized explore-exploit decision-making task with three social contexts:

1. **Solo Condition**: Individual decision-making in isolation
2. **Duo Condition**: Decision-making with one social partner present
3. **Trio Condition**: Decision-making with two social partners present

### Task Structure

Each trial consisted of:
- **Choice Phase**: Selection between explore and exploit options
- **Outcome Phase**: Reward delivery based on choice
- **Inter-trial Interval**: Variable delay between trials

### Social Context Manipulation

- **Partner Presence**: Visual and auditory access to conspecifics
- **Social Rank**: Established dominance hierarchy (R1, R2, R3)
- **Context Order**: Counterbalanced across subjects

## Data Collection

### Behavioral Measures

1. **Primary Outcome**: Binary choice (explore/exploit)
2. **Secondary Measures**:
   - Response latency
   - Reward magnitude
   - Social proximity
   - Gaze direction

### Data Recording

- **Software**: Custom behavioral testing platform
- **Sampling Rate**: 60 Hz
- **Duration**: 30-45 minutes per session
- **Trials per Session**: 50-100 trials

### Quality Control

- **Exclusion Criteria**: Trials with technical failures
- **Missing Data**: < 5% of total trials
- **Outlier Detection**: Response times > 3 SD from mean

## Statistical Analysis

### Data Preprocessing

1. **Filtering**: Retain only OIT_RE trial types
2. **Variable Creation**:
   - `context`: Factor (solo, duo, trio)
   - `explore`: Binary outcome (0/1)
   - `partner_count`: Numeric (0, 1, 2)
   - `rank`: Factor (1, 2, 3)

3. **Missing Data**: Complete case analysis

### Primary Statistical Model

**Bayesian Hierarchical Logistic Regression**

```r
explore ~ partner_count + rank + context + (1|monkey_id)
```

**Model Components**:
- **Fixed Effects**: Social context, rank, partner count
- **Random Effects**: Individual monkey intercepts
- **Family**: Bernoulli (logistic)
- **Prior**: Weakly informative priors

### Model Specification

```r
# Prior specifications
prior <- c(
  prior(normal(0, 2), class = "b"),
  prior(normal(0, 1), class = "sd")
)

# Model fitting
fit <- brm(
  explore ~ partner_count + rank + context + (1|monkey_id),
  data = df,
  family = bernoulli(),
  prior = prior,
  chains = 4,
  iter = 2000,
  seed = 123
)
```

### Model Diagnostics

1. **Convergence**: R-hat < 1.1
2. **Effective Sample Size**: ESS > 400
3. **Posterior Predictive Checks**: Visual inspection
4. **Cross-validation**: LOO-CV for model comparison

### Effect Size Calculations

- **Odds Ratios**: Exponentiated coefficients
- **Credible Intervals**: 95% HPD intervals
- **Marginal Effects**: Average predicted probabilities

## Validation Procedures

### Internal Validation

1. **Cross-validation**: Leave-one-out cross-validation
2. **Bootstrap**: 1000 resamples for confidence intervals
3. **Sensitivity Analysis**: Different prior specifications

### External Validation

1. **Holdout Sample**: 20% of data for validation
2. **Predictive Accuracy**: ROC-AUC and calibration
3. **Model Comparison**: AIC, BIC, LOO-CV

## Reproducibility Measures

### Code Organization

- **Modular Design**: Separate functions for each analysis step
- **Version Control**: Git repository with tagged releases
- **Documentation**: Comprehensive code comments

### Data Management

- **Raw Data**: Preserved in original format
- **Processed Data**: Scripts for data cleaning
- **Metadata**: Complete variable descriptions

### Computational Environment

- **R Version**: 4.0.0 or higher
- **Package Versions**: Pinned to specific versions
- **Random Seeds**: Set for reproducibility

## Ethical Considerations

### Animal Welfare

- **Housing**: Enriched environments with social groups
- **Training**: Positive reinforcement methods
- **Monitoring**: Daily health and behavioral assessments

### Data Privacy

- **Anonymization**: Individual identifiers removed
- **Access Control**: Limited to research team
- **Storage**: Secure, encrypted data storage

## Limitations

### Sample Size

- **Power**: Limited by small sample size (n=6)
- **Generalization**: Results may not extend to other populations
- **Individual Differences**: Large variance between subjects

### Experimental Design

- **Context Effects**: Order effects possible despite counterbalancing
- **Social Complexity**: Limited to three social contexts
- **Temporal Effects**: No long-term follow-up

### Statistical Assumptions

- **Independence**: Trials may not be fully independent
- **Linearity**: Assumed linear effects of continuous predictors
- **Normality**: Random effects assumed normal

## Future Directions

### Methodological Improvements

1. **Larger Sample**: Increase sample size for better power
2. **Longer Sessions**: Extended data collection periods
3. **Additional Measures**: Physiological and neural correlates

### Analytical Extensions

1. **Time Series**: Modeling temporal dynamics
2. **Network Analysis**: Social network effects
3. **Machine Learning**: Alternative modeling approaches

### Theoretical Development

1. **Mechanistic Models**: Process models of decision-making
2. **Individual Differences**: Personality and cognitive factors
3. **Evolutionary Context**: Comparative studies across species 