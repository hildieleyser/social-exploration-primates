# Methodology: Social Frames of Reference Analysis

## Overview

This document provides detailed methodological information for the social frames of reference analysis in explore-exploit decision-making.

## Experimental Design

### Paradigm: Explore-Exploit Task

The explore-exploit paradigm presents subjects with a fundamental decision-making challenge:

- **Exploration**: Choosing novel options with uncertain outcomes
- **Exploitation**: Choosing familiar options with known rewards  
- **Inaction**: Abstaining from choice (neither explore nor exploit)

### Social Context Manipulation

Social complexity was manipulated across three levels:

1. **Individual (Solo)**: Single subject making decisions alone
   - Baseline cognitive load
   - No social monitoring required
   - Individual decision-making only

2. **Dyadic (Duo)**: Two subjects making decisions together
   - Moderate cognitive load
   - Social monitoring of one partner
   - Coordination requirements

3. **Triadic (Trio)**: Three subjects making decisions together
   - High cognitive load
   - Social monitoring of two partners
   - Complex coordination requirements

### Theoretical Framework

The theoretical model proposes that social complexity increases cognitive load through four mechanisms:

1. **Social Monitoring Demands**: Need to track and process information about partners' behaviors
2. **Coordination Requirements**: Need to synchronize decisions and actions with partners
3. **Competition for Resources**: Increased competition reduces exploration incentives
4. **Theory of Mind Computations**: Need to predict and model partners' intentions and strategies

**Hypothesis**: Increasing social complexity will reduce exploration behavior due to increased cognitive load.

## Data Structure

### Hierarchical Organization

The data follows a nested hierarchical structure:

```
Level 1: Population (All subjects)
├── Level 2: Individual subjects (N=6)
│   ├── Level 3: Experimental blocks (N=88)
│   │   └── Level 4: Individual trials (N=1,782)
```

### Subjects

Six non-human primates participated:
- CHOCOLAT
- DALI  
- EBI
- FRAN
- ICE
- ANEMONE

### Variables

#### Dependent Variable
- **OUTCOME**: Trinomial choice (explore/exploit/none)

#### Independent Variables
- **CONDITION**: Social context (solo/duo/trio)
- **monkey**: Individual subject identifier
- **BLOCK_No**: Experimental block identifier
- **PAIRED_WITH**: Partner identity in social conditions
- **RELATIVE_RANK**: Social dominance rank (1=dominant, 3=subordinate)

#### Continuous Predictors
- **SUBJECTIVE_CHOSEN_VALUE**: Subjective value of chosen option
- **subjective_exploit**: Expected value of exploitation
- **expected_explore**: Running expectation for exploration

## Statistical Analysis

### Primary Model: Multinomial Logistic Regression

The analysis uses multinomial logistic regression to model the trinomial choice outcome.

#### Model Specification

**Level 1 - Likelihood:**
```
Y_ijkl ~ Multinomial(π_exploit, π_explore, π_none)
```

**Level 2 - Linear Predictors:**
Using 'exploit' as the reference category:
```
log(π_explore / π_exploit) = β₀ + X'β
log(π_none / π_exploit) = γ₀ + X'γ
```

**Expanded Linear Predictor:**
```
X'β = β₁(Social_Complexity) + β₂(Partner_Presence) + β₃(Rank) + 
      β₄(Subjective_Value) + β₅(Exploit_Preference) + β₆(Explore_Expectation)
```

#### Variable Coding

- **Social Complexity**: Dummy coded (solo=reference, duo, trio)
- **Partner Presence**: Binary (0=no partner, 1=partnered)
- **Rank**: Continuous (1-3, standardized)
- **Subjective Values**: Continuous (standardized)

### Model Comparison Strategy

Multiple models are fitted and compared:

1. **Model 1**: Social complexity only
2. **Model 2**: Social complexity + individual effects
3. **Model 3**: Full model with all predictors

Model comparison uses:
- Akaike Information Criterion (AIC)
- Likelihood ratio tests
- Deviance statistics

### Statistical Tests

#### Primary Analyses
- **Chi-square test**: Association between condition and outcome
- **ANOVA**: Exploration rates across conditions
- **Multinomial logistic regression**: Full model estimation

#### Effect Size Calculations
- **Cohen's d**: Standardized effect sizes for pairwise comparisons
- **Odds ratios**: From logistic regression coefficients
- **Percentage point differences**: Raw effect sizes

#### Post-hoc Analyses
- **Pairwise t-tests**: Between-condition comparisons
- **Individual difference analyses**: Subject-level variation
- **Rank effect analyses**: Dominance hierarchy effects

## Data Processing Pipeline

### Data Cleaning Steps

1. **Filter experimental trials**: Include only 'OIT_RE' trial types
2. **Clean outcome variable**: Standardize outcome coding
3. **Handle missing values**: Remove trials with missing key variables
4. **Validate data integrity**: Check for logical inconsistencies

### Variable Transformations

1. **Standardization**: Continuous predictors standardized (mean=0, SD=1)
2. **Dummy coding**: Categorical variables converted to dummy codes
3. **Reference categories**: Establish meaningful reference levels

### Quality Control

1. **Data validation**: Check ranges and distributions
2. **Outlier detection**: Identify and handle extreme values
3. **Missing data patterns**: Assess missingness mechanisms
4. **Model diagnostics**: Check assumptions and fit

## Model Assumptions

### Multinomial Logistic Regression Assumptions

1. **Independence**: Observations are independent
2. **Linearity**: Linear relationship between predictors and log-odds
3. **No multicollinearity**: Predictors are not highly correlated
4. **Adequate sample size**: Sufficient observations per category

### Assumption Checking

- **Independence**: Addressed through hierarchical modeling approach
- **Linearity**: Assessed through residual plots and model diagnostics
- **Multicollinearity**: Checked using variance inflation factors
- **Sample size**: Verified adequate cell counts for all outcome categories

## Interpretation Framework

### Effect Size Guidelines

- **Small effect**: Cohen's d < 0.2
- **Medium effect**: Cohen's d = 0.2-0.8  
- **Large effect**: Cohen's d > 0.8

### Statistical Significance

- **Alpha level**: 0.05
- **Multiple comparisons**: Bonferroni correction when appropriate
- **Confidence intervals**: 95% confidence intervals reported

### Practical Significance

Effects are evaluated for both statistical and practical significance, considering:
- Magnitude of behavioral changes
- Theoretical importance
- Real-world implications

## Limitations

### Design Limitations
- Observational rather than fully experimental design
- Limited number of subjects (N=6)
- Unbalanced design across conditions

### Statistical Limitations
- Hierarchical structure not fully modeled in primary analysis
- Potential temporal dependencies not explicitly modeled
- Individual differences may not be fully captured

### Generalizability
- Results specific to study species and context
- Laboratory setting may not reflect natural behavior
- Social group composition effects not systematically varied

## Future Directions

### Methodological Improvements
- Full hierarchical Bayesian modeling
- Temporal dynamics modeling
- Network analysis of social interactions

### Theoretical Extensions
- Cross-species comparisons
- Neural mechanism investigations
- Developmental trajectory analysis 