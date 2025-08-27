# Model Specification Diagrams - Summary

## Overview
These diagrams provide visual explanations of the mathematical models used in the exploration analysis, replacing the useless G6 calibration plot with informative model specification diagrams.

## Generated Diagrams

### 1. **Mathematical Equation Diagram** (`mathematical_equation_diagram.png/pdf`)
**Purpose**: Shows the exact mathematical specification of the model
**Content**:
- **Equation**: P(Explore) = logit(β₀ + β₁×Rank + β₂×Partners + uᵢ)
- **Components**:
  - P(Explore): Probability of exploration (response variable)
  - β₀: Intercept (baseline exploration tendency)
  - β₁: Coefficient for rank effect
  - β₂: Coefficient for partner count effect
  - uᵢ: Random intercept for individual i
- **Mathematical Details**: Logistic regression with random intercepts

### 2. **Predictor Variables Diagram** (`predictor_variables_diagram.png/pdf`)
**Purpose**: Explains all predictor variables used in the model
**Content**:
- **Relative Rank**: Categorical (1=Dominant, 2=Intermediate, 3=Subordinate)
- **Partner Count**: Continuous (0=Solo, 1=Duo, 2=Trio)
- **Individual ID**: Random effect (FRAN, DALI, EBI, CHOCOLAT, ICE, ANEMONE)
- **Expected Reward**: Continuous (0-1 scale)
- **Descriptions**: What each variable represents and how it's coded

### 3. **Model Comparison Diagram** (`model_comparison_diagram.png/pdf`)
**Purpose**: Shows all models tested and their specifications
**Content**:
- **Baseline**: explore ~ 1 + (1|id) - Chance baseline
- **Main Effects**: explore ~ rank + partners + (1|id) - Main effects only
- **Interaction**: explore ~ rank × partners + (1|id) - Include interactions
- **Relative Rank**: explore ~ relative_rank + partners + (1|id) - Relative rank model
- **Absolute Rank**: explore ~ absolute_rank + partners + (1|id) - Absolute rank model
- **Parameters**: Number of parameters in each model
- **Complexity**: Low/Medium/High complexity classification

### 4. **Bayesian Inference Diagram** (`bayesian_inference_diagram.png/pdf`)
**Purpose**: Explains the Bayesian inference process
**Content**:
- **Prior**: Initial beliefs about parameters
- **Likelihood**: Data likelihood given parameters
- **Posterior**: Updated beliefs after seeing data
- **Predictions**: Predictions for new data
- **Model Comparison**: LOO, AIC, BIC comparison methods
- **Flow**: Shows the sequential process of Bayesian inference

### 5. **Comprehensive Model Specification** (`comprehensive_model_specification.png/pdf`)
**Purpose**: Complete overview of the model specification
**Content**:
- **Response Variable**: Binary: explore (1) vs exploit (0)
- **Distribution**: Bernoulli(πᵢ)
- **Link Function**: logit(πᵢ) = ηᵢ
- **Fixed Effects**: β₀ + β₁×rank + β₂×partners
- **Random Effects**: uᵢ ~ N(0, σ²) for individual i
- **Prior Distributions**: Normal priors for β, Half-Cauchy for σ
- **Inference Method**: Hamiltonian Monte Carlo (Stan)
- **Model Comparison**: LOO cross-validation, AIC, BIC
- **Validation**: Posterior predictive checks
- **Predictions**: P(explore|new_data)

### 6. **Model Structure Flow** (`model_structure_flow.png/pdf`)
**Purpose**: Shows the flow from predictors to final output
**Content**:
- **Predictors**: Rank, Partners, Context
- **Linear Predictor**: η = β₀ + β₁×rank + β₂×partners
- **Logistic Function**: P(explore) = 1/(1 + exp(-η))
- **Probability**: Exploration probability
- **Random Effect**: uᵢ ~ N(0, σ²)
- **Individual Variation**: Individual-specific baseline
- **Binary Choice**: explore (1) or exploit (0)
- **Model Fit**: Bayesian inference with brms

## Mathematical Details

### Model Specification
```
yᵢ ~ Bernoulli(πᵢ)
logit(πᵢ) = ηᵢ
ηᵢ = β₀ + β₁×rankᵢ + β₂×partnersᵢ + uᵢ
uᵢ ~ N(0, σ²)
```

### Prior Specifications
```
β₀ ~ N(0, 10)  # Intercept prior
β₁ ~ N(0, 5)   # Rank coefficient prior
β₂ ~ N(0, 5)   # Partner coefficient prior
σ ~ Half-Cauchy(0, 2)  # Random effect SD prior
```

### Model Comparison Metrics
- **LOO**: Leave-one-out cross-validation
- **AIC**: Akaike Information Criterion
- **BIC**: Bayesian Information Criterion
- **ELPD**: Expected Log Predictive Density

## Key Insights from Model Specification

### 1. **Model Structure**
- Binary response variable (explore vs exploit)
- Logistic link function for probability modeling
- Random intercepts for individual differences
- Fixed effects for rank and social context

### 2. **Predictor Effects**
- **Rank Effect**: β₁ captures how rank influences exploration
- **Social Context**: β₂ captures how partner count influences exploration
- **Individual Variation**: uᵢ captures individual-specific baseline tendencies

### 3. **Bayesian Approach**
- Uses informative priors based on domain knowledge
- Hamiltonian Monte Carlo sampling for robust inference
- Full posterior distributions for uncertainty quantification
- Model comparison using multiple criteria

### 4. **Model Comparison Results**
- Relative rank model performs better than absolute rank
- Main effects model captures most variation
- Interaction model shows minimal improvement
- All models perform above chance level

## Advantages of These Diagrams

### 1. **Mathematical Transparency**
- Clear equation specification
- Explicit parameter meanings
- Prior distribution specifications
- Model comparison criteria

### 2. **Visual Clarity**
- Color-coded components
- Logical flow from predictors to output
- Step-by-step process explanation
- Comprehensive model overview

### 3. **Educational Value**
- Explains Bayesian inference process
- Shows model comparison methods
- Demonstrates predictor coding
- Illustrates random effects structure

### 4. **Poster-Ready**
- Professional appearance
- Clear mathematical notation
- Informative content
- Replace useless calibration plot

## Usage in Poster

These diagrams can replace G6 and provide:
1. **Model Structure**: Clear explanation of the mathematical model
2. **Predictor Details**: What variables are used and how
3. **Bayesian Process**: How the inference works
4. **Model Comparison**: How different models are compared
5. **Mathematical Rigor**: Proper statistical specification

## Files Generated
- `mathematical_equation_diagram.png/pdf`
- `predictor_variables_diagram.png/pdf`
- `model_comparison_diagram.png/pdf`
- `bayesian_inference_diagram.png/pdf`
- `comprehensive_model_specification.png/pdf`
- `model_structure_flow.png/pdf`

All files are high-resolution (300 DPI) and suitable for professional poster presentation. 