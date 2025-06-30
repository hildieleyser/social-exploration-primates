# Mathematical Models in Social Decision-Making: Literature Review and Model Development

## Abstract

This document provides a comprehensive review of mathematical models used in social decision-making research, with particular focus on explore-exploit paradigms in social contexts. We detail the theoretical foundations, mathematical formulations, and justification for our hierarchical multinomial logistic regression approach.

## 1. Literature Review: Mathematical Models in Social Decision-Making

### 1.1 Foundational Models in Explore-Exploit Decision-Making

#### Multi-Armed Bandit Models
The explore-exploit dilemma was first formalized in the context of multi-armed bandit problems (Robbins, 1952; Berry & Fristedt, 1985). These models assume:

```
V_t(a) = μ_a + ε_t
```

Where `V_t(a)` is the value of action `a` at time `t`, `μ_a` is the true mean reward, and `ε_t` is noise.

**Key Papers:**
- Robbins, H. (1952). Some aspects of the sequential design of experiments. *Bulletin of the American Mathematical Society*, 58(5), 527-535.
- Sutton, R. S., & Barto, A. G. (2018). *Reinforcement learning: An introduction*. MIT Press.

#### Reinforcement Learning Models
Q-learning and temporal difference models extended bandit approaches:

```
Q_{t+1}(s,a) = Q_t(s,a) + α[r_{t+1} + γ max_a Q_t(s',a') - Q_t(s,a)]
```

**Key Papers:**
- Watkins, C. J., & Dayan, P. (1992). Q-learning. *Machine Learning*, 8(3-4), 279-292.
- Daw, N. D., et al. (2006). Cortical substrates for exploratory decisions in humans. *Nature*, 441(7095), 876-879.

### 1.2 Social Context in Decision-Making Models

#### Social Learning Models
Bandura's social cognitive theory was formalized mathematically:

```
P(action|social_info) = f(individual_preference + social_influence)
```

**Key Papers:**
- Bandura, A. (1977). *Social learning theory*. Prentice Hall.
- Henrich, J., & McElreath, R. (2003). The evolution of cultural evolution. *Evolutionary Anthropology*, 12(3), 123-135.

#### Game Theoretic Approaches
Nash equilibrium models in social contexts:

```
u_i(s_i, s_{-i}) = Σ_j p_j * payoff_ij
```

**Key Papers:**
- Nash, J. (1950). Equilibrium points in n-person games. *PNAS*, 36(1), 48-49.
- Fehr, E., & Schmidt, K. M. (1999). A theory of fairness, competition, and cooperation. *Quarterly Journal of Economics*, 114(3), 817-868.

### 1.3 Hierarchical Models in Behavioral Research

#### Multilevel Models for Nested Data
Recognition that behavioral data is inherently hierarchical led to mixed-effects models:

```
Y_ij = β_0 + β_1X_ij + u_j + ε_ij
```

**Key Papers:**
- Raudenbush, S. W., & Bryk, A. S. (2002). *Hierarchical linear models*. Sage Publications.
- Gelman, A., & Hill, J. (2006). *Data analysis using regression and multilevel/hierarchical models*. Cambridge University Press.

#### Bayesian Hierarchical Models
Bayesian approaches to hierarchical modeling:

```
θ_j ~ Normal(μ_θ, σ_θ²)
μ_θ ~ Normal(μ_0, σ_0²)
```

**Key Papers:**
- Gelman, A., et al. (2013). *Bayesian data analysis*. CRC Press.
- McElreath, R. (2020). *Statistical rethinking: A Bayesian course with examples in R and Stan*. CRC Press.

### 1.4 Multinomial Models in Choice Behavior

#### Multinomial Logistic Regression
Extension of logistic regression to multiple categories:

```
P(Y = k) = exp(X'β_k) / Σ_j exp(X'β_j)
```

**Key Papers:**
- McFadden, D. (1974). Conditional logit analysis of qualitative choice behavior. *Frontiers in Econometrics*, 105-142.
- Train, K. E. (2009). *Discrete choice methods with simulation*. Cambridge University Press.

#### Mixed Logit Models
Incorporation of random effects in choice models:

```
P_nit = ∫ L_nit(β) f(β|θ) dβ
```

**Key Papers:**
- Revelt, D., & Train, K. (1998). Mixed logit with repeated choices. *Review of Economics and Statistics*, 80(4), 647-657.
- Hensher, D. A., & Greene, W. H. (2003). The mixed logit model. *Applied Economics*, 35(15), 1603-1618.

## 2. Model Development and Justification

### 2.1 Why Hierarchical Multinomial Logistic Regression?

Our choice of hierarchical multinomial logistic regression was driven by several key considerations:

#### 2.1.1 Data Structure Requirements
- **Nested data**: Trials within blocks within individuals
- **Multiple outcome categories**: Explore, exploit, none
- **Individual differences**: Need to account for subject-level variation
- **Unbalanced design**: Different numbers of trials per condition

#### 2.1.2 Theoretical Justification
The trinomial choice structure reflects the cognitive architecture of decision-making:

1. **Exploration**: Novelty-seeking, information-gathering behavior
2. **Exploitation**: Value-maximizing, reward-seeking behavior  
3. **Inaction**: Disengagement, withdrawal from choice

This maps onto dual-process theories (Kahneman, 2011) and reinforcement learning frameworks (Sutton & Barto, 2018).

#### 2.1.3 Statistical Advantages
- **Flexible covariate effects**: Different predictors can affect each outcome differently
- **Proper uncertainty quantification**: Hierarchical structure captures multiple sources of variation
- **Missing data robustness**: Maximum likelihood handles incomplete observations
- **Model comparison**: AIC/BIC allow systematic model selection

### 2.2 Model Development Process

#### 2.2.1 Literature Synthesis
We synthesized approaches from:
- **Behavioral economics**: Explore-exploit paradigms (Cohen et al., 2007)
- **Social psychology**: Social facilitation and inhibition (Zajonc, 1965)
- **Cognitive science**: Dual-process theories (Evans & Stanovich, 2013)
- **Comparative psychology**: Individual differences in primates (Herrmann et al., 2007)

#### 2.2.2 Model Selection Criteria
We evaluated models based on:
- **Theoretical grounding**: Does the model reflect known cognitive mechanisms?
- **Statistical fit**: AIC, BIC, likelihood ratio tests
- **Interpretability**: Can parameters be meaningfully interpreted?
- **Robustness**: Does the model perform well under different conditions?

## 3. Mathematical Model Specification

### 3.1 Complete Model Formulation

#### Level 1: Observation Model
For trial `i` within block `j` within individual `k`:

```
Y_ijk ~ Multinomial(π_ijk^{explore}, π_ijk^{exploit}, π_ijk^{none})
```

#### Level 2: Linear Predictors
Using exploit as the reference category:

```
log(π_ijk^{explore} / π_ijk^{exploit}) = X_ijk' β^{explore} + u_jk^{explore} + v_k^{explore}

log(π_ijk^{none} / π_ijk^{exploit}) = X_ijk' β^{none} + u_jk^{none} + v_k^{none}
```

#### Level 3: Random Effects Structure
```
u_jk ~ MVN(0, Σ_block)
v_k ~ MVN(0, Σ_individual)
```

### 3.2 Expanded Linear Predictor

```
X_ijk' β = β_0 + β_1(SocialComplexity_ijk) + β_2(Rank_ijk) + 
           β_3(SubjectiveValue_ijk) + β_4(ExploitPreference_ijk) + 
           β_5(ExploreExpectation_ijk) + β_6(PartnerPresence_ijk)
```

### 3.3 Probability Calculations

```
π_ijk^{explore} = exp(η_ijk^{explore}) / (1 + exp(η_ijk^{explore}) + exp(η_ijk^{none}))

π_ijk^{exploit} = 1 / (1 + exp(η_ijk^{explore}) + exp(η_ijk^{none}))

π_ijk^{none} = exp(η_ijk^{none}) / (1 + exp(η_ijk^{explore}) + exp(η_ijk^{none}))
```

## 4. Key References

### Theoretical Foundations
- Cohen, J. D., et al. (2007). Should I stay or should I go? How the human brain manages the trade-off between exploitation and exploration. *Philosophical Transactions of the Royal Society B*, 362(1481), 933-942.
- Kahneman, D. (2011). *Thinking, fast and slow*. Farrar, Straus and Giroux.
- Zajonc, R. B. (1965). Social facilitation. *Science*, 149(3681), 269-274.

### Statistical Methods
- Agresti, A. (2013). *Categorical data analysis*. Wiley.
- Gelman, A., & Hill, J. (2006). *Data analysis using regression and multilevel/hierarchical models*. Cambridge University Press.
- McFadden, D. (1974). Conditional logit analysis of qualitative choice behavior. *Frontiers in Econometrics*, 105-142.

### Comparative Psychology
- Herrmann, E., et al. (2007). Humans have evolved specialized skills of social cognition. *Science*, 317(5843), 1360-1366.
- Tomasello, M., & Call, J. (1997). *Primate cognition*. Oxford University Press.

### Social Decision-Making
- Henrich, J., & McElreath, R. (2003). The evolution of cultural evolution. *Evolutionary Anthropology*, 12(3), 123-135.
- Mesoudi, A. (2011). *Cultural evolution: How Darwinian theory can explain human culture*. University of Chicago Press.

## 5. Model Validation and Robustness

### 5.1 Cross-Validation
- **K-fold cross-validation**: 10-fold CV to assess generalization
- **Individual-level validation**: Leave-one-subject-out analysis
- **Temporal validation**: Train on early blocks, test on later blocks

### 5.2 Sensitivity Analysis
- **Prior sensitivity**: Robust to different prior specifications
- **Outlier detection**: Influence diagnostics for extreme observations
- **Missing data**: Multiple imputation for robustness checks

### 5.3 Model Comparison
- **Nested model tests**: Likelihood ratio tests for model components
- **Information criteria**: AIC, BIC, WAIC for model selection
- **Predictive accuracy**: Out-of-sample prediction performance

## 6. Future Extensions

### 6.1 Temporal Dynamics
Incorporation of time-varying effects:
```
β_t = β_{t-1} + w_t
```

### 6.2 Network Effects
Social network structure in group contexts:
```
Y_i ~ f(X_i, Σ_j A_ij Y_j)
```

### 6.3 Non-Linear Effects
Spline and kernel methods for complex relationships:
```
f(x) = Σ_k β_k B_k(x)
```

This comprehensive framework provides the theoretical and mathematical foundation for understanding how social frames of reference influence explore-exploit decision-making in non-human primates. 