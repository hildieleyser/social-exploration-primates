# Mathematical Models in Social Decision-Making: My Literature Review and Model Development

## Abstract

This document provides my comprehensive review of mathematical models used in social decision-making research, with particular focus on explore-exploit paradigms in social contexts. I detail the theoretical foundations, mathematical formulations, and justification for my hierarchical multinomial logistic regression approach.

## 1. My Literature Review: Mathematical Models in Social Decision-Making

### 1.1 Foundational Models in Explore-Exploit Decision-Making That Informed My Work

#### Multi-Armed Bandit Models
I began by examining how the explore-exploit dilemma was first formalized in the context of multi-armed bandit problems (Robbins, 1952; Berry & Fristedt, 1985). These models assume:

```
V_t(a) = μ_a + ε_t
```

Where `V_t(a)` is the value of action `a` at time `t`, `μ_a` is the true mean reward, and `ε_t` is noise.

**Key Papers I Referenced:**
- Robbins, H. (1952). Some aspects of the sequential design of experiments. *Bulletin of the American Mathematical Society*, 58(5), 527-535.
- Sutton, R. S., & Barto, A. G. (2018). *Reinforcement learning: An introduction*. MIT Press.

#### Reinforcement Learning Models
I also considered how Q-learning and temporal difference models extended bandit approaches:

```
Q_{t+1}(s,a) = Q_t(s,a) + α[r_{t+1} + γ max_a Q_t(s',a') - Q_t(s,a)]
```

**Key Papers I Studied:**
- Watkins, C. J., & Dayan, P. (1992). Q-learning. *Machine Learning*, 8(3-4), 279-292.
- Daw, N. D., et al. (2006). Cortical substrates for exploratory decisions in humans. *Nature*, 441(7095), 876-879.

### 1.2 Social Context in Decision-Making Models That Influenced My Approach

#### Social Learning Models
I examined how Bandura's social cognitive theory was formalized mathematically:

```
P(action|social_info) = f(individual_preference + social_influence)
```

**Key Papers I Incorporated:**
- Bandura, A. (1977). *Social learning theory*. Prentice Hall.
- Henrich, J., & McElreath, R. (2003). The evolution of cultural evolution. *Evolutionary Anthropology*, 12(3), 123-135.

#### Game Theoretic Approaches
I considered Nash equilibrium models in social contexts:

```
u_i(s_i, s_{-i}) = Σ_j p_j * payoff_ij
```

**Key Papers I Reviewed:**
- Nash, J. (1950). Equilibrium points in n-person games. *PNAS*, 36(1), 48-49.
- Fehr, E., & Schmidt, K. M. (1999). A theory of fairness, competition, and cooperation. *Quarterly Journal of Economics*, 114(3), 817-868.

### 1.3 Hierarchical Models in Behavioral Research That Guided My Method

#### Multilevel Models for Nested Data
I recognized that my behavioral data is inherently hierarchical, which led me to mixed-effects models:

```
Y_ij = β_0 + β_1X_ij + u_j + ε_ij
```

**Key Papers I Applied:**
- Raudenbush, S. W., & Bryk, A. S. (2002). *Hierarchical linear models*. Sage Publications.
- Gelman, A., & Hill, J. (2006). *Data analysis using regression and multilevel/hierarchical models*. Cambridge University Press.

#### Bayesian Hierarchical Models
I explored Bayesian approaches to hierarchical modeling:

```
θ_j ~ Normal(μ_θ, σ_θ²)
μ_θ ~ Normal(μ_0, σ_0²)
```

**Key Papers I Consulted:**
- Gelman, A., et al. (2013). *Bayesian data analysis*. CRC Press.
- McElreath, R. (2020). *Statistical rethinking: A Bayesian course with examples in R and Stan*. CRC Press.

### 1.4 Multinomial Models in Choice Behavior That Shaped My Analysis

#### Multinomial Logistic Regression
I adopted the extension of logistic regression to multiple categories:

```
P(Y = k) = exp(X'β_k) / Σ_j exp(X'β_j)
```

**Key Papers I Built Upon:**
- McFadden, D. (1974). Conditional logit analysis of qualitative choice behavior. *Frontiers in Econometrics*, 105-142.
- Train, K. E. (2009). *Discrete choice methods with simulation*. Cambridge University Press.

#### Mixed Logit Models
I incorporated random effects in choice models:

```
P_nit = ∫ L_nit(β) f(β|θ) dβ
```

**Key Papers I Referenced:**
- Revelt, D., & Train, K. (1998). Mixed logit with repeated choices. *Review of Economics and Statistics*, 80(4), 647-657.
- Hensher, D. A., & Greene, W. H. (2003). The mixed logit model. *Applied Economics*, 35(15), 1603-1618.

## 2. My Model Development and Justification

### 2.1 Why I Chose Hierarchical Multinomial Logistic Regression

My choice of hierarchical multinomial logistic regression was driven by several key considerations based on my data and research questions:

#### 2.1.1 My Data Structure Requirements
- **Nested data**: I had trials within blocks within individuals
- **Multiple outcome categories**: I observed explore, exploit, none behaviors
- **Individual differences**: I needed to account for subject-level variation
- **Unbalanced design**: I had different numbers of trials per condition

#### 2.1.2 My Theoretical Justification
I designed the trinomial choice structure to reflect the cognitive architecture of decision-making as I understood it:

1. **Exploration**: Novelty-seeking, information-gathering behavior I observed
2. **Exploitation**: Value-maximizing, reward-seeking behavior I documented
3. **Inaction**: Disengagement, withdrawal from choice I recorded

This maps onto dual-process theories (Kahneman, 2011) and reinforcement learning frameworks (Sutton & Barto, 2018) that informed my theoretical approach.

#### 2.1.3 Statistical Advantages I Gained
- **Flexible covariate effects**: Different predictors could affect each outcome differently in my model
- **Proper uncertainty quantification**: My hierarchical structure captures multiple sources of variation
- **Missing data robustness**: Maximum likelihood handles incomplete observations in my dataset
- **Model comparison**: AIC/BIC allow me systematic model selection

### 2.2 My Model Development Process

#### 2.2.1 How I Synthesized the Literature
I synthesized approaches from:
- **Behavioral economics**: Explore-exploit paradigms (Cohen et al., 2007)
- **Social psychology**: Social facilitation and inhibition (Zajonc, 1965)
- **Cognitive science**: Dual-process theories (Evans & Stanovich, 2013)
- **Comparative psychology**: Individual differences in primates (Herrmann et al., 2007)

#### 2.2.2 My Model Selection Criteria
I evaluated models based on:
- **Theoretical grounding**: Does my model reflect known cognitive mechanisms?
- **Statistical fit**: AIC, BIC, likelihood ratio tests I performed
- **Interpretability**: Can I meaningfully interpret the parameters?
- **Robustness**: Does my model perform well under different conditions?

## 3. My Mathematical Model Specification

### 3.1 My Complete Model Formulation

#### Level 1: My Observation Model
For trial `i` within block `j` within individual `k` in my dataset:

```
Y_ijk ~ Multinomial(π_ijk^{explore}, π_ijk^{exploit}, π_ijk^{none})
```

#### Level 2: My Linear Predictors
Using exploit as my reference category:

```
log(π_ijk^{explore} / π_ijk^{exploit}) = X_ijk' β^{explore} + u_jk^{explore} + v_k^{explore}

log(π_ijk^{none} / π_ijk^{exploit}) = X_ijk' β^{none} + u_jk^{none} + v_k^{none}
```

#### Level 3: My Random Effects Structure
```
u_jk ~ MVN(0, Σ_block)
v_k ~ MVN(0, Σ_individual)
```

### 3.2 My Expanded Linear Predictor

```
X_ijk' β = β_0 + β_1(SocialComplexity_ijk) + β_2(Rank_ijk) + 
           β_3(SubjectiveValue_ijk) + β_4(ExploitPreference_ijk) + 
           β_5(ExploreExpectation_ijk) + β_6(PartnerPresence_ijk)
```

### 3.3 My Probability Calculations

```
π_ijk^{explore} = exp(η_ijk^{explore}) / (1 + exp(η_ijk^{explore}) + exp(η_ijk^{none}))

π_ijk^{exploit} = 1 / (1 + exp(η_ijk^{explore}) + exp(η_ijk^{none}))

π_ijk^{none} = exp(η_ijk^{none}) / (1 + exp(η_ijk^{explore}) + exp(η_ijk^{none}))
```

## 4. Key References That Informed My Work

### Theoretical Foundations I Drew From
- Cohen, J. D., et al. (2007). Should I stay or should I go? How the human brain manages the trade-off between exploitation and exploration. *Philosophical Transactions of the Royal Society B*, 362(1481), 933-942.
- Kahneman, D. (2011). *Thinking, fast and slow*. Farrar, Straus and Giroux.
- Zajonc, R. B. (1965). Social facilitation. *Science*, 149(3681), 269-274.

### Statistical Methods I Applied
- Agresti, A. (2013). *Categorical data analysis*. Wiley.
- Gelman, A., & Hill, J. (2006). *Data analysis using regression and multilevel/hierarchical models*. Cambridge University Press.
- McFadden, D. (1974). Conditional logit analysis of qualitative choice behavior. *Frontiers in Econometrics*, 105-142.

### Comparative Psychology Literature I Consulted
- Herrmann, E., et al. (2007). Humans have evolved specialized skills of social cognition. *Science*, 317(5843), 1360-1366.
- Tomasello, M., & Call, J. (1997). *Primate cognition*. Oxford University Press.

### Social Decision-Making Research I Built Upon
- Henrich, J., & McElreath, R. (2003). The evolution of cultural evolution. *Evolutionary Anthropology*, 12(3), 123-135.
- Mesoudi, A. (2011). *Cultural evolution: How Darwinian theory can explain human culture*. University of Chicago Press.

## 5. My Model Validation and Robustness Checks

### 5.1 Cross-Validation I Performed
- **K-fold cross-validation**: 10-fold CV I used to assess generalization
- **Individual-level validation**: Leave-one-subject-out analysis I conducted
- **Temporal validation**: I trained on early blocks, tested on later blocks

### 5.2 Sensitivity Analysis I Conducted
- **Prior sensitivity**: I confirmed robustness to different prior specifications
- **Outlier detection**: I used influence diagnostics for extreme observations
- **Missing data**: I performed multiple imputation for robustness checks

### 5.3 Model Comparison I Undertook
- **Nested model tests**: Likelihood ratio tests I ran for model components
- **Information criteria**: AIC, BIC, WAIC I used for model selection
- **Predictive accuracy**: Out-of-sample prediction performance I evaluated

## 6. Future Extensions I'm Considering

### 6.1 Temporal Dynamics
I'm considering incorporating time-varying effects:
```
β_t = β_{t-1} + w_t
```

### 6.2 Network Effects
I might explore social network structure in group contexts:
```
Y_i ~ f(X_i, Σ_j A_ij Y_j)
```

### 6.3 Non-Linear Effects
I could implement spline and kernel methods for complex relationships:
```
f(x) = Σ_k β_k B_k(x)
```

This comprehensive framework provides the theoretical and mathematical foundation for my understanding of how social frames of reference influence explore-exploit decision-making in non-human primates. 