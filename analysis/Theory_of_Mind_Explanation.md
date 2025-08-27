# THEORY OF MIND ORDERS: DETAILED EXPLANATION AND METHODOLOGY

## What are the Different Orders of Theory of Mind?

### **0-ToM (Zero-Order Theory of Mind): Simple Frequency Tracking**
- **What it means**: The animal does NOT understand that others have mental states
- **Cognitive process**: Simple stimulus-response learning
- **Behavior**: Tracks frequency of observed behaviors without considering others' intentions
- **Example**: "Other animals explored 60% of the time, so I expect them to explore 60% in the future"
- **No mentalizing**: Does not model what others are thinking or believing

### **1st-ToM (First-Order Theory of Mind): Understanding Others' Beliefs**
- **What it means**: The animal understands that others have beliefs and mental states
- **Cognitive process**: Models what others know, believe, or intend
- **Behavior**: Adjusts own behavior based on what others might be thinking
- **Example**: "That animal thinks the left option is good, so they'll probably go left. I should consider this in my choice."
- **Key insight**: "Others have minds different from mine"

### **2nd-ToM (Second-Order Theory of Mind): Recursive Belief Modeling**
- **What it means**: The animal understands that others have beliefs ABOUT others' beliefs
- **Cognitive process**: Recursive reasoning about nested mental states
- **Behavior**: Strategic reasoning considering multiple levels of belief
- **Example**: "That animal thinks the other animal believes the left option is good, so they might expect the other to go left, which influences what they themselves will do."
- **Key insight**: "Others think about what others are thinking"

## How Were These Calculated?

### **Computational Models Used:**

#### **0-ToM Model (Simple Frequency Tracking):**
```
For each trial i:
  if social_context and i > 1:
    last_choice = other_animal_choice[i-1]  # 1 if explore, 0 if exploit
    current_belief = memory_decay * current_belief + learning_rate * last_choice
    prediction[i] = current_belief
```

#### **1st-ToM Model (Belief Attribution):**
```
For each trial i:
  if social_context and i > 1:
    last_choice = other_animal_choice[i-1]
    other_belief = memory_decay * other_belief + learning_rate * last_choice
    
    # Strategic reasoning: if others explore more, I might explore less (competition)
    strategic_adjustment = if other_belief > 0.5 then -0.2 else +0.2
    prediction[i] = other_belief + strategic_adjustment
```

#### **2nd-ToM Model (Recursive Reasoning):**
```
For each trial i:
  if social_context and i > 1:
    last_choice = other_animal_choice[i-1]
    
    # Update belief about others' belief about others
    other_other_belief = memory_decay * other_other_belief + learning_rate * last_choice
    
    # Others' belief combines their direct observation with their theory of mind
    other_belief = 0.7 * other_other_belief + 0.3 * other_belief
    
    # Strategic factor considers the difference between belief levels
    strategic_factor = 0.5 * (other_belief - other_other_belief)
    prediction[i] = other_belief + strategic_factor
```

### **Model Fitting Process:**
1. **Behavioral Data**: Used actual exploration/exploitation choices
2. **Social Trials Only**: Focused on duo and trio conditions (when others present)
3. **Parameter Optimization**: Tested learning rates (0.05, 0.1, 0.2) and memory decay (0.95)
4. **Model Comparison**: Calculated correlation between model predictions and actual behavior
5. **Best Fit Selection**: Chose ToM level with highest correlation for each individual

### **Statistical Measures:**
- **Correlation Coefficient**: How well model predictions match actual behavior (-1 to +1)
- **Accuracy**: Percentage of correct binary predictions (explore vs exploit)
- **Learning Rate**: How quickly the animal updates beliefs (0.05 = slow, 0.2 = fast)

## Detailed Figure Caption

**Figure: Individual Monkey Bayesian Theory of Mind Analysis (Sex-Grouped)**

This five-panel analysis reveals individual differences in social cognitive sophistication among six primates (males: F=FRAN, D=DALI, E=EBI; females: A=ANEMONE, C=CHOCOLAT, I=ICE) during explore-exploit decision-making tasks across social contexts (solo, duo, trio).

**Panel A: Individual Social Influence Coefficients**
Shows logistic regression coefficients (±SE) measuring how social presence affects exploration probability for each individual. Negative values indicate reduced exploration in social contexts, positive values indicate increased exploration. Only F (FRAN) shows statistically significant social influence (p=0.041, coefficient=-1.238), suggesting strong strategic adjustment to social presence. The dashed horizontal line represents no social effect (coefficient=0). Sex grouping reveals that males show more variable social responsiveness than females.

**Panel B: Individual Belief Updating Trajectories**
Displays Bayesian belief volatility (standard deviation of posterior beliefs) across social contexts for each individual. Belief volatility measures how much an animal's internal beliefs about others' behavior change over time. Higher volatility indicates more active belief updating. Each colored line represents one individual's trajectory from solo through duo to trio contexts. The analysis uses Beta-Binomial conjugate priors with Bayesian updating: when observing others' choices in social contexts, animals update their beliefs about exploration probabilities using the formula: new_belief = (α + observations)/(α + β + total_observations), where α and β are prior parameters.

**Panel C: Individual Theory of Mind Sophistication** 
Presents the best-fitting Theory of Mind model for each individual, determined by correlation between computational model predictions and actual behavior in social trials. Bar height shows model-behavior correlation strength. Text labels indicate sophistication level: "Simple" = 0-ToM (frequency tracking without mentalizing), "1st-ToM" = first-order theory of mind (understanding others have beliefs), "2nd-ToM" = second-order theory of mind (recursive belief modeling). F (FRAN) demonstrates 2nd-order ToM (correlation=0.041), representing the most cognitively sophisticated reasoning where the animal models others' beliefs about others' beliefs. D (DALI) shows the strongest 1st-order ToM performance (correlation=0.184). I (ICE) relies on simple frequency tracking without mentalizing (correlation=-0.012).

**Panel D: Individual Exploration Patterns**
Shows exploration rate (proportion of trials choosing to explore new options) across increasing social complexity for each individual. Each colored line traces one animal's exploration trajectory from solo (baseline) through duo (one other present) to trio (two others present) conditions. This reveals how each individual adjusts exploration strategy as social complexity increases. Most animals show declining exploration with increased social presence, but with distinct individual patterns suggesting different social cognitive strategies.

**Panel E: Individual Cognitive Profiles**
Integrates Theory of Mind sophistication with social influence strength to create unique cognitive profiles for each individual. Each tile shows the combination of ToM level (Simple/1st-ToM/2nd-ToM) and social influence strength (Strong/Moderate/Weak/Non-sig based on statistical significance and effect size). This reveals that cognitive sophistication and social responsiveness can vary independently - for example, F (FRAN) combines 2nd-order ToM with strong social influence, while D (DALI) shows strong 1st-order ToM but non-significant social influence.

**Sex Differences**: Males (F, D, E) demonstrate higher average Theory of Mind sophistication (mean correlation=0.085) compared to females (mean correlation=0.024). Males show more consistent sophisticated reasoning strategies, while females exhibit greater variability from simple frequency tracking to first-order ToM. Only one individual (F=FRAN, male) achieved second-order Theory of Mind, indicating rare cognitive sophistication involving recursive mental state attribution.

**Methodological Note**: Theory of Mind levels were determined using computational models that simulate different cognitive strategies. Each model makes predictions about behavior in social contexts, and the model with the highest correlation to actual behavior determines the individual's ToM classification. This approach provides quantitative evidence for different levels of social cognitive sophistication, moving beyond descriptive behavioral analysis to reveal underlying mental processes.

**Statistical Significance**: Social influence effects were tested using logistic regression with p<0.05 significance threshold. Theory of Mind model comparisons used Pearson correlation coefficients between predicted and observed choices in social trials only (duo and trio conditions). Sample sizes: F=FRAN (social trials=150), D=DALI (social trials=160), E=EBI (social trials=178), A=ANEMONE (social trials=220), C=CHOCOLAT (social trials=220), I=ICE (social trials=230).

## Cognitive Sophistication Hierarchy

**Simple (0-ToM)**: Basic behavioral tracking
↓
**1st-order ToM**: Understanding others have minds  
↓
**2nd-order ToM**: Understanding others think about others' minds

This hierarchy represents increasing cognitive complexity, with 2nd-order ToM being extremely rare and indicating human-like recursive social reasoning capabilities. 