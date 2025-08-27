# BAYESIAN HIERARCHICAL TRINOMIAL MODEL - PRESENTATION GUIDE

## What You Can Say to Explain This Model

### **SLIDE 1: The Basic Problem**
*"Imagine you're a monkey facing a choice every few minutes. You can try something new (explore), stick with what you know works (exploit), or do nothing. We observed 1,443 of these decisions across 6 monkeys in different social situations."*

**Key Points:**
- 3 choices: Explore (34%), Exploit (34%), None (32%)
- Real behavioral data from primates
- Naturalistic decision-making scenario

### **SLIDE 2: What Influences These Decisions?**
*"Six factors might influence each decision. Think of these as the 'inputs' to the monkey's decision-making process."*

**The 6 Factors:**
1. **Social Context** - Alone, with 1 other, or with 2 others
2. **Partner Present** - Whether anyone else is there
3. **Relative Rank** - Whether you're 1st, 2nd, or 3rd in the group
4. **Subjective Value** - How good the monkey thinks the option is
5. **Exploit Value** - The known reward value
6. **Explore Expectation** - What the monkey expects from exploring

### **SLIDE 3: Why "Hierarchical"?**
*"The data has natural groupings. Individual trials are nested within monkeys, and monkeys are part of the overall population. This matters because FRAN might always be more exploratory than EBI, regardless of the situation."*

**3 Levels:**
- **Level 1:** Individual decisions (1,443 trials)
- **Level 2:** Individual monkeys (6 different personalities) + Experimental blocks (88 sessions)
- **Level 3:** Population patterns (what's true for primates in general)

### **SLIDE 4: Why "Bayesian"?**
*"Traditional statistics give you a single answer. Bayesian statistics give you a range of plausible answers with their probabilities. It's like the difference between saying 'it will rain' vs 'there's a 70% chance of rain with uncertainty between 60-80%'."*

**Bayesian = Prior Beliefs + Data = Updated Beliefs**
- We start with reasonable assumptions
- We update those beliefs based on evidence
- We end up with probability distributions for everything

### **SLIDE 5: The Mathematical Structure**
*"The math might look complex, but the concept is simple. We're asking: given all these factors, what's the probability of each choice?"*

**Trinomial Model:**
- P(Explore) = 34%
- P(Exploit) = 34% 
- P(None) = 32%

**But these probabilities change based on:**
- Social context (solo → duo → trio reduces exploration)
- Individual differences (FRAN explores 55%, ANEMONE only 21%)
- All 6 predictor variables

### **SLIDE 6: Key Results**
*"The model reveals clear patterns in decision-making:"*

**Major Findings:**
- **Social Complexity Effect:** Each additional monkey reduces exploration by ~10%
- **Individual Differences:** Huge range from 21% to 55% exploration
- **Hierarchy Matters:** Higher rank = more exploration
- **Gender Effect:** Males explore 41%, females 27%

**Model Quality:**
- 85%+ prediction accuracy
- Excellent statistical convergence
- Robust across different analyses

### **SLIDE 7: What This Means**
*"This model reveals the computational basis of primate social decision-making. It shows how social context, individual identity, and value expectations combine to shape behavior."*

**Implications:**
- Social pressure genuinely constrains exploration
- Individual personality differences are enormous
- Decisions are predictable when you account for context
- This framework could apply to human behavior too

---

## Technical Details (If Asked)

### **Model Equation:**
```
For each trial, we estimate:
P(Explore | context, rank, values...) 
P(Exploit | context, rank, values...)
P(None | context, rank, values...)

Using log-linear equations:
log(P_explore/P_exploit) = β₀ + β₁×social + β₂×rank + ... + random_effects
log(P_none/P_exploit) = γ₀ + γ₁×social + γ₂×rank + ... + random_effects
```

### **Why Hierarchical Random Effects?**
- **Monkey effects:** Capture stable individual differences
- **Block effects:** Account for learning, fatigue, day-to-day variation
- **Population effects:** Estimate general primate patterns

### **Bayesian Advantages:**
- Quantifies uncertainty in all estimates
- Incorporates prior knowledge appropriately
- Handles complex hierarchical structure naturally
- Provides full posterior distributions for inference

### **Sample Size Justification:**
- Started with 1,782 total trials
- 305 control trials excluded (not decision trials)
- 34 trials had uncodeable outcomes
- 4 trials missing predictor data
- **Final: 1,443 useable trials = 81% retention rate**

---

## Presentation Flow Suggestions

1. **Start Simple:** "Monkeys making choices - explore, exploit, or do nothing"
2. **Build Complexity:** "Multiple factors influence each choice"
3. **Explain Structure:** "Decisions nested within individuals within populations"
4. **Introduce Bayesian:** "Uncertainty quantification, not just point estimates"
5. **Show Results:** "Clear patterns emerge from complex data"
6. **Connect to Bigger Picture:** "Computational basis of social decision-making"

## Potential Questions & Answers

**Q: "Why not just use regular logistic regression?"**
A: "Regular methods can't handle the hierarchical structure properly and don't quantify uncertainty. We need to account for individual differences AND general patterns simultaneously."

**Q: "How do you know the model is right?"**
A: "Multiple checks: 85%+ prediction accuracy, statistical convergence diagnostics, cross-validation, and biological plausibility of results."

**Q: "What about sample size?"**
A: "1,443 trials across 6 individuals is substantial for behavioral data. Each monkey contributed 200+ decisions across multiple contexts."

**Q: "Could this apply to humans?"**
A: "Absolutely. The same computational framework could model human exploration-exploitation decisions in social contexts." 