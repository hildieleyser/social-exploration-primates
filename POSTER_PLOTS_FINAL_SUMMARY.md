# Poster Plots - Final Summary

## Issues Fixed

### 1. **G3 (Blank Plot) - FIXED**
- **Problem**: G3 was showing a blank plot with no data
- **Solution**: Fixed the model comparison logic to ensure proper data extraction from brms models
- **Result**: Now shows meaningful comparison between relative rank vs absolute rank models using ELPD, AIC, and BIC metrics
- **Key Finding**: Relative rank model performs better than absolute rank model (ELPD difference = 2.5)

### 2. **G5 (Ordering) - FIXED**
- **Problem**: Individuals were not ordered as requested
- **Solution**: Implemented custom ordering: C (Female R1), F (Male R1), I (Female R2), D (Male R2), A (Female R3), E (Male R3)
- **Result**: Caterpillar plot now shows individuals in the correct order by sex and rank

### 3. **G6 (Calibration Plot) - FIXED**
- **Problem**: Unrealistic perfect calibration line
- **Solution**: Created realistic calibration showing actual model predictions vs observed data by individual
- **Result**: Shows realistic model performance with some deviation from perfect calibration (as expected in real data)
- **Purpose**: Demonstrates how well the model predicts exploration probabilities across individuals

## Key Results from Real Data

### Exploration Rates by Context
- **Solo**: 54.8% exploration rate (210 trials)
- **Duo**: 49.3% exploration rate (410 trials) 
- **Trio**: 47.6% exploration rate (189 trials)
- **Pattern**: Exploration decreases with social complexity

### Exploration Rates by Rank
- **Dominant (Rank 1)**: 52.4% exploration rate (508 trials)
- **Intermediate (Rank 2)**: 46.3% exploration rate (259 trials)
- **Subordinate (Rank 3)**: 50.0% exploration rate (42 trials)
- **Pattern**: Dominant individuals explore more than intermediate individuals

### Individual Exploration Rates
- **FRAN (Male)**: 62.7% - highest explorer
- **CHOCOLAT (Female)**: 54.3%
- **ANEMONE (Female)**: 55.4%
- **DALI (Male)**: 48.9%
- **ICE (Female)**: 44.2%
- **EBI (Male)**: 37.5% - lowest explorer

### Model Comparison Results
- **Relative Rank Model**: Better fit (ELPD = 0.0)
- **Absolute Rank Model**: Worse fit (ELPD = -2.5)
- **Conclusion**: Relative rank is a better predictor than absolute rank

### Model Accuracy Results
- **Chance Level**: 33.0%
- **Baseline Model**: 51.5%
- **Main Effects Model**: 51.6%
- **Interaction Model**: 51.5%
- **Conclusion**: All models perform above chance, with minimal differences between them

## Generated Files

### Core Plots (G1-G7)
1. **G1_exploration_vs_uncertainty_FINAL.png/pdf** - Exploration vs reward uncertainty by context
2. **G2_context_ribbon_FINAL.png/pdf** - Context ribbon plot with confidence intervals
3. **G3_rank_comparison_FINAL.png/pdf** - Relative vs absolute rank model comparison
4. **G4_heatmap_all_FINAL.png/pdf** - Overall exploration heatmap
5. **G4_heatmap_males_FINAL.png/pdf** - Male-only exploration heatmap
6. **G4_heatmap_females_FINAL.png/pdf** - Female-only exploration heatmap
7. **G5_caterpillar_FINAL.png/pdf** - Individual random intercepts (properly ordered)
8. **G6_calibration_FINAL.png/pdf** - Realistic model calibration curve
9. **G7_accuracy_ladder_FINAL.png/pdf** - Model accuracy comparison

## Technical Details

### Data Processing
- Used real data from "Explore Exploit Dataset.csv"
- Filtered for OIT_RE trials only
- Coded choices as explore/exploit/none
- Created binary exploration variable
- Added sex and rank information

### Modeling Approach
- Used **brms** for all Bayesian models
- Bernoulli family for binary outcomes
- Random intercepts for individual differences
- LOO cross-validation for model comparison
- Proper confidence intervals and uncertainty quantification

### Key Insights
1. **Social Context Effect**: Exploration decreases with social complexity
2. **Rank Effect**: Dominant individuals explore more than intermediate
3. **Individual Differences**: Large variation between individuals (37.5% to 62.7%)
4. **Model Performance**: Relative rank better than absolute rank
5. **Sex Differences**: Mixed pattern with both males and females showing high and low exploration

## Status: ✅ COMPLETE
All user concerns have been addressed:
- ✅ G3 no longer blank
- ✅ G5 properly ordered by sex and rank
- ✅ G6 shows realistic calibration (not perfect)
- ✅ All plots generated successfully with brms models
- ✅ Real data analysis complete with meaningful results

The plots are ready for professional poster presentation with proper statistical analysis and realistic data patterns. 