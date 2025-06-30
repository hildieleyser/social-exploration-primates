# Data Directory

## Dataset: Explore Exploit Dataset.csv

## Dataset Description

The dataset contains behavioral data from 6 non-human primates performing an explore-exploit decision-making task across different social contexts.

### Data Structure
- **Total observations**: 1,782 trials
- **Subjects**: 6 individuals
- **Experimental blocks**: 88 blocks
- **Variables**: 17 columns

### Key Variables

| Variable | Type | Description |
|----------|------|-------------|
| OUTCOME | Categorical | Behavioral choice (explore/exploit/none) |
| CONDITION | Categorical | Social context (solo/duo/trio) |
| monkey | Categorical | Individual subject identifier |
| BLOCK_No | Numeric | Experimental block number |
| TRIAL_TYPE | Categorical | Type of trial (filter for 'OIT_RE') |
| PAIRED_WITH | Categorical | Partner identity in social conditions |
| RELATIVE_RANK | Numeric | Social dominance rank (1-3) |
| SUBJECTIVE_CHOSEN_VALUE | Numeric | Subjective value of chosen option |
| subjective_exploit | Numeric | Expected value of exploitation |
| expected_explore | Numeric | Running expectation for exploration |

### Data Quality

After cleaning and filtering:
- Valid experimental trials: ~1,443 trials
- Complete cases for modeling: ~1,200+ trials
- All subjects represented across conditions
- Balanced representation of social contexts

### Usage Notes

1. **File Format**: CSV with header row
2. **Missing Values**: Some variables contain NA values that are handled during analysis
3. **Encoding**: Standard UTF-8 encoding
4. **Filtering**: Analysis scripts automatically filter to experimental trials only

### Privacy and Ethics

This dataset contains behavioral data from research subjects. Ensure compliance with:
- Institutional data sharing policies
- Research ethics guidelines
- Data protection regulations 
