# Social Frames of Reference in Explore-Exploit Decision-Making

A comprehensive analysis of how social context influences decision-making behavior in non-human primates.

## Research Question

How do social frames of reference influence explore-exploit trade-offs in non-human primates?

## Abstract

This repository contains the complete analysis pipeline for investigating the effects of social complexity on explore-exploit decision-making in non-human primates. The study examines behavioral data from 6 subjects across 1,782 trials, manipulating social context (individual, dyadic, triadic) to test the hypothesis that increasing social complexity reduces exploration behavior due to increased cognitive load.

## Key Findings

- **Social Complexity Effect**: Clear gradient showing reduced exploration with increased social complexity (Solo > Duo > Trio)
- **Individual Differences**: Substantial variation between subjects (range: 21.8% to 56.6% exploration rates)
- **Statistical Significance**: Strong evidence for social context effects (p < 0.001)
- **Effect Size**: Medium to large effect sizes supporting theoretical predictions

## Repository Structure

```
├── README.md                           # This file
├── data/
│   └── Explore Exploit Dataset.csv    # Raw behavioral data (not included - add your own)
├── analysis/
│   ├── Social_Frames_Python_Analysis.ipynb  # Python/Colab notebook
│   ├── Comprehensive_Analysis_Notebook.R    # R analysis script
│   └── Social_Frames_Analysis_Complete.R    # Complete R analysis
├── results/
│   ├── figures/                        # Generated plots and visualizations
│   └── COMPREHENSIVE_ANALYSIS_SUMMARY.md    # Detailed results summary
├── docs/
│   └── methodology.md                  # Detailed methodology documentation
└── requirements.txt                    # Python dependencies
```

## Getting Started

### Prerequisites

**For Python Analysis:**
- Python 3.7+
- Jupyter Notebook or Google Colab access
- Required packages (see requirements.txt)

**For R Analysis:**
- R 4.0+
- Required packages: tidyverse, ggplot2, dplyr, lme4, broom

### Installation

1. Clone this repository:
```bash
git clone https://github.com/yourusername/social-frames-analysis.git
cd social-frames-analysis
```

2. For Python analysis:
```bash
pip install -r requirements.txt
```

3. For R analysis, install required packages:
```r
install.packages(c("tidyverse", "ggplot2", "dplyr", "lme4", "broom"))
```

### Usage

**Python Analysis (Recommended for Google Colab):**
1. Upload `Social_Frames_Python_Analysis.ipynb` to Google Colab
2. Upload your dataset when prompted
3. Run all cells to generate complete analysis

**R Analysis:**
1. Open `Comprehensive_Analysis_Notebook.R` in RStudio
2. Set working directory to repository root
3. Run script sections sequentially

## Data Description

The dataset contains 1,782 behavioral trials from 6 non-human primates across different social contexts:

- **Subjects**: 6 individuals (CHOCOLAT, DALI, EBI, FRAN, ICE, ANEMONE)
- **Conditions**: Solo, Duo, Trio (social complexity manipulation)
- **Outcomes**: Explore, Exploit, None (trinomial choice)
- **Variables**: 17 columns including behavioral choices, social context, rank, and subjective values

## Methodology

### Experimental Design
- **Paradigm**: Explore-exploit decision-making task
- **Social Manipulation**: Three levels of social complexity
- **Data Structure**: Hierarchical (Population > Individual > Block > Trial)

### Statistical Analysis
- **Primary Model**: Multinomial logistic regression
- **Key Predictors**: Social complexity, individual differences, rank effects
- **Statistical Tests**: Chi-square, ANOVA, effect size calculations

### Theoretical Framework
Social complexity increases cognitive load through:
1. Social monitoring demands
2. Coordination requirements  
3. Competition for resources
4. Theory of mind computations

## Results Summary

### Main Effects
- **Social Complexity**: Significant reduction in exploration with increased social complexity
- **Individual Differences**: Large variation between subjects exceeding contextual effects
- **Rank Effects**: Dominance hierarchy influences exploration patterns

### Statistical Results
- Chi-square test: p < 0.001
- Effect size (Solo vs Trio): Cohen's d = medium to large
- Model fit: Significant improvement with individual effects

## Files Description

### Analysis Scripts
- `Social_Frames_Python_Analysis.ipynb`: Complete Python analysis for Google Colab
- `Comprehensive_Analysis_Notebook.R`: Complete R analysis with model fitting
- `Social_Frames_Analysis_Complete.R`: Extended R analysis with advanced modeling

### Documentation
- `COMPREHENSIVE_ANALYSIS_SUMMARY.md`: Detailed results and interpretation
- `methodology.md`: Complete methodological documentation

## Citation

If you use this code or methodology, please cite:

```
[Your Name] (2024). Social Frames of Reference in Explore-Exploit Decision-Making: 
A Comprehensive Analysis of Non-Human Primate Behavioral Data. 
GitHub repository: https://github.com/yourusername/social-frames-analysis
```

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Contact

For questions about the analysis or methodology, please contact [your email].

## Acknowledgments

- Data collection team and research collaborators
- Statistical analysis methodology adapted from hierarchical modeling literature
- Visualization techniques inspired by publication-ready figure standards 