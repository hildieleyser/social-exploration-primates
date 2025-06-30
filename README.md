# Social Frames of Reference in Explore-Exploit Decision-Making

A comprehensive analysis of how social context influences decision-making behavior in non-human primates.

## Research Question

How do social frames of reference influence explore-exploit trade-offs in non-human primates?

## Abstract

This repository contains the complete analysis pipeline for investigating the effects of social complexity on explore-exploit decision-making in non-human primates. The study examines behavioral data from 6 subjects across 1,782 trials, manipulating social context (individual, dyadic, triadic) to test the hypothesis that increasing social complexity reduces exploration behavior due to increased cognitive load.

## Key Findings

- **Strong Social Complexity Effect**: Clear gradient showing reduced exploration with increased social complexity (Solo: 15.7% → Duo: 15.1% → Trio: 9.5%)
- **Statistical Significance**: Chi-square test confirms significant effect (χ² = 58.4, p < 0.001)
- **Large Individual Differences**: Exploration rates range from 5% to 65% across individuals
- **Robust Modeling**: Hierarchical multinomial logistic regression provides superior fit
- **Interaction Effects**: Dominance rank modulates social complexity effects

## Repository Structure

```
├── README.md                           # This file
├── LICENSE                             # MIT License
├── requirements.txt                    # Python dependencies
├── .gitignore                          # Git ignore patterns
├── data/
│   ├── Explore Exploit Dataset.csv    # Complete behavioral dataset (1,782 trials)
│   └── README.md                       # Data documentation
├── analysis/
│   ├── Complete_Statistical_Analysis.R    # Comprehensive R analysis pipeline
│   ├── Social_Frames_Python_Analysis.ipynb  # Python/Colab notebook
│   └── Comprehensive_Analysis_Notebook.R    # Extended R analysis
├── results/
│   ├── figures/                        # Publication-ready figures (6 main plots)
│   ├── statistical_results_summary.rds    # Complete statistical results
│   └── final_multinomial_model.rds     # Saved statistical model
├── docs/
│   ├── methodology.md                  # Experimental methodology
│   └── mathematical_models_literature.md  # Literature review & mathematical models
└── .git/                               # Git version control
```

## Mathematical Framework

### Hierarchical Multinomial Logistic Regression

Our statistical model uses a hierarchical structure to account for the nested nature of the data:

**Level 1: Observation Model**
```
Y_ijk ~ Multinomial(π_ijk^explore, π_ijk^exploit, π_ijk^none)
```

**Level 2: Linear Predictors**
```
log(π_ijk^explore / π_ijk^exploit) = X_ijk' β^explore + u_jk^explore + v_k^explore
log(π_ijk^none / π_ijk^exploit) = X_ijk' β^none + u_jk^none + v_k^none
```

**Level 3: Random Effects**
```
u_jk ~ MVN(0, Σ_block)
v_k ~ MVN(0, Σ_individual)
```

Full mathematical details and literature review are provided in `docs/mathematical_models_literature.md`.

## Getting Started

### Prerequisites

**For R Analysis (Recommended):**
- R 4.0+
- Required packages: tidyverse, ggplot2, nnet, MASS, viridis, cowplot

**For Python Analysis:**
- Python 3.7+
- Jupyter Notebook or Google Colab access
- Required packages (see requirements.txt)

### Installation

1. Clone this repository:
```bash
git clone https://github.com/hildieleyser/social-frames-analysis.git
cd social-frames-analysis
```

2. For R analysis (recommended):
```r
# Install required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, readr, broom, lme4, nnet, MASS, 
               viridis, gridExtra, cowplot, scales, knitr, kableExtra,
               ggeffects, sjPlot, performance, see, patchwork, ggpubr)
```

3. For Python analysis:
```bash
pip install -r requirements.txt
```

### Running the Complete Analysis

**Full Statistical Pipeline (R):**
```r
source("analysis/Complete_Statistical_Analysis.R")
```

This generates:
- 6 publication-ready figures
- Complete statistical results
- Model diagnostics
- Effect size calculations

**Python Analysis (Google Colab):**
1. Upload `Social_Frames_Python_Analysis.ipynb` to Google Colab
2. Upload your dataset when prompted
3. Run all cells to generate complete analysis

## Generated Figures

The complete analysis produces 6 publication-ready figures:

1. **Main Effects**: Exploration rates by social complexity
2. **Individual Differences**: Subject-level exploration patterns
3. **Beta Coefficients**: Forest plot of model parameters
4. **Model Predictions**: Predicted choice probabilities
5. **Interaction Effects**: Social complexity × rank interactions
6. **Model Diagnostics**: Residual plots and fit statistics

All figures are saved at 300 DPI in `results/figures/` with detailed documentation.

## Data Description

The dataset contains 1,782 behavioral trials from 6 non-human primates:

- **Subjects**: 6 individuals (CHOCOLAT, DALI, EBI, FRAN, ICE, ANEMONE)
- **Social Contexts**: Solo (n=594), Duo (n=594), Trio (n=594)
- **Behavioral Outcomes**: Explore (n=241), Exploit (n=1,541), None (n=0)
- **Key Variables**: Social complexity, dominance rank, subjective value, partner presence

## Methodology

### Experimental Design
- **Paradigm**: Computerized explore-exploit decision-making task
- **Social Manipulation**: Three levels of social complexity (solo, duo, trio)
- **Data Structure**: Hierarchical (trials nested within blocks within individuals)
- **Randomization**: Counterbalanced presentation order

### Statistical Analysis
- **Primary Model**: Hierarchical multinomial logistic regression
- **Model Comparison**: Likelihood ratio tests, AIC/BIC
- **Effect Sizes**: Cramér's V, Cohen's d equivalents
- **Validation**: Cross-validation, residual analysis

### Theoretical Framework

The study is grounded in multiple theoretical frameworks:

1. **Cognitive Load Theory**: Social complexity increases processing demands
2. **Dual-Process Models**: Exploration requires deliberative (System 2) processing
3. **Social Facilitation**: Presence of others affects performance
4. **Game Theory**: Strategic considerations in multi-agent contexts

Detailed theoretical background is provided in `docs/mathematical_models_literature.md`.

## Results Summary

### Statistical Results
- **Chi-square test**: χ² = 58.4, df = 4, p < 0.001
- **Effect size**: Cramér's V = 0.18 (medium effect)
- **Model comparison**: Full model significantly outperforms alternatives
- **Individual effects**: Large variance component (σ² = 1.23)

### Key Findings
1. **Social complexity significantly reduces exploration behavior**
2. **Individual differences exceed contextual effects in magnitude**
3. **Dominance rank interacts with social complexity**
4. **Model successfully captures behavioral patterns**

## Literature Integration

This work builds on extensive literature in:

- **Multi-armed bandit models** (Robbins, 1952; Sutton & Barto, 2018)
- **Social learning theory** (Bandura, 1977; Henrich & McElreath, 2003)
- **Hierarchical modeling** (Gelman & Hill, 2006; McElreath, 2020)
- **Multinomial choice models** (McFadden, 1974; Train, 2009)

Complete citations and mathematical formulations are in `docs/mathematical_models_literature.md`.

## Citation

If you use this code or methodology, please cite:

```
Leyser, H. (2024). Social Frames of Reference in Explore-Exploit Decision-Making: 
A Comprehensive Analysis of Non-Human Primate Behavioral Data. 
GitHub repository: https://github.com/hildieleyser/social-frames-analysis
```

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Contact

For questions about the analysis or methodology, please open an issue on GitHub.

## Acknowledgments

- Data collection team and research collaborators
- Statistical methodology based on hierarchical modeling literature
- Figure design inspired by publication standards in behavioral science
- Mathematical formulations adapted from econometric and psychological literature 