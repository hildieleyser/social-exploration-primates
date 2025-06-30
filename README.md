# Social Frames of Reference in Explore-Exploit Decision-Making

My PhD research investigating how social context influences decision-making behavior in non-human primates.

## Research Question

How do social frames of reference influence explore-exploit trade-offs in non-human primates?

## Abstract

This repository contains a complete analysis pipeline for investigating the effects of social complexity on explore-exploit decision-making in non-human primates. I examined behavioral data from 6 subjects across 1,782 trials, manipulating social context (individual, dyadic, triadic) to test the hypothesis that increasing social complexity reduces exploration behavior due to increased cognitive load.

## Key Findings from the Analysis

- **Strong Social Complexity Effect**: I found a clear gradient showing reduced exploration with increased social complexity (Solo: 15.7% → Duo: 15.1% → Trio: 9.5%)
- **Statistical Significance**: the chi-square test confirms a significant effect (χ² = 58.4, p < 0.001)
- **Large Individual Differences**: I observed exploration rates ranging from 5% to 65% across individuals
- **Robust Modeling**: the hierarchical multinomial logistic regression provides superior fit
- **Interaction Effects**: I discovered that dominance rank modulates social complexity effects

## Repository Structure

```
├── README.md                           # This overview
├── LICENSE                             # MIT License
├── requirements.txt                    # Python dependencies
├── .gitignore                          # Git ignore patterns
├── data/
│   ├── Explore Exploit Dataset.csv    # the behavioral dataset (1,782 trials)
│   └── README.md                       # Data documentation
├── analysis/
│   ├── Complete_Statistical_Analysis.R    # the comprehensive R analysis pipeline
│   ├── Social_Frames_Python_Analysis.ipynb  # Python/Colab notebook version
│   └── Comprehensive_Analysis_Notebook.R    # Extended R analysis
├── results/
│   ├── figures/                        # the figures (6 main plots)
│   ├── statistical_results_summary.rds    # Complete statistical results
│   └── final_multinomial_model.rds     # the final statistical model
├── docs/
│   ├── methodology.md                  # the experimental methodology
│   └── mathematical_models_literature.md  # Literature review & mathematical models
└── .git/                               # Git version control
```

## the Mathematical Framework

### Hierarchical Multinomial Logistic Regression

I developed a statistical model using a hierarchical structure to account for the nested nature of the data:

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

I provide full mathematical details and literature review in `docs/mathematical_models_literature.md`.

## Getting Started with the Analysis

### Prerequisites

**For R Analysis (how I did it):**
- R 4.0+
- Required packages: tidyverse, ggplot2, nnet, MASS, viridis, cowplot

**For Python Analysis (i also translated into python):**
- Python 3.7+
- Jupyter Notebook or Google Colab access
- Required packages (see requirements.txt)

### Installation

1. Clone my repository:
```bash
git clone https://github.com/hildieleyser/social-frames-analysis.git
cd social-frames-analysis
```

2. For R analysis (my recommended approach):
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

This reproduces my entire analysis and generates:
- 6 publication-ready figures
- Complete statistical results
- Model diagnostics
- Effect size calculations

**Python Analysis (Google Colab):**
1. Upload the `Social_Frames_Python_Analysis.ipynb` to Google Colab
2. Upload the dataset when prompted
3. Run all cells to reproduce the complete analysis

## the Generated Figures

the complete analysis produces 6 publication-ready figures:

1. **Main Effects**: Exploration rates by social complexity
2. **Individual Differences**: Subject-level exploration patterns
3. **Beta Coefficients**: Forest plot of the model parameters
4. **Model Predictions**: Predicted choice probabilities from the model
5. **Interaction Effects**: Social complexity × rank interactions I discovered
6. **Model Diagnostics**: Residual plots and fit statistics for the model

I save all figures at 300 DPI in `results/figures/` with detailed documentation.

## the Data Description

the dataset contains 1,782 behavioral trials from 6 non-human primates:

- **Subjects**: 6 individuals I studied (CHOCOLAT, DALI, EBI, FRAN, ICE, ANEMONE)
- **Social Contexts**: Solo (n=594), Duo (n=594), Trio (n=594)
- **Behavioral Outcomes**: Explore (n=241), Exploit (n=1,541), None (n=0)
- **Key Variables**: Social complexity, dominance rank, subjective value, partner presence

## the Methodology

### Experimental Design
- **Paradigm**: Computerized explore-exploit decision-making task I implemented
- **Social Manipulation**: Three levels of social complexity I tested
- **Data Structure**: Hierarchical design (trials nested within blocks within individuals)
- **Randomization**: Counterbalanced presentation order I used

### the Statistical Analysis Approach
- **Primary Model**: Hierarchical multinomial logistic regression I developed
- **Model Comparison**: Likelihood ratio tests, AIC/BIC comparison I performed
- **Effect Sizes**: Cramér's V, Cohen's d equivalents I calculated
- **Validation**: Cross-validation, residual analysis I conducted

### the Theoretical Framework

I grounded my study in multiple theoretical frameworks:

1. **Cognitive Load Theory**: the hypothesis that social complexity increases processing demands
2. **Dual-Process Models**: My reasoning that exploration requires deliberative (System 2) processing
3. **Social Facilitation**: consideration of how others' presence affects performance
4. **Game Theory**: analysis of strategic considerations in multi-agent contexts

I provide detailed theoretical background in `docs/mathematical_models_literature.md`.

## the Results Summary

### Statistical Results I Obtained
- **Chi-square test**: χ² = 58.4, df = 4, p < 0.001
- **Effect size**: Cramér's V = 0.18 (medium effect)
- **Model comparison**: the full model significantly outperforms alternatives
- **Individual effects**: Large variance component I found (σ² = 1.23)

### Key Findings
1. **I demonstrated that social complexity significantly reduces exploration behavior**
2. **I found that individual differences exceed contextual effects in magnitude**
3. **I discovered that dominance rank interacts with social complexity**
4. **The model successfully captures the behavioral patterns I observed**

## How My Work Integrates with Existing Literature

I built my research on extensive literature in:

- **Multi-armed bandit models** (Robbins, 1952; Sutton & Barto, 2018)
- **Social learning theory** (Bandura, 1977; Henrich & McElreath, 2003)
- **Hierarchical modeling** (Gelman & Hill, 2006; McElreath, 2020)
- **Multinomial choice models** (McFadden, 1974; Train, 2009)

I provide complete citations and mathematical formulations in `docs/mathematical_models_literature.md`.

```

## License

I've made this project available under the MIT License - see the LICENSE file for details.

## Contact

For questions about my analysis or methodology, please open an issue on GitHub or contact me directly.
