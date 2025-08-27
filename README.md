# Social Context Effects on Exploration-Exploitation Decisions in Non-Human Primates

## Overview

This repository contains the complete analysis and visualization code for investigating how social context influences exploration-exploitation decision-making in non-human primates. The study examines behavioral data from 6 monkeys across three social conditions (solo, duo, trio) to understand how partner presence and social rank affect exploration strategies.

## Citation

If you use this code or data in your research, please cite:

```
Leyser, H. (2024). Social Context Effects on Exploration-Exploitation Decisions in Non-Human Primates. 
GitHub Repository. https://github.com/hildieleyser/social-exploration-primates
```

## Repository Structure

```
├── data/                          # Raw and processed data
│   ├── Explore Exploit Dataset.csv # Primary behavioral dataset
│   └── README.md                  # Data documentation
├── analysis/                      # Analysis scripts
│   ├── Complete_Statistical_Analysis.R  # Main analysis pipeline
│   ├── make_better_plots.R        # Poster-quality visualizations
│   └── model_diagrams_simple.R    # Model specification diagrams
├── results/                       # Generated outputs
│   ├── figures/                   # All generated plots
│   ├── tables/                    # Statistical tables
│   └── models/                    # Fitted model objects
├── docs/                          # Documentation
│   ├── methodology.md             # Detailed methodology
│   └── variable_definitions.md    # Variable descriptions
├── requirements/                  # Dependencies
│   └── R_packages.txt            # Required R packages
└── README.md                      # This file
```

## Quick Start

### Prerequisites

- R (version 4.0.0 or higher)
- Required R packages (see `requirements/R_packages.txt`)

### Installation

1. Clone this repository:
```bash
git clone https://github.com/hildieleyser/social-exploration-primates.git
cd social-exploration-primates
```

2. Install required R packages:
```r
source("requirements/install_packages.R")
```

3. Run the main analysis:
```r
source("analysis/Complete_Statistical_Analysis.R")
```

## Data

The primary dataset (`data/Explore Exploit Dataset.csv`) contains 1,477 trials from 6 monkeys across three social contexts:

- **Solo condition**: Individual decision-making
- **Duo condition**: Decision-making with one partner
- **Trio condition**: Decision-making with two partners

### Key Variables

- `monkey`: Subject identifier (A, C, D, E, F, I)
- `CONDITION`: Social context (solo, duo, trio)
- `OUTCOME`: Decision outcome (explore, exploit, none)
- `RELATIVE_RANK`: Social rank (1, 2, 3)
- `expected_explore`: Expected reward for exploration
- `TRIAL_TYPE`: Trial type (OIT_RE for main analysis)

## Analysis Pipeline

### 1. Data Preprocessing
- Filter for relevant trial types (OIT_RE)
- Create derived variables (context, explore rate, partner count)
- Handle missing data and outliers

### 2. Statistical Modeling
- Bayesian hierarchical logistic regression using `brms`
- Model: `explore ~ partner_count + rank + context + (1|monkey_id)`
- Random intercepts for individual monkeys

### 3. Visualization
- Uncertainty vs. exploration relationships
- Individual monkey trajectories
- Observed vs. predicted exploration rates
- Model effects and diagnostics

## Key Findings

1. **Social Context Suppression**: Exploration rates decrease with increasing partner count
2. **Rank Effects**: Lower-ranked individuals show different exploration patterns
3. **Individual Variation**: Significant individual differences in exploration strategies
4. **Uncertainty-Driven**: Exploration is positively associated with reward uncertainty

## Reproducibility

All analyses are fully reproducible with the provided code and data. The repository includes:

- Complete data preprocessing pipeline
- Statistical analysis scripts with detailed comments
- Model diagnostics and validation
- Publication-quality visualizations
- Comprehensive documentation

## Dependencies

### R Packages
- `brms` (2.22.0): Bayesian modeling
- `ggplot2` (3.4.0): Visualization
- `dplyr` (1.1.0): Data manipulation
- `tidyr` (1.3.0): Data tidying
- `tidybayes` (3.0.0): Bayesian analysis tools
- `emmeans` (1.8.0): Marginal means
- `viridis` (0.6.0): Color palettes

### System Requirements
- R >= 4.0.0
- 8GB RAM (for Bayesian model fitting)
- 2GB disk space

## Contributing

This is a research repository. For questions or collaboration, please contact the corresponding author.

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Acknowledgments

- Research participants and collaborators
- Open source R community
- Funding agencies (if applicable)

## Contact

For questions about this analysis or repository:
- Email: [your-email@institution.edu]
- GitHub Issues: [repository-issues-link]

## Version History

- v1.0.0 (2024-01-XX): Initial release with complete analysis pipeline
- v1.1.0 (2024-01-XX): Added model diagnostics and validation
- v1.2.0 (2024-01-XX): Enhanced visualizations and documentation
