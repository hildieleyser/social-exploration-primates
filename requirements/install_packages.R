# Package Installation Script for Social Context Effects Analysis
# This script installs all required R packages for the analysis

# Function to install packages if not already installed
install_if_missing <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE, quietly = TRUE)) {
      install.packages(package, dependencies = TRUE)
      library(package, character.only = TRUE)
    } else {
      cat("Package", package, "is already installed.\n")
    }
  }
}

# Core packages for data manipulation
core_packages <- c(
  "dplyr", "tidyr", "readr", "stringr", "tibble"
)

# Visualization packages
viz_packages <- c(
  "ggplot2", "viridis", "scales", "gridExtra", "patchwork", "cowplot"
)

# Bayesian modeling packages
bayes_packages <- c(
  "brms", "rstan", "tidybayes", "bayesplot"
)

# Statistical analysis packages
stats_packages <- c(
  "emmeans", "lme4", "nlme"
)

# Model diagnostics packages
diag_packages <- c(
  "performance", "see", "sjPlot", "ggeffects"
)

# Utility packages
util_packages <- c(
  "knitr", "rmarkdown", "kableExtra", "DT"
)

# Additional visualization packages
extra_viz_packages <- c(
  "ggpubr", "ggrepel", "ggridges", "ggforce"
)

# Data import/export packages
io_packages <- c(
  "haven", "writexl", "openxlsx"
)

# Combine all packages
all_packages <- c(
  core_packages, viz_packages, bayes_packages, stats_packages,
  diag_packages, util_packages, extra_viz_packages, io_packages
)

# Install packages
cat("Installing required packages...\n")
install_if_missing(all_packages)

# Check for successful installation
cat("\nChecking package installation...\n")
failed_packages <- character(0)

for (package in all_packages) {
  if (!require(package, character.only = TRUE, quietly = TRUE)) {
    failed_packages <- c(failed_packages, package)
  }
}

if (length(failed_packages) > 0) {
  cat("Warning: The following packages failed to install:\n")
  cat(paste(failed_packages, collapse = ", "), "\n")
  cat("You may need to install them manually or check for system dependencies.\n")
} else {
  cat("All packages installed successfully!\n")
}

# Print session info for reproducibility
cat("\nSession information:\n")
cat("R version:", R.version.string, "\n")
cat("Platform:", R.version$platform, "\n")
cat("Date:", Sys.Date(), "\n")

# Save session info to file
sink("requirements/session_info.txt")
sessionInfo()
sink()

cat("\nSession information saved to requirements/session_info.txt\n")
cat("You are now ready to run the analysis scripts.\n") 