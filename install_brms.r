# Install brms package with proper CRAN mirror setup
options(repos = c(CRAN = "https://cran.rstudio.com/"))

cat("Setting up CRAN mirror and installing brms...\n")
cat("This may take 10-15 minutes as brms has many dependencies.\n\n")

# Install required packages
packages_needed <- c("brms", "ggplot2", "dplyr", "tidyr", "bayesplot", "loo", "cmdstanr")

for (pkg in packages_needed) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("Installing", pkg, "...\n")
    install.packages(pkg, dependencies = TRUE)
  } else {
    cat(pkg, "already installed.\n")
  }
}

# Test brms installation
cat("\nTesting brms installation...\n")
library(brms)
cat("brms version:", packageVersion("brms"), "\n")

# Check Stan installation
cat("Checking Stan backend...\n")
if (require(cmdstanr, quietly = TRUE)) {
  cat("cmdstanr available\n")
} else {
  cat("Using rstan backend\n")
}

cat("\nInstallation complete!\n") 