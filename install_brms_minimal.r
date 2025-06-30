# Minimal brms installation script
options(repos = c(CRAN = "https://cran.rstudio.com/"))

cat("Attempting minimal brms installation...\n")

# Try installing just the basic packages first
basic_packages <- c("ggplot2", "dplyr")

for (pkg in basic_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("Installing", pkg, "...\n")
    install.packages(pkg, dependencies = FALSE, type = "binary")
  } else {
    cat(pkg, "already installed.\n")
  }
}

# Try installing brms directly without dependencies
cat("\nAttempting to install brms from binary...\n")
try({
  install.packages("brms", dependencies = FALSE, type = "binary")
  library(brms)
  cat("brms successfully installed!\n")
  cat("brms version:", packageVersion("brms"), "\n")
}, silent = FALSE)

cat("\nInstallation attempt complete.\n") 