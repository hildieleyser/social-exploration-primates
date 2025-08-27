#!/usr/bin/env Rscript

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(grid)

# Set theme
theme_set(theme_minimal(base_size = 12) +
          theme(panel.grid.minor = element_blank()))

# Color scheme
COLORS <- c(
  "explore" = "#D8A7FF",
  "exploit" = "#DB4DB1", 
  "none" = "#F2C94C",
  "context" = c("solo" = "#8E9BFF", "duo" = "#FF8C42", "trio" = "#EB4559"),
  "rank" = "#642B73",
  "model" = "#2E8B57",
  "predictor" = "#4682B4",
  "response" = "#DC143C"
)

# ============================================================================
# Model Specification Diagrams
# ============================================================================

# 1. Mathematical Equation Diagram
create_equation_diagram <- function() {
  # Create equation components
  eq_data <- data.frame(
    x = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15),
    y = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
    label = c("P(Explore)", "=", "logit", "(", "β₀", "+", "β₁", "×", "Rank", "+", "β₂", "×", "Partners", "+", "uᵢ"),
    type = c("response", "equals", "function", "open", "intercept", "plus", "coef", "times", "predictor", "plus", "coef", "times", "predictor", "plus", "random")
  )
  
  p <- ggplot(eq_data, aes(x = x, y = y)) +
    geom_text(aes(label = label, color = type), size = 5, fontface = "bold") +
    scale_color_manual(values = c(
      "response" = COLORS["response"],
      "equals" = "black",
      "function" = COLORS["model"],
      "open" = "black",
      "intercept" = COLORS["model"],
      "plus" = "black",
      "coef" = COLORS["model"],
      "times" = "black",
      "predictor" = COLORS["predictor"],
      "random" = COLORS["rank"]
    )) +
    labs(title = "Mathematical Specification",
         subtitle = "Logistic regression with random intercepts") +
    theme_void() +
    theme(legend.position = "none",
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 12, hjust = 0.5))
  
  return(p)
}

# 2. Predictor Variables Diagram
create_predictor_diagram <- function() {
  # Create predictor summary
  predictor_data <- data.frame(
    predictor = c("Relative Rank", "Partner Count", "Individual ID", "Expected Reward"),
    type = c("Categorical", "Continuous", "Random Effect", "Continuous"),
    levels = c("1 (Dominant), 2 (Intermediate), 3 (Subordinate)", 
               "0 (Solo), 1 (Duo), 2 (Trio)",
               "FRAN, DALI, EBI, CHOCOLAT, ICE, ANEMONE",
               "Continuous (0-1)"),
    description = c("Social hierarchy position within group",
                   "Number of social partners present",
                   "Individual-specific baseline tendency",
                   "Expected value of exploration outcome")
  )
  
  p <- ggplot(predictor_data, aes(x = predictor, y = 1)) +
    geom_tile(aes(fill = type), alpha = 0.7) +
    geom_text(aes(label = predictor), color = "white", fontface = "bold", size = 4) +
    geom_text(aes(label = paste("Type:", type)), y = 0.7, size = 3) +
    geom_text(aes(label = levels), y = 0.4, size = 2.5, hjust = 0.5) +
    geom_text(aes(label = description), y = 0.1, size = 2, hjust = 0.5) +
    scale_fill_manual(values = c(
      "Categorical" = COLORS["predictor"],
      "Continuous" = COLORS["model"],
      "Random Effect" = COLORS["rank"]
    )) +
    labs(title = "Predictor Variables",
         subtitle = "Variables used in the exploration model") +
    theme_void() +
    theme(legend.position = "none",
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 12, hjust = 0.5))
  
  return(p)
}

# 3. Model Comparison Diagram
create_model_comparison <- function() {
  # Create model comparison table
  models <- data.frame(
    model = c("Baseline", "Main Effects", "Interaction", "Relative Rank", "Absolute Rank"),
    formula = c("explore ~ 1 + (1|id)", 
                "explore ~ rank + partners + (1|id)",
                "explore ~ rank × partners + (1|id)",
                "explore ~ relative_rank + partners + (1|id)",
                "explore ~ absolute_rank + partners + (1|id)"),
    parameters = c(2, 4, 6, 4, 4),
    complexity = c("Low", "Medium", "High", "Medium", "Medium"),
    purpose = c("Chance baseline", "Main effects only", "Include interactions", "Relative rank", "Absolute rank")
  )
  
  p <- ggplot(models, aes(x = model, y = 1)) +
    geom_tile(aes(fill = complexity), alpha = 0.7) +
    geom_text(aes(label = model), color = "white", fontface = "bold", size = 3) +
    geom_text(aes(label = formula), y = 0.7, size = 2.5, hjust = 0.5) +
    geom_text(aes(label = paste("Params:", parameters)), y = 0.4, size = 2.5) +
    geom_text(aes(label = purpose), y = 0.1, size = 2, hjust = 0.5) +
    scale_fill_manual(values = c(
      "Low" = COLORS["explore"],
      "Medium" = COLORS["model"],
      "High" = COLORS["rank"]
    )) +
    labs(title = "Model Comparison",
         subtitle = "Different model specifications tested") +
    theme_void() +
    theme(legend.position = "none",
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 12, hjust = 0.5))
  
  return(p)
}

# 4. Bayesian Inference Diagram
create_bayesian_diagram <- function() {
  # Create Bayesian inference flow
  bayes_data <- data.frame(
    x = c(1, 2, 3, 4, 5),
    y = c(1, 1, 1, 1, 1),
    label = c("Prior", "Likelihood", "Posterior", "Predictions", "Model Comparison"),
    description = c("Initial beliefs about parameters",
                   "Data likelihood given parameters",
                   "Updated beliefs after seeing data",
                   "Predictions for new data",
                   "LOO, AIC, BIC comparison")
  )
  
  p <- ggplot(bayes_data, aes(x = x, y = y)) +
    geom_point(aes(color = label), size = 10) +
    geom_text(aes(label = label), color = "white", fontface = "bold", size = 3) +
    geom_text(aes(label = description), y = 0.7, size = 2.5, hjust = 0.5) +
    geom_segment(aes(x = x, xend = x + 0.8, y = y, yend = y), 
                 arrow = arrow(length = unit(0.2, "cm")), size = 1) +
    scale_color_manual(values = c(
      "Prior" = COLORS["explore"],
      "Likelihood" = COLORS["model"],
      "Posterior" = COLORS["rank"],
      "Predictions" = COLORS["response"],
      "Model Comparison" = COLORS["predictor"]
    )) +
    labs(title = "Bayesian Inference Process",
         subtitle = "Using brms for model fitting and comparison") +
    theme_void() +
    theme(legend.position = "none",
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 12, hjust = 0.5))
  
  return(p)
}

# 5. Comprehensive Model Specification
create_comprehensive_model_spec <- function() {
  # Create a more detailed model specification
  spec_data <- data.frame(
    x = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    y = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
    component = c("Response Variable", "Distribution", "Link Function", "Fixed Effects", 
                  "Random Effects", "Prior Distributions", "Inference Method", "Model Comparison", "Validation", "Predictions"),
    details = c("Binary: explore (1) vs exploit (0)",
                "Bernoulli(πᵢ)",
                "logit(πᵢ) = ηᵢ",
                "β₀ + β₁×rank + β₂×partners",
                "uᵢ ~ N(0, σ²) for individual i",
                "Normal priors for β, Half-Cauchy for σ",
                "Hamiltonian Monte Carlo (Stan)",
                "LOO cross-validation, AIC, BIC",
                "Posterior predictive checks",
                "P(explore|new_data)"),
    color = c("response", "distribution", "link", "fixed", "random", "prior", "inference", "comparison", "validation", "prediction")
  )
  
  p <- ggplot(spec_data, aes(x = x, y = y)) +
    geom_point(aes(color = color), size = 12) +
    geom_text(aes(label = component), color = "white", fontface = "bold", size = 3) +
    geom_text(aes(label = details), y = 0.7, size = 2.5, hjust = 0.5) +
    scale_color_manual(values = c(
      "response" = COLORS["response"],
      "distribution" = COLORS["explore"],
      "link" = COLORS["model"],
      "fixed" = COLORS["predictor"],
      "random" = COLORS["rank"],
      "prior" = COLORS["context"]["solo"],
      "inference" = COLORS["context"]["duo"],
      "comparison" = COLORS["context"]["trio"],
      "validation" = COLORS["model"],
      "prediction" = COLORS["response"]
    )) +
    labs(title = "Comprehensive Model Specification",
         subtitle = "Bayesian logistic regression with random intercepts") +
    theme_void() +
    theme(legend.position = "none",
          plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 14, hjust = 0.5))
  
  return(p)
}

# 6. Model Structure Flow Diagram
create_model_flow <- function() {
  # Create model structure flow
  flow_data <- data.frame(
    x = c(1, 2, 3, 4, 5, 6, 7, 8),
    y = c(1, 1, 1, 1, 1, 1, 1, 1),
    label = c("Predictors", "Linear Predictor", "Logistic Function", "Probability", "Random Effect", "Individual Variation", "Binary Choice", "Model Fit"),
    description = c("Rank, Partners, Context",
                   "η = β₀ + β₁×rank + β₂×partners",
                   "P(explore) = 1/(1 + exp(-η))",
                   "Exploration probability",
                   "uᵢ ~ N(0, σ²)",
                   "Individual-specific baseline",
                   "explore (1) or exploit (0)",
                   "Bayesian inference with brms")
  )
  
  p <- ggplot(flow_data, aes(x = x, y = y)) +
    geom_point(aes(color = label), size = 10) +
    geom_text(aes(label = label), color = "white", fontface = "bold", size = 3) +
    geom_text(aes(label = description), y = 0.7, size = 2.5, hjust = 0.5) +
    geom_segment(aes(x = x, xend = x + 0.8, y = y, yend = y), 
                 arrow = arrow(length = unit(0.2, "cm")), size = 1) +
    scale_color_manual(values = c(
      "Predictors" = COLORS["predictor"],
      "Linear Predictor" = COLORS["model"],
      "Logistic Function" = COLORS["model"],
      "Probability" = COLORS["response"],
      "Random Effect" = COLORS["rank"],
      "Individual Variation" = COLORS["rank"],
      "Binary Choice" = COLORS["response"],
      "Model Fit" = COLORS["model"]
    )) +
    labs(title = "Model Structure Flow",
         subtitle = "From predictors to binary choice") +
    theme_void() +
    theme(legend.position = "none",
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 12, hjust = 0.5))
  
  return(p)
}

# Generate all diagrams
p1 <- create_equation_diagram()
p2 <- create_predictor_diagram()
p3 <- create_model_comparison()
p4 <- create_bayesian_diagram()
p5 <- create_comprehensive_model_spec()
p6 <- create_model_flow()

# Save individual diagrams
ggsave("mathematical_equation_diagram.png", p1, width = 12, height = 8, dpi = 300)
ggsave("mathematical_equation_diagram.pdf", p1, width = 12, height = 8)

ggsave("predictor_variables_diagram.png", p2, width = 12, height = 8, dpi = 300)
ggsave("predictor_variables_diagram.pdf", p2, width = 12, height = 8)

ggsave("model_comparison_diagram.png", p3, width = 12, height = 8, dpi = 300)
ggsave("model_comparison_diagram.pdf", p3, width = 12, height = 8)

ggsave("bayesian_inference_diagram.png", p4, width = 12, height = 8, dpi = 300)
ggsave("bayesian_inference_diagram.pdf", p4, width = 12, height = 8)

ggsave("comprehensive_model_specification.png", p5, width = 16, height = 10, dpi = 300)
ggsave("comprehensive_model_specification.pdf", p5, width = 16, height = 10)

ggsave("model_structure_flow.png", p6, width = 12, height = 8, dpi = 300)
ggsave("model_structure_flow.pdf", p6, width = 12, height = 8)

cat("Model specification diagrams generated successfully!\n")
cat("Files saved as PNG and PDF formats.\n")
cat("These diagrams explain the mathematical details and model structure.\n") 