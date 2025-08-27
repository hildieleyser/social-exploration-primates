#!/usr/bin/env Rscript

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(grid)
library(patchwork)
library(ggtext)
library(showtext)

# Add fonts
font_add_google("Roboto", "roboto")
showtext_auto()

# Set theme
theme_set(theme_minimal(base_family = "roboto") +
          theme(panel.grid.minor = element_blank(),
                text = element_text(size = 12)))

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

# 1. Basic Model Structure Diagram
create_model_structure <- function() {
  # Create nodes for the diagram
  nodes <- data.frame(
    x = c(1, 2, 3, 4, 5, 6, 7, 8),
    y = c(1, 1, 1, 1, 2, 2, 3, 3),
    label = c("Relative Rank", "Partner Count", "Individual ID", "Context", 
              "Linear Predictor", "Logistic Function", "Exploration Probability", "Binary Choice"),
    type = c("predictor", "predictor", "random", "context", "linear", "link", "probability", "response")
  )
  
  # Create edges
  edges <- data.frame(
    from_x = c(1, 2, 3, 4, 5, 6, 7),
    from_y = c(1, 1, 1, 1, 2, 2, 3),
    to_x = c(5, 5, 5, 5, 6, 7, 8),
    to_y = c(2, 2, 2, 2, 3, 3, 3)
  )
  
  p <- ggplot() +
    # Add edges
    geom_segment(data = edges, aes(x = from_x, y = from_y, xend = to_x, yend = to_y),
                 arrow = arrow(length = unit(0.2, "cm")), size = 1, color = "gray50") +
    # Add nodes
    geom_point(data = nodes, aes(x = x, y = y, color = type), size = 8) +
    geom_text(data = nodes, aes(x = x, y = y, label = label), 
              color = "white", size = 3, fontface = "bold") +
    # Color scheme
    scale_color_manual(values = c(
      "predictor" = COLORS["predictor"],
      "random" = COLORS["rank"],
      "context" = COLORS["context"]["duo"],
      "linear" = COLORS["model"],
      "link" = COLORS["model"],
      "probability" = COLORS["response"],
      "response" = COLORS["response"]
    )) +
    # Labels
    labs(title = "Model Structure: Exploration Decision Process",
         subtitle = "Flow from predictors to binary choice") +
    theme_void() +
    theme(legend.position = "none",
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 12, hjust = 0.5))
  
  return(p)
}

# 2. Mathematical Equation Diagram
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

# 3. Predictor Variables Diagram
create_predictor_diagram <- function() {
  # Load data to get actual values
  raw <- read.csv("Explore Exploit Dataset.csv")
  
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

# 4. Model Comparison Diagram
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

# 5. Bayesian Inference Diagram
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

# 6. Data Flow Diagram
create_data_flow <- function() {
  # Create data processing flow
  flow_data <- data.frame(
    x = c(1, 2, 3, 4, 5, 6),
    y = c(1, 1, 1, 1, 1, 1),
    label = c("Raw Data", "Filter Trials", "Code Choices", "Add Variables", "Model Data", "Generate Plots"),
    description = c("CSV file with all trials",
                   "OIT_RE trials only",
                   "explore/exploit/none",
                   "rank, context, sex",
                   "brms models",
                   "ggplot2 visualizations")
  )
  
  p <- ggplot(flow_data, aes(x = x, y = y)) +
    geom_point(aes(color = label), size = 10) +
    geom_text(aes(label = label), color = "white", fontface = "bold", size = 3) +
    geom_text(aes(label = description), y = 0.7, size = 2.5, hjust = 0.5) +
    geom_segment(aes(x = x, xend = x + 0.8, y = y, yend = y), 
                 arrow = arrow(length = unit(0.2, "cm")), size = 1) +
    scale_color_manual(values = c(
      "Raw Data" = COLORS["explore"],
      "Filter Trials" = COLORS["model"],
      "Code Choices" = COLORS["rank"],
      "Add Variables" = COLORS["response"],
      "Model Data" = COLORS["predictor"],
      "Generate Plots" = COLORS["context"]["duo"]
    )) +
    labs(title = "Data Processing Pipeline",
         subtitle = "From raw data to final analysis") +
    theme_void() +
    theme(legend.position = "none",
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 12, hjust = 0.5))
  
  return(p)
}

# Generate all diagrams
p1 <- create_model_structure()
p2 <- create_equation_diagram()
p3 <- create_predictor_diagram()
p4 <- create_model_comparison()
p5 <- create_bayesian_diagram()
p6 <- create_data_flow()

# Save individual diagrams
ggsave("model_structure_diagram.png", p1, width = 12, height = 8, dpi = 300)
ggsave("model_structure_diagram.pdf", p1, width = 12, height = 8)

ggsave("mathematical_equation_diagram.png", p2, width = 12, height = 8, dpi = 300)
ggsave("mathematical_equation_diagram.pdf", p2, width = 12, height = 8)

ggsave("predictor_variables_diagram.png", p3, width = 12, height = 8, dpi = 300)
ggsave("predictor_variables_diagram.pdf", p3, width = 12, height = 8)

ggsave("model_comparison_diagram.png", p4, width = 12, height = 8, dpi = 300)
ggsave("model_comparison_diagram.pdf", p4, width = 12, height = 8)

ggsave("bayesian_inference_diagram.png", p5, width = 12, height = 8, dpi = 300)
ggsave("bayesian_inference_diagram.pdf", p5, width = 12, height = 8)

ggsave("data_flow_diagram.png", p6, width = 12, height = 8, dpi = 300)
ggsave("data_flow_diagram.pdf", p6, width = 12, height = 8)

# Create a comprehensive model specification diagram
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

p_comprehensive <- create_comprehensive_model_spec()
ggsave("comprehensive_model_specification.png", p_comprehensive, width = 16, height = 10, dpi = 300)
ggsave("comprehensive_model_specification.pdf", p_comprehensive, width = 16, height = 10)

cat("Model specification diagrams generated successfully!\n")
cat("Files saved as PNG and PDF formats.\n")
cat("These diagrams explain the mathematical details and model structure.\n") 