#!/usr/bin/env Rscript

# Enhanced Bayesian Model Visualizations with Creative Shapes - Fixed Layout
# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(grid)
library(ggforce)
library(viridis)
library(RColorBrewer)
library(scales)

# Modern color palette - Black, Red, Grey
COLORS <- list(
  primary = c("#000000", "#d32f2f"),
  secondary = c("#424242", "#b71c1c"),
  accent = c("#757575", "#c62828"),
  success = c("#212121", "#d32f2f"),
  warning = c("#616161", "#b71c1c"),
  danger = c("#424242", "#c62828"),
  dark = c("#000000", "#212121"),
  light = c("#f5f5f5", "#eeeeee")
)

# Custom theme with modern aesthetics
theme_modern <- function() {
  theme_minimal(base_size = 16) +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = alpha("grey80", 0.3), linewidth = 0.3),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 24, face = "bold", hjust = 0.5, 
                               margin = margin(b = 15), color = "#000000"),
      plot.subtitle = element_text(size = 18, hjust = 0.5, 
                                  margin = margin(b = 15), color = "#424242"),
      axis.title = element_text(size = 14, face = "bold", color = "#000000"),
      axis.text = element_text(size = 12, color = "#424242"),
      legend.position = "none",
      plot.margin = margin(25, 25, 25, 25)
    )
}

# ============================================================================
# 1. Decision Tree with Organic Shapes - Fixed Layout
# ============================================================================

create_organic_decision_tree <- function() {
  set.seed(42)
  
  # Generate decision nodes with better spacing
  decision_data <- data.frame(
    x = c(5, 2, 5, 8),
    y = c(8, 5, 5, 5),
    size = c(1.2, 1.0, 1.0, 1.0),
    label = c("DECISION\nPOINT", "EXPLORE\n494 trials\n(34.2%)", 
              "EXPLOIT\n494 trials\n(34.2%)", "NONE\n455 trials\n(31.6%)"),
    type = c("root", "explore", "exploit", "none"),
    color = c("#000000", "#d32f2f", "#424242", "#757575")
  )
  
  # Create connections with better positioning
  connections <- data.frame(
    x = c(5, 5, 5),
    y = c(7.2, 7.2, 7.2),
    xend = c(2, 5, 8),
    yend = c(6.0, 6.0, 6.0)
  )
  
  # Mathematical details box - repositioned
  math_details <- data.frame(
    x = c(0.5, 0.5, 0.5, 0.5),
    y = c(2.8, 2.4, 2.0, 1.6),
    label = c("Y[ijk] ~ Multinomial(p1, p2, p3)",
              "p1 = P(explore), p2 = P(exploit), p3 = P(none)",
              "log(p1/p3) = a1 + X*beta1 + u[i] + v[j]",
              "log(p2/p3) = a2 + X*beta2 + u[i] + v[j]")
  )
  
  p <- ggplot() +
    # Clean connections
    geom_segment(data = connections, 
                 aes(x = x, y = y, xend = xend, yend = yend),
                 linewidth = 2, color = "#000000") +
    
    # Decision nodes as clean circles
    geom_circle(data = decision_data, 
                aes(x0 = x, y0 = y, r = size, fill = color)) +
    
    # Labels with mathematical typography
    geom_text(data = decision_data, 
              aes(x = x, y = y, label = label),
              color = "white", fontface = "bold", size = 4.5,
              lineheight = 0.9) +
    
    # Mathematical details in elegant box - better positioned
    annotate("rect", xmin = 0.2, xmax = 5.8, ymin = 1.2, ymax = 3.2,
             fill = alpha("#000000", 0.9), color = NA) +
    geom_text(data = math_details, 
              aes(x = x, y = y, label = label),
              color = "white", size = 3.5, hjust = 0) +
    
    # Summary statistics - repositioned
    annotate("rect", xmin = 6.2, xmax = 9.8, ymin = 1.2, ymax = 3.2,
             fill = alpha("#d32f2f", 0.9), color = NA) +
    annotate("text", x = 8.0, y = 2.2, 
             label = "N = 1,443 decision trials\nacross 6 monkeys\n\nThree-way classification\nwith balanced outcomes",
             color = "white", size = 4, hjust = 0.5, lineheight = 1.2) +
    
    scale_fill_identity() +
    xlim(0, 10) + ylim(0, 10) +
    labs(title = "Decision Tree: Primate Choice Architecture",
         subtitle = "Mathematical representation of behavioral outcomes") +
    theme_modern() +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank())
  
  return(p)
}

# ============================================================================
# 2. Hierarchical Model as Nested Rectangles - Fixed Layout
# ============================================================================

create_hierarchical_structure <- function() {
  # Define rectangular boxes for each level with better spacing
  levels_data <- data.frame(
    x = c(5, 2.5, 7.5, 5),
    y = c(8, 5.5, 5.5, 2.5),
    width = c(7, 3.5, 3.5, 5),
    height = c(1.2, 1.2, 1.2, 1.2),
    label = c("Population Level:\nbeta ~ Normal(0, 2.5)\nsigma ~ Half-Cauchy(0, 1)",
              "Individual Level:\nu[i] ~ Normal(0, sigma_u^2)\ni = 1,...,6",
              "Block Level:\nv[j] ~ Normal(0, sigma_v^2)\nj = 1,...,88",
              "Trial Level:\nY[ijk] ~ Multinomial(p1, p2, p3)\nk = 1,...,1,443"),
    level = c("level3", "level2a", "level2b", "level1"),
    color = c("#000000", "#d32f2f", "#424242", "#757575")
  )
  
  # Create connecting arrows with better positioning
  arrows <- data.frame(
    x = c(4.2, 5.8, 2.5, 7.5),
    y = c(7.4, 7.4, 4.9, 4.9),
    xend = c(3.2, 6.8, 4.2, 5.8),
    yend = c(6.1, 6.1, 3.1, 3.1)
  )
  
  p <- ggplot() +
    # Background
    geom_rect(aes(xmin = 0, xmax = 10, ymin = 0, ymax = 10), 
              fill = alpha("#f5f5f5", 0.2)) +
    
    # Rectangular hierarchy boxes
    geom_rect(data = levels_data, 
              aes(xmin = x - width/2, xmax = x + width/2, 
                  ymin = y - height/2, ymax = y + height/2, fill = color), 
              alpha = 0.9, color = "white", linewidth = 2) +
    
    # Connecting arrows
    geom_segment(data = arrows, 
                 aes(x = x, y = y, xend = xend, yend = yend), 
                 color = "#000000", linewidth = 2, 
                 arrow = arrow(length = unit(0.25, "cm"))) +
    
    # Mathematical equations in boxes
    geom_text(data = levels_data, 
              aes(x = x, y = y, label = label), 
              color = "white", fontface = "bold", size = 3.2, lineheight = 0.9) +
    
    # Level labels - repositioned
    geom_text(data = data.frame(x = 0.5, y = c(8, 5.5, 5.5, 2.5), 
                               label = c("LEVEL 3", "LEVEL 2A", "LEVEL 2B", "LEVEL 1")), 
              aes(x = x, y = y, label = label), 
              color = "#000000", fontface = "bold", size = 4, hjust = 0.5,
              angle = 90) +
    
    scale_fill_identity() +
    xlim(0, 10) + ylim(0, 10) +
    labs(title = "Hierarchical Model: Multi-Level Structure",
         subtitle = "Mathematical specification of nested dependencies") +
    theme_modern() +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank())
  
  return(p)
}

# ============================================================================
# 3. Bayesian Framework as Flowing Shapes - Fixed Layout
# ============================================================================

create_flowing_bayesian <- function() {
  # Create mathematical Bayesian framework with better spacing
  
  # Prior distribution (left)
  prior_data <- data.frame(
    x = seq(0.5, 2.5, length.out = 50),
    y = dnorm(seq(-2, 2, length.out = 50), 0, 1) * 1.5 + 6,
    component = "prior"
  )
  
  # Likelihood (center)
  likelihood_data <- data.frame(
    x = seq(3.5, 5.5, length.out = 50),
    y = dnorm(seq(-2, 2, length.out = 50), 0.5, 0.8) * 1.5 + 6,
    component = "likelihood"
  )
  
  # Posterior distribution (right)
  posterior_data <- data.frame(
    x = seq(6.5, 8.5, length.out = 50),
    y = dnorm(seq(-2, 2, length.out = 50), 0.3, 0.6) * 1.5 + 6,
    component = "posterior"
  )
  
  # Mathematical equations with better positioning
  equations <- data.frame(
    x = c(1.5, 4.5, 7.5),
    y = c(8.2, 8.2, 8.2),
    label = c("PRIOR\nπ(θ)", "LIKELIHOOD\nL(θ|data)", "POSTERIOR\nπ(θ|data)"),
    box_color = c("#000000", "#d32f2f", "#424242")
  )
  
  # Mathematical operators with better positioning
  operators <- data.frame(
    x = c(3, 6),
    y = c(6, 6),
    label = c("×", "∝"),
    size = c(12, 12)
  )
  
  # Detailed mathematical specifications - repositioned
  math_details <- data.frame(
    x = c(0.5, 0.5, 0.5, 0.5),
    y = c(3.5, 3.0, 2.5, 2.0),
    label = c("Prior: β ~ Normal(0, 2.5)",
              "Likelihood: Y ~ Multinomial(p1, p2, p3)",
              "Posterior: π(θ|data) ∝ π(θ) × L(θ|data)",
              "MCMC: Stan sampling with 4 chains, 2000 iterations")
  )
  
  p <- ggplot() +
    # Mathematical distributions
    geom_line(data = prior_data, 
              aes(x = x, y = y), 
              color = "#000000", linewidth = 3) +
    geom_line(data = likelihood_data, 
              aes(x = x, y = y), 
              color = "#d32f2f", linewidth = 3) +
    geom_line(data = posterior_data, 
              aes(x = x, y = y), 
              color = "#424242", linewidth = 3) +
    
    # Mathematical operators
    geom_text(data = operators, 
              aes(x = x, y = y, label = label), 
              color = "#000000", size = operators$size, fontface = "bold") +
    
    # Component labels in boxes
    geom_rect(data = equations, 
              aes(xmin = x - 0.9, xmax = x + 0.9, ymin = y - 0.5, ymax = y + 0.5,
                  fill = box_color), alpha = 0.9) +
    geom_text(data = equations, 
              aes(x = x, y = y, label = label), 
              color = "white", fontface = "bold", size = 4.5, lineheight = 0.9) +
    
    # Mathematical details box - repositioned
    annotate("rect", xmin = 0.2, xmax = 5.3, ymin = 1.6, ymax = 3.9,
             fill = alpha("#000000", 0.9), color = NA) +
    geom_text(data = math_details, 
              aes(x = x, y = y, label = label), 
              color = "white", size = 3.5, hjust = 0) +
    
    # Bayes theorem equation - repositioned
    annotate("rect", xmin = 5.7, xmax = 9.8, ymin = 1.6, ymax = 3.9,
             fill = alpha("#d32f2f", 0.9), color = NA) +
    annotate("text", x = 7.75, y = 2.75, 
             label = "Bayes' Theorem:\n\nπ(θ|data) = \n  π(θ) × L(θ|data)\n  ────────────────\n  ∫ π(θ) × L(θ|data) dθ",
             color = "white", size = 3.5, fontface = "bold", hjust = 0.5, lineheight = 1.1) +
    
    scale_fill_identity() +
    xlim(0, 10) + ylim(0, 10) +
    labs(title = "Bayesian Inference: Mathematical Framework",
         subtitle = "Probabilistic updating with mathematical precision") +
    theme_modern() +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank())
  
  return(p)
}

# ============================================================================
# 4. Predictor Network as Molecular Structure - Fixed Layout
# ============================================================================

create_molecular_predictors <- function() {
  # Create mathematical predictor network with better spacing
  center_x <- 5
  center_y <- 6
  
  # Predictor variables - repositioned to avoid overlap
  angles <- seq(0, 2*pi, length.out = 7)[-7]
  radius <- 2.8
  
  predictors <- data.frame(
    x = c(center_x, center_x + radius * cos(angles)),
    y = c(center_y, center_y + radius * sin(angles)),
    label = c("DECISION", "Social\nContext", "Partner\nPresent", "Relative\nRank", 
              "Subjective\nValue", "Exploit\nValue", "Explore\nExpectation"),
    type = c("center", rep("predictor", 6)),
    size = c(1.0, rep(0.8, 6)),
    color = c("#000000", "#d32f2f", "#424242", "#757575", "#b71c1c", "#616161", "#c62828")
  )
  
  # Create connections
  connections <- data.frame(
    x = rep(center_x, 6),
    y = rep(center_y, 6),
    xend = predictors$x[2:7],
    yend = predictors$y[2:7],
    strength = c("strong", "medium", "strong", "medium", "strong", "medium")
  )
  
  # Mathematical specifications - repositioned
  math_details <- data.frame(
    x = c(0.5, 0.5, 0.5, 0.5),
    y = c(2.8, 2.4, 2.0, 1.6),
    label = c("Social Context: β₁ ~ Normal(0, 2.5)",
              "Partner Present: β₂ ~ Normal(0, 2.5)",
              "Relative Rank: β₃ ~ Normal(0, 2.5)",
              "Subjective Value: β₄ ~ Normal(0, 2.5)")
  )
  
  p <- ggplot() +
    # Clean connections with different strengths
    geom_segment(data = connections[connections$strength == "strong", ], 
                 aes(x = x, y = y, xend = xend, yend = yend), 
                 color = "#000000", linewidth = 2.5, alpha = 0.8) +
    geom_segment(data = connections[connections$strength == "medium", ], 
                 aes(x = x, y = y, xend = xend, yend = yend), 
                 color = "#424242", linewidth = 2, alpha = 0.6) +
    
    # Predictor nodes
    geom_circle(data = predictors, 
                aes(x0 = x, y0 = y, r = size, fill = color), 
                alpha = 0.9, color = "white", linewidth = 2) +
    
    # Predictor labels
    geom_text(data = predictors, 
              aes(x = x, y = y, label = label), 
              color = "white", fontface = "bold", size = 3.2, lineheight = 0.8) +
    
    # Mathematical details box - repositioned
    annotate("rect", xmin = 0.2, xmax = 5.0, ymin = 1.2, ymax = 3.2,
             fill = alpha("#000000", 0.9), color = NA) +
    geom_text(data = math_details, 
              aes(x = x, y = y, label = label), 
              color = "white", size = 3.2, hjust = 0) +
    
    # Model equation - repositioned
    annotate("rect", xmin = 5.4, xmax = 9.8, ymin = 1.2, ymax = 3.2,
             fill = alpha("#d32f2f", 0.9), color = NA) +
    annotate("text", x = 7.6, y = 2.2, 
             label = "Linear Predictor:\nη = α + β₁X₁ + β₂X₂ + β₃X₃ + β₄X₄\n\nLink Function:\nlog(p₁/p₃) = η₁\nlog(p₂/p₃) = η₂",
             color = "white", size = 3.2, hjust = 0.5, lineheight = 1.1) +
    
    scale_fill_identity() +
    xlim(0, 10) + ylim(0, 10) +
    labs(title = "Predictor Network: Mathematical Variables",
         subtitle = "Network representation of predictor relationships") +
    theme_modern() +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank())
  
  return(p)
}

# ============================================================================
# 5. Mathematical Specification as Layered Structure - Fixed Layout
# ============================================================================

create_layered_math <- function() {
  # Create clean mathematical specification with better spacing
  
  # Mathematical layers - repositioned to avoid overlap
  layers_data <- data.frame(
    layer = c("Observation", "Linear", "Prior"),
    x = c(5, 5, 5),
    y = c(2.5, 5.5, 8.0),
    width = c(8, 7, 6),
    height = c(1.5, 1.5, 1.5),
    color = c("#000000", "#d32f2f", "#424242")
  )
  
  # Mathematical equations for each layer
  equations <- data.frame(
    x = c(5, 5, 5),
    y = c(2.5, 5.5, 8.0),
    label = c("Y[ijk] ~ Multinomial(p₁, p₂, p₃)",
              "log(p₁/p₃) = α₁ + X_{ijk}β₁ + u_i + v_j\nlog(p₂/p₃) = α₂ + X_{ijk}β₂ + u_i + v_j",
              "β ~ Normal(0, 2.5)\nσᵤ, σᵥ ~ Half-Cauchy(0, 1)\nu_i ~ Normal(0, σᵤ²), v_j ~ Normal(0, σᵥ²)"),
    level = c("Observation Model", "Linear Predictor", "Bayesian Priors"),
    color = c("#000000", "#d32f2f", "#424242")
  )
  
  # Connecting arrows
  arrows <- data.frame(
    x = c(5, 5),
    y = c(3.3, 6.3),
    xend = c(5, 5),
    yend = c(4.7, 7.2)
  )
  
  p <- ggplot() +
    # Mathematical layers
    geom_rect(data = layers_data, 
              aes(xmin = x - width/2, xmax = x + width/2, 
                  ymin = y - height/2, ymax = y + height/2, fill = color), 
              alpha = 0.9, color = "white", linewidth = 2) +
    
    # Connecting arrows
    geom_segment(data = arrows, 
                 aes(x = x, y = y, xend = xend, yend = yend), 
                 color = "#000000", linewidth = 2, 
                 arrow = arrow(length = unit(0.25, "cm"))) +
    
    # Mathematical equations in boxes
    geom_text(data = equations, 
              aes(x = x, y = y, label = label), 
              color = "white", fontface = "bold", size = 3.5, lineheight = 0.9) +
    
    # Level labels
    geom_text(data = equations, 
              aes(x = x, y = y - 1.0, label = level), 
              color = equations$color, fontface = "bold", size = 4.5) +
    
    # Index explanations - repositioned
    annotate("rect", xmin = 0.5, xmax = 4.5, ymin = 0.2, ymax = 1.2,
             fill = alpha("#000000", 0.9), color = NA) +
    annotate("text", x = 2.5, y = 0.7, 
             label = "Indices:\ni = 1,...,6 (monkeys)\nj = 1,...,88 (blocks)\nk = 1,...,1,443 (trials)",
             color = "white", size = 3.5, hjust = 0.5, lineheight = 1.1) +
    
    # Model summary - repositioned
    annotate("rect", xmin = 5.5, xmax = 9.5, ymin = 0.2, ymax = 1.2,
             fill = alpha("#d32f2f", 0.9), color = NA) +
    annotate("text", x = 7.5, y = 0.7, 
             label = "Hierarchical Multinomial Model\nMCMC: 4 chains, 2000 iterations\nConvergence: R̂ < 1.01",
             color = "white", size = 3.5, hjust = 0.5, lineheight = 1.1) +
    
    scale_fill_identity() +
    xlim(0, 10) + ylim(0, 10) +
    labs(title = "Mathematical Specification: Complete Model",
         subtitle = "Hierarchical structure of the Bayesian model") +
    theme_modern() +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank())
  
  return(p)
}

# ============================================================================
# Generate all enhanced visualizations
# ============================================================================

cat("Creating enhanced visualizations with fixed layouts...\n")

# Generate all plots
p1 <- create_organic_decision_tree()
p2 <- create_hierarchical_structure()
p3 <- create_flowing_bayesian()
p4 <- create_molecular_predictors()
p5 <- create_layered_math()

# Save high-resolution versions
ggsave("organic_decision_tree.png", p1, width = 14, height = 10, dpi = 300, bg = "white")
ggsave("hierarchical_structure.png", p2, width = 14, height = 10, dpi = 300, bg = "white")
ggsave("flowing_bayesian.png", p3, width = 14, height = 10, dpi = 300, bg = "white")
ggsave("molecular_predictors.png", p4, width = 14, height = 10, dpi = 300, bg = "white")
ggsave("layered_math.png", p5, width = 14, height = 10, dpi = 300, bg = "white")

# Save PDF versions
ggsave("organic_decision_tree.pdf", p1, width = 14, height = 10, bg = "white")
ggsave("hierarchical_structure.pdf", p2, width = 14, height = 10, bg = "white")
ggsave("flowing_bayesian.pdf", p3, width = 14, height = 10, bg = "white")
ggsave("molecular_predictors.pdf", p4, width = 14, height = 10, bg = "white")
ggsave("layered_math.pdf", p5, width = 14, height = 10, bg = "white")

# Create a combined visualization with better spacing
combined_plot <- grid.arrange(p1, p2, p3, p4, p5, ncol = 2, nrow = 3)

# Save combined plot
ggsave("combined_visualizations.png", combined_plot, width = 20, height = 24, dpi = 300, bg = "white")
ggsave("combined_visualizations.pdf", combined_plot, width = 20, height = 24, bg = "white")

cat("All visualizations completed successfully!\n")
cat("Generated files:\n")
cat("- organic_decision_tree.png/pdf\n")
cat("- hierarchical_structure.png/pdf\n")
cat("- flowing_bayesian.png/pdf\n")
cat("- molecular_predictors.png/pdf\n")
cat("- layered_math.png/pdf\n")
cat("- combined_visualizations.png/pdf\n")
