#!/usr/bin/env Rscript

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(grid)

# Set theme for white background
theme_set(theme_minimal(base_size = 12) +
          theme(panel.grid.minor = element_blank(),
                panel.background = element_rect(fill = "white"),
                plot.background = element_rect(fill = "white")))

# Color scheme for white background
COLORS <- c(
  "explore" = "#D8A7FF",
  "exploit" = "#DB4DB1", 
  "none" = "#F2C94C",
  "context" = c("solo" = "#8E9BFF", "duo" = "#FF8C42", "trio" = "#EB4559"),
  "rank" = "#642B73",
  "model" = "#2E8B57",
  "predictor" = "#4682B4",
  "response" = "#DC143C",
  "purple" = "#8B5A96",
  "blue" = "#4682B4",
  "green" = "#2E8B57",
  "red" = "#DC143C"
)

# ============================================================================
# 1. Decision Tree Diagram (like the first example)
# ============================================================================

create_decision_tree <- function() {
  # Create decision tree data
  tree_data <- data.frame(
    x = c(5, 2, 5, 8, 5),
    y = c(8, 6, 6, 6, 4),
    label = c("DECISION", "EXPLORE", "EXPLOIT", "NONE", "N = 1,443 decision trials\nacross 6 monkeys"),
    type = c("decision", "outcome", "outcome", "outcome", "summary"),
    trials = c("", "494 trials\n(34.2%)", "494 trials\n(34.2%)", "455 trials\n(31.6%)", "")
  )
  
  # Create arrows
  arrows <- data.frame(
    x = c(5, 5, 5),
    y = c(7.5, 7.5, 7.5),
    xend = c(2, 5, 8),
    yend = c(6.5, 6.5, 6.5)
  )
  
  p <- ggplot() +
    # Add arrows
    geom_segment(data = arrows, aes(x = x, y = y, xend = xend, yend = yend),
                 arrow = arrow(length = unit(0.2, "cm")), size = 1.5, color = COLORS["purple"]) +
    # Add boxes
    geom_rect(data = tree_data[1, ], aes(xmin = x - 1.5, xmax = x + 1.5, ymin = y - 0.5, ymax = y + 0.5),
              fill = COLORS["purple"], color = "white", size = 1) +
    geom_rect(data = tree_data[2:4, ], aes(xmin = x - 1.2, xmax = x + 1.2, ymin = y - 0.4, ymax = y + 0.4),
              fill = COLORS["purple"], color = "white", size = 1) +
    geom_rect(data = tree_data[5, ], aes(xmin = x - 2, xmax = x + 2, ymin = y - 0.3, ymax = y + 0.3),
              fill = COLORS["purple"], color = "white", size = 1) +
    # Add labels
    geom_text(data = tree_data, aes(x = x, y = y, label = label), 
              color = "white", fontface = "bold", size = 4) +
    geom_text(data = tree_data[2:4, ], aes(x = x, y = y - 0.8, label = trials), 
              color = "black", size = 3) +
    # Set limits
    xlim(0, 10) + ylim(0, 9) +
    labs(title = "Decision Tree: Exploration Choices") +
    theme_void() +
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
  
  return(p)
}

# ============================================================================
# 2. Hierarchical Model Structure (like the second example)
# ============================================================================

create_hierarchical_model <- function() {
  # Create hierarchical levels
  levels_data <- data.frame(
    x = c(5, 3, 7, 5),
    y = c(8, 6, 6, 4),
    label = c("LEVEL 3: POPULATION PARAMETERS", 
              "LEVEL 2A: MONKEYS (N=6)",
              "LEVEL 2B: BLOCKS (N=88)", 
              "LEVEL 1: INDIVIDUAL TRIALS (N = 1,443)"),
    level = c("level3", "level2a", "level2b", "level1")
  )
  
  # Create arrows
  arrows <- data.frame(
    x = c(5, 5, 3, 7),
    y = c(7.5, 5.5, 5.5, 5.5),
    xend = c(3, 7, 5, 5),
    yend = c(6.5, 6.5, 4.5, 4.5)
  )
  
  p <- ggplot() +
    # Add arrows
    geom_segment(data = arrows, aes(x = x, y = y, xend = xend, yend = yend),
                 arrow = arrow(length = unit(0.2, "cm")), size = 1.5, color = COLORS["purple"]) +
    # Add boxes
    geom_rect(data = levels_data[1, ], aes(xmin = x - 2.5, xmax = x + 2.5, ymin = y - 0.4, ymax = y + 0.4),
              fill = COLORS["purple"], color = "white", size = 1) +
    geom_rect(data = levels_data[2:3, ], aes(xmin = x - 1.5, xmax = x + 1.5, ymin = y - 0.4, ymax = y + 0.4),
              fill = COLORS["purple"], color = "white", size = 1) +
    geom_rect(data = levels_data[4, ], aes(xmin = x - 2.5, xmax = x + 2.5, ymin = y - 0.4, ymax = y + 0.4),
              fill = COLORS["purple"], color = "white", size = 1) +
    # Add labels
    geom_text(data = levels_data, aes(x = x, y = y, label = label), 
              color = "white", fontface = "bold", size = 3) +
    # Add summary text
    annotate("text", x = 5, y = 2, label = "Accounts for individual differences AND general patterns", 
             color = "black", size = 4, fontface = "bold") +
    # Set limits
    xlim(0, 10) + ylim(0, 9) +
    labs(title = "Hierarchical Model Structure") +
    theme_void() +
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
  
  return(p)
}

# ============================================================================
# 3. Bayesian Inference Framework (like the third example)
# ============================================================================

create_bayesian_framework <- function() {
  # Create Bayesian components
  bayes_data <- data.frame(
    x = c(2, 4, 6, 6),
    y = c(5, 5, 5, 3),
    label = c("PRIOR\nKNOWLEDGE", "DATA\nEVIDENCE", "POSTERIOR\nDISTRIBUTIONS", "Quantified uncertainty\nfor all parameters"),
    type = c("prior", "data", "posterior", "description")
  )
  
  # Create operators
  operators <- data.frame(
    x = c(3, 5),
    y = c(5, 5),
    label = c("+", "=")
  )
  
  # Create arrow
  arrow_data <- data.frame(
    x = 6, y = 4.5, xend = 6, yend = 3.5
  )
  
  p <- ggplot() +
    # Add boxes
    geom_rect(data = bayes_data[1:3, ], aes(xmin = x - 1, xmax = x + 1, ymin = y - 0.5, ymax = y + 0.5),
              fill = COLORS["purple"], color = "white", size = 1) +
    # Add operators
    geom_text(data = operators, aes(x = x, y = y, label = label), 
              color = "black", size = 6, fontface = "bold") +
    # Add arrow
    geom_segment(data = arrow_data, aes(x = x, y = y, xend = xend, yend = yend),
                 arrow = arrow(length = unit(0.2, "cm")), size = 1.5, color = "black") +
    # Add labels
    geom_text(data = bayes_data, aes(x = x, y = y, label = label), 
              color = "white", fontface = "bold", size = 3) +
    geom_text(data = bayes_data[4, ], aes(x = x, y = y, label = label), 
              color = "black", size = 3) +
    # Set limits
    xlim(0, 8) + ylim(0, 6) +
    labs(title = "Bayesian Inference Framework") +
    theme_void() +
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
  
  return(p)
}

# ============================================================================
# 4. Model Predictors (like the third example)
# ============================================================================

create_model_predictors <- function() {
  # Create central decision box
  decision_data <- data.frame(x = 5, y = 3, label = "DECISION")
  
  # Create predictor boxes
  predictors_data <- data.frame(
    x = c(5, 7, 7, 7, 3, 3),
    y = c(5, 4, 3, 2, 4, 2),
    label = c("Relative Rank", "Partner Present", "Social Context", 
              "Explore Expectation", "Exploit Value", "Subjective Value")
  )
  
  # Create arrows from predictors to decision
  arrows <- data.frame(
    x = c(5, 6.5, 6.5, 6.5, 3.5, 3.5),
    y = c(4.5, 4, 3, 2, 4, 2),
    xend = c(5, 5.5, 5.5, 5.5, 4.5, 4.5),
    yend = c(3.5, 3.5, 3.5, 3.5, 3.5, 3.5)
  )
  
  p <- ggplot() +
    # Add central decision box
    geom_rect(data = decision_data, aes(xmin = x - 1.5, xmax = x + 1.5, ymin = y - 0.5, ymax = y + 0.5),
              fill = COLORS["purple"], color = "white", size = 1) +
    # Add predictor boxes
    geom_rect(data = predictors_data, aes(xmin = x - 1, xmax = x + 1, ymin = y - 0.3, ymax = y + 0.3),
              fill = COLORS["purple"], color = "white", size = 1) +
    # Add arrows
    geom_segment(data = arrows, aes(x = x, y = y, xend = xend, yend = yend),
                 arrow = arrow(length = unit(0.15, "cm")), size = 1, color = "black") +
    # Add labels
    geom_text(data = decision_data, aes(x = x, y = y, label = label), 
              color = "white", fontface = "bold", size = 4) +
    geom_text(data = predictors_data, aes(x = x, y = y, label = label), 
              color = "white", fontface = "bold", size = 3) +
    # Set limits
    xlim(0, 10) + ylim(0, 6) +
    labs(title = "Model Predictors") +
    theme_void() +
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
  
  return(p)
}

# ============================================================================
# 5. Mathematical Model Specification (like the fourth example)
# ============================================================================

create_mathematical_specification <- function() {
  # Create specification levels
  spec_data <- data.frame(
    x = rep(5, 17),
    y = c(9, 8.5, 8, 7.5, 7, 6.5, 6, 5.5, 5, 4.5, 4, 3.5, 3, 2.5, 2, 1.5, 1),
    label = c("LEVEL 1: OBSERVATION MODEL",
              "Y[ijk] ~ Multinomial(1, p.explore, p.exploit, p.none)",
              "where:",
              "log(p.explore/p.exploit) = alpha1 + X*beta1 + u1[monkey] + v1[block]",
              "log(p.none/p.exploit) = alpha2 + X*beta2 + u2[monkey] + v2[block]",
              "",
              "LEVEL 2: PREDICTORS (X)",
              "X = [Social Context, Partner Present, Relative Rank, Subjective Value, Exploit Value, Explore Expectation]",
              "",
              "LEVEL 3: BAYESIAN PRIORS",
              "beta ~ Normal(0, 2.5)",
              "sigma ~ Half-Cauchy(0, 1)",
              "u[monkey], v[block] ~ Normal(0, sigma^2)",
              "",
              "Reference category: EXPLOIT (p.exploit in denominator)",
              "",
              "Model fitted using MCMC with 4,000 iterations"),
    type = c("header", "equation", "text", "equation", "equation", "space", 
             "header", "predictors", "space", "header", "prior", "prior", "prior", 
             "space", "reference", "space", "fitting"),
    color = c("header", "equation", "text", "equation", "equation", "space",
              "header", "predictors", "space", "header", "prior", "prior", "prior",
              "space", "reference", "space", "fitting")
  )
  
  p <- ggplot(spec_data, aes(x = x, y = y)) +
    # Add colored boxes for headers
    geom_rect(data = spec_data[spec_data$type == "header", ], 
              aes(xmin = x - 4, xmax = x + 4, ymin = y - 0.2, ymax = y + 0.2),
              fill = COLORS["purple"], color = "white", size = 1) +
    geom_rect(data = spec_data[spec_data$type == "equation", ], 
              aes(xmin = x - 4, xmax = x + 4, ymin = y - 0.15, ymax = y + 0.15),
              fill = COLORS["blue"], color = "white", size = 1) +
    geom_rect(data = spec_data[spec_data$type == "predictors", ], 
              aes(xmin = x - 4, xmax = x + 4, ymin = y - 0.15, ymax = y + 0.15),
              fill = COLORS["green"], color = "white", size = 1) +
    geom_rect(data = spec_data[spec_data$type == "prior", ], 
              aes(xmin = x - 4, xmax = x + 4, ymin = y - 0.15, ymax = y + 0.15),
              fill = COLORS["red"], color = "white", size = 1) +
    # Add labels
    geom_text(aes(label = label, color = type), size = 2.5, hjust = 0.5) +
    scale_color_manual(values = c(
      "header" = "white",
      "equation" = "white", 
      "text" = "black",
      "predictors" = "white",
      "prior" = "white",
      "reference" = "black",
      "fitting" = "black",
      "space" = "white"
    )) +
    # Set limits
    xlim(0, 10) + ylim(0, 10) +
    labs(title = "Mathematical Model Specification") +
    theme_void() +
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          legend.position = "none")
  
  return(p)
}

# Generate all diagrams
p1 <- create_decision_tree()
p2 <- create_hierarchical_model()
p3 <- create_bayesian_framework()
p4 <- create_model_predictors()
p5 <- create_mathematical_specification()

# Save individual diagrams
ggsave("decision_tree_diagram.png", p1, width = 12, height = 8, dpi = 300, bg = "white")
ggsave("decision_tree_diagram.pdf", p1, width = 12, height = 8, bg = "white")

ggsave("hierarchical_model_diagram.png", p2, width = 12, height = 8, dpi = 300, bg = "white")
ggsave("hierarchical_model_diagram.pdf", p2, width = 12, height = 8, bg = "white")

ggsave("bayesian_framework_diagram.png", p3, width = 12, height = 8, dpi = 300, bg = "white")
ggsave("bayesian_framework_diagram.pdf", p3, width = 12, height = 8, bg = "white")

ggsave("model_predictors_diagram.png", p4, width = 12, height = 8, dpi = 300, bg = "white")
ggsave("model_predictors_diagram.pdf", p4, width = 12, height = 8, bg = "white")

ggsave("mathematical_specification_diagram.png", p5, width = 12, height = 10, dpi = 300, bg = "white")
ggsave("mathematical_specification_diagram.pdf", p5, width = 12, height = 10, bg = "white")

cat("Mathematically accurate model diagrams generated successfully!\n")
cat("Files saved as PNG and PDF formats with white backgrounds.\n")
cat("These diagrams match the style of the examples with proper mathematical notation.\n") 