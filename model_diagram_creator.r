# Hierarchical Model Diagram Creator
# Creates an intuitive diagram showing the data structure and Bayesian model

library(graphics)

# ================================================================================
# CREATE HIERARCHICAL MODEL DIAGRAM
# ================================================================================

create_model_diagram <- function() {
  
  # Set up the plot
  png("Hierarchical_Model_Diagram.png", width = 4800, height = 3200, res = 600)
  
  par(mar = c(2, 2, 4, 2), family = "sans")
  
  # Create empty plot
  plot(c(0, 10), c(0, 8), type = "n", axes = FALSE, xlab = "", ylab = "")
  
  # Colors
  pop_color <- "#e8f4f8"      # Light blue for population
  monkey_color <- "#d4e6f1"   # Medium blue for monkeys  
  block_color <- "#aed6f1"    # Darker blue for blocks
  trial_color <- "#85c1e9"    # Darkest blue for trials
  
  # ================================
  # LEVEL 1: POPULATION
  # ================================
  rect(1, 7, 9, 7.5, col = pop_color, border = "black", lwd = 2)
  text(5, 7.25, "Population of Monkeys", cex = 1.2, font = 2)
  text(5, 6.8, "Fixed Effects: Social Complexity, Rank, Expectation, Known Value", cex = 0.9)
  
  # ================================
  # LEVEL 2: INDIVIDUAL MONKEYS
  # ================================
  monkey_positions <- c(1.5, 3, 4.5, 6, 7.5, 9)
  monkey_names <- c("CHOCOLAT", "DALI", "EBI", "FRAN", "ICE", "ANEMONE")
  
  for(i in 1:6) {
    x <- monkey_positions[i]
    rect(x-0.4, 5.5, x+0.4, 6.2, col = monkey_color, border = "black", lwd = 1.5)
    text(x, 5.85, monkey_names[i], cex = 0.7, font = 2)
    
    # Draw line from population to monkey
    lines(c(5, x), c(7, 6.2), lwd = 1.5, col = "gray60")
  }
  
  text(5, 5.2, "Individual Monkeys (Random Intercepts + Slopes)", cex = 1, font = 2)
  text(5, 4.9, "Each monkey has different baseline exploration + social sensitivity", cex = 0.8)
  
  # ================================
  # LEVEL 3: BLOCKS WITHIN MONKEYS
  # ================================
  # Show blocks for first 3 monkeys to avoid clutter
  for(i in 1:3) {
    x <- monkey_positions[i]
    
    # Block 1
    rect(x-0.6, 3.8, x-0.2, 4.3, col = block_color, border = "black", lwd = 1)
    text(x-0.4, 4.05, "B1", cex = 0.6, font = 2)
    lines(c(x, x-0.4), c(5.5, 4.3), lwd = 1, col = "gray60")
    
    # Block 2  
    rect(x-0.1, 3.8, x+0.3, 4.3, col = block_color, border = "black", lwd = 1)
    text(x+0.1, 4.05, "B2", cex = 0.6, font = 2)
    lines(c(x, x+0.1), c(5.5, 4.3), lwd = 1, col = "gray60")
    
    # Block 3
    rect(x+0.4, 3.8, x+0.8, 4.3, col = block_color, border = "black", lwd = 1)
    text(x+0.6, 4.05, "B3", cex = 0.6, font = 2)
    lines(c(x, x+0.6), c(5.5, 4.3), lwd = 1, col = "gray60")
  }
  
  # Dots for other monkeys
  for(i in 4:6) {
    x <- monkey_positions[i]
    text(x, 4.05, "...", cex = 1.2, font = 2)
    lines(c(x, x), c(5.5, 4.3), lwd = 1, col = "gray60")
  }
  
  text(5, 3.5, "Blocks within Monkeys (Random Intercepts)", cex = 1, font = 2)
  text(5, 3.2, "Each block has different baseline (day effects, fatigue, etc.)", cex = 0.8)
  
  # ================================
  # LEVEL 4: TRIALS WITHIN BLOCKS
  # ================================
  # Show trials for first block of first monkey
  x <- monkey_positions[1] - 0.4
  
  # Trial circles
  trial_x <- seq(x-0.3, x+0.3, length.out = 5)
  for(j in 1:5) {
    if(j <= 3) {
      # Draw actual circles for first 3 trials
      circle_x <- trial_x[j]
      symbols(circle_x, 2.5, circles = 0.08, add = TRUE, 
              bg = trial_color, fg = "black", lwd = 1, inches = FALSE)
      text(circle_x, 2.5, paste0("T", j), cex = 0.4, font = 2)
      lines(c(x, circle_x), c(3.8, 2.58), lwd = 0.8, col = "gray60")
    } else {
      # Dots for remaining trials
      text(trial_x[j], 2.5, "...", cex = 0.8, font = 2)
    }
  }
  
  # Show outcome variables
  text(trial_x[1], 2.1, "explore=1", cex = 0.5)
  text(trial_x[2], 2.1, "exploit=0", cex = 0.5)
  text(trial_x[3], 2.1, "explore=1", cex = 0.5)
  
  text(5, 1.8, "Trials within Blocks", cex = 1, font = 2)
  text(5, 1.5, "Binary outcome: Explore (1) vs. Exploit (0)", cex = 0.8)
  
  # ================================
  # MODEL SPECIFICATION BOX
  # ================================
  rect(0.5, 0.2, 9.5, 1.2, col = "#f8f9fa", border = "black", lwd = 2)
  text(5, 1, "Bayesian Hierarchical Model Specification", cex = 1.1, font = 2)
  text(5, 0.7, "explore_choice ~ social_complexity + rank + expectation + known_value +", cex = 0.8)
  text(5, 0.5, "(1 + social_complexity | monkey_id) + (1 | block_id)", cex = 0.8)
  text(5, 0.3, "family = bernoulli(link = 'logit')", cex = 0.8)
  
  # Title
  title("Hierarchical Data Structure: Social Decision-Making in Primates", 
        cex.main = 1.4, font.main = 2, line = 2)
  
  dev.off()
  
  cat("Model diagram created: Hierarchical_Model_Diagram.png\n")
}

# ================================================================================
# CREATE DATA MAPPING DIAGRAM  
# ================================================================================

create_data_mapping_diagram <- function() {
  
  png("Data_Mapping_Diagram.png", width = 4800, height = 3200, res = 600)
  
  par(mar = c(2, 2, 4, 2), family = "sans")
  
  # Create empty plot
  plot(c(0, 10), c(0, 10), type = "n", axes = FALSE, xlab = "", ylab = "")
  
  # ================================
  # RESEARCH QUESTION BOX
  # ================================
  rect(1, 8.5, 9, 9.5, col = "#fff3cd", border = "#856404", lwd = 3)
  text(5, 9, "RESEARCH QUESTION:", cex = 1.2, font = 2, col = "#856404")
  text(5, 8.7, "How do social frames of reference influence explore-exploit decisions?", cex = 1, font = 1)
  
  # ================================
  # VARIABLES MAPPING
  # ================================
  
  # Social Complexity
  rect(0.5, 7, 3, 8, col = "#d4e6f1", border = "black", lwd = 2)
  text(1.75, 7.7, "SOCIAL COMPLEXITY", cex = 0.9, font = 2)
  text(1.75, 7.4, "0 = Individual (solo)", cex = 0.7)
  text(1.75, 7.2, "1 = Dyadic (duo)", cex = 0.7)
  text(1.75, 7.0, "2 = Triadic (trio)", cex = 0.7)
  
  # Outcome Variable
  rect(7, 7, 9.5, 8, col = "#d5e8d4", border = "black", lwd = 2)
  text(8.25, 7.7, "OUTCOME", cex = 0.9, font = 2)
  text(8.25, 7.4, "1 = Explore", cex = 0.7)
  text(8.25, 7.2, "0 = Exploit", cex = 0.7)
  
  # Control Variables
  rect(1, 5.5, 4.5, 6.5, col = "#f0f0f0", border = "black", lwd = 2)
  text(2.75, 6.2, "CONTROL VARIABLES", cex = 0.9, font = 2)
  text(2.75, 5.95, "• Rank (1-3, dominant to subordinate)", cex = 0.7)
  text(2.75, 5.75, "• Running Expectation (0-1)", cex = 0.7)
  text(2.75, 5.55, "• Known Value (subjective value)", cex = 0.7)
  
  # Random Effects
  rect(5.5, 5.5, 9, 6.5, col = "#ffe6e6", border = "black", lwd = 2)
  text(7.25, 6.2, "RANDOM EFFECTS", cex = 0.9, font = 2)
  text(7.25, 5.95, "• Individual monkey differences", cex = 0.7)
  text(7.25, 5.75, "• Block-level variation", cex = 0.7)
  text(7.25, 5.55, "• Day effects", cex = 0.7)
  
  # ================================
  # THEORETICAL FRAMEWORK
  # ================================
  rect(1, 3.5, 9, 5, col = "#e8f5e8", border = "black", lwd = 2)
  text(5, 4.7, "THEORETICAL FRAMEWORK", cex = 1.1, font = 2)
  text(5, 4.4, "Social Complexity → Cognitive Load → Exploration Behavior", cex = 0.9, font = 1)
  text(5, 4.1, "Mechanisms:", cex = 0.8, font = 2)
  text(5, 3.9, "• Social monitoring demands reduce available cognitive resources", cex = 0.75)
  text(5, 3.7, "• Coordination requirements constrain individual exploration", cex = 0.75)
  text(5, 3.5, "• Competition increases exploitation of known resources", cex = 0.75)
  
  # ================================
  # PREDICTIONS
  # ================================
  rect(1, 1.5, 9, 3, col = "#fff0e6", border = "black", lwd = 2)
  text(5, 2.7, "PREDICTIONS", cex = 1.1, font = 2)
  text(5, 2.4, "H1: Exploration decreases with social complexity", cex = 0.9)
  text(5, 2.1, "H2: Individual differences in social sensitivity", cex = 0.9)
  text(5, 1.8, "H3: Dominance rank moderates social effects", cex = 0.9)
  
  # ================================
  # DATA STRUCTURE
  # ================================
  rect(1, 0.2, 9, 1.2, col = "#f8f9fa", border = "black", lwd = 2)
  text(5, 0.9, "DATA STRUCTURE", cex = 1, font = 2)
  text(5, 0.6, "1,454 trials × 6 monkeys × 3 social contexts × multiple blocks", cex = 0.8)
  text(5, 0.4, "Hierarchical: Trials → Blocks → Monkeys → Population", cex = 0.8)
  
  # Title
  title("Research Framework: Social Context and Exploration Behavior", 
        cex.main = 1.4, font.main = 2, line = 2)
  
  dev.off()
  
  cat("Data mapping diagram created: Data_Mapping_Diagram.png\n")
}

# Create both diagrams
create_model_diagram()
create_data_mapping_diagram()

cat("\nBoth diagrams created successfully!\n")
cat("1. Hierarchical_Model_Diagram.png - Shows the nested data structure\n")
cat("2. Data_Mapping_Diagram.png - Maps research question to variables\n") 