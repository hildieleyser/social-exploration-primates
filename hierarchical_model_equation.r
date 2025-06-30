# Create Beautiful Hierarchical Model Equation Figure
# Based on the handwritten notes showing the full hierarchical structure

library(graphics)

# Create the model equation figure
png("Figure_7_Hierarchical_Model_Equation.png", width = 1600, height = 1200, res = 300)

# Set up the plot area
par(mar = c(2, 2, 4, 2), bg = "white")
plot(0, 0, type = "n", xlim = c(0, 10), ylim = c(0, 10), 
     xaxt = "n", yaxt = "n", bty = "n", xlab = "", ylab = "")

# Title
text(5, 9.5, "Hierarchical Multinomial Model for Social Decision-Making", 
     cex = 2.2, font = 2, col = "#2C3E50")

# Model specification
text(5, 8.8, "Explore-Exploit Choice Model", cex = 1.6, font = 2, col = "#34495E")

# Main equation header
text(5, 8.2, "Level 1: Trial-Level Model", cex = 1.4, font = 2, col = "#E74C3C")

# The main trinomial model equation (matching handwritten notes)
text(5, 7.6, "Y[ijk] ~ Multinomial(1, θ[ijk])", 
     cex = 1.3, col = "#2C3E50")

# Expectation equation (matching handwritten notes)
text(1, 7.0, "Expectation:", cex = 1.2, font = 2, col = "#E74C3C")
text(5.5, 7.0, "Within a block, the running", cex = 1.1, col = "#2C3E50")
text(5.5, 6.7, "average of explore responses", cex = 1.1, col = "#2C3E50")
text(5.5, 6.4, "initial start value = 0.5", cex = 1.1, col = "#2C3E50")

# Variables definition (matching handwritten notes)
text(1, 5.8, "i = monkey, j = trial, k = block, l = day", 
     cex = 1.1, col = "#7F8C8D")

# The detailed equation (matching handwritten notes)
text(5, 5.2, "Y[ijk] | θ[ijk]", cex = 1.2, col = "#2C3E50")

# Logit equations - simplified for display
text(5, 4.6, "ExploreFreq[ijk] = γ00 + γ10(Social Context[ijk]) + γ02(Partner[ijkl]) + r0ij", 
     cex = 1.0, col = "#27AE60")

text(5, 4.1, "                + γ05(Rank[ijk]) + γ04(Preference[ijkl])", 
     cex = 1.0, col = "#27AE60")

text(5, 3.6, "                + γ03(Known Value[ijk])", 
     cex = 1.0, col = "#27AE60")

text(5, 3.1, "                + γ06(Expectation[ijk]) + u0j + εij", 
     cex = 1.0, col = "#27AE60")

# Level 2 header
text(5, 2.4, "Level 2: Individual-Level Random Effects", cex = 1.4, font = 2, col = "#E74C3C")

# Random effects
text(5, 1.9, "u0j ~ N(0, τ²00)", cex = 1.1, col = "#8E44AD")
text(5, 1.5, "r0ij ~ N(0, σ²)", cex = 1.1, col = "#8E44AD")

# Level 3 header  
text(5, 0.9, "Level 3: Context-Level Effects", cex = 1.4, font = 2, col = "#E74C3C")

# Context effects
text(5, 0.4, "Social Complexity: Individual → Dyadic → Triadic", 
     cex = 1.1, col = "#F39C12")

# Add border
rect(0.2, 0.1, 9.8, 9.8, border = "#BDC3C7", lwd = 2)

dev.off()

cat("Hierarchical model equation figure created: Figure_7_Hierarchical_Model_Equation.png\n") 