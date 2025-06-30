# Separate Beautiful Andrew Heiss-Style Plots
# Each plot will be saved as a high-quality individual figure

library(graphics)
library(stats)

# Andrew Heiss color palette
ah_colors <- list(
  blue = "#2E86C1",
  red = "#E74C3C", 
  green = "#27AE60",
  orange = "#F39C12",
  purple = "#8E44AD",
  gray = "#34495E",
  light_blue = "#85C1E9",
  light_red = "#F1948A",
  light_green = "#82E0AA"
)

# Load and prepare data
cat("Loading and preparing data...\n")
data <- read.csv("Explore Exploit Dataset.csv", stringsAsFactors = FALSE)

# Data preprocessing
clean_choices <- function(outcome) {
  result <- rep(NA, length(outcome))
  result[outcome == "exploit_pink"] <- 0  # Exploit high-value
  result[outcome == "exploit_blue"] <- 1  # Exploit low-value
  result[outcome == "explore"] <- 2       # Explore
  return(result)
}

data$choice <- clean_choices(data$OUTCOME)
main_trials <- data[data$TRIAL_TYPE == "OIT_RE" & !is.na(data$choice), ]

# Create model variables
main_trials$social_context <- factor(main_trials$CONDITION, 
                                     levels = c("solo", "duo", "trio"),
                                     labels = c("Individual", "Dyadic", "Triadic"))
main_trials$social_complexity <- as.numeric(main_trials$social_context) - 1
main_trials$dominance_status <- ifelse(main_trials$social_context == "Individual", 0, 
                                      main_trials$RELATIVE_RANK)
main_trials$monkey_id <- as.factor(main_trials$monkey)

# Fit models
cat("Fitting models...\n")

# Model 1: Exploration vs High-Value Exploitation
explore_vs_high <- ifelse(main_trials$choice == 2, 1, ifelse(main_trials$choice == 0, 0, NA))
valid_eh <- !is.na(explore_vs_high)

model_data_eh <- data.frame(
  outcome = explore_vs_high[valid_eh],
  social_complexity = main_trials$social_complexity[valid_eh],
  dominance_status = main_trials$dominance_status[valid_eh],
  monkey_id = main_trials$monkey_id[valid_eh],
  social_context = main_trials$social_context[valid_eh]
)

model_explore <- glm(outcome ~ social_complexity + dominance_status + monkey_id, 
                    data = model_data_eh, family = binomial(link = "logit"))

# Model 2: High-Value vs Low-Value Exploitation
high_vs_low <- ifelse(main_trials$choice == 0, 1, ifelse(main_trials$choice == 1, 0, NA))
valid_hl <- !is.na(high_vs_low)

model_data_hl <- data.frame(
  outcome = high_vs_low[valid_hl],
  social_complexity = main_trials$social_complexity[valid_hl], 
  dominance_status = main_trials$dominance_status[valid_hl],
  monkey_id = main_trials$monkey_id[valid_hl],
  social_context = main_trials$social_context[valid_hl]
)

model_value <- glm(outcome ~ social_complexity + dominance_status + monkey_id, 
                  data = model_data_hl, family = binomial(link = "logit"))

# Function to create prediction data
create_predictions <- function(model, data, var_name, var_values) {
  pred_data <- data[1:length(var_values), ]
  pred_data[[var_name]] <- var_values
  
  # Set other variables to their means/modes
  if("social_complexity" %in% names(pred_data) && var_name != "social_complexity") {
    pred_data$social_complexity <- mean(data$social_complexity)
  }
  if("dominance_status" %in% names(pred_data) && var_name != "dominance_status") {
    pred_data$dominance_status <- mean(data$dominance_status)
  }
  if("monkey_id" %in% names(pred_data)) {
    pred_data$monkey_id <- data$monkey_id[1]  # Reference level
  }
  
  predictions <- predict(model, newdata = pred_data, type = "response", se.fit = TRUE)
  
  data.frame(
    x = var_values,
    predicted = predictions$fit,
    se = predictions$se.fit,
    lower = pmax(0, predictions$fit - 1.96 * predictions$se.fit),
    upper = pmin(1, predictions$fit + 1.96 * predictions$se.fit)
  )
}

# ================================================================================
# FIGURE 1: SOCIAL COMPLEXITY EFFECTS ON EXPLORATION
# ================================================================================

cat("Creating Figure 1: Social Complexity Effects...\n")

png("Figure_1_Social_Complexity_Effects.png", width = 1200, height = 900, res = 300)

# Set up beautiful plot
par(mar = c(6, 6, 5, 3), family = "serif")

social_pred <- create_predictions(model_explore, model_data_eh, "social_complexity", 0:2)

# Create the plot
plot(social_pred$x, social_pred$predicted, 
     type = "n", 
     xlim = c(-0.3, 2.3), ylim = c(0, 1),
     xlab = "", ylab = "",
     main = "",
     xaxt = "n", yaxt = "n",
     bty = "n")

# Add subtle grid
for(i in seq(0, 1, 0.2)) {
  abline(h = i, col = "gray95", lwd = 0.5)
}

# Add confidence ribbon
polygon(c(social_pred$x, rev(social_pred$x)), 
         c(social_pred$lower, rev(social_pred$upper)),
         col = paste0(ah_colors$blue, "30"), border = NA)

# Add prediction line
lines(social_pred$x, social_pred$predicted, 
      col = ah_colors$blue, lwd = 4)

# Add points
points(social_pred$x, social_pred$predicted, 
       pch = 21, bg = ah_colors$blue, col = "white", 
       cex = 2.5, lwd = 3)

# Beautiful axes
axis(1, at = 0:2, labels = c("Individual", "Dyadic", "Triadic"), 
     cex.axis = 1.3, col.axis = ah_colors$gray, font = 2)
axis(2, at = seq(0, 1, 0.2), labels = paste0(seq(0, 100, 20), "%"), 
     cex.axis = 1.3, col.axis = ah_colors$gray, las = 1)

# Beautiful labels
mtext("Social Context", side = 1, line = 4, cex = 1.6, font = 2, col = ah_colors$gray)
mtext("Probability of Exploration", side = 2, line = 4, cex = 1.6, font = 2, col = ah_colors$gray)
mtext("Social Complexity Effects on Exploration Behavior", side = 3, line = 2, 
      cex = 1.8, font = 2, col = ah_colors$gray)

# Add effect size annotation
text(1, 0.9, paste0("β = ", round(coef(model_explore)[2], 3), 
                   "\np = ", round(summary(model_explore)$coefficients[2, 4], 3)),
     cex = 1.2, font = 2, col = ah_colors$blue,
     bg = "white", box.col = ah_colors$blue, box.lwd = 2)

dev.off()

# ================================================================================
# FIGURE 2: HIERARCHY EFFECTS ON EXPLORATION  
# ================================================================================

cat("Creating Figure 2: Hierarchy Effects...\n")

png("Figure_2_Hierarchy_Effects.png", width = 1200, height = 900, res = 300)

par(mar = c(6, 6, 5, 3), family = "serif")

dom_pred <- create_predictions(model_explore, model_data_eh[model_data_eh$dominance_status > 0, ], 
                              "dominance_status", 1:3)

# Create the plot
plot(dom_pred$x, dom_pred$predicted, 
     type = "n",
     xlim = c(0.5, 3.5), ylim = c(0, 1),
     xlab = "", ylab = "",
     main = "",
     xaxt = "n", yaxt = "n",
     bty = "n")

# Add subtle grid
for(i in seq(0, 1, 0.2)) {
  abline(h = i, col = "gray95", lwd = 0.5)
}

# Add confidence ribbon
polygon(c(dom_pred$x, rev(dom_pred$x)), 
         c(dom_pred$lower, rev(dom_pred$upper)),
         col = paste0(ah_colors$red, "30"), border = NA)

# Add prediction line
lines(dom_pred$x, dom_pred$predicted, 
      col = ah_colors$red, lwd = 4)

# Add points
points(dom_pred$x, dom_pred$predicted, 
       pch = 21, bg = ah_colors$red, col = "white", 
       cex = 2.5, lwd = 3)

# Beautiful axes
axis(1, at = 1:3, labels = c("Dominant", "Middle", "Subordinate"), 
     cex.axis = 1.3, col.axis = ah_colors$gray, font = 2)
axis(2, at = seq(0, 1, 0.2), labels = paste0(seq(0, 100, 20), "%"), 
     cex.axis = 1.3, col.axis = ah_colors$gray, las = 1)

# Beautiful labels
mtext("Dominance Rank", side = 1, line = 4, cex = 1.6, font = 2, col = ah_colors$gray)
mtext("Probability of Exploration", side = 2, line = 4, cex = 1.6, font = 2, col = ah_colors$gray)
mtext("Hierarchy Effects on Exploration Behavior", side = 3, line = 2, 
      cex = 1.8, font = 2, col = ah_colors$gray)

# Add effect size annotation
text(2.5, 0.9, paste0("β = ", round(coef(model_explore)[3], 3), 
                      "\np = ", round(summary(model_explore)$coefficients[3, 4], 3)),
     cex = 1.2, font = 2, col = ah_colors$red,
     bg = "white", box.col = ah_colors$red, box.lwd = 2)

dev.off()

# ================================================================================
# FIGURE 3: INDIVIDUAL DIFFERENCES
# ================================================================================

cat("Creating Figure 3: Individual Differences...\n")

png("Figure_3_Individual_Differences.png", width = 1400, height = 900, res = 300)

par(mar = c(7, 6, 5, 3), family = "serif")

individual_data <- aggregate(model_data_eh$outcome, 
                           by = list(model_data_eh$monkey_id), 
                           FUN = mean)
names(individual_data) <- c("monkey", "exploration_rate")

# Create the plot
plot(1:nrow(individual_data), individual_data$exploration_rate,
     type = "n",
     xlim = c(0.5, nrow(individual_data) + 0.5), ylim = c(0, 1),
     xlab = "", ylab = "",
     main = "",
     xaxt = "n", yaxt = "n",
     bty = "n")

# Add subtle grid
for(i in seq(0, 1, 0.2)) {
  abline(h = i, col = "gray95", lwd = 0.5)
}

# Create beautiful gradient colors for individuals
individual_colors <- c(ah_colors$green, ah_colors$orange, ah_colors$purple, 
                      ah_colors$blue, ah_colors$red, ah_colors$gray)

# Add bars with gradient effect
for(i in 1:nrow(individual_data)) {
  rect(i - 0.35, 0, i + 0.35, individual_data$exploration_rate[i],
       col = individual_colors[i], border = "white", lwd = 3)
  
  # Add value labels on top of bars
  text(i, individual_data$exploration_rate[i] + 0.05, 
       paste0(round(individual_data$exploration_rate[i] * 100, 1), "%"),
       cex = 1.1, font = 2, col = individual_colors[i])
}

# Beautiful axes
axis(1, at = 1:nrow(individual_data), labels = individual_data$monkey, 
     cex.axis = 1.3, col.axis = ah_colors$gray, font = 2, las = 2)
axis(2, at = seq(0, 1, 0.2), labels = paste0(seq(0, 100, 20), "%"), 
     cex.axis = 1.3, col.axis = ah_colors$gray, las = 1)

# Beautiful labels
mtext("Individual", side = 1, line = 5, cex = 1.6, font = 2, col = ah_colors$gray)
mtext("Exploration Rate", side = 2, line = 4, cex = 1.6, font = 2, col = ah_colors$gray)
mtext("Individual Differences in Exploration Behavior", side = 3, line = 2, 
      cex = 1.8, font = 2, col = ah_colors$gray)

dev.off()

# ================================================================================
# FIGURE 4: VALUE SENSITIVITY
# ================================================================================

cat("Creating Figure 4: Value Sensitivity...\n")

png("Figure_4_Value_Sensitivity.png", width = 1200, height = 900, res = 300)

par(mar = c(6, 6, 5, 3), family = "serif")

model2_social_pred <- create_predictions(model_value, model_data_hl, "social_complexity", 0:2)

# Create the plot
plot(model2_social_pred$x, model2_social_pred$predicted, 
     type = "n",
     xlim = c(-0.3, 2.3), ylim = c(0, 1),
     xlab = "", ylab = "",
     main = "",
     xaxt = "n", yaxt = "n",
     bty = "n")

# Add subtle grid
for(i in seq(0, 1, 0.2)) {
  abline(h = i, col = "gray95", lwd = 0.5)
}

# Add confidence ribbon
polygon(c(model2_social_pred$x, rev(model2_social_pred$x)), 
         c(model2_social_pred$lower, rev(model2_social_pred$upper)),
         col = paste0(ah_colors$green, "30"), border = NA)

# Add prediction line
lines(model2_social_pred$x, model2_social_pred$predicted, 
      col = ah_colors$green, lwd = 4)

# Add points
points(model2_social_pred$x, model2_social_pred$predicted, 
       pch = 21, bg = ah_colors$green, col = "white", 
       cex = 2.5, lwd = 3)

# Beautiful axes
axis(1, at = 0:2, labels = c("Individual", "Dyadic", "Triadic"), 
     cex.axis = 1.3, col.axis = ah_colors$gray, font = 2)
axis(2, at = seq(0, 1, 0.2), labels = paste0(seq(0, 100, 20), "%"), 
     cex.axis = 1.3, col.axis = ah_colors$gray, las = 1)

# Beautiful labels
mtext("Social Context", side = 1, line = 4, cex = 1.6, font = 2, col = ah_colors$gray)
mtext("Probability of High-Value Choice", side = 2, line = 4, cex = 1.6, font = 2, col = ah_colors$gray)
mtext("Social Effects on Value Sensitivity", side = 3, line = 2, 
      cex = 1.8, font = 2, col = ah_colors$gray)

# Add effect size annotation
text(1, 0.9, paste0("β = ", round(coef(model_value)[2], 3), 
                   "\np = ", round(summary(model_value)$coefficients[2, 4], 3)),
     cex = 1.2, font = 2, col = ah_colors$green,
     bg = "white", box.col = ah_colors$green, box.lwd = 2)

dev.off()

# ================================================================================
# FIGURE 5: HIERARCHY EFFECTS ON VALUE SENSITIVITY
# ================================================================================

cat("Creating Figure 5: Hierarchy Effects on Value...\n")

png("Figure_5_Hierarchy_Value_Effects.png", width = 1200, height = 900, res = 300)

par(mar = c(6, 6, 5, 3), family = "serif")

dom_value_pred <- create_predictions(model_value, model_data_hl[model_data_hl$dominance_status > 0, ], 
                                    "dominance_status", 1:3)

# Create the plot
plot(dom_value_pred$x, dom_value_pred$predicted, 
     type = "n",
     xlim = c(0.5, 3.5), ylim = c(0, 1),
     xlab = "", ylab = "",
     main = "",
     xaxt = "n", yaxt = "n",
     bty = "n")

# Add subtle grid
for(i in seq(0, 1, 0.2)) {
  abline(h = i, col = "gray95", lwd = 0.5)
}

# Add confidence ribbon
polygon(c(dom_value_pred$x, rev(dom_value_pred$x)), 
         c(dom_value_pred$lower, rev(dom_value_pred$upper)),
         col = paste0(ah_colors$orange, "30"), border = NA)

# Add prediction line
lines(dom_value_pred$x, dom_value_pred$predicted, 
      col = ah_colors$orange, lwd = 4)

# Add points
points(dom_value_pred$x, dom_value_pred$predicted, 
       pch = 21, bg = ah_colors$orange, col = "white", 
       cex = 2.5, lwd = 3)

# Beautiful axes
axis(1, at = 1:3, labels = c("Dominant", "Middle", "Subordinate"), 
     cex.axis = 1.3, col.axis = ah_colors$gray, font = 2)
axis(2, at = seq(0, 1, 0.2), labels = paste0(seq(0, 100, 20), "%"), 
     cex.axis = 1.3, col.axis = ah_colors$gray, las = 1)

# Beautiful labels
mtext("Dominance Rank", side = 1, line = 4, cex = 1.6, font = 2, col = ah_colors$gray)
mtext("Probability of High-Value Choice", side = 2, line = 4, cex = 1.6, font = 2, col = ah_colors$gray)
mtext("Hierarchy Effects on Value Sensitivity", side = 3, line = 2, 
      cex = 1.8, font = 2, col = ah_colors$gray)

# Add effect size annotation and significance
sig_text <- if(summary(model_value)$coefficients[3, 4] < 0.05) "**" else "*"
text(2.5, 0.9, paste0("β = ", round(coef(model_value)[3], 3), 
                      "\np = ", round(summary(model_value)$coefficients[3, 4], 3), sig_text),
     cex = 1.2, font = 2, col = ah_colors$orange,
     bg = "white", box.col = ah_colors$orange, box.lwd = 2)

dev.off()

# ================================================================================
# FIGURE 6: MODEL COMPARISON
# ================================================================================

cat("Creating Figure 6: Model Comparison...\n")

png("Figure_6_Model_Comparison.png", width = 1200, height = 900, res = 300)

par(mar = c(6, 6, 5, 3), family = "serif")

aic_values <- c(AIC(model_explore), AIC(model_value))
model_names <- c("Exploration\nModel", "Value\nModel")

# Create the plot
plot(1:2, aic_values,
     type = "n",
     xlim = c(0.5, 2.5), ylim = c(min(aic_values) - 20, max(aic_values) + 20),
     xlab = "", ylab = "",
     main = "",
     xaxt = "n", yaxt = "n",
     bty = "n")

# Add subtle grid
aic_range <- seq(floor(min(aic_values)/20)*20, ceiling(max(aic_values)/20)*20, 20)
for(i in aic_range) {
  abline(h = i, col = "gray95", lwd = 0.5)
}

# Add bars
colors_models <- c(ah_colors$blue, ah_colors$green)
for(i in 1:2) {
  rect(i - 0.25, min(aic_values) - 20, i + 0.25, aic_values[i],
       col = colors_models[i], border = "white", lwd = 3)
  
  # Add AIC values as text
  text(i, aic_values[i] + 10, round(aic_values[i], 1), 
       cex = 1.3, font = 2, col = colors_models[i])
}

# Beautiful axes
axis(1, at = 1:2, labels = model_names, cex.axis = 1.3, col.axis = ah_colors$gray, font = 2)
axis(2, at = aic_range, cex.axis = 1.3, col.axis = ah_colors$gray, las = 1)

# Beautiful labels
mtext("Model", side = 1, line = 4, cex = 1.6, font = 2, col = ah_colors$gray)
mtext("AIC (lower = better fit)", side = 2, line = 4, cex = 1.6, font = 2, col = ah_colors$gray)
mtext("Model Comparison: Information Criteria", side = 3, line = 2, 
      cex = 1.8, font = 2, col = ah_colors$gray)

# Add interpretation
text(1.5, max(aic_values) + 5, "Lower AIC indicates better model fit", 
     cex = 1.2, font = 2, col = ah_colors$gray)

dev.off()

cat("All individual figures created successfully!\n")
cat("Files saved:\n")
cat("- Figure_1_Social_Complexity_Effects.png\n")
cat("- Figure_2_Hierarchy_Effects.png\n") 
cat("- Figure_3_Individual_Differences.png\n")
cat("- Figure_4_Value_Sensitivity.png\n")
cat("- Figure_5_Hierarchy_Value_Effects.png\n")
cat("- Figure_6_Model_Comparison.png\n") 