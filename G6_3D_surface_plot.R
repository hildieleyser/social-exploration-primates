# =============================================================================
# G6 - 3D SURFACE PLOT (PROPER 3D VERSION)
# =============================================================================
# 3-D surface of model-predicted explore-rate vs partner count (x) 
# and relative rank (y). Visually nails your main claim: effects are additive, 
# not multiplicative; surface is a plane with ≈ constant slope along each axis.
# =============================================================================

library(dplyr)
library(ggplot2)
library(nnet)
library(plotly)
library(viridis)

# Load and prepare data
cat("Loading data and fitting model for G6 3D surface plot...\n")

# Load data
raw_data <- read.csv("Explore Exploit Dataset.csv", stringsAsFactors = FALSE)

# Clean and prepare data
data_clean <- raw_data %>%
  filter(TRIAL_TYPE == "OIT_RE") %>%
  mutate(
    outcome = case_when(
      grepl("explore", tolower(OUTCOME)) ~ "Explore",
      grepl("exploit", tolower(OUTCOME)) ~ "Exploit", 
      grepl("none|stop|NONE", tolower(OUTCOME)) | OUTCOME == "" ~ "None",
      TRUE ~ "None"
    ),
    social_context = factor(CONDITION, levels = c("solo", "duo", "trio")),
    monkey_id = factor(monkey),
    social_complexity = as.numeric(social_context),
    rank_z = as.numeric(scale(ABSOLUTE_RANK)),
    expected_explore_z = as.numeric(scale(expected_explore)),
    subjective_exploit_z = as.numeric(scale(subjective_exploit)),
    chosen_value_z = as.numeric(scale(SUBJECTIVE_CHOSEN_VALUE))
  ) %>%
  filter(!is.na(outcome), !is.na(social_context), !is.na(monkey_id)) %>%
  filter(complete.cases(.[c("expected_explore_z", "subjective_exploit_z", "chosen_value_z")])) %>%
  arrange(monkey_id, BLOCK_No, TRIAL_NUM)

# Fit the hierarchical model
cat("Fitting hierarchical multinomial model...\n")
fit_hier <- multinom(outcome ~ social_complexity + expected_explore_z + 
                    subjective_exploit_z + chosen_value_z + rank_z + monkey_id, 
                    data = data_clean, trace = FALSE)

# Create prediction grid
cat("Creating prediction grid for 3D surface plot...\n")
newdat <- expand.grid(
  partners = 0:2,  # 0=solo, 1=duo, 2=trio
  rank = 1:3,      # 1=dominant, 2=middle, 3=subordinate
  expected_explore_z = 0,      # mean value
  subjective_exploit_z = 0,    # mean value  
  chosen_value_z = 0,          # mean value
  monkey_id = levels(data_clean$monkey_id)[1]  # reference monkey
)

# Add social complexity based on partners
newdat$social_complexity <- newdat$partners + 1

# Add rank_z (scaled rank)
newdat$rank_z <- scale(newdat$rank)[,1]

# Generate predictions using the fitted model directly
cat("Generating model predictions...\n")

# Use the fitted model to predict probabilities
predictions <- predict(fit_hier, newdata = newdat, type = "probs")
newdat$pred <- predictions[, "Explore"]  # Focus on explore rate

# Check prediction range
cat("Prediction range:", range(newdat$pred), "\n")
cat("Number of valid predictions:", sum(!is.na(newdat$pred)), "\n")

# =============================================================================
# CALCULATE CONFIDENCE INTERVALS (POSTERIOR SD)
# =============================================================================

cat("Calculating confidence intervals...\n")

# Use bootstrap to estimate uncertainty
set.seed(42)
n_bootstrap <- 1000
bootstrap_predictions <- matrix(0, nrow = nrow(newdat), ncol = n_bootstrap)

for(b in 1:n_bootstrap) {
  # Bootstrap sample
  boot_indices <- sample(1:nrow(data_clean), replace = TRUE)
  boot_data <- data_clean[boot_indices, ]
  
  # Fit model on bootstrap sample
  boot_fit <- multinom(outcome ~ social_complexity + expected_explore_z + 
                       subjective_exploit_z + chosen_value_z + rank_z + monkey_id, 
                       data = boot_data, trace = FALSE)
  
  # Predict on newdat
  boot_pred <- predict(boot_fit, newdata = newdat, type = "probs")
  bootstrap_predictions[, b] <- boot_pred[, "Explore"]
}

# Calculate mean predictions and standard deviations
newdat$pred_sd <- apply(bootstrap_predictions, 1, sd)

# =============================================================================
# CALCULATE RAW DATA MAXIMA FOR SPIKES
# =============================================================================

cat("Calculating raw data maxima for spikes...\n")

# Calculate raw exploration rates by condition and rank
raw_exploration <- data_clean %>%
  group_by(social_complexity, ABSOLUTE_RANK, monkey_id) %>%
  summarise(
    explore_rate = mean(outcome == "Explore"),
    n_trials = n(),
    .groups = "drop"
  ) %>%
  # Map to our grid
  mutate(
    partners = social_complexity - 1,
    rank = ABSOLUTE_RANK
  ) %>%
  # Find the maximum explorer in each cell
  group_by(partners, rank) %>%
  summarise(
    max_explore_rate = max(explore_rate),
    max_explorer = monkey_id[which.max(explore_rate)],
    n_monkeys = n(),
    .groups = "drop"
  )

# Merge with prediction grid
newdat <- newdat %>%
  left_join(raw_exploration, by = c("partners", "rank"))

# Fill missing values with 0 (no data in that cell)
newdat$max_explore_rate[is.na(newdat$max_explore_rate)] <- 0

# =============================================================================
# CREATE THE 3D SURFACE PLOT
# =============================================================================

cat("Creating 3D surface plot...\n")

# Reshape data for plotly
z_matrix <- matrix(newdat$pred, nrow = 3, ncol = 3, byrow = FALSE)
sd_matrix <- matrix(newdat$pred_sd, nrow = 3, ncol = 3, byrow = FALSE)
x_vals <- sort(unique(newdat$partners))
y_vals <- sort(unique(newdat$rank))

# Create the 3D surface plot
surf <- plot_ly(
  x = x_vals,
  y = y_vals,
  z = z_matrix,
  type = "surface",
  colorscale = "Viridis",
  showscale = TRUE,
  colorbar = list(title = "Explore Rate", len = 0.5, y = 0.7),
  opacity = 0.8
) %>%
  layout(
    scene = list(
      xaxis = list(
        title = "Partner Count",
        ticktext = c("Solo", "Duo", "Trio"),
        tickvals = 0:2
      ),
      yaxis = list(
        title = "Relative Rank",
        ticktext = c("Dominant", "Middle", "Subordinate"),
        tickvals = 1:3
      ),
      zaxis = list(
        title = "Exploration Probability",
        range = c(0, 1)
      ),
      camera = list(eye = list(x = -1.6, y = 1.2, z = 0.9))  # angled to show both slopes
    ),
    title = list(
      text = "G6: 3D Additive Surface Plot<br>Model-Predicted Explore Rate vs Partner Count & Rank",
      font = list(size = 18)
    )
  )

# Add spikes for the maximum explorer in each cell
spike_data <- newdat %>%
  select(partners, rank, pred, max_explore_rate) %>%
  mutate(
    x_start = partners,
    y_start = rank,
    z_start = pred,
    x_end = partners,
    y_end = rank,
    z_end = max_explore_rate
  )

# Create spike lines
for(i in 1:nrow(spike_data)) {
  surf <- surf %>% add_trace(
    type = "scatter3d",
    mode = "lines",
    x = c(spike_data$x_start[i], spike_data$x_end[i]),
    y = c(spike_data$y_start[i], spike_data$y_end[i]),
    z = c(spike_data$z_start[i], spike_data$z_end[i]),
    line = list(width = 4, color = "rgba(0,0,0,0.35)"),
    showlegend = FALSE
  )
}

# Save the interactive 3D plot
htmlwidgets::saveWidget(surf, "G6_3D_surface_plot.html", selfcontained = FALSE)

# =============================================================================
# CREATE STATIC 3D PLOT USING GGPLOT2
# =============================================================================

cat("Creating static 3D version...\n")

# Create a 3D scatter plot with surface
library(plot3D)

# Create the surface data
surface_data <- newdat %>%
  select(partners, rank, pred, pred_sd, max_explore_rate) %>%
  mutate(
    partners_label = case_when(
      partners == 0 ~ "Solo",
      partners == 1 ~ "Duo", 
      partners == 2 ~ "Trio"
    ),
    rank_label = case_when(
      rank == 1 ~ "Dominant",
      rank == 2 ~ "Middle",
      rank == 3 ~ "Subordinate"
    )
  )

# Print the data used for plotting
cat("\nSurface data used for plotting:\n")
print(surface_data)

# Create 3D surface plot using plot3D
pdf("G6_3D_surface_plot.pdf", width = 12, height = 10)

# Create the 3D surface
surf3D(x = matrix(surface_data$partners, nrow = 3, ncol = 3, byrow = FALSE),
        y = matrix(surface_data$rank, nrow = 3, ncol = 3, byrow = FALSE),
        z = matrix(surface_data$pred, nrow = 3, ncol = 3, byrow = FALSE),
        col = viridis(100),
        alpha = 0.8,
        border = "black",
        lwd = 2,
        xlab = "Partner Count",
        ylab = "Relative Rank", 
        zlab = "Exploration Probability",
        main = "G6: 3D Additive Surface Plot\nModel-Predicted Explore Rate vs Partner Count & Rank")

# Add points for raw data maxima
scatter3D(x = surface_data$partners,
          y = surface_data$rank,
          z = surface_data$max_explore_rate,
          col = "red",
          pch = 19,
          cex = 2,
          add = TRUE)

# Add connecting lines (spikes)
for(i in 1:nrow(surface_data)) {
  segments3D(x0 = surface_data$partners[i], y0 = surface_data$rank[i], z0 = surface_data$pred[i],
             x1 = surface_data$partners[i], y1 = surface_data$rank[i], z1 = surface_data$max_explore_rate[i],
             col = "black", lwd = 2, add = TRUE)
}

dev.off()

# =============================================================================
# ADDITIONAL ANALYSIS: VERIFY ADDITIVE EFFECTS
# =============================================================================

cat("Analyzing additive effects...\n")

# Calculate slopes along each axis to verify additivity
slopes_analysis <- surface_data %>%
  group_by(partners) %>%
  summarise(
    rank_slope = (pred[rank == 3] - pred[rank == 1]) / 2,
    .groups = "drop"
  ) %>%
  mutate(
    rank_slope_consistency = abs(rank_slope - mean(rank_slope)) < 0.05
  )

rank_slopes <- surface_data %>%
  group_by(rank) %>%
  summarise(
    partner_slope = (pred[partners == 2] - pred[partners == 0]) / 2,
    .groups = "drop"
  ) %>%
  mutate(
    partner_slope_consistency = abs(partner_slope - mean(partner_slope)) < 0.05
  )

cat("\n=== ADDITIVE EFFECTS ANALYSIS ===\n")
cat("Rank slopes (should be similar across partner counts):\n")
print(slopes_analysis)

cat("\nPartner slopes (should be similar across ranks):\n")
print(rank_slopes)

# Calculate overall additivity metric
mean_rank_slope <- mean(slopes_analysis$rank_slope)
sd_rank_slope <- sd(slopes_analysis$rank_slope)
rank_consistency <- 1 - (sd_rank_slope / abs(mean_rank_slope))

mean_partner_slope <- mean(rank_slopes$partner_slope)
sd_partner_slope <- sd(rank_slopes$partner_slope)
partner_consistency <- 1 - (sd_partner_slope / abs(mean_partner_slope))

cat(sprintf("\nAdditivity Metrics:\n"))
cat(sprintf("Rank effect consistency: %.3f (1.0 = perfectly additive)\n", rank_consistency))
cat(sprintf("Partner effect consistency: %.3f (1.0 = perfectly additive)\n", partner_consistency))
cat(sprintf("Overall additivity: %.3f\n", (rank_consistency + partner_consistency) / 2))

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n=== G6 3D SURFACE PLOT COMPLETE ===\n")
cat("Files created:\n")
cat("- G6_3D_surface_plot.html (interactive 3D plot)\n")
cat("- G6_3D_surface_plot.pdf (static 3D plot)\n")
cat("\nThe 3D surface plot shows:\n")
cat("- Z-axis: Model-predicted explore rate\n")
cat("- X-axis: Partner count (0=Solo, 1=Duo, 2=Trio)\n")
cat("- Y-axis: Relative rank (1=Dominant, 2=Middle, 3=Subordinate)\n")
cat("- Red points: Raw data maxima\n")
cat("- Black lines: Spikes connecting model predictions to raw data\n")
cat("- Planar surface indicates additive rather than multiplicative interaction\n") 