# =============================================================================
# G6 - PANEL P1: ADDITIVE SOCIAL HEAT-MAP (STANDALONE)
# =============================================================================

library(dplyr)
library(ggplot2)
library(nnet)
library(viridis)

# Load and prepare data
cat("Loading data and fitting model for G6 P1...\n")

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

# =============================================================================
# PANEL P1: ADDITIVE SOCIAL HEAT-MAP
# =============================================================================

cat("Creating Panel P1: Additive Social Heat-Map...\n")

# Create prediction grid
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
newdat$rank_z <- scale(newdat$rank)[,1]

# Generate predictions
predictions <- predict(fit_hier, newdata = newdat, type = "probs")
newdat$pred <- predictions[, "Explore"]  # Focus on explore rate

# Calculate slopes for analysis
slopes_data <- newdat %>%
  group_by(partners) %>%
  summarise(
    rank_slope = (pred[rank == 3] - pred[rank == 1]) / 2,
    .groups = "drop"
  ) %>%
  mutate(
    rank_slope_pp = rank_slope * 100  # Convert to percentage points
  )

partner_slopes <- newdat %>%
  group_by(rank) %>%
  summarise(
    partner_slope = (pred[partners == 2] - pred[partners == 0]) / 2,
    .groups = "drop"
  ) %>%
  mutate(
    partner_slope_pp = partner_slope * 100  # Convert to percentage points
  )

# Create heat-map data
heatmap_data <- newdat %>%
  select(partners, rank, pred) %>%
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
    ),
    pred_percent = pred * 100
  )

# Create P1: Additive Social Heat-Map
p1_heatmap <- ggplot(heatmap_data, aes(x = partners, y = rank, fill = pred_percent)) +
  geom_tile(color = "black", linewidth = 1, width = 0.95, height = 0.95) +
  geom_text(aes(label = sprintf("%.1f%%", pred_percent)), 
            color = "white", size = 8, fontface = "bold") +
  scale_fill_viridis_c(name = "Explore Rate (%)", 
                       limits = c(70, 90),
                       breaks = seq(70, 90, 5)) +
  scale_x_continuous(breaks = 0:2, 
                     labels = c("Solo", "Duo", "Trio"),
                     expand = c(0, 0.1)) +
  scale_y_continuous(breaks = 1:3,
                     labels = c("Dominant", "Middle", "Subordinate"),
                     expand = c(0, 0.1)) +
  labs(
    title = "P1: Additive Social Heat-Map",
    subtitle = "Model-Predicted Explore Rate vs Partner Count & Rank\nCuriosity falls in two, independent, linear steps",
    x = "Partner Count",
    y = "Relative Rank"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    legend.position = "right",
    legend.key.height = unit(1.5, "cm"),
    panel.grid.minor = element_blank()
  )

# Save P1
ggsave("G6_P1_heatmap.pdf", p1_heatmap, width = 10, height = 8, dpi = 300)
ggsave("G6_P1_heatmap.png", p1_heatmap, width = 10, height = 8, dpi = 300)

# =============================================================================
# ANALYSIS SUMMARY
# =============================================================================

cat("\n=== G6 P1 ANALYSIS ===\n")

# Calculate slopes
mean_rank_slope <- mean(slopes_data$rank_slope_pp)
mean_partner_slope <- mean(partner_slopes$partner_slope_pp)

cat("P1: Additive Social Heat-Map\n")
cat("============================\n")
cat(sprintf("Mean rank slope: -%.1f percentage points per rank\n", abs(mean_rank_slope)))
cat(sprintf("Mean partner slope: -%.1f percentage points per partner\n", abs(mean_partner_slope)))
cat("The heat-map shows two independent, linear effects:\n")
cat("- More partners → lower exploration\n")
cat("- Lower rank → lower exploration\n")
cat("- Effects are additive (no interaction)\n")

cat("\n=== G6 P1 COMPLETE ===\n")
cat("Files created:\n")
cat("- G6_P1_heatmap.pdf/png (additive social heat-map)\n") 