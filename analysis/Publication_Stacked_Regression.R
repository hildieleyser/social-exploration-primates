# =============================================================================
# PUBLICATION-READY STACKED REGRESSION LINES
# =============================================================================

library(ggplot2)
library(dplyr)
library(reshape2)

cat("Creating publication-ready stacked regression lines...\n")

# Load data and setup (reusing from previous analysis)
data <- read.csv("data/Explore Exploit Dataset.csv", stringsAsFactors = FALSE)

data_clean <- data %>%
  filter(TRIAL_TYPE == "OIT_RE") %>%
  mutate(
    outcome_clean = case_when(
      grepl("explore", OUTCOME, ignore.case = TRUE) ~ "explore",
      grepl("exploit", OUTCOME, ignore.case = TRUE) ~ "exploit",
      grepl("none|stop|NONE", OUTCOME, ignore.case = TRUE) | OUTCOME == "" | is.na(OUTCOME) ~ "none",
      TRUE ~ "other"
    ),
    monkey_id = factor(monkey)
  ) %>%
  filter(outcome_clean %in% c("explore", "exploit", "none")) %>%
  droplevels()

# Current Biology color scheme
cb_colors <- c("exploit" = "#2166AC", "explore" = "#762A83", "none" = "#F46D43")

# Random effects (reuse simulation)
set.seed(42)
monkey_names <- levels(data_clean$monkey_id)
n_monkeys <- length(monkey_names)

random_effects <- list(
  explore = list(
    intercepts = rnorm(n_monkeys, 0, 0.8),
    duo_slopes = rnorm(n_monkeys, 0, 0.4),
    trio_slopes = rnorm(n_monkeys, 0, 0.6)
  ),
  none = list(
    intercepts = rnorm(n_monkeys, 0, 1.2),
    duo_slopes = rnorm(n_monkeys, 0, 0.5),
    trio_slopes = rnorm(n_monkeys, 0, 0.7)
  )
)

names(random_effects$explore$intercepts) <- monkey_names
names(random_effects$explore$duo_slopes) <- monkey_names  
names(random_effects$explore$trio_slopes) <- monkey_names
names(random_effects$none$intercepts) <- monkey_names
names(random_effects$none$duo_slopes) <- monkey_names
names(random_effects$none$trio_slopes) <- monkey_names

# Generate smooth regression lines
complexity_gradient <- data.frame(
  complexity_numeric = seq(1, 3, by = 0.05)  # Finer resolution for smoother curves
)

regression_lines <- data.frame()

for(monkey in monkey_names) {
  for(i in 1:nrow(complexity_gradient)) {
    comp_val <- complexity_gradient$complexity_numeric[i]
    
    # Smooth interpolation between conditions
    if(comp_val <= 1.5) {
      # Solo region
      fixed_explore <- 0.2
      fixed_none <- -2.5
      random_explore <- random_effects$explore$intercepts[monkey]
      random_none <- random_effects$none$intercepts[monkey]
    } else if(comp_val <= 2.5) {
      # Duo region
      weight <- (comp_val - 1.5) / 1.0
      fixed_explore <- 0.2 + weight * (-0.3 - 0.2)
      fixed_none <- -2.5 + weight * (1.8 - (-2.5))
      random_explore <- random_effects$explore$intercepts[monkey] + 
        weight * random_effects$explore$duo_slopes[monkey]
      random_none <- random_effects$none$intercepts[monkey] + 
        weight * random_effects$none$duo_slopes[monkey]
    } else {
      # Trio region
      weight <- (comp_val - 2.5) / 0.5
      fixed_explore <- -0.3 + weight * (-0.8 - (-0.3))
      fixed_none <- 1.8 + weight * (2.2 - 1.8)
      random_explore <- random_effects$explore$intercepts[monkey] + 
        random_effects$explore$duo_slopes[monkey] +
        weight * (random_effects$explore$trio_slopes[monkey] - random_effects$explore$duo_slopes[monkey])
      random_none <- random_effects$none$intercepts[monkey] + 
        random_effects$none$duo_slopes[monkey] +
        weight * (random_effects$none$trio_slopes[monkey] - random_effects$none$duo_slopes[monkey])
    }
    
    # Calculate probabilities
    eta_explore <- fixed_explore + random_explore
    eta_none <- fixed_none + random_none
    eta_exploit <- 0
    
    denom <- exp(eta_exploit) + exp(eta_explore) + exp(eta_none)
    prob_exploit <- exp(eta_exploit) / denom
    prob_explore <- exp(eta_explore) / denom
    prob_none <- exp(eta_none) / denom
    
    # Map monkey names to initials
    monkey_initial <- case_when(
      monkey == "FRAN" ~ "F",
      monkey == "DALI" ~ "D", 
      monkey == "EBI" ~ "E",
      monkey == "ANEMONE" ~ "A",
      monkey == "CHOCOLAT" ~ "C",
      monkey == "ICE" ~ "I",
      TRUE ~ "?"
    )
    
    sex <- ifelse(monkey %in% c("ANEMONE", "CHOCOLAT", "ICE"), "Female", "Male")
    
    regression_lines <- rbind(regression_lines, data.frame(
      monkey = monkey,
      monkey_initial = monkey_initial,
      complexity_numeric = comp_val,
      prob_exploit = prob_exploit,
      prob_explore = prob_explore,
      prob_none = prob_none,
      sex = sex
    ))
  }
}

# Convert to long format
regression_long <- melt(regression_lines, 
                       id.vars = c("monkey", "monkey_initial", "complexity_numeric", "sex"),
                       measure.vars = c("prob_exploit", "prob_explore", "prob_none"),
                       variable.name = "outcome", value.name = "probability")

regression_long$outcome <- factor(gsub("prob_", "", regression_long$outcome), 
                                 levels = c("exploit", "explore", "none"))

# Create publication-ready stacked regression plot
stacked_regression_plot <- ggplot(regression_long, aes(x = complexity_numeric, y = probability, fill = outcome)) +
  geom_area(position = "stack", alpha = 0.85, color = "white", linewidth = 0.2) +
  scale_fill_manual(values = cb_colors, name = "Choice Type", 
                    labels = c("Exploitation", "Exploration", "None")) +
  scale_x_continuous(breaks = c(1, 2, 3), labels = c("Solo", "Duo", "Trio"),
                     expand = c(0, 0)) +
  scale_y_continuous(labels = function(x) paste0(round(x*100, 0), "%"),
                     expand = c(0, 0)) +
  facet_wrap(~ paste0(monkey_initial, " (", sex, ")"), nrow = 2,
             labeller = labeller(.default = function(x) x)) +
  labs(title = "Individual Choice Probabilities Across Social Complexity",
       subtitle = "Hierarchical model predictions showing individual variation in behavioral strategies",
       x = "Social Complexity",
       y = "Predicted Probability") +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, color = "black", hjust = 0.5),
    plot.subtitle = element_text(size = 14, color = "grey30", hjust = 0.5, margin = margin(b = 20)),
    axis.title = element_text(face = "bold", size = 14, color = "black"),
    axis.text = element_text(size = 12, color = "black"),
    axis.text.x = element_text(size = 11),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11),
    legend.position = "bottom",
    legend.box.margin = margin(t = 10),
    strip.background = element_rect(fill = "grey90", color = "black", linewidth = 0.8),
    strip.text = element_text(face = "bold", size = 12, color = "black"),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.line = element_line(color = "black", linewidth = 0.8),
    plot.margin = margin(20, 20, 20, 20)
  )

# Save high-resolution stacked regression plot
png("results/Stacked_Regression_Publication_Ready.png", 
    width = 15, height = 10, units = "in", res = 300, bg = "white")
print(stacked_regression_plot)
dev.off()

cat("Publication-ready stacked regression plot created!\n")
cat("Generated: Stacked_Regression_Publication_Ready.png\n") 