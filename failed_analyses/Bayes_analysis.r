# Load required libraries
library(brms)
library(dplyr)
library(marginaleffects)
library(ggplot2)

# Read the data
data <- read.csv("Explore-Exploit-Dataset.csv")

# Inspect the structure and unique values of OUTCOME
table(data$OUTCOME)

# Option 1: Collapse all exploit_* into "exploit" and keep "explore" as is
data <- data %>%
  mutate(
    decision = ifelse(grepl("exploit_", OUTCOME), "exploit", as.character(OUTCOME))
  ) %>%
  filter(decision %in% c("exploit", "explore")) # Keep only these two categories

# If you want to keep all exploit flavors as separate categories, uncomment below:
# data$decision <- data$OUTCOME

# Convert to factor
data$decision <- factor(data$decision)
data$monkey <- factor(data$monkey)
data$CONDITION <- factor(data$CONDITION)

# Use 'expected_explore' as expectation variable
data$expectation <- data$expected_explore

# Fit Bayesian hierarchical multinomial model
# If decision is binary (exploit vs explore), use family = bernoulli()
# If decision has more than two categories, use family = categorical()

# Here we use family = bernoulli() for binary outcome
fit <- brm(
  decision ~ CONDITION + expectation + (1 | monkey),
  data = data,
  family = bernoulli(),
  chains = 4,
  iter = 2000,
  cores = 4
)

# If you want multinomial (more than two outcome categories), use:
# fit <- brm(
#   decision ~ CONDITION + expectation + (1 | monkey),
#   data = data,
#   family = categorical(),
#   chains = 4,
#   iter = 2000,
#   cores = 4
# )

# Model summary and interpretation
summary(fit)

# Plotting predicted probabilities
preds <- predictions(
  fit,
  newdata = datagrid(
    CONDITION = unique(data$CONDITION),
    expectation = seq(min(data$expectation, na.rm = TRUE), 
                      max(data$expectation, na.rm = TRUE), 
                      length.out = 20),
    monkey = NA
  )
)

ggplot(preds, aes(x = expectation, y = estimate, color = CONDITION)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = CONDITION), alpha = 0.2) +
  labs(title = "Predicted Probability of Exploit vs Explore",
       y = "Predicted Probability",
       x = "Expectation Value") +
  theme_minimal()
