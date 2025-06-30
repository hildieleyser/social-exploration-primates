# Debug data structure
data <- read.csv("Explore Exploit Dataset.csv", stringsAsFactors = FALSE)

cat("Total rows:", nrow(data), "\n")
cat("Columns:", ncol(data), "\n")
cat("Column names:", paste(colnames(data), collapse = ", "), "\n\n")

# Check outcomes
cat("Unique OUTCOME values:\n")
print(table(data$OUTCOME, useNA = "ifany"))

# Check trial types
cat("\nUnique TRIAL_TYPE values:\n")
print(table(data$TRIAL_TYPE, useNA = "ifany"))

# Create trinomial outcomes
create_trinomial_outcome <- function(outcome) {
  result <- rep(NA, length(outcome))
  explore_pattern <- grepl("explore|Explore", outcome, ignore.case = TRUE)
  exploit_pattern <- grepl("exploit|Exploit", outcome, ignore.case = TRUE)
  none_pattern <- grepl("none|NONE|stop|non$", outcome, ignore.case = TRUE)
  
  result[explore_pattern] <- "explore"
  result[exploit_pattern] <- "exploit"
  result[none_pattern] <- "none"
  return(result)
}

data$trinomial_outcome <- create_trinomial_outcome(data$OUTCOME)

cat("\nTrinomial outcomes:\n")
print(table(data$trinomial_outcome, useNA = "ifany"))

# Filter experimental data
exp_data <- data[data$TRIAL_TYPE == "OIT_RE" & !is.na(data$trinomial_outcome), ]

cat("\nFiltered data rows:", nrow(exp_data), "\n")

if(nrow(exp_data) > 0) {
  cat("Monkey column exists:", "monkey" %in% colnames(exp_data), "\n")
  cat("CONDITION column exists:", "CONDITION" %in% colnames(exp_data), "\n")
  cat("expected_explore column exists:", "expected_explore" %in% colnames(exp_data), "\n")
  cat("SUBJECTIVE_CHOSEN_VALUE column exists:", "SUBJECTIVE_CHOSEN_VALUE" %in% colnames(exp_data), "\n")
  
  if("monkey" %in% colnames(exp_data)) {
    cat("Unique monkeys:", paste(unique(exp_data$monkey), collapse = ", "), "\n")
  }
} 