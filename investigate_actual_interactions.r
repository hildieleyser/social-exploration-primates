# INVESTIGATE ACTUAL INTERACTION PATTERNS
# Check if groups are truly independent or if there are cross-group interactions

# Load data
data_raw <- read.csv("Explore Exploit Dataset.csv")

# Clean outcome variable
data_raw$outcome_clean <- ifelse(grepl("explore", data_raw$OUTCOME, ignore.case = TRUE), "explore",
                               ifelse(grepl("exploit", data_raw$OUTCOME, ignore.case = TRUE), "exploit", 
                                     ifelse(data_raw$OUTCOME %in% c("NONE", "none", "stop"), "none", "other")))

data_valid <- data_raw[!is.na(data_raw$outcome_clean) & data_raw$outcome_clean != "other", ]

# Define the two groups as user specified
group1_monkeys <- c("ANEMONE", "ICE", "CHOCOLAT")
group2_monkeys <- c("EBI", "FRAN", "DALI")

cat("=== INVESTIGATING ACTUAL INTERACTION PATTERNS ===\n")
cat("Group 1:", paste(group1_monkeys, collapse = ", "), "\n")
cat("Group 2:", paste(group2_monkeys, collapse = ", "), "\n\n")

# Check all unique monkey names in the data
unique_monkeys <- unique(data_valid$monkey)
cat("Unique monkeys in dataset:", paste(unique_monkeys, collapse = ", "), "\n\n")

# Check all unique PAIRED_WITH values
unique_pairs <- unique(data_valid$PAIRED_WITH)
cat("Unique PAIRED_WITH values:\n")
print(unique_pairs)
cat("\n")

# Check for cross-group interactions
cat("=== CHECKING FOR CROSS-GROUP INTERACTIONS ===\n")

cross_group_interactions <- data.frame()

for(actor in unique_monkeys) {
  actor_data <- data_valid[data_valid$monkey == actor, ]
  
  # Determine actor's group
  actor_group <- if(actor %in% group1_monkeys) "Group1" else if(actor %in% group2_monkeys) "Group2" else "Unknown"
  
  # Check each partner mentioned in PAIRED_WITH
  for(partner_field in unique(actor_data$PAIRED_WITH)) {
    if(partner_field != "" && !is.na(partner_field)) {
      
      # Check if any group2 monkey is mentioned when actor is group1, or vice versa
      if(actor_group == "Group1") {
        # Check if any group2 monkeys are mentioned
        for(g2_monkey in group2_monkeys) {
          if(grepl(g2_monkey, partner_field, fixed = TRUE)) {
            cross_interactions <- data_valid[data_valid$monkey == actor & data_valid$PAIRED_WITH == partner_field, ]
            if(nrow(cross_interactions) > 0) {
              cross_group_interactions <- rbind(cross_group_interactions, 
                                              data.frame(Actor = actor, 
                                                        Actor_Group = "Group1",
                                                        Partner_Field = partner_field,
                                                        Detected_Partner = g2_monkey,
                                                        Partner_Group = "Group2",
                                                        Trial_Count = nrow(cross_interactions)))
            }
          }
        }
      } else if(actor_group == "Group2") {
        # Check if any group1 monkeys are mentioned
        for(g1_monkey in group1_monkeys) {
          if(grepl(g1_monkey, partner_field, fixed = TRUE)) {
            cross_interactions <- data_valid[data_valid$monkey == actor & data_valid$PAIRED_WITH == partner_field, ]
            if(nrow(cross_interactions) > 0) {
              cross_group_interactions <- rbind(cross_group_interactions, 
                                              data.frame(Actor = actor, 
                                                        Actor_Group = "Group2",
                                                        Partner_Field = partner_field,
                                                        Detected_Partner = g1_monkey,
                                                        Partner_Group = "Group1",
                                                        Trial_Count = nrow(cross_interactions)))
            }
          }
        }
      }
    }
  }
}

if(nrow(cross_group_interactions) > 0) {
  cat("WARNING: CROSS-GROUP INTERACTIONS DETECTED!\n")
  print(cross_group_interactions)
} else {
  cat("✓ NO CROSS-GROUP INTERACTIONS DETECTED - Groups are truly independent\n")
}

cat("\n=== DETAILED INTERACTION ANALYSIS BY GROUP ===\n")

# Analyze Group 1 interactions only
cat("\nGROUP 1 INTERACTIONS (ANEMONE, ICE, CHOCOLAT):\n")
group1_data <- data_valid[data_valid$monkey %in% group1_monkeys, ]

for(actor in group1_monkeys) {
  actor_data <- group1_data[group1_data$monkey == actor, ]
  cat("\n", actor, "interactions:\n")
  
  # Solo trials
  solo_trials <- actor_data[actor_data$CONDITION == "solo", ]
  cat("  Solo:", nrow(solo_trials), "trials\n")
  
  # Paired trials
  paired_trials <- actor_data[actor_data$PAIRED_WITH != "" & !is.na(actor_data$PAIRED_WITH), ]
  if(nrow(paired_trials) > 0) {
    unique_partners <- unique(paired_trials$PAIRED_WITH)
    for(partner in unique_partners) {
      partner_trials <- paired_trials[paired_trials$PAIRED_WITH == partner, ]
      cat("  With", partner, ":", nrow(partner_trials), "trials\n")
    }
  }
}

# Analyze Group 2 interactions only
cat("\nGROUP 2 INTERACTIONS (EBI, FRAN, DALI):\n")
group2_data <- data_valid[data_valid$monkey %in% group2_monkeys, ]

for(actor in group2_monkeys) {
  actor_data <- group2_data[group2_data$monkey == actor, ]
  cat("\n", actor, "interactions:\n")
  
  # Solo trials
  solo_trials <- actor_data[actor_data$CONDITION == "solo", ]
  cat("  Solo:", nrow(solo_trials), "trials\n")
  
  # Paired trials
  paired_trials <- actor_data[actor_data$PAIRED_WITH != "" & !is.na(actor_data$PAIRED_WITH), ]
  if(nrow(paired_trials) > 0) {
    unique_partners <- unique(paired_trials$PAIRED_WITH)
    for(partner in unique_partners) {
      partner_trials <- paired_trials[paired_trials$PAIRED_WITH == partner, ]
      cat("  With", partner, ":", nrow(partner_trials), "trials\n")
    }
  }
}

# Create separate matrices for each group
cat("\n=== CREATING SEPARATE MATRICES FOR EACH GROUP ===\n")

# Group 1 Matrix
group1_matrix <- matrix(NA, nrow = 3, ncol = 3)
rownames(group1_matrix) <- group1_monkeys
colnames(group1_matrix) <- group1_monkeys

# Group 2 Matrix  
group2_matrix <- matrix(NA, nrow = 3, ncol = 3)
rownames(group2_matrix) <- group2_monkeys
colnames(group2_matrix) <- group2_monkeys

# Helper function
safe_rate <- function(subset_data, outcome) {
  if(nrow(subset_data) == 0) return(NA)
  sum(subset_data$outcome_clean == outcome, na.rm = TRUE) / nrow(subset_data) * 100
}

# Fill Group 1 matrix
for(i in 1:3) {
  for(j in 1:3) {
    actor <- group1_monkeys[i]
    partner <- group1_monkeys[j]
    
    if(i == j) {
      # Solo performance
      solo_data <- data_valid[data_valid$monkey == actor & data_valid$CONDITION == "solo", ]
      group1_matrix[i, j] <- safe_rate(solo_data, "explore")
    } else {
      # Paired performance within group
      paired_data <- data_valid[data_valid$monkey == actor & 
                               grepl(partner, data_valid$PAIRED_WITH, fixed = TRUE), ]
      group1_matrix[i, j] <- safe_rate(paired_data, "explore")
    }
  }
}

# Fill Group 2 matrix
for(i in 1:3) {
  for(j in 1:3) {
    actor <- group2_monkeys[i]
    partner <- group2_monkeys[j]
    
    if(i == j) {
      # Solo performance
      solo_data <- data_valid[data_valid$monkey == actor & data_valid$CONDITION == "solo", ]
      group2_matrix[i, j] <- safe_rate(solo_data, "explore")
    } else {
      # Paired performance within group
      paired_data <- data_valid[data_valid$monkey == actor & 
                               grepl(partner, data_valid$PAIRED_WITH, fixed = TRUE), ]
      group2_matrix[i, j] <- safe_rate(paired_data, "explore")
    }
  }
}

cat("\nGROUP 1 EXPLORATION MATRIX:\n")
print(round(group1_matrix, 1))

cat("\nGROUP 2 EXPLORATION MATRIX:\n")
print(round(group2_matrix, 1))

# Save the correct matrices
write.csv(group1_matrix, "group1_exploration_matrix.csv")
write.csv(group2_matrix, "group2_exploration_matrix.csv")

cat("\n=== SUMMARY ===\n")
cat("✓ Saved group1_exploration_matrix.csv\n")
cat("✓ Saved group2_exploration_matrix.csv\n")
if(nrow(cross_group_interactions) > 0) {
  cat("⚠️  WARNING: Cross-group interactions detected - please verify data structure\n")
} else {
  cat("✓ Confirmed: Groups are independent (no cross-group interactions)\n")
} 