# GDINA Q-Matrix Refinement Tool

## Overview
This R script provides a high-performance, automated tool for validating and refining Q-matrices in Cognitive Diagnosis Models (CDM), specifically designed for the **GDINA** framework.

It utilizes **parallel processing** to rapidly evaluate item fit and iteratively expands the Q-matrix to satisfy success/non-mastery probability thresholds.

## Key Features
* **Parallel Processing:** Uses the `parallel` package to distribute item evaluation across CPU cores for maximum speed.
* **Automated Expansion:** Iteratively adds attributes if items do not meet fit criteria.
* **Customizable Thresholds:** Users can define specific probabilities for "Success" (P(1)) and "Non-Mastery" (P(0)).
* **GDINA Integration:** Built directly on top of the `GDINA` package R objects.

## Prerequisites
Ensure you have the following R packages installed:

```r
install.packages("GDINA")
install.packages("parallel")

# Load the script locally
source("Iterative_Q_Refinement")

# Example Workflow

# 1. Prepare your data (Example)
# my_data <- read.csv("responses.csv")
# my_q    <- read.csv("q_matrix.csv")

# 2. Run Refinement
new_q_matrix <- refine_q_matrix(
  response_data = my_data,
  initial_q_matrix = my_q,
  success_threshold = 0.80,    # Default: P(1) must be >= 0.80
  nonmastery_threshold = 0.35, # Default: P(0) must be <= 0.35
  max_attributes_per_item = 10,# Limit complexity per item
  n_cores = 4                  # Optional: Limit CPU usage
)

# 3. View Results
print(new_q_matrix)

# 4. Check Item Probabilities of the new matrix
display_item_probabilities(my_data, new_q_matrix)

Parameter	Description	Default
response_data	The binary response matrix (Items x Students).	(Required)
initial_q_matrix	The starting Q-matrix.	(Required)
success_threshold	Minimum probability required for the "all-attributes-mastered" class.	0.8
nonmastery_threshold	Maximum probability allowed for the "no-attributes-mastered" class.	0.35
max_attributes_per_item	Maximum attributes allowed for a single item.	10
max_total_attributes	Maximum number of attributes allowed in the whole model.	nrow(Q)/4
n_cores	Number of CPU cores to use.	detectCores() - 1
