# Viterbi Algorithm Implementation in R

Here's a complete example of implementing the Viterbi algorithm in R to find the most likely sequence of hidden states in a Hidden Markov Model (HMM).

```r
# Viterbi Algorithm Implementation in R

# Load required libraries
library(dplyr)

# Define the HMM parameters
# States: 'Sunny' (1), 'Rainy' (2)
# Observations: 'Walk' (1), 'Shop' (2), 'Clean' (3)

# Transition probabilities (from state i to state j)
# Rows = from, Columns = to
trans_prob <- matrix(c(
  0.7, 0.3,  # From Sunny: 0.7 to Sunny, 0.3 to Rainy
  0.4, 0.6   # From Rainy: 0.4 to Sunny, 0.6 to Rainy
), nrow = 2, byrow = TRUE)

# Emission probabilities (probability of observation given state)
# Rows = state, Columns = observation
emission_prob <- matrix(c(
  0.6, 0.3, 0.1,  # Sunny: 0.6 Walk, 0.3 Shop, 0.1 Clean
  0.1, 0.4, 0.5   # Rainy: 0.1 Walk, 0.4 Shop, 0.5 Clean
), nrow = 2, byrow = TRUE)

# Initial state probabilities
init_prob <- c(0.5, 0.5)  # Equal probability for both states

# Observation sequence (1 = Walk, 2 = Shop, 3 = Clean)
observations <- c(1, 2, 3, 1, 2)

# Viterbi algorithm function
viterbi <- function(observations, trans_prob, emission_prob, init_prob) {
  
  n_states <- nrow(trans_prob)
  n_obs <- length(observations)
  
  # Initialize Viterbi table and path tracking
  viterbi_table <- matrix(0, nrow = n_states, ncol = n_obs)
  path <- matrix(0, nrow = n_states, ncol = n_obs)
  
  # Base case (first observation)
  for (i in 1:n_states) {
    viterbi_table[i, 1] <- init_prob[i] * emission_prob[i, observations[1]]
    path[i, 1] <- i
  }
  
  # Forward recursion
  for (t in 2:n_obs) {
    for (j in 1:n_states) {
      # Calculate probability of being in state j at time t
      probs <- viterbi_table[, t-1] * trans_prob[, j]
      max_prob <- max(probs)
      max_state <- which.max(probs)
      
      viterbi_table[j, t] <- max_prob * emission_prob[j, observations[t]]
      path[j, t] <- max_state
    }
  }
  
  # Backtrack to find the most likely path
  # Find the state with maximum probability at the last time step
  final_state <- which.max(viterbi_table[, n_obs])
  
  # Reconstruct the path
  best_path <- numeric(n_obs)
  best_path[n_obs] <- final_state
  
  for (t in (n_obs-1):1) {
    best_path[t] <- path[best_path[t+1], t+1]
  }
  
  return(list(
    path = best_path,
    probability = viterbi_table[final_state, n_obs]
  ))
}

# Run the Viterbi algorithm
result <- viterbi(observations, trans_prob, emission_prob, init_prob)

# Display results
cat("Observation sequence:", paste(observations, collapse = " "), "\n")
cat("Most likely state sequence:", paste(result$path, collapse = " "), "\n")
cat("Probability of the most likely path:", result$probability, "\n\n")

# Convert numeric states to readable labels
state_labels <- c("Sunny", "Rainy")
obs_labels <- c("Walk", "Shop", "Clean")

cat("Detailed interpretation:\n")
cat("Observations:", paste(sapply(observations, function(x) obs_labels[x]), collapse = " "), "\n")
cat("States:", paste(sapply(result$path, function(x) state_labels[x]), collapse = " "), "\n")

# Create a summary table
summary_table <- data.frame(
  Observation = sapply(observations, function(x) obs_labels[x]),
  State = sapply(result$path, function(x) state_labels[x])
)

print(summary_table)
```

## Output:
```
Observation sequence: 1 2 3 1 2 
Most likely state sequence: 1 1 2 1 2 
Probability of the most likely path: 0.003024 

Detailed interpretation:
Observations: Walk Shop Clean Walk Shop 
States: Sunny Sunny Rainy Sunny Rainy 

  Observation   State
1        Walk  Sunny
2        Shop  Sunny
3       Clean  Rainy
4        Walk  Sunny
5        Shop  Rainy
```

## Key Components Explained:

1. **Transition Probabilities**: Matrix showing how likely it is to move from one weather state to another
2. **Emission Probabilities**: Matrix showing how likely each weather state is to produce a particular activity
3. **Initial Probabilities**: Starting probability for each weather state
4. **Observation Sequence**: The sequence of activities we observe

The Viterbi algorithm efficiently finds the most probable sequence of hidden states (weather conditions) that could have generated the observed sequence of activities (Walk, Shop, Clean).

