# Q-Learning Algorithm in R

Here's a complete example of implementing Q-learning in R to solve a simple grid world problem:

```r
# Q-Learning Implementation in R
# Simple Grid World Example

# Load required libraries
library(dplyr)

# Set seed for reproducibility
set.seed(123)

# Define grid world parameters
grid_size <- 4
num_states <- grid_size * grid_size
num_actions <- 4  # Up, Down, Left, Right

# Define action mappings
actions <- list(
  up = c(-1, 0),
  down = c(1, 0),
  left = c(0, -1),
  right = c(0, 1)
)

# Create reward matrix (simple example)
# -1 for normal states, 0 for goal, -10 for obstacles
rewards <- matrix(-1, nrow = grid_size, ncol = grid_size)
rewards[1, 1] <- 0  # Goal state
rewards[2, 2] <- -10  # Obstacle
rewards[3, 3] <- -10  # Obstacle

# Q-table initialization
Q <- matrix(0, nrow = num_states, ncol = num_actions)

# Hyperparameters
alpha <- 0.1    # Learning rate
gamma <- 0.9    # Discount factor
epsilon <- 0.1  # Exploration rate

# Convert state coordinates to state number
state_to_number <- function(row, col) {
  return((row - 1) * grid_size + col)
}

# Convert state number to coordinates
number_to_state <- function(state_num) {
  row <- ceiling(state_num / grid_size)
  col <- state_num - (row - 1) * grid_size
  return(c(row, col))
}

# Check if state is valid (within bounds and not obstacle)
is_valid_state <- function(row, col) {
  if (row < 1 || row > grid_size || col < 1 || col > grid_size) {
    return(FALSE)
  }
  if (rewards[row, col] == -10) {  # Obstacle
    return(FALSE)
  }
  return(TRUE)
}

# Get next state given current state and action
get_next_state <- function(current_state, action) {
  current_coords <- number_to_state(current_state)
  action_vec <- actions[[action]]
  
  new_row <- current_coords[1] + action_vec[1]
  new_col <- current_coords[2] + action_vec[2]
  
  # If invalid, stay in same state
  if (!is_valid_state(new_row, new_col)) {
    return(current_state)
  }
  
  return(state_to_number(new_row, new_col))
}

# Choose action using epsilon-greedy policy
choose_action <- function(state, Q, epsilon) {
  if (runif(1) < epsilon) {
    # Explore: choose random action
    return(sample(1:num_actions, 1))
  } else {
    # Exploit: choose best action
    q_values <- Q[state, ]
    best_action <- which.max(q_values)
    return(best_action)
  }
}

# Q-learning algorithm
q_learning <- function(num_episodes = 1000, max_steps = 100) {
  # Initialize Q-table
  Q <- matrix(0, nrow = num_states, ncol = num_actions)
  
  # Store episode rewards
  episode_rewards <- numeric(num_episodes)
  
  for (episode in 1:num_episodes) {
    # Start from random state (avoiding obstacles)
    current_state <- sample(1:num_states, 1)
    while (rewards[number_to_state(current_state)[1], number_to_state(current_state)[2]] == -10) {
      current_state <- sample(1:num_states, 1)
    }
    
    total_reward <- 0
    step <- 0
    
    # Run episode
    while (step < max_steps) {
      # Choose action
      action <- choose_action(current_state, Q, epsilon)
      
      # Get next state and reward
      next_state <- get_next_state(current_state, names(actions)[action])
      reward <- rewards[number_to_state(next_state)[1], number_to_state(next_state)[2]]
      
      # Update Q-value
      old_q <- Q[current_state, action]
      max_next_q <- max(Q[next_state, ])
      new_q <- old_q + alpha * (reward + gamma * max_next_q - old_q)
      Q[current_state, action] <- new_q
      
      total_reward <- total_reward + reward
      current_state <- next_state
      
      # Check if goal reached
      if (reward == 0) {
        break
      }
      
      step <- step + 1
    }
    
    episode_rewards[episode] <- total_reward
  }
  
  return(list(Q = Q, rewards = episode_rewards))
}

# Run Q-learning
results <- q_learning(num_episodes = 1000)

# Extract Q-table and rewards
Q_table <- results$Q
episode_rewards <- results$rewards

# Display Q-table for first few states
cat("Q-Table (first 10 states):\n")
print(round(Q_table[1:10, ], 2))

# Plot learning curve
plot(episode_rewards, type = "l", 
     main = "Q-Learning Learning Curve", 
     xlab = "Episode", 
     ylab = "Total Reward",
     col = "blue", 
     lwd = 1)

# Find optimal policy
find_optimal_policy <- function(Q, grid_size) {
  policy <- matrix(0, nrow = grid_size, ncol = grid_size)
  
  for (row in 1:grid_size) {
    for (col in 1:grid_size) {
      if (rewards[row, col] != -10) {  # Not obstacle
        state <- state_to_number(row, col)
        if (state <= nrow(Q)) {
          best_action <- which.max(Q[state, ])
          policy[row, col] <- best_action
        }
      }
    }
  }
  
  return(policy)
}

# Display optimal policy
optimal_policy <- find_optimal_policy(Q_table, grid_size)
cat("\nOptimal Policy (1=Up, 2=Down, 3=Left, 4=Right):\n")
print(optimal_policy)

# Function to visualize the grid world
visualize_grid <- function(rewards, policy) {
  grid <- matrix(0, nrow = grid_size, ncol = grid_size)
  
  for (row in 1:grid_size) {
    for (col in 1:grid_size) {
      if (rewards[row, col] == 0) {
        grid[row, col] <- "G"  # Goal
      } else if (rewards[row, col] == -10) {
        grid[row, col] <- "X"  # Obstacle
      } else {
        grid[row, col] <- " "  # Normal
      }
    }
  }
  
  # Add policy arrows
  policy_symbols <- c("↑", "↓", "←", "→")
  
  cat("\nGrid World with Optimal Policy:\n")
  for (row in 1:grid_size) {
    for (col in 1:grid_size) {
      if (grid[row, col] == "X") {
        cat("X ")
      } else if (grid[row, col] == "G") {
        cat("G ")
      } else {
        action <- policy[row, col]
        if (action > 0) {
          cat(policy_symbols[action], " ")
        } else {
          cat(". ")
        }
      }
    }
    cat("\n")
  }
}

# Visualize the result
visualize_grid(rewards, optimal_policy)

# Summary statistics
cat("\nLearning Summary:\n")
cat("Final Q-Table dimensions:", dim(Q_table), "\n")
cat("Average reward per episode:", round(mean(episode_rewards), 2), "\n")
cat("Best episode reward:", max(episode_rewards), "\n")
```

## Key Components Explained:

### 1. **Environment Setup**
- 4x4 grid world with obstacles and goal state
- 4 possible actions (up, down, left, right)
- Reward structure: -1 for normal states, 0 for goal, -10 for obstacles

### 2. **Q-Learning Algorithm**
- **Q-table**: Stores state-action values
- **Epsilon-greedy**: Balances exploration vs exploitation
- **Update rule**: Q(s,a) = Q(s,a) + α[r + γmaxQ(s',a') - Q(s,a)]

### 3. **Key Parameters**
- `alpha`: Learning rate (0.1)
- `gamma`: Discount factor (0.9)
- `epsilon`: Exploration rate (0.1)

### 4. **Output**
- Q-table showing learned values
- Learning curve showing reward improvement
- Optimal policy visualization

This implementation demonstrates the core principles of Q-learning in a simple but complete example that can be easily modified for more complex environments.

