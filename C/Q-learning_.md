# Q-Learning Algorithm in C

Here's a complete implementation of a Q-learning algorithm in C for a simple grid world environment:

```c
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>

#define GRID_SIZE 4
#define ACTIONS 4
#define STATES (GRID_SIZE * GRID_SIZE)
#define EPSILON 0.1
#define ALPHA 0.1
#define GAMMA 0.9
#define EPISODES 1000

// Action definitions: 0=up, 1=right, 2=down, 3=left
enum Action {
    UP = 0,
    RIGHT = 1,
    DOWN = 2,
    LEFT = 3
};

// Structure to represent the Q-table
typedef struct {
    double q_table[STATES][ACTIONS];
} QTable;

// Initialize Q-table with zeros
void initialize_q_table(QTable* q_table) {
    for (int i = 0; i < STATES; i++) {
        for (int j = 0; j < ACTIONS; j++) {
            q_table->q_table[i][j] = 0.0;
        }
    }
}

// Convert 2D coordinates to 1D state
int get_state(int row, int col) {
    return row * GRID_SIZE + col;
}

// Convert 1D state to 2D coordinates
void get_coordinates(int state, int* row, int* col) {
    *row = state / GRID_SIZE;
    *col = state % GRID_SIZE;
}

// Get valid actions for a given state (avoiding walls)
int get_valid_actions(int state, int* valid_actions) {
    int row, col;
    get_coordinates(state, &row, &col);
    
    int count = 0;
    
    // Check up (0)
    if (row > 0) valid_actions[count++] = UP;
    
    // Check right (1)
    if (col < GRID_SIZE - 1) valid_actions[count++] = RIGHT;
    
    // Check down (2)
    if (row < GRID_SIZE - 1) valid_actions[count++] = DOWN;
    
    // Check left (3)
    if (col > 0) valid_actions[count++] = LEFT;
    
    return count;
}

// Choose action using epsilon-greedy policy
int choose_action(QTable* q_table, int state, double epsilon) {
    // Random action
    if (rand() < epsilon * RAND_MAX) {
        int valid_actions[ACTIONS];
        int count = get_valid_actions(state, valid_actions);
        return valid_actions[rand() % count];
    }
    
    // Greedy action
    double max_q = -INFINITY;
    int best_action = 0;
    
    for (int action = 0; action < ACTIONS; action++) {
        if (q_table->q_table[state][action] > max_q) {
            max_q = q_table->q_table[state][action];
            best_action = action;
        }
    }
    
    return best_action;
}

// Get next state and reward for given action
void get_next_state_reward(int state, int action, int* next_state, int* reward) {
    int row, col;
    get_coordinates(state, &row, &col);
    
    // Move according to action
    switch (action) {
        case UP:
            row--;
            break;
        case RIGHT:
            col++;
            break;
        case DOWN:
            row++;
            break;
        case LEFT:
            col--;
            break;
    }
    
    // Check if moved outside grid (bounce back)
    if (row < 0) row = 0;
    if (row >= GRID_SIZE) row = GRID_SIZE - 1;
    if (col < 0) col = 0;
    if (col >= GRID_SIZE) col = GRID_SIZE - 1;
    
    *next_state = get_state(row, col);
    
    // Reward: -1 for each step, +10 for reaching goal (state 15)
    if (*next_state == STATES - 1) {
        *reward = 10;
    } else {
        *reward = -1;
    }
}

// Q-learning update rule
void update_q_value(QTable* q_table, int state, int action, int reward, int next_state) {
    double current_q = q_table->q_table[state][action];
    double max_next_q = -INFINITY;
    
    // Find maximum Q-value for next state
    for (int a = 0; a < ACTIONS; a++) {
        if (q_table->q_table[next_state][a] > max_next_q) {
            max_next_q = q_table->q_table[next_state][a];
        }
    }
    
    // Q-learning update formula
    double new_q = current_q + ALPHA * (reward + GAMMA * max_next_q - current_q);
    q_table->q_table[state][action] = new_q;
}

// Print the Q-table
void print_q_table(QTable* q_table) {
    printf("\nQ-Table:\n");
    for (int i = 0; i < STATES; i++) {
        printf("State %2d: ", i);
        for (int j = 0; j < ACTIONS; j++) {
            printf("A%d:%6.2f ", j, q_table->q_table[i][j]);
        }
        printf("\n");
    }
}

// Print the optimal policy
void print_policy(QTable* q_table) {
    printf("\nOptimal Policy:\n");
    for (int i = 0; i < STATES; i++) {
        int best_action = 0;
        double max_q = q_table->q_table[i][0];
        
        for (int j = 1; j < ACTIONS; j++) {
            if (q_table->q_table[i][j] > max_q) {
                max_q = q_table->q_table[i][j];
                best_action = j;
            }
        }
        
        char action_char;
        switch (best_action) {
            case UP: action_char = '^'; break;
            case RIGHT: action_char = '>'; break;
            case DOWN: action_char = 'v'; break;
            case LEFT: action_char = '<'; break;
        }
        printf("%c ", action_char);
        
        if ((i + 1) % GRID_SIZE == 0) printf("\n");
    }
}

// Run one episode
int run_episode(QTable* q_table) {
    int state = 0;  // Start at top-left corner
    int steps = 0;
    int max_steps = 100;
    
    while (state != STATES - 1 && steps < max_steps) {
        int action = choose_action(q_table, state, EPSILON);
        int next_state, reward;
        get_next_state_reward(state, action, &next_state, &reward);
        update_q_value(q_table, state, action, reward, next_state);
        state = next_state;
        steps++;
    }
    
    return steps;
}

int main() {
    // Initialize random seed
    srand(time(NULL));
    
    // Create Q-table
    QTable q_table;
    initialize_q_table(&q_table);
    
    printf("Starting Q-Learning for %d episodes...\n", EPISODES);
    
    // Main learning loop
    for (int episode = 0; episode < EPISODES; episode++) {
        int steps = run_episode(&q_table);
        
        // Print progress every 100 episodes
        if ((episode + 1) % 100 == 0) {
            printf("Episode %d completed in %d steps\n", episode + 1, steps);
        }
    }
    
    // Print final results
    print_q_table(&q_table);
    print_policy(&q_table);
    
    printf("\nTraining completed!\n");
    
    return 0;
}
```

## Key Components Explained

### 1. **Q-Table Structure**
- 2D array storing Q-values for each state-action pair
- Dimensions: `STATES × ACTIONS` (16 × 4 for 4×4 grid)

### 2. **Core Functions**
- `initialize_q_table()`: Sets all Q-values to zero
- `choose_action()`: Implements epsilon-greedy policy
- `get_next_state_reward()`: Handles state transitions and rewards
- `update_q_value()`: Implements the Q-learning update rule

### 3. **Q-Learning Formula**
```
Q(s,a) ← Q(s,a) + α[r + γ max_a' Q(s',a') - Q(s,a)]
```

### 4. **Environment Setup**
- 4×4 grid world with start (0,0) and goal (3,3)
- Reward: -1 per step, +10 for reaching goal
- Actions: Up, Right, Down, Left (with boundary constraints)

### 5. **Training Process**
- 1000 episodes of learning
- Epsilon-greedy exploration (10% random actions)
- Learning rate α = 0.1
- Discount factor γ = 0.9

This implementation demonstrates the fundamental principles of Q-learning in a simple grid world environment, showing how an agent learns optimal behavior through trial and error.

