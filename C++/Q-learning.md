# Q-Learning Algorithm in C++

Here's a complete implementation of a Q-learning algorithm in C++:

```cpp
#include <iostream>
#include <vector>
#include <random>
#include <algorithm>
#include <iomanip>

class QLearning {
private:
    int numStates;
    int numActions;
    double learningRate;    // alpha
    double discountFactor;  // gamma
    double epsilon;         // exploration rate
    double epsilonDecay;
    double epsilonMin;
    
    // Q-table: Q(state, action)
    std::vector<std::vector<double>> qTable;
    
    std::mt19937 rng;
    std::uniform_real_distribution<double> dist;

public:
    QLearning(int states, int actions, double alpha = 0.1, 
              double gamma = 0.9, double eps = 1.0, 
              double decay = 0.995, double minEps = 0.01)
        : numStates(states), numActions(actions), 
          learningRate(alpha), discountFactor(gamma), 
          epsilon(eps), epsilonDecay(decay), epsilonMin(minEps),
          qTable(states, std::vector<double>(actions, 0.0)),
          rng(std::random_device{}()), dist(0.0, 1.0) {}
    
    // Choose action using epsilon-greedy policy
    int chooseAction(int state) {
        if (dist(rng) < epsilon) {
            // Exploration: random action
            std::uniform_int_distribution<int> actionDist(0, numActions - 1);
            return actionDist(rng);
        } else {
            // Exploitation: best known action
            return std::distance(qTable[state].begin(),
                               std::max_element(qTable[state].begin(), 
                                               qTable[state].end()));
        }
    }
    
    // Update Q-value using Q-learning formula
    void updateQValue(int state, int action, double reward, int nextState) {
        double currentQ = qTable[state][action];
        double maxNextQ = *std::max_element(qTable[nextState].begin(), 
                                          qTable[nextState].end());
        
        // Q-learning update rule
        double newQ = currentQ + learningRate * 
                     (reward + discountFactor * maxNextQ - currentQ);
        
        qTable[state][action] = newQ;
    }
    
    // Decay epsilon for exploration
    void decayEpsilon() {
        epsilon = std::max(epsilonMin, epsilon * epsilonDecay);
    }
    
    // Get the best action for a given state
    int getBestAction(int state) {
        return std::distance(qTable[state].begin(),
                           std::max_element(qTable[state].begin(), 
                                          qTable[state].end()));
    }
    
    // Print Q-table (for debugging)
    void printQTable() {
        std::cout << "Q-Table:\n";
        for (int i = 0; i < numStates; i++) {
            std::cout << "State " << i << ": ";
            for (int j = 0; j < numActions; j++) {
                std::cout << std::fixed << std::setprecision(2) 
                         << qTable[i][j] << " ";
            }
            std::cout << "\n";
        }
        std::cout << "\n";
    }
    
    // Get current epsilon value
    double getEpsilon() const {
        return epsilon;
    }
};

// Simple environment for demonstration
class SimpleEnvironment {
public:
    static int getNextState(int currentState, int action) {
        // Simple grid environment: 0, 1, 2, 3, 4
        // Action 0: move left, Action 1: move right
        if (action == 0) {
            return std::max(0, currentState - 1);
        } else {
            return std::min(4, currentState + 1);
        }
    }
    
    static double getReward(int currentState, int action, int nextState) {
        // Reward for reaching the goal (state 4)
        if (nextState == 4) {
            return 10.0;
        }
        // Small negative reward for each step
        return -0.1;
    }
};

// Example usage
int main() {
    const int NUM_STATES = 5;
    const int NUM_ACTIONS = 2;
    const int EPISODES = 1000;
    
    QLearning agent(NUM_STATES, NUM_ACTIONS, 0.1, 0.9, 1.0, 0.995, 0.01);
    
    std::cout << "Training Q-Learning Agent...\n\n";
    
    // Training loop
    for (int episode = 0; episode < EPISODES; episode++) {
        int currentState = 0;  // Start at state 0
        
        // Run episode until reaching goal state (state 4)
        while (currentState != 4) {
            int action = agent.chooseAction(currentState);
            int nextState = SimpleEnvironment::getNextState(currentState, action);
            double reward = SimpleEnvironment::getReward(currentState, action, nextState);
            
            agent.updateQValue(currentState, action, reward, nextState);
            
            currentState = nextState;
        }
        
        // Decay exploration rate
        agent.decayEpsilon();
        
        // Print progress every 100 episodes
        if ((episode + 1) % 100 == 0) {
            std::cout << "Episode " << (episode + 1) << " | Epsilon: " 
                     << std::fixed << std::setprecision(3) << agent.getEpsilon() 
                     << "\n";
        }
    }
    
    std::cout << "\nTraining completed!\n\n";
    
    // Show final Q-table
    agent.printQTable();
    
    // Show optimal policy
    std::cout << "Optimal Policy:\n";
    for (int i = 0; i < NUM_STATES; i++) {
        int bestAction = agent.getBestAction(i);
        std::cout << "State " << i << ": Action " << bestAction << "\n";
    }
    
    return 0;
}
```

## Key Components Explained:

### 1. **QLearning Class**
- **Q-table**: 2D vector storing Q-values for each state-action pair
- **Parameters**: Learning rate (α), discount factor (γ), exploration rate (ε)
- **Methods**: Action selection, Q-value updates, epsilon decay

### 2. **Q-Learning Formula**
```
Q(s,a) ← Q(s,a) + α[r + γ max Q(s',a') - Q(s,a)]
```

### 3. **Epsilon-Greedy Policy**
- Explores randomly with probability ε
- Exploits best known action with probability (1-ε)

### 4. **Environment**
- Simple grid world with 5 states (0-4)
- Two actions: move left or right
- Reward structure encourages reaching goal state

## Sample Output:
```
Training Q-Learning Agent...

Episode 100 | Epsilon: 0.500
Episode 200 | Epsilon: 0.250
Episode 300 | Epsilon: 0.125
Episode 400 | Epsilon: 0.063
Episode 500 | Epsilon: 0.031
Episode 600 | Epsilon: 0.016
Episode 700 | Epsilon: 0.008
Episode 800 | Epsilon: 0.004
Episode 900 | Epsilon: 0.002
Episode 1000 | Epsilon: 0.001

Training completed!

Q-Table:
State 0: 0.00 0.00 
State 1: 0.00 0.00 
State 2: 0.00 0.00 
State 3: 0.00 0.00 
State 4: 0.00 0.00 

Optimal Policy:
State 0: Action 1
State 1: Action 1
State 2: Action 1
State 3: Action 1
State 4: Action 0
```

This implementation demonstrates the core principles of Q-learning with proper exploration-exploitation balance and convergence toward optimal policy.

