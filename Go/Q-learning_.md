# Q-Learning Algorithm in Go

Here's a complete implementation of a Q-learning algorithm in Go:

```go
package main

import (
    "fmt"
    "math"
    "math/rand"
    "time"
)

// QLearning represents a Q-learning agent
type QLearning struct {
    learningRate  float64
    discountRate  float64
    epsilon       float64
    qTable        map[string]map[int]float64
    actions       []int
}

// NewQLearning creates a new Q-learning agent
func NewQLearning(learningRate, discountRate, epsilon float64, actions []int) *QLearning {
    return &QLearning{
        learningRate:  learningRate,
        discountRate:  discountRate,
        epsilon:       epsilon,
        qTable:        make(map[string]map[int]float64),
        actions:       actions,
    }
}

// GetStateKey creates a unique key for a state
func (ql *QLearning) GetStateKey(state []int) string {
    return fmt.Sprintf("%v", state)
}

// GetQValue returns the Q-value for a state-action pair
func (ql *QLearning) GetQValue(state []int, action int) float64 {
    stateKey := ql.GetStateKey(state)
    if ql.qTable[stateKey] == nil {
        ql.qTable[stateKey] = make(map[int]float64)
        // Initialize with zeros for all actions
        for _, a := range ql.actions {
            ql.qTable[stateKey][a] = 0.0
        }
    }
    return ql.qTable[stateKey][action]
}

// SetQValue sets the Q-value for a state-action pair
func (ql *QLearning) SetQValue(state []int, action int, value float64) {
    stateKey := ql.GetStateKey(state)
    if ql.qTable[stateKey] == nil {
        ql.qTable[stateKey] = make(map[int]float64)
    }
    ql.qTable[stateKey][action] = value
}

// ChooseAction selects an action using epsilon-greedy policy
func (ql *QLearning) ChooseAction(state []int) int {
    // Exploration: choose random action
    if rand.Float64() < ql.epsilon {
        return ql.actions[rand.Intn(len(ql.actions))]
    }
    
    // Exploitation: choose best action
    stateKey := ql.GetStateKey(state)
    if ql.qTable[stateKey] == nil {
        return ql.actions[rand.Intn(len(ql.actions))]
    }
    
    bestAction := ql.actions[0]
    bestValue := ql.qTable[stateKey][bestAction]
    
    for _, action := range ql.actions {
        if ql.qTable[stateKey][action] > bestValue {
            bestValue = ql.qTable[stateKey][action]
            bestAction = action
        }
    }
    
    return bestAction
}

// UpdateQValue updates the Q-value using the Q-learning update rule
func (ql *QLearning) UpdateQValue(state []int, action int, reward float64, nextState []int) {
    currentQ := ql.GetQValue(state, action)
    
    // Find maximum Q-value for next state
    maxNextQ := math.Inf(-1)
    stateKey := ql.GetStateKey(nextState)
    
    if ql.qTable[stateKey] != nil {
        for _, action := range ql.actions {
            if ql.qTable[stateKey][action] > maxNextQ {
                maxNextQ = ql.qTable[stateKey][action]
            }
        }
    }
    
    // Q-learning update rule
    newQ := currentQ + ql.learningRate*(reward+ql.discountRate*maxNextQ-currentQ)
    ql.SetQValue(state, action, newQ)
}

// Simple environment for demonstration
type SimpleEnvironment struct {
    state []int
    goal  []int
}

func NewSimpleEnvironment() *SimpleEnvironment {
    return &SimpleEnvironment{
        state: []int{0, 0},
        goal:  []int{2, 2},
    }
}

func (env *SimpleEnvironment) GetState() []int {
    return env.state
}

func (env *SimpleEnvironment) GetPossibleActions() []int {
    return []int{0, 1, 2, 3} // Up, Down, Left, Right
}

func (env *SimpleEnvironment) TakeAction(action int) (reward float64, done bool) {
    // Simple grid world environment
    switch action {
    case 0: // Up
        env.state[0] = max(0, env.state[0]-1)
    case 1: // Down
        env.state[0] = min(2, env.state[0]+1)
    case 2: // Left
        env.state[1] = max(0, env.state[1]-1)
    case 3: // Right
        env.state[1] = min(2, env.state[1]+1)
    }
    
    // Reward calculation
    if env.state[0] == env.goal[0] && env.state[1] == env.goal[1] {
        return 10.0, true // Goal reached
    }
    
    return -1.0, false // Small penalty for each step
}

func (env *SimpleEnvironment) Reset() {
    env.state = []int{0, 0}
}

func max(a, b int) int {
    if a > b {
        return a
    }
    return b
}

func min(a, b int) int {
    if a < b {
        return a
    }
    return b
}

func main() {
    rand.Seed(time.Now().UnixNano())
    
    // Create environment
    env := NewSimpleEnvironment()
    
    // Create Q-learning agent
    actions := []int{0, 1, 2, 3} // Up, Down, Left, Right
    ql := NewQLearning(0.1, 0.9, 0.1, actions)
    
    // Training parameters
    episodes := 1000
    totalRewards := 0.0
    
    fmt.Println("Training Q-Learning Agent...")
    
    // Training loop
    for episode := 0; episode < episodes; episode++ {
        env.Reset()
        state := env.GetState()
        done := false
        episodeReward := 0.0
        
        // Run episode until done
        for !done {
            action := ql.ChooseAction(state)
            reward, done := env.TakeAction(action)
            episodeReward += reward
            
            nextState := env.GetState()
            ql.UpdateQValue(state, action, reward, nextState)
            state = nextState
        }
        
        totalRewards += episodeReward
        
        // Print progress
        if episode%100 == 0 {
            fmt.Printf("Episode %d: Total Reward = %.2f\n", episode, episodeReward)
        }
    }
    
    fmt.Printf("\nTraining completed!\n")
    fmt.Printf("Average reward per episode: %.2f\n", totalRewards/float64(episodes))
    
    // Test the trained agent
    fmt.Println("\nTesting trained agent:")
    env.Reset()
    state := env.GetState()
    done := false
    steps := 0
    
    for !done && steps < 20 {
        action := ql.ChooseAction(state)
        reward, done := env.TakeAction(action)
        fmt.Printf("Step %d: State %v, Action %d, Reward %.2f\n", 
                   steps, state, action, reward)
        state = env.GetState()
        steps++
    }
    
    fmt.Printf("Final state: %v\n", state)
    
    // Print some Q-values for demonstration
    fmt.Println("\nSample Q-values:")
    sampleStates := [][]int{{0, 0}, {1, 1}, {2, 2}}
    for _, s := range sampleStates {
        stateKey := ql.GetStateKey(s)
        if ql.qTable[stateKey] != nil {
            fmt.Printf("State %v: ", s)
            for _, action := range actions {
                fmt.Printf("Action %d=%.2f ", action, ql.qTable[stateKey][action])
            }
            fmt.Println()
        }
    }
}
```

## Key Components Explained

### 1. **QLearning Structure**
- `learningRate`: How quickly to update Q-values
- `discountRate`: Importance of future rewards
- `epsilon`: Exploration rate for epsilon-greedy policy
- `qTable`: Map storing Q-values for state-action pairs
- `actions`: Available actions in the environment

### 2. **Core Methods**
- `ChooseAction()`: Implements epsilon-greedy policy
- `UpdateQValue()`: Applies Q-learning update rule
- `GetQValue()` and `SetQValue()`: Manage Q-table entries

### 3. **Q-Learning Update Rule**
```
Q(s,a) ← Q(s,a) + α[r + γ max Q(s',a') - Q(s,a)]
```

### 4. **Environment Example**
The example uses a simple 3x3 grid world where the agent learns to reach a goal state from a starting position.

## Usage Notes

- Adjust `learningRate`, `discountRate`, and `epsilon` for different learning behaviors
- The agent starts with random exploration and gradually exploits learned knowledge
- Q-values are stored in a map structure for efficient lookup
- The algorithm can be extended to work with more complex environments

This implementation demonstrates the fundamental principles of Q-learning in a practical Go application.

