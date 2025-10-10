# Q-Learning Algorithm in JavaScript

Here's a complete implementation of a Q-learning algorithm in JavaScript:

```javascript
class QLearningAgent {
    constructor(learningRate = 0.1, discountFactor = 0.9, epsilon = 0.1) {
        this.learningRate = learningRate;
        this.discountFactor = discountFactor;
        this.epsilon = epsilon;
        this.qTable = new Map();
    }

    // Get Q-value for state-action pair
    getQValue(state, action) {
        const stateKey = JSON.stringify(state);
        if (!this.qTable.has(stateKey)) {
            this.qTable.set(stateKey, new Map());
        }
        
        const actions = this.qTable.get(stateKey);
        if (!actions.has(action)) {
            actions.set(action, 0);
        }
        
        return actions.get(action);
    }

    // Set Q-value for state-action pair
    setQValue(state, action, value) {
        const stateKey = JSON.stringify(state);
        if (!this.qTable.has(stateKey)) {
            this.qTable.set(stateKey, new Map());
        }
        
        this.qTable.get(stateKey).set(action, value);
    }

    // Choose action using epsilon-greedy policy
    chooseAction(state, availableActions) {
        // Exploration: choose random action
        if (Math.random() < this.epsilon) {
            return availableActions[Math.floor(Math.random() * availableActions.length)];
        }
        
        // Exploitation: choose best action
        let bestAction = availableActions[0];
        let bestQValue = this.getQValue(state, bestAction);
        
        for (let i = 1; i < availableActions.length; i++) {
            const action = availableActions[i];
            const qValue = this.getQValue(state, action);
            if (qValue > bestQValue) {
                bestQValue = qValue;
                bestAction = action;
            }
        }
        
        return bestAction;
    }

    // Update Q-value using Q-learning formula
    updateQValue(state, action, reward, nextState, availableActions) {
        const currentQ = this.getQValue(state, action);
        
        // Find maximum Q-value for next state
        let maxNextQ = -Infinity;
        for (const nextAction of availableActions) {
            const qValue = this.getQValue(nextState, nextAction);
            if (qValue > maxNextQ) {
                maxNextQ = qValue;
            }
        }
        
        // Q-learning update formula
        const newQ = currentQ + this.learningRate * 
            (reward + this.discountFactor * maxNextQ - currentQ);
        
        this.setQValue(state, action, newQ);
    }

    // Get all Q-values for debugging
    getQTable() {
        const result = {};
        for (const [stateKey, actions] of this.qTable.entries()) {
            result[stateKey] = {};
            for (const [action, value] of actions.entries()) {
                result[stateKey][action] = value;
            }
        }
        return result;
    }
}

// Example usage with a simple grid world
class GridWorld {
    constructor(rows, cols) {
        this.rows = rows;
        this.cols = cols;
        this.agent = new QLearningAgent(0.1, 0.9, 0.1);
        this.state = [0, 0]; // Start position
        this.goal = [rows - 1, cols - 1];
        this.obstacles = [[1, 1], [2, 2]];
    }

    // Get available actions from current state
    getAvailableActions() {
        const [row, col] = this.state;
        const actions = [];
        
        if (row > 0) actions.push('up');
        if (row < this.rows - 1) actions.push('down');
        if (col > 0) actions.push('left');
        if (col < this.cols - 1) actions.push('right');
        
        return actions;
    }

    // Move agent and return reward
    move(action) {
        const [row, col] = this.state;
        let newRow = row;
        let newCol = col;
        
        switch (action) {
            case 'up': newRow--; break;
            case 'down': newRow++; break;
            case 'left': newCol--; break;
            case 'right': newCol++; break;
        }
        
        // Check if move is valid (not out of bounds or obstacle)
        if (newRow >= 0 && newRow < this.rows && 
            newCol >= 0 && newCol < this.cols &&
            !this.isObstacle(newRow, newCol)) {
            this.state = [newRow, newCol];
        }
        
        return this.getReward();
    }

    // Check if position is obstacle
    isObstacle(row, col) {
        return this.obstacles.some(obs => obs[0] === row && obs[1] === col);
    }

    // Get reward for current state
    getReward() {
        if (this.state[0] === this.goal[0] && this.state[1] === this.goal[1]) {
            return 10; // Goal reward
        } else if (this.isObstacle(this.state[0], this.state[1])) {
            return -10; // Obstacle penalty
        } else {
            return -1; // Step penalty
        }
    }

    // Check if episode is done
    isDone() {
        return this.state[0] === this.goal[0] && this.state[1] === this.goal[1];
    }

    // Reset to start position
    reset() {
        this.state = [0, 0];
    }
}

// Training function
function trainAgent(episodes = 1000) {
    const gridWorld = new GridWorld(4, 4);
    const agent = gridWorld.agent;
    
    console.log("Training Q-Learning Agent...");
    
    for (let episode = 0; episode < episodes; episode++) {
        gridWorld.reset();
        let totalReward = 0;
        let steps = 0;
        
        while (!gridWorld.isDone() && steps < 100) {
            const availableActions = gridWorld.getAvailableActions();
            const action = agent.chooseAction(gridWorld.state, availableActions);
            const reward = gridWorld.move(action);
            const nextState = gridWorld.state;
            
            totalReward += reward;
            steps++;
            
            // Update Q-value
            agent.updateQValue(
                gridWorld.state, 
                action, 
                reward, 
                nextState, 
                availableActions
            );
        }
        
        if (episode % 100 === 0) {
            console.log(`Episode ${episode}: Total Reward = ${totalReward}, Steps = ${steps}`);
        }
    }
    
    console.log("Training completed!");
    return agent;
}

// Example of how to use the Q-learning agent
console.log("=== Q-Learning Example ===");

// Train the agent
const trainedAgent = trainAgent(1000);

// Test the trained agent
console.log("\n=== Testing Trained Agent ===");
const testWorld = new GridWorld(4, 4);
let totalTestReward = 0;
let testSteps = 0;

testWorld.reset();
console.log(`Starting position: [${testWorld.state[0]}, ${testWorld.state[1]}]`);

while (!testWorld.isDone() && testSteps < 100) {
    const availableActions = testWorld.getAvailableActions();
    const action = testWorld.agent.chooseAction(testWorld.state, availableActions);
    const reward = testWorld.move(action);
    
    console.log(`Step ${testSteps + 1}: Moved ${action} to [${testWorld.state[0]}, ${testWorld.state[1]}], Reward: ${reward}`);
    
    totalTestReward += reward;
    testSteps++;
    
    if (testWorld.isDone()) {
        console.log("Goal reached!");
        break;
    }
}

console.log(`Test completed! Total reward: ${totalTestReward}, Steps: ${testSteps}`);
```

## Key Components Explained

### 1. **QLearningAgent Class**
- **Q-Table**: Stores state-action values in a Map structure
- **getQValue()**: Retrieves Q-value for a state-action pair
- **setQValue()**: Sets Q-value for a state-action pair
- **chooseAction()**: Implements epsilon-greedy policy
- **updateQValue()**: Updates Q-values using the Q-learning formula

### 2. **Q-Learning Formula**
```
Q(s,a) = Q(s,a) + α[r + γ * max(Q(s',a')) - Q(s,a)]
```

Where:
- α = learning rate
- γ = discount factor
- r = reward
- s = current state
- a = action
- s' = next state

### 3. **GridWorld Environment**
- Simple 4x4 grid with start and goal positions
- Obstacles that penalize the agent
- Rewards: +10 for goal, -10 for obstacles, -1 for each step

### 4. **Training Process**
- Runs multiple episodes of learning
- Agent explores and exploits based on epsilon-greedy policy
- Updates Q-values after each action

This implementation demonstrates how Q-learning can learn optimal policies through trial and error in a simple environment.

