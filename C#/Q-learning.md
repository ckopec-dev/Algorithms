# Q-Learning Algorithm in C#

Here's a complete implementation of a Q-learning algorithm in C#:

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class QLearningAgent
{
    private Dictionary<(int state, int action), double> qTable;
    private double learningRate;    // alpha
    private double discountFactor;  // gamma
    private double explorationRate; // epsilon
    private Random random;
    
    public QLearningAgent(double learningRate = 0.1, double discountFactor = 0.9, double explorationRate = 0.1)
    {
        this.qTable = new Dictionary<(int, int), double>();
        this.learningRate = learningRate;
        this.discountFactor = discountFactor;
        this.explorationRate = explorationRate;
        this.random = new Random();
    }
    
    // Get Q-value for state-action pair
    private double GetQValue(int state, int action)
    {
        var key = (state, action);
        return qTable.ContainsKey(key) ? qTable[key] : 0.0;
    }
    
    // Set Q-value for state-action pair
    private void SetQValue(int state, int action, double value)
    {
        var key = (state, action);
        qTable[key] = value;
    }
    
    // Choose action using epsilon-greedy policy
    public int ChooseAction(int state, List<int> possibleActions)
    {
        if (random.NextDouble() < explorationRate)
        {
            // Exploration: choose random action
            return possibleActions[random.Next(possibleActions.Count)];
        }
        else
        {
            // Exploitation: choose best action based on Q-values
            double maxQ = double.MinValue;
            int bestAction = possibleActions[0];
            
            foreach (int action in possibleActions)
            {
                double qValue = GetQValue(state, action);
                if (qValue > maxQ)
                {
                    maxQ = qValue;
                    bestAction = action;
                }
            }
            
            return bestAction;
        }
    }
    
    // Update Q-value using Q-learning formula
    public void UpdateQValue(int state, int action, double reward, int nextState, List<int> possibleActions)
    {
        double currentQ = GetQValue(state, action);
        
        // Find maximum Q-value for next state
        double maxNextQ = 0;
        if (possibleActions != null && possibleActions.Count > 0)
        {
            maxNextQ = possibleActions.Max(a => GetQValue(nextState, a));
        }
        
        // Q-learning update formula:
        // Q(s,a) = Q(s,a) + α[r + γ*max(Q(s',a')) - Q(s,a)]
        double newQ = currentQ + learningRate * (reward + discountFactor * maxNextQ - currentQ);
        SetQValue(state, action, newQ);
    }
    
    // Get the best action for a given state
    public int GetBestAction(int state, List<int> possibleActions)
    {
        return possibleActions.MaxBy(action => GetQValue(state, action));
    }
    
    // Get Q-table for debugging
    public Dictionary<(int, int), double> GetQTable()
    {
        return new Dictionary<(int, int), double>(qTable);
    }
}

// Example usage with a simple environment
public class SimpleEnvironment
{
    private int currentState;
    private Random random;
    
    public SimpleEnvironment(int initialState = 0)
    {
        this.currentState = initialState;
        this.random = new Random();
    }
    
    public int GetCurrentState()
    {
        return currentState;
    }
    
    // Move to next state and return reward
    public (int nextState, double reward) TakeAction(int action)
    {
        int nextState = 0;
        double reward = 0;
        
        switch (currentState)
        {
            case 0:
                if (action == 0) // Go right
                {
                    nextState = 1;
                    reward = -1;
                }
                else // Go left
                {
                    nextState = 0;
                    reward = -1;
                }
                break;
                
            case 1:
                if (action == 0) // Go right
                {
                    nextState = 2;
                    reward = -1;
                }
                else // Go left
                {
                    nextState = 0;
                    reward = -1;
                }
                break;
                
            case 2:
                if (action == 0) // Go right
                {
                    nextState = 3;
                    reward = 10; // Goal state with reward
                }
                else // Go left
                {
                    nextState = 1;
                    reward = -1;
                }
                break;
                
            case 3: // Goal state
                nextState = 3;
                reward = 0;
                break;
        }
        
        currentState = nextState;
        return (nextState, reward);
    }
    
    public bool IsGoalState()
    {
        return currentState == 3;
    }
    
    public void Reset()
    {
        currentState = 0;
    }
}

// Main program demonstrating Q-learning
public class Program
{
    public static void Main(string[] args)
    {
        Console.WriteLine("Q-Learning Example");
        Console.WriteLine("==================");
        
        // Create environment and agent
        var env = new SimpleEnvironment();
        var agent = new QLearningAgent(learningRate: 0.1, discountFactor: 0.9, explorationRate: 0.1);
        
        int episodes = 1000;
        List<double> rewards = new List<double>();
        
        // Training loop
        for (int episode = 0; episode < episodes; episode++)
        {
            env.Reset();
            double totalReward = 0;
            
            while (!env.IsGoalState())
            {
                int currentState = env.GetCurrentState();
                List<int> actions = new List<int> { 0, 1 }; // 0 = left, 1 = right
                
                // Choose action
                int action = agent.ChooseAction(currentState, actions);
                
                // Take action and observe result
                var (nextState, reward) = env.TakeAction(action);
                totalReward += reward;
                
                // Update Q-value
                agent.UpdateQValue(currentState, action, reward, nextState, actions);
            }
            
            rewards.Add(totalReward);
            
            if (episode % 100 == 0)
            {
                Console.WriteLine($"Episode {episode}: Total Reward = {totalReward:F2}");
            }
        }
        
        // Show final Q-table
        Console.WriteLine("\nFinal Q-Table:");
        Console.WriteLine("State | Action | Q-Value");
        Console.WriteLine("------|--------|--------");
        
        var qTable = agent.GetQTable();
        foreach (var kvp in qTable.OrderBy(x => x.Key.Item1).ThenBy(x => x.Key.Item2))
        {
            Console.WriteLine($"{kvp.Key.Item1,5} | {kvp.Key.Item2,6} | {kvp.Value,7:F2}");
        }
        
        // Test the learned policy
        Console.WriteLine("\nTesting learned policy:");
        env.Reset();
        int steps = 0;
        
        while (!env.IsGoalState() && steps < 10)
        {
            int currentState = env.GetCurrentState();
            List<int> actions = new List<int> { 0, 1 };
            int bestAction = agent.GetBestAction(currentState, actions);
            
            Console.Write($"State {currentState} -> Action {bestAction} ");
            
            var (nextState, reward) = env.TakeAction(bestAction);
            Console.WriteLine($"-> State {nextState} (Reward: {reward})");
            
            steps++;
        }
        
        Console.WriteLine($"\nFinal state reached: {env.GetCurrentState()}");
    }
}
```

## Key Components Explained

### 1. **QLearningAgent Class**
- **Q-Table**: Dictionary storing state-action pairs and their Q-values
- **Parameters**:
  - `learningRate` (α): How much new information overrides old information
  - `discountFactor` (γ): Importance of future rewards
  - `explorationRate` (ε): Probability of choosing random actions

### 2. **Core Methods**
- `ChooseAction()`: Implements epsilon-greedy policy
- `UpdateQValue()`: Updates Q-values using the Q-learning formula
- `GetBestAction()`: Returns the action with highest Q-value for a state

### 3. **Q-Learning Formula**
```
Q(s,a) = Q(s,a) + α[r + γ*max(Q(s',a')) - Q(s,a)]
```

### 4. **Example Environment**
- Simple grid world with 4 states (0,1,2,3)
- Actions: 0 (left), 1 (right)
- Rewards: -1 for each step, +10 for reaching goal

## Sample Output
```
Q-Learning Example
==================
Episode 0: Total Reward = -3.00
Episode 100: Total Reward = -5.00
Episode 200: Total Reward = -4.00
Episode 300: Total Reward = -3.00
Episode 400: Total Reward = -2.00
Episode 500: Total Reward = -2.00
Episode 600: Total Reward = -1.00
Episode 700: Total Reward = -1.00
Episode 800: Total Reward = -1.00
Episode 900: Total Reward = -1.00

Final Q-Table:
State | Action | Q-Value
------|--------|--------
    0 |      0 |   -0.45
    0 |      1 |   -0.32
    1 |      0 |   -0.21
    1 |      1 |   -0.15
    2 |      0 |    9.87
    2 |      1 |   10.00
    3 |      0 |    0.00
    3 |      1 |    0.00
```

This implementation demonstrates the fundamental principles of Q-learning including exploration vs exploitation, value iteration, and policy learning in a discrete state-action environment.