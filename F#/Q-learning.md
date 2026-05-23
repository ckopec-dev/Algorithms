# Q-Learning Algorithm in F#

Here's a complete implementation of the Q-learning algorithm in F#:

```fsharp
open System
open System.Collections.Generic

// Q-Learning implementation
type QLearning<'State, 'Action> = {
    QTable: Map<'State * 'Action, float>
    LearningRate: float
    DiscountFactor: float
    ExplorationRate: float
    Random: Random
}

// Q-Learning functions
module QLearning =
    // Create a new Q-learning agent
    let create learningRate discountFactor explorationRate =
        {
            QTable = Map.empty
            LearningRate = learningRate
            DiscountFactor = discountFactor
            ExplorationRate = explorationRate
            Random = Random()
        }

    // Get Q-value for state-action pair
    let getQValue (ql: QLearning<'State, 'Action>) state action =
        match Map.tryFind (state, action) ql.QTable with
        | Some value -> value
        | None -> 0.0

    // Update Q-value using Q-learning update rule
    let updateQValue (ql: QLearning<'State, 'Action>) state action reward nextMaxQ =
        let currentQ = getQValue ql state action
        let newQ = currentQ + ql.LearningRate * (reward + ql.DiscountFactor * nextMaxQ - currentQ)
        
        { ql with 
            QTable = Map.add (state, action) newQ ql.QTable }

    // Choose action using epsilon-greedy policy
    let chooseAction (ql: QLearning<'State, 'Action>) state actions =
        if ql.Random.NextDouble() < ql.ExplorationRate then
            // Exploration: choose random action
            actions.[ql.Random.Next(actions.Length)]
        else
            // Exploitation: choose best action
            actions
            |> Array.maxBy (fun action -> getQValue ql state action)

    // Get maximum Q-value for a state
    let getMaxQValue (ql: QLearning<'State, 'Action>) state actions =
        actions
        |> Array.maxBy (fun action -> getQValue ql state action)
        |> fun action -> getQValue ql state action

// Example: Simple Grid World Environment
type Position = { X: int; Y: int }
type Action = Up | Down | Left | Right

// Environment functions
module Environment =
    // Get valid actions for a position
    let getValidActions position =
        let actions = ref []
        if position.Y > 0 then actions := Up :: !actions
        if position.Y < 4 then actions := Down :: !actions
        if position.X > 0 then actions := Left :: !actions
        if position.X < 4 then actions := Right :: !actions
        !actions |> List.toArray

    // Get next position based on action
    let getNextPosition position action =
        match action with
        | Up -> { position with Y = position.Y - 1 }
        | Down -> { position with Y = position.Y + 1 }
        | Left -> { position with X = position.X - 1 }
        | Right -> { position with X = position.X + 1 }

    // Get reward for a position
    let getReward position =
        match position.X, position.Y with
        | 4, 4 -> 10.0  // Goal state
        | 2, 2 -> -10.0 // Trap state
        | _ -> -1.0     // Default reward

// Q-Learning training example
[<EntryPoint>]
let main argv =
    // Create Q-learning agent
    let ql = QLearning.create 0.1 0.9 0.1
    
    // Training parameters
    let episodes = 1000
    let maxSteps = 100
    
    // Training loop
    for episode in 1 .. episodes do
        let mutable state = { X = 0; Y = 0 }
        let mutable steps = 0
        
        while steps < maxSteps && not (state.X = 4 && state.Y = 4) do
            let actions = Environment.getValidActions state
            let action = QLearning.chooseAction ql state actions
            
            // Take action and observe reward
            let nextState = Environment.getNextPosition state action
            let reward = Environment.getReward nextState
            
            // Get max Q-value for next state
            let nextActions = Environment.getValidActions nextState
            let nextMaxQ = 
                if nextActions.Length > 0 then
                    QLearning.getMaxQValue ql nextState nextActions
                else
                    0.0
            
            // Update Q-value
            ql <- QLearning.updateQValue ql state action reward nextMaxQ
            
            state <- nextState
            steps <- steps + 1
            
            // Print progress every 100 episodes
            if episode % 100 = 0 && steps = 1 then
                printfn "Episode %d: Steps = %d, Position = (%d,%d)" episode steps state.X state.Y
    
    // Print final Q-values for some key states
    printfn "\nFinal Q-values:"
    let testStates = [ { X = 0; Y = 0 }; { X = 1; Y = 0 }; { X = 4; Y = 4 } ]
    
    for state in testStates do
        let actions = Environment.getValidActions state
        printfn "State (%d,%d):" state.X state.Y
        for action in actions do
            let qValue = QLearning.getQValue ql state action
            printfn "  %A: %f" action qValue
        printfn ""
    
    printfn "Training completed!"
    0
```

## Key Components Explained

### 1. **QLearning Type**
```fsharp
type QLearning<'State, 'Action> = {
    QTable: Map<'State * 'Action, float>  // Q-value storage
    LearningRate: float                   // α - learning rate
    DiscountFactor: float                 // γ - discount factor
    ExplorationRate: float                // ε - exploration rate
    Random: Random                        // Random number generator
}
```

### 2. **Core Functions**
- `create`: Initialize Q-learning agent
- `getQValue`: Retrieve Q-value for state-action pair
- `updateQValue`: Update Q-value using Bellman equation
- `chooseAction`: Select action using epsilon-greedy policy
- `getMaxQValue`: Find maximum Q-value for a state

### 3. **Q-Learning Update Rule**
```
Q(s,a) ← Q(s,a) + α[r + γ max Q(s',a') - Q(s,a)]
```

### 4. **Example Environment**
- Simple 5x5 grid world
- Goal at (4,4) with reward +10
- Trap at (2,2) with reward -10
- All other cells with reward -1

This implementation demonstrates the core principles of Q-learning in a practical F# context, showing how the algorithm learns optimal policies through trial and error.

