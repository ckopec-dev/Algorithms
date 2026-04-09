# Q-Learning Algorithm in Pascal

Here's a complete implementation of a Q-learning algorithm in Pascal:

```pascal
program QLearningExample;

uses
  crt;

const
  NUM_STATES = 4;
  NUM_ACTIONS = 2;
  ALPHA = 0.1;    // Learning rate
  GAMMA = 0.9;    // Discount factor
  EPSILON = 0.1;  // Exploration rate

type
  TQTable = array[1..NUM_STATES, 1..NUM_ACTIONS] of real;
  TReward = array[1..NUM_STATES, 1..NUM_ACTIONS] of integer;

var
  QTable: TQTable;
  Reward: TReward;
  currentState, action, nextState: integer;
  reward: integer;
  epsilon: real;
  episode: integer;
  totalReward: integer;

// Initialize Q-table with zeros
procedure InitializeQTable;
var
  i, j: integer;
begin
  for i := 1 to NUM_STATES do
    for j := 1 to NUM_ACTIONS do
      QTable[i, j] := 0.0;
end;

// Initialize reward matrix
procedure InitializeReward;
var
  i, j: integer;
begin
  // Example reward matrix (you can modify this based on your problem)
  for i := 1 to NUM_STATES do
    for j := 1 to NUM_ACTIONS do
      Reward[i, j] := 0;
  
  // Set some example rewards
  Reward[1, 1] := 0; Reward[1, 2] := 0;
  Reward[2, 1] := 10; Reward[2, 2] := 0;
  Reward[3, 1] := 0; Reward[3, 2] := 5;
  Reward[4, 1] := 0; Reward[4, 2] := 0;
end;

// Epsilon-greedy action selection
function SelectAction(state: integer): integer;
var
  randomValue: real;
begin
  randomValue := random;
  
  if randomValue < epsilon then
  begin
    // Exploration: choose random action
    SelectAction := random(NUM_ACTIONS) + 1;
  end
  else
  begin
    // Exploitation: choose best action
    if QTable[state, 1] > QTable[state, 2] then
      SelectAction := 1
    else
      SelectAction := 2;
  end;
end;

// Q-learning update rule
procedure UpdateQTable(currentState, action, nextState: integer; reward: integer);
var
  maxQNextState: real;
  QValue: real;
begin
  // Find maximum Q-value for next state
  if QTable[nextState, 1] > QTable[nextState, 2] then
    maxQNextState := QTable[nextState, 1]
  else
    maxQNextState := QTable[nextState, 2];
  
  // Q-learning update formula
  QValue := QTable[currentState, action] + 
            ALPHA * (reward + GAMMA * maxQNextState - QTable[currentState, action]);
  
  QTable[currentState, action] := QValue;
end;

// Print Q-table
procedure PrintQTable;
var
  i, j: integer;
begin
  writeln('Q-Table:');
  writeln('State | Action 1 | Action 2');
  writeln('------|----------|---------');
  for i := 1 to NUM_STATES do
  begin
    write(i:5, ' | ');
    for j := 1 to NUM_ACTIONS do
      write(QTable[i, j]:8:2, ' | ');
    writeln;
  end;
  writeln;
end;

// Main Q-learning algorithm
procedure RunQLearning;
var
  i: integer;
begin
  writeln('Starting Q-Learning Algorithm');
  writeln('============================');
  
  InitializeQTable;
  InitializeReward;
  
  // Training episodes
  for episode := 1 to 1000 do
  begin
    // Reset to initial state
    currentState := 1;
    totalReward := 0;
    
    // Run one episode
    for i := 1 to 10 do
    begin
      // Select action
      action := SelectAction(currentState);
      
      // Simulate environment transition
      // In a real environment, this would be replaced by actual state transitions
      case currentState of
        1: begin
             if action = 1 then begin nextState := 2; reward := Reward[1, 1]; end
             else begin nextState := 1; reward := Reward[1, 2]; end;
           end;
        2: begin
             if action = 1 then begin nextState := 3; reward := Reward[2, 1]; end
             else begin nextState := 1; reward := Reward[2, 2]; end;
           end;
        3: begin
             if action = 1 then begin nextState := 4; reward := Reward[3, 1]; end
             else begin nextState := 1; reward := Reward[3, 2]; end;
           end;
        4: begin
             if action = 1 then begin nextState := 4; reward := Reward[4, 1]; end
             else begin nextState := 4; reward := Reward[4, 2]; end;
           end;
      end;
      
      // Update Q-value
      UpdateQTable(currentState, action, nextState, reward);
      
      // Update state
      currentState := nextState;
      totalReward := totalReward + reward;
    end;
    
    // Decay epsilon (exploration rate)
    if episode mod 100 = 0 then
      epsilon := epsilon * 0.9;
  end;
  
  writeln('Training completed!');
  writeln('Final Q-Table:');
  PrintQTable;
  
  writeln('Policy (best actions for each state):');
  for i := 1 to NUM_STATES do
  begin
    if QTable[i, 1] > QTable[i, 2] then
      writeln('State ', i, ': Take Action 1')
    else
      writeln('State ', i, ': Take Action 2');
  end;
end;

begin
  randomize;
  RunQLearning;
end.
```

## Key Components Explained:

### 1. **Data Structures**
- `TQTable`: 2D array storing Q-values for each state-action pair
- `TReward`: Reward matrix for the environment

### 2. **Key Parameters**
- `ALPHA`: Learning rate (how quickly to update Q-values)
- `GAMMA`: Discount factor (importance of future rewards)
- `EPSILON`: Exploration rate (probability of choosing random action)

### 3. **Main Functions**
- `InitializeQTable()`: Sets all Q-values to zero
- `SelectAction()`: Implements epsilon-greedy policy
- `UpdateQTable()`: Applies the Q-learning update rule
- `RunQLearning()`: Main training loop

### 4. **Q-Learning Formula**
```
Q(s,a) ← Q(s,a) + α[r + γ max Q(s',a') - Q(s,a)]
```

This implementation demonstrates the core principles of Q-learning in Pascal, including exploration vs exploitation, reward-based learning, and iterative policy improvement.

