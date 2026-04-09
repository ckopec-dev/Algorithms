# Alpha-Beta Pruning Algorithm in Pascal

Here's a complete implementation of the Alpha-Beta pruning algorithm in Pascal:

```pascal
program AlphaBetaPruning;

type
  // Node structure for game tree
  TNode = record
    Value: Integer;
    IsLeaf: Boolean;
    Children: array[1..8] of TNode;
    ChildCount: Integer;
  end;
  
  // Game state structure
  TGameState = record
    Board: array[1..3, 1..3] of Integer; // 3x3 tic-tac-toe board
    IsMaximizing: Boolean;
  end;

// Function to evaluate the current game state
function EvaluateState(const State: TGameState): Integer;
begin
  // Simple evaluation for tic-tac-toe
  // Check for win conditions
  if (State.Board[1,1] = 1) and (State.Board[1,2] = 1) and (State.Board[1,3] = 1) then
    Result := 10
  else if (State.Board[2,1] = 1) and (State.Board[2,2] = 1) and (State.Board[2,3] = 1) then
    Result := 10
  else if (State.Board[3,1] = 1) and (State.Board[3,2] = 1) and (State.Board[3,3] = 1) then
    Result := 10
  else if (State.Board[1,1] = 1) and (State.Board[2,1] = 1) and (State.Board[3,1] = 1) then
    Result := 10
  else if (State.Board[1,2] = 1) and (State.Board[2,2] = 1) and (State.Board[3,2] = 1) then
    Result := 10
  else if (State.Board[1,3] = 1) and (State.Board[2,3] = 1) and (State.Board[3,3] = 1) then
    Result := 10
  else if (State.Board[1,1] = 1) and (State.Board[2,2] = 1) and (State.Board[3,3] = 1) then
    Result := 10
  else if (State.Board[1,3] = 1) and (State.Board[2,2] = 1) and (State.Board[3,1] = 1) then
    Result := 10
  else if (State.Board[1,1] = -1) and (State.Board[1,2] = -1) and (State.Board[1,3] = -1) then
    Result := -10
  else if (State.Board[2,1] = -1) and (State.Board[2,2] = -1) and (State.Board[2,3] = -1) then
    Result := -10
  else if (State.Board[3,1] = -1) and (State.Board[3,2] = -1) and (State.Board[3,3] = -1) then
    Result := -10
  else if (State.Board[1,1] = -1) and (State.Board[2,1] = -1) and (State.Board[3,1] = -1) then
    Result := -10
  else if (State.Board[1,2] = -1) and (State.Board[2,2] = -1) and (State.Board[3,2] = -1) then
    Result := -10
  else if (State.Board[1,3] = -1) and (State.Board[2,3] = -1) and (State.Board[3,3] = -1) then
    Result := -10
  else if (State.Board[1,1] = -1) and (State.Board[2,2] = -1) and (State.Board[3,3] = -1) then
    Result := -10
  else if (State.Board[1,3] = -1) and (State.Board[2,2] = -1) and (State.Board[3,1] = -1) then
    Result := -10
  else
    Result := 0; // No winner yet
end;

// Function to generate all possible moves
function GenerateMoves(const State: TGameState): array of TGameState;
var
  i, j, k: Integer;
  NewState: TGameState;
begin
  SetLength(Result, 0);
  k := 0;
  
  for i := 1 to 3 do
    for j := 1 to 3 do
      if State.Board[i,j] = 0 then
      begin
        SetLength(Result, k + 1);
        NewState := State;
        if State.IsMaximizing then
          NewState.Board[i,j] := 1
        else
          NewState.Board[i,j] := -1;
        NewState.IsMaximizing := not State.IsMaximizing;
        Result[k] := NewState;
        Inc(k);
      end;
end;

// Alpha-Beta pruning algorithm
function AlphaBeta(const State: TGameState; Depth: Integer; Alpha, Beta: Integer; IsMaximizing: Boolean): Integer;
var
  Moves: array of TGameState;
  i, Eval: Integer;
  BestValue: Integer;
begin
  // Base case: if depth is 0 or game is over
  if (Depth = 0) or (EvaluateState(State) <> 0) then
  begin
    Result := EvaluateState(State);
    Exit;
  end;
  
  if IsMaximizing then
  begin
    BestValue := -MaxInt;
    Moves := GenerateMoves(State);
    
    for i := 0 to High(Moves) do
    begin
      Eval := AlphaBeta(Moves[i], Depth - 1, Alpha, Beta, False);
      BestValue := Max(BestValue, Eval);
      Alpha := Max(Alpha, BestValue);
      
      // Alpha-Beta pruning
      if Beta <= Alpha then
        Break; // Prune the remaining branches
    end;
    
    Result := BestValue;
  end
  else
  begin
    BestValue := MaxInt;
    Moves := GenerateMoves(State);
    
    for i := 0 to High(Moves) do
    begin
      Eval := AlphaBeta(Moves[i], Depth - 1, Alpha, Beta, True);
      BestValue := Min(BestValue, Eval);
      Beta := Min(Beta, BestValue);
      
      // Alpha-Beta pruning
      if Beta <= Alpha then
        Break; // Prune the remaining branches
    end;
    
    Result := BestValue;
  end;
end;

// Function to find the best move using Alpha-Beta
function FindBestMove(const State: TGameState; Depth: Integer): Integer;
var
  Moves: array of TGameState;
  i, BestMove, BestValue, Eval: Integer;
begin
  Moves := GenerateMoves(State);
  BestMove := -1;
  BestValue := -MaxInt;
  
  for i := 0 to High(Moves) do
  begin
    Eval := AlphaBeta(Moves[i], Depth - 1, -MaxInt, MaxInt, False);
    if Eval > BestValue then
    begin
      BestValue := Eval;
      BestMove := i;
    end;
  end;
  
  Result := BestMove;
end;

// Main program
var
  GameState: TGameState;
  BestMove: Integer;
begin
  // Initialize a sample game state (empty board, maximizing player)
  GameState.Board[1,1] := 0; GameState.Board[1,2] := 0; GameState.Board[1,3] := 0;
  GameState.Board[2,1] := 0; GameState.Board[2,2] := 0; GameState.Board[2,3] := 0;
  GameState.Board[3,1] := 0; GameState.Board[3,2] := 0; GameState.Board[3,3] := 0;
  GameState.IsMaximizing := True;
  
  Writeln('Alpha-Beta Pruning Algorithm Demo');
  Writeln('================================');
  Writeln('Initial game state (empty board):');
  
  // Display board
  Writeln('  1   2   3');
  Writeln('1 ', GameState.Board[1,1], ' | ', GameState.Board[1,2], ' | ', GameState.Board[1,3]);
  Writeln('  ---+---+---');
  Writeln('2 ', GameState.Board[2,1], ' | ', GameState.Board[2,2], ' | ', GameState.Board[2,3]);
  Writeln('  ---+---+---');
  Writeln('3 ', GameState.Board[3,1], ' | ', GameState.Board[3,2], ' | ', GameState.Board[3,3]);
  
  // Find best move with depth 4
  BestMove := FindBestMove(GameState, 4);
  Writeln('Best move found with depth 4: ', BestMove);
  
  // Run Alpha-Beta with depth 5
  Writeln('Running Alpha-Beta with depth 5...');
  Writeln('Best value:', AlphaBeta(GameState, 5, -MaxInt, MaxInt, True));
  
  Writeln('Alpha-Beta pruning completed successfully!');
end.
```

## Key Features of this Implementation:

1. **Game Tree Structure**: Uses a recursive node structure to represent the game tree
2. **Evaluation Function**: Simple tic-tac-toe evaluation function
3. **Move Generation**: Generates all possible moves from current state
4. **Alpha-Beta Logic**: Implements the core alpha-beta pruning algorithm
5. **Pruning Mechanism**: Properly implements alpha-beta pruning to reduce search space
6. **Minimax Integration**: Combines with minimax for optimal decision making

## How Alpha-Beta Pruning Works:

- **Alpha**: Best value that the maximizing player can guarantee at that level or above
- **Beta**: Best value that the minimizing player can guarantee at that level or above
- **Pruning**: When `Beta ≤ Alpha`, further branches are pruned (not evaluated)
- **Efficiency**: Significantly reduces the number of nodes evaluated compared to plain minimax

The algorithm is particularly useful for games with large search spaces like chess, where it can dramatically improve performance by avoiding unnecessary evaluations.

