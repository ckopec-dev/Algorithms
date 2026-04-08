# Minimax Algorithm in Pascal

Here's a complete implementation of the Minimax algorithm in Pascal for a simple game (Tic-Tac-Toe):

```pascal
program MinimaxExample;

type
  Board = array[1..3, 1..3] of integer;
  Position = record
    row, col: integer;
  end;

const
  EMPTY = 0;
  PLAYER_X = 1;
  PLAYER_O = -1;

var
  gameBoard: Board;

// Initialize the board
procedure InitializeBoard;
var
  i, j: integer;
begin
  for i := 1 to 3 do
    for j := 1 to 3 do
      gameBoard[i, j] := EMPTY;
end;

// Check if the board is full
function IsBoardFull: boolean;
var
  i, j: integer;
begin
  for i := 1 to 3 do
    for j := 1 to 3 do
      if gameBoard[i, j] = EMPTY then
      begin
        IsBoardFull := false;
        exit;
      end;
  IsBoardFull := true;
end;

// Check if a player has won
function CheckWinner: integer;
var
  i: integer;
begin
  // Check rows
  for i := 1 to 3 do
    if (gameBoard[i, 1] = gameBoard[i, 2]) and (gameBoard[i, 2] = gameBoard[i, 3]) then
      if gameBoard[i, 1] <> EMPTY then
      begin
        CheckWinner := gameBoard[i, 1];
        exit;
      end;

  // Check columns
  for i := 1 to 3 do
    if (gameBoard[1, i] = gameBoard[2, i]) and (gameBoard[2, i] = gameBoard[3, i]) then
      if gameBoard[1, i] <> EMPTY then
      begin
        CheckWinner := gameBoard[1, i];
        exit;
      end;

  // Check diagonals
  if (gameBoard[1, 1] = gameBoard[2, 2]) and (gameBoard[2, 2] = gameBoard[3, 3]) then
    if gameBoard[1, 1] <> EMPTY then
    begin
      CheckWinner := gameBoard[1, 1];
      exit;
    end;

  if (gameBoard[1, 3] = gameBoard[2, 2]) and (gameBoard[2, 2] = gameBoard[3, 1]) then
    if gameBoard[1, 3] <> EMPTY then
    begin
      CheckWinner := gameBoard[1, 3];
      exit;
    end;

  CheckWinner := EMPTY;
end;

// Evaluate the board position
function EvaluateBoard: integer;
var
  winner: integer;
begin
  winner := CheckWinner;
  if winner = PLAYER_X then
    EvaluateBoard := 10
  else if winner = PLAYER_O then
    EvaluateBoard := -10
  else
    EvaluateBoard := 0;
end;

// Check if a move is valid
function IsMoveValid(row, col: integer): boolean;
begin
  IsMoveValid := (row >= 1) and (row <= 3) and (col >= 1) and (col <= 3) and 
                 (gameBoard[row, col] = EMPTY);
end;

// Minimax algorithm
function Minimax(board: Board; depth: integer; isMaximizing: boolean): integer;
var
  i, j, bestScore, score: integer;
  tempBoard: Board;
begin
  // Check terminal states
  bestScore := EvaluateBoard;
  if bestScore = 10 then
  begin
    Minimax := bestScore - depth;
    exit;
  end
  else if bestScore = -10 then
  begin
    Minimax := bestScore + depth;
    exit;
  end
  else if IsBoardFull then
  begin
    Minimax := 0;
    exit;
  end;

  if isMaximizing then
  begin
    bestScore := -1000;
    for i := 1 to 3 do
      for j := 1 to 3 do
        if board[i, j] = EMPTY then
        begin
          tempBoard := board;
          tempBoard[i, j] := PLAYER_X;
          score := Minimax(tempBoard, depth + 1, false);
          if score > bestScore then
            bestScore := score;
        end;
  end
  else
  begin
    bestScore := 1000;
    for i := 1 to 3 do
      for j := 1 to 3 do
        if board[i, j] = EMPTY then
        begin
          tempBoard := board;
          tempBoard[i, j] := PLAYER_O;
          score := Minimax(tempBoard, depth + 1, true);
          if score < bestScore then
            bestScore := score;
        end;
  end;

  Minimax := bestScore;
end;

// Find the best move for the AI
function FindBestMove: Position;
var
  i, j, bestScore, score: integer;
  bestMove: Position;
  tempBoard: Board;
begin
  bestScore := -1000;
  bestMove.row := 0;
  bestMove.col := 0;

  for i := 1 to 3 do
    for j := 1 to 3 do
      if gameBoard[i, j] = EMPTY then
      begin
        tempBoard := gameBoard;
        tempBoard[i, j] := PLAYER_X;
        score := Minimax(tempBoard, 0, false);
        if score > bestScore then
        begin
          bestScore := score;
          bestMove.row := i;
          bestMove.col := j;
        end;
      end;

  FindBestMove := bestMove;
end;

// Display the board
procedure DisplayBoard;
var
  i, j: integer;
  symbol: char;
begin
  writeln('Current Board:');
  for i := 1 to 3 do
  begin
    for j := 1 to 3 do
    begin
      case gameBoard[i, j] of
        EMPTY: symbol := ' ';
        PLAYER_X: symbol := 'X';
        PLAYER_O: symbol := 'O';
      end;
      write(' ', symbol, ' |');
    end;
    writeln;
    if i < 3 then writeln('-------------');
  end;
  writeln;
end;

// Main game loop
procedure PlayGame;
var
  row, col: integer;
  move: Position;
  winner: integer;
begin
  InitializeBoard;
  writeln('Tic-Tac-Toe Game with Minimax AI');
  writeln('You are O, AI is X');
  writeln;

  while true do
  begin
    DisplayBoard;
    
    // Player's turn (O)
    writeln('Your turn (O)');
    write('Enter row (1-3): ');
    readln(row);
    write('Enter column (1-3): ');
    readln(col);
    
    if IsMoveValid(row, col) then
    begin
      gameBoard[row, col] := PLAYER_O;
      
      // Check if player won
      winner := CheckWinner;
      if winner = PLAYER_O then
      begin
        DisplayBoard;
        writeln('Congratulations! You win!');
        break;
      end;
      
      // Check if board is full
      if IsBoardFull then
      begin
        DisplayBoard;
        writeln('It''s a tie!');
        break;
      end;
      
      // AI's turn (X)
      writeln('AI is thinking...');
      move := FindBestMove;
      gameBoard[move.row, move.col] := PLAYER_X;
      
      // Check if AI won
      winner := CheckWinner;
      if winner = PLAYER_X then
      begin
        DisplayBoard;
        writeln('AI wins! Better luck next time.');
        break;
      end;
      
      // Check if board is full
      if IsBoardFull then
      begin
        DisplayBoard;
        writeln('It''s a tie!');
        break;
      end;
    end
    else
    begin
      writeln('Invalid move! Try again.');
    end;
  end;
end;

begin
  PlayGame;
end.
```

## Key Components of the Minimax Implementation:

### 1. **Board Representation**
- 3x3 array to represent the game board
- Constants for EMPTY, PLAYER_X, and PLAYER_O

### 2. **Core Functions**
- `EvaluateBoard`: Returns score (10 for X win, -10 for O win, 0 for tie)
- `Minimax`: Recursive function that implements the minimax algorithm
- `FindBestMove`: Determines the optimal move for the AI

### 3. **Minimax Logic**
- **Maximizing Player (X)**: Tries to maximize the score
- **Minimizing Player (O)**: Tries to minimize the score
- **Depth**: Tracks the recursion depth to favor quicker wins
- **Terminal States**: Checks for wins, losses, or ties

### 4. **Algorithm Features**
- **Alpha-Beta Pruning**: Not implemented here but could be added
- **Depth Consideration**: Prefers quicker wins
- **Recursive Search**: Explores all possible game states

### 5. **How It Works**
1. The AI evaluates all possible moves
2. For each move, it recursively evaluates the opponent's responses
3. It chooses the move that maximizes its chances of winning
4. The algorithm considers the worst-case scenario for each move

This implementation demonstrates the fundamental principles of the Minimax algorithm in a practical game context.

