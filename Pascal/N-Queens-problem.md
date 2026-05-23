# N-Queens Problem in Pascal

Here's a complete implementation of the N-Queens problem solver in Pascal using backtracking:

```pascal
program NQueens;

const
  MAX_N = 10;

type
  TBoard = array[1..MAX_N] of integer;

var
  board: TBoard;
  n: integer;
  solutions: integer;

// Check if placing a queen at position (row, col) is safe
function isSafe(row, col: integer): boolean;
var
  i: integer;
begin
  // Check column
  for i := 1 to row - 1 do
    if board[i] = col then
    begin
      isSafe := false;
      exit;
    end;
  
  // Check diagonal (top-left to bottom-right)
  for i := 1 to row - 1 do
    if board[i] = col - (row - i) then
    begin
      isSafe := false;
      exit;
    end;
  
  // Check diagonal (top-right to bottom-left)
  for i := 1 to row - 1 do
    if board[i] = col + (row - i) then
    begin
      isSafe := false;
      exit;
    end;
  
  isSafe := true;
end;

// Print the board configuration
procedure printBoard;
var
  i, j: integer;
begin
  writeln('Solution ', solutions);
  for i := 1 to n do
  begin
    for j := 1 to n do
    begin
      if board[i] = j then
        write('Q ')
      else
        write('. ');
    end;
    writeln;
  end;
  writeln;
end;

// Recursive backtracking function to solve N-Queens
procedure solveQueens(row: integer);
var
  col: integer;
begin
  if row > n then
  begin
    // Found a solution
    inc(solutions);
    printBoard;
    exit;
  end;
  
  for col := 1 to n do
  begin
    if isSafe(row, col) then
    begin
      board[row] := col;
      solveQueens(row + 1);
    end;
  end;
end;

// Main program
begin
  writeln('N-Queens Problem Solver');
  writeln('======================');
  
  write('Enter the value of N (1-10): ');
  readln(n);
  
  if (n < 1) or (n > MAX_N) then
  begin
    writeln('Invalid input! Please enter N between 1 and 10.');
    halt;
  end;
  
  solutions := 0;
  writeln('Solving ', n, '-Queens problem...');
  writeln;
  
  solveQueens(1);
  
  writeln('Total solutions found: ', solutions);
end.
```

## How it works:

1. **Data Structure**: Uses an array `board` where `board[i]` represents the column position of the queen in row `i`.

2. **Safety Check**: The `isSafe` function checks if placing a queen at position (row, col) conflicts with existing queens:
   - Same column
   - Same diagonal (both directions)

3. **Backtracking Algorithm**: 
   - Try placing a queen in each column of the current row
   - If valid, recursively solve for the next row
   - If no valid position exists, backtrack to previous row

4. **Output**: Displays each valid solution as a board configuration

## Sample Output for N=4:
```
N-Queens Problem Solver
======================
Enter the value of N (1-10): 4
Solving 4-Queens problem...

Solution 1
. Q . .
. . . Q
Q . . .
. . Q .

Solution 2
. . Q .
Q . . .
. . . Q
. Q . .

Total solutions found: 2
```

## Key Features:
- **Backtracking**: Efficiently explores all possible configurations
- **Safety Checking**: Optimized conflict detection
- **Visualization**: Shows actual board layouts
- **Counting**: Tracks total number of solutions
- **Input Validation**: Handles invalid inputs gracefully

The algorithm has time complexity of O(N!) in the worst case, which is typical for backtracking solutions to constraint satisfaction problems.

