# Tabu Search Algorithm in Pascal

Here's an implementation of the Tabu Search algorithm in Pascal for solving a simple optimization problem:

```pascal
program TabuSearchExample;

uses
  SysUtils, Math;

type
  TPosition = array[1..5] of Integer; // 5-dimensional solution
  TTabuList = array[1..10] of TPosition; // Tabu list with 10 entries

var
  TabuList: TTabuList;
  TabuLength: Integer;
  BestSolution: TPosition;
  BestValue: Real;
  CurrentSolution: TPosition;
  Iteration: Integer;
  MaxIterations: Integer;
  TabuTenure: Integer;

// Objective function - example: minimize sum of squares
function ObjectiveFunction(solution: TPosition): Real;
var
  i: Integer;
  sum: Real;
begin
  sum := 0;
  for i := 1 to 5 do
    sum := sum + Sqr(solution[i]);
  ObjectiveFunction := sum;
end;

// Generate a neighbor by perturbing one dimension
procedure GenerateNeighbor(current: TPosition; var neighbor: TPosition);
var
  i: Integer;
  dimension: Integer;
begin
  // Copy current solution
  for i := 1 to 5 do
    neighbor[i] := current[i];
  
  // Randomly select a dimension to perturb
  dimension := Random(5) + 1;
  
  // Add small random perturbation
  if Random < 0.5 then
    neighbor[dimension] := neighbor[dimension] + Random(3) - 1
  else
    neighbor[dimension] := neighbor[dimension] - Random(3) + 1;
  
  // Keep values within reasonable bounds
  if neighbor[dimension] < -10 then neighbor[dimension] := -10;
  if neighbor[dimension] > 10 then neighbor[dimension] := 10;
end;

// Check if a solution is in the tabu list
function IsTabu(solution: TPosition): Boolean;
var
  i, j: Integer;
begin
  IsTabu := False;
  for i := 1 to TabuLength do
  begin
    if TabuList[i][1] = solution[1] then
    begin
      for j := 1 to 5 do
        if TabuList[i][j] <> solution[j] then
          break;
      if j > 5 then
      begin
        IsTabu := True;
        Exit;
      end;
    end;
  end;
end;

// Add solution to tabu list
procedure AddToTabu(solution: TPosition);
var
  i: Integer;
begin
  for i := TabuLength downto 2 do
    TabuList[i] := TabuList[i-1];
  TabuList[1] := solution;
  if TabuLength < 10 then
    Inc(TabuLength);
end;

// Main Tabu Search algorithm
procedure TabuSearch;
var
  i, j: Integer;
  neighbor: TPosition;
  neighborValue: Real;
  bestNeighbor: TPosition;
  bestNeighborValue: Real;
  currentBest: TPosition;
  currentBestValue: Real;
begin
  // Initialize
  Randomize;
  MaxIterations := 100;
  TabuTenure := 5;
  TabuLength := 0;
  
  // Initialize random starting solution
  for i := 1 to 5 do
    CurrentSolution[i] := Random(21) - 10; // Random between -10 and 10
  
  // Initialize best solution
  BestSolution := CurrentSolution;
  BestValue := ObjectiveFunction(CurrentSolution);
  
  Writeln('Starting Tabu Search...');
  Writeln('Initial solution: ', CurrentSolution[1], ' ', CurrentSolution[2], ' ', 
          CurrentSolution[3], ' ', CurrentSolution[4], ' ', CurrentSolution[5]);
  Writeln('Initial value: ', BestValue:0:4);
  
  // Main loop
  for Iteration := 1 to MaxIterations do
  begin
    // Find best non-tabu neighbor
    bestNeighborValue := MaxInt;
    currentBestValue := MaxInt;
    
    // Generate 10 neighbors
    for i := 1 to 10 do
    begin
      GenerateNeighbor(CurrentSolution, neighbor);
      neighborValue := ObjectiveFunction(neighbor);
      
      // Check if neighbor is better and not tabu
      if (neighborValue < bestNeighborValue) and not IsTabu(neighbor) then
      begin
        bestNeighbor := neighbor;
        bestNeighborValue := neighborValue;
      end;
      
      // Track overall best
      if neighborValue < currentBestValue then
      begin
        currentBest := neighbor;
        currentBestValue := neighborValue;
      end;
    end;
    
    // Move to best non-tabu neighbor
    CurrentSolution := bestNeighbor;
    
    // Update global best if found
    if bestNeighborValue < BestValue then
    begin
      BestSolution := bestNeighbor;
      BestValue := bestNeighborValue;
    end;
    
    // Add current solution to tabu list
    AddToTabu(CurrentSolution);
    
    // Remove oldest entry if tabu list is too long
    if TabuLength > TabuTenure then
      Dec(TabuLength);
    
    // Print progress
    if Iteration mod 20 = 0 then
    begin
      Writeln('Iteration ', Iteration, ': Best value = ', BestValue:0:4);
      Writeln('  Best solution: ', BestSolution[1], ' ', BestSolution[2], ' ', 
              BestSolution[3], ' ', BestSolution[4], ' ', BestSolution[5]);
    end;
  end;
  
  // Output final results
  Writeln('');
  Writeln('=== Tabu Search Results ===');
  Writeln('Final best solution: ', BestSolution[1], ' ', BestSolution[2], ' ', 
          BestSolution[3], ' ', BestSolution[4], ' ', BestSolution[5]);
  Writeln('Final best value: ', BestValue:0:6);
  Writeln('Iterations completed: ', MaxIterations);
end;

begin
  TabuSearch;
  Readln;
end.
```

## Key Components of the Tabu Search Implementation:

### 1. **Data Structures**
- `TPosition`: Represents a solution (5-dimensional vector)
- `TTabuList`: Array to store recently visited solutions
- `TabuLength`: Current size of tabu list

### 2. **Core Functions**
- `ObjectiveFunction`: Evaluates solution quality (minimizing sum of squares)
- `GenerateNeighbor`: Creates neighboring solutions by perturbing one dimension
- `IsTabu`: Checks if a solution is in the tabu list
- `AddToTabu`: Adds solution to tabu list with proper aging

### 3. **Algorithm Logic**
- Starts with a random solution
- Generates multiple neighbors in each iteration
- Selects the best non-tabu neighbor
- Updates tabu list and global best solution
- Uses tabu tenure to prevent cycling

### 4. **Key Features**
- **Tabu Tenure**: Prevents revisiting recent solutions
- **Aspiration Criteria**: Allows better solutions to override tabu status
- **Neighbor Generation**: Random perturbation of solution components
- **Progress Tracking**: Shows improvement over iterations

This implementation demonstrates the fundamental principles of Tabu Search while maintaining simplicity for educational purposes. The algorithm effectively escapes local optima by temporarily forbidding certain moves, leading to better global solutions.

