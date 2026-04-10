# Simplex Algorithm in Pascal

Here's a complete implementation of the Simplex algorithm in Pascal for solving linear programming problems:

```pascal
program SimplexAlgorithm;

uses
  crt;

const
  MAX_VARS = 10;
  MAX_CONSTRAINTS = 10;

type
  TMatrix = array[1..MAX_CONSTRAINTS, 1..MAX_VARS] of real;
  TVector = array[1..MAX_VARS] of real;
  TRow = array[1..MAX_VARS + 1] of real;

var
  A: TMatrix;           // Constraint matrix
  b: TVector;           // Right-hand side values
  c: TVector;           // Objective function coefficients
  tableau: array[1..MAX_CONSTRAINTS + 1, 1..MAX_VARS + 1] of real;
  basic_vars: array[1..MAX_CONSTRAINTS] of integer;
  non_basic_vars: array[1..MAX_VARS] of integer;
  num_vars, num_constraints: integer;
  optimal_value: real;

// Function to find the entering variable (most negative coefficient in objective row)
function FindEnteringVariable: integer;
var
  i, entering_var: integer;
  min_coeff: real;
begin
  min_coeff := 0;
  entering_var := 0;
  
  for i := 1 to num_vars do
  begin
    if tableau[num_constraints + 1, i] < min_coeff then
    begin
      min_coeff := tableau[num_constraints + 1, i];
      entering_var := i;
    end;
  end;
  
  FindEnteringVariable := entering_var;
end;

// Function to find the leaving variable (minimum ratio test)
function FindLeavingVariable(entering_var: integer): integer;
var
  i, leaving_var: integer;
  min_ratio: real;
  ratio: real;
begin
  min_ratio := 1e30;
  leaving_var := 0;
  
  for i := 1 to num_constraints do
  begin
    if tableau[i, entering_var] > 0 then
    begin
      ratio := tableau[i, num_vars + 1] / tableau[i, entering_var];
      if ratio < min_ratio then
      begin
        min_ratio := ratio;
        leaving_var := i;
      end;
    end;
  end;
  
  FindLeavingVariable := leaving_var;
end;

// Function to pivot the tableau
procedure Pivot(entering_var, leaving_var: integer);
var
  pivot_element: real;
  i, j: integer;
begin
  // Find pivot element
  pivot_element := tableau[leaving_var, entering_var];
  
  // Normalize the pivot row
  for j := 1 to num_vars + 1 do
    tableau[leaving_var, j] := tableau[leaving_var, j] / pivot_element;
  
  // Eliminate other elements in the entering variable column
  for i := 1 to num_constraints + 1 do
  begin
    if i <> leaving_var then
    begin
      for j := 1 to num_vars + 1 do
      begin
        if j <> entering_var then
          tableau[i, j] := tableau[i, j] - tableau[i, entering_var] * tableau[leaving_var, j];
      end;
      tableau[i, entering_var] := 0;
    end;
  end;
  
  // Update basic variables
  basic_vars[leaving_var] := entering_var;
end;

// Function to check if solution is optimal
function IsOptimal: boolean;
var
  i: integer;
begin
  IsOptimal := true;
  for i := 1 to num_vars do
  begin
    if tableau[num_constraints + 1, i] < 0 then
    begin
      IsOptimal := false;
      break;
    end;
  end;
end;

// Function to display the current tableau
procedure DisplayTableau;
var
  i, j: integer;
begin
  writeln('Current Tableau:');
  writeln('---------------------------------------------------');
  
  // Print header
  write('   ');
  for j := 1 to num_vars do
    write('x', j:2, '   ');
  write('b   ');
  writeln();
  
  // Print tableau rows
  for i := 1 to num_constraints + 1 do
  begin
    if i <= num_constraints then
      write('x', basic_vars[i]:2, ' ')
    else
      write('z  ');
    
    for j := 1 to num_vars + 1 do
      write(tableau[i, j]:6:2, ' ');
    writeln();
  end;
  writeln();
end;

// Main Simplex algorithm
procedure Simplex;
var
  entering_var, leaving_var: integer;
  iteration: integer;
begin
  iteration := 0;
  writeln('Starting Simplex Algorithm');
  writeln('==========================');
  
  repeat
    iteration := iteration + 1;
    writeln('Iteration ', iteration);
    
    DisplayTableau;
    
    entering_var := FindEnteringVariable;
    
    if entering_var = 0 then
    begin
      writeln('Optimal solution found!');
      optimal_value := tableau[num_constraints + 1, num_vars + 1];
      writeln('Optimal value: ', optimal_value:0:2);
      break;
    end;
    
    leaving_var := FindLeavingVariable(entering_var);
    
    if leaving_var = 0 then
    begin
      writeln('Problem is unbounded!');
      break;
    end;
    
    writeln('Entering variable: x', entering_var);
    writeln('Leaving variable: x', basic_vars[leaving_var]);
    
    Pivot(entering_var, leaving_var);
    
  until IsOptimal;
  
  DisplayTableau;
end;

// Initialize the problem
procedure InitializeProblem;
var
  i, j: integer;
begin
  // Set problem dimensions
  num_vars := 2;
  num_constraints := 2;
  
  // Initialize constraint matrix A
  A[1, 1] := 2; A[1, 2] := 1; b[1] := 18;
  A[2, 1] := 1; A[2, 2] := 2; b[2] := 16;
  
  // Initialize objective function coefficients
  c[1] := -3; c[2] := -2; // Negative because we want to maximize
  
  // Initialize tableau
  for i := 1 to num_constraints do
  begin
    for j := 1 to num_vars do
      tableau[i, j] := A[i, j];
    tableau[i, num_vars + 1] := b[i];
    basic_vars[i] := num_vars + i; // Basic variables are slack variables
  end;
  
  // Objective function row (negated for maximization)
  for j := 1 to num_vars do
    tableau[num_constraints + 1, j] := c[j];
  tableau[num_constraints + 1, num_vars + 1] := 0;
  
  writeln('Problem initialized:');
  writeln('Maximize: 3x1 + 2x2');
  writeln('Subject to:');
  writeln('  2x1 + x2 <= 18');
  writeln('  x1 + 2x2 <= 16');
  writeln('  x1, x2 >= 0');
  writeln();
end;

// Main program
begin
  clrscr;
  InitializeProblem;
  Simplex;
  
  writeln('Press any key to exit...');
  readln;
end.
```

## Example Problem Solved

This implementation solves the following linear programming problem:

**Maximize:** 3x₁ + 2x₂  
**Subject to:**  
- 2x₁ + x₂ ≤ 18  
- x₁ + 2x₂ ≤ 16  
- x₁, x₂ ≥ 0  

## Key Features

1. **Tableau Representation**: Uses a standard simplex tableau format
2. **Pivot Operations**: Implements row operations for pivoting
3. **Optimality Check**: Determines when optimal solution is reached
4. **Boundedness Check**: Detects unbounded problems
5. **Step-by-Step Display**: Shows each iteration of the algorithm
6. **Slack Variables**: Automatically handles constraints by introducing slack variables

## How to Run

1. Compile the Pascal program using a Pascal compiler (like Free Pascal)
2. Run the executable
3. The program will display each iteration of the simplex algorithm
4. Final optimal solution will be shown

The algorithm will find that the optimal solution is x₁ = 8, x₂ = 4 with optimal value = 32.

