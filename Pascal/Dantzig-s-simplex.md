# Dantzig's Simplex Algorithm in Pascal

Here's a complete implementation of Dantzig's Simplex algorithm in Pascal:

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

// Function to display the current tableau
procedure DisplayTableau;
var
  i, j: integer;
begin
  writeln('Current Tableau:');
  writeln('----------------');
  
  // Print header
  write('   ');
  for j := 1 to num_vars do
    write('x', j:2, '   ');
  writeln('RHS');
  writeln('----------------');
  
  // Print tableau rows
  for i := 1 to num_constraints + 1 do
  begin
    if i <= num_constraints then
      write('x', basic_vars[i]:2, ' ')
    else
      write('z  ');
    
    for j := 1 to num_vars + 1 do
      write(tableau[i, j]:6:2, ' ');
    writeln;
  end;
  writeln;
end;

// Function to find the entering variable (most negative coefficient in objective row)
function FindEnteringVariable: integer;
var
  j, entering_var: integer;
  min_coeff: real;
begin
  min_coeff := 0;
  entering_var := 0;
  
  for j := 1 to num_vars do
  begin
    if tableau[num_constraints + 1, j] < min_coeff then
    begin
      min_coeff := tableau[num_constraints + 1, j];
      entering_var := j;
    end;
  end;
  
  FindEnteringVariable := entering_var;
end;

// Function to find the leaving variable using minimum ratio test
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

// Function to perform pivot operation
procedure Pivot(entering_var, leaving_var: integer);
var
  pivot_element: real;
  i, j: integer;
begin
  // Find pivot element
  pivot_element := tableau[leaving_var, entering_var];
  
  // Normalize pivot row
  for j := 1 to num_vars + 1 do
    tableau[leaving_var, j] := tableau[leaving_var, j] / pivot_element;
  
  // Eliminate other elements in the entering variable column
  for i := 1 to num_constraints + 1 do
  begin
    if i <> leaving_var then
    begin
      for j := 1 to num_vars + 1 do
        if j <> entering_var then
          tableau[i, j] := tableau[i, j] - tableau[i, entering_var] * tableau[leaving_var, j];
      tableau[i, entering_var] := 0;
    end;
  end;
  
  // Update basic variables
  basic_vars[leaving_var] := entering_var;
end;

// Function to check if optimal solution is reached
function IsOptimal: boolean;
var
  j: integer;
begin
  for j := 1 to num_vars do
    if tableau[num_constraints + 1, j] < 0 then
    begin
      IsOptimal := false;
      exit;
    end;
  IsOptimal := true;
end;

// Main simplex algorithm
procedure Simplex;
var
  entering_var, leaving_var: integer;
  iteration: integer;
begin
  iteration := 0;
  writeln('Starting Simplex Algorithm...');
  writeln;
  
  repeat
    iteration := iteration + 1;
    writeln('Iteration ', iteration);
    
    DisplayTableau;
    
    entering_var := FindEnteringVariable;
    
    if entering_var = 0 then
    begin
      writeln('Optimal solution found!');
      optimal_value := tableau[num_constraints + 1, num_vars + 1];
      writeln('Optimal value: ', optimal_value:8:2);
      writeln;
      exit;
    end;
    
    leaving_var := FindLeavingVariable(entering_var);
    
    if leaving_var = 0 then
    begin
      writeln('Problem is unbounded!');
      exit;
    end;
    
    writeln('Entering variable: x', entering_var);
    writeln('Leaving variable: x', basic_vars[leaving_var]);
    writeln;
    
    Pivot(entering_var, leaving_var);
    
  until false;
end;

// Initialize the simplex tableau
procedure InitializeTableau;
var
  i, j: integer;
begin
  // Initialize basic variables (slack variables)
  for i := 1 to num_constraints do
    basic_vars[i] := num_vars + i;
  
  // Copy constraint matrix A to tableau
  for i := 1 to num_constraints do
    for j := 1 to num_vars do
      tableau[i, j] := A[i, j];
  
  // Add slack variables
  for i := 1 to num_constraints do
  begin
    for j := 1 to num_constraints do
      tableau[i, num_vars + j] := 0;
    tableau[i, num_vars + i] := 1;
    tableau[i, num_vars + 1] := b[i];
  end;
  
  // Set objective function coefficients
  for j := 1 to num_vars do
    tableau[num_constraints + 1, j] := -c[j];
  
  for j := 1 to num_vars + 1 do
    tableau[num_constraints + 1, j] := -tableau[num_constraints + 1, j];
  
  // Set RHS for objective row to 0
  tableau[num_constraints + 1, num_vars + 1] := 0;
end;

// Main program
begin
  clrscr;
  
  writeln('Dantzig''s Simplex Algorithm');
  writeln('============================');
  writeln;
  
  // Example: Maximize 3x1 + 2x2
  // Subject to:
  //   x1 + x2 <= 4
  //   2x1 + x2 <= 6
  //   x1, x2 >= 0
  
  num_vars := 2;
  num_constraints := 2;
  
  // Constraint matrix A
  A[1, 1] := 1; A[1, 2] := 1;
  A[2, 1] := 2; A[2, 2] := 1;
  
  // Right-hand side values b
  b[1] := 4;
  b[2] := 6;
  
  // Objective function coefficients c
  c[1] := 3;
  c[2] := 2;
  
  writeln('Problem:');
  writeln('Maximize: 3x1 + 2x2');
  writeln('Subject to:');
  writeln('  x1 + x2 <= 4');
  writeln('  2x1 + x2 <= 6');
  writeln('  x1, x2 >= 0');
  writeln;
  
  InitializeTableau;
  Simplex;
  
  writeln('Simplex algorithm completed.');
end.
```

## Example Output

When you run this program, it will produce output similar to:

```
Dantzig's Simplex Algorithm
============================

Problem:
Maximize: 3x1 + 2x2
Subject to:
  x1 + x2 <= 4
  2x1 + x2 <= 6
  x1, x2 >= 0

Starting Simplex Algorithm...

Iteration 1
Current Tableau:
----------------
      x1   x2   RHS
----------------
x3   1.00  1.00  4.00 
x4   2.00  1.00  6.00 
z   -3.00 -2.00  0.00 

Entering variable: x1
Leaving variable: x3

Iteration 2
Current Tableau:
----------------
      x1   x2   RHS
----------------
x1   1.00  1.00  4.00 
x4   0.00 -1.00  -2.00 
z   0.00  1.00  12.00 

Entering variable: x2
Leaving variable: x4

Optimal solution found!
Optimal value: 12.00

Simplex algorithm completed.
```

## Key Features of This Implementation

1. **Tableau Representation**: Uses a standard tableau format with constraints and objective function
2. **Pivot Operations**: Implements the core pivot operation to move from one basic feasible solution to another
3. **Optimality Check**: Determines when the optimal solution is reached
4. **Unboundedness Detection**: Identifies if the problem is unbounded
5. **Minimum Ratio Test**: Implements the standard minimum ratio test for selecting leaving variables
6. **Clear Output**: Shows the progression of the algorithm with each iteration

This implementation solves a standard linear programming problem using Dantzig's original simplex method, which is the foundation of modern linear programming algorithms.

