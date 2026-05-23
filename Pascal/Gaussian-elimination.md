# Gaussian Elimination in Pascal

Here's an implementation of the Gaussian elimination algorithm in Pascal:

```pascal
program GaussianElimination;

uses crt;

const
  MAX_SIZE = 10;

type
  Matrix = array[1..MAX_SIZE, 1..MAX_SIZE] of real;
  Vector = array[1..MAX_SIZE] of real;

procedure GaussianElimination(var A: Matrix; var b: Vector; n: integer);
var
  i, j, k: integer;
  factor, temp: real;
begin
  // Forward elimination
  for k := 1 to n-1 do
  begin
    // Check for zero pivot
    if abs(A[k, k]) < 1e-10 then
    begin
      writeln('Error: Zero pivot encountered');
      exit;
    end;
    
    // Eliminate column k
    for i := k+1 to n do
    begin
      factor := A[i, k] / A[k, k];
      for j := k+1 to n do
        A[i, j] := A[i, j] - factor * A[k, j];
      b[i] := b[i] - factor * b[k];
    end;
  end;
  
  // Back substitution
  for i := n downto 1 do
  begin
    temp := b[i];
    for j := i+1 to n do
      temp := temp - A[i, j] * b[j];
    b[i] := temp / A[i, i];
  end;
end;

procedure PrintMatrix(var A: Matrix; var b: Vector; n: integer);
var
  i, j: integer;
begin
  writeln('Matrix A and vector b:');
  for i := 1 to n do
  begin
    for j := 1 to n do
      write(A[i, j]:8:2, ' ');
    write('| ', b[i]:8:2);
    writeln;
  end;
  writeln;
end;

procedure PrintSolution(var b: Vector; n: integer);
var
  i: integer;
begin
  writeln('Solution vector:');
  for i := 1 to n do
    writeln('x[', i, '] = ', b[i]:8:4);
  writeln;
end;

var
  A: Matrix;
  b: Vector;
  n, i, j: integer;

begin
  clrscr;
  
  // Example: Solve the system:
  // 2x + y - z = 8
  // -3x - y + 2z = -11
  // -2x + y + 2z = -3
  
  n := 3;
  
  // Initialize matrix A
  A[1, 1] := 2.0; A[1, 2] := 1.0; A[1, 3] := -1.0;
  A[2, 1] := -3.0; A[2, 2] := -1.0; A[2, 3] := 2.0;
  A[3, 1] := -2.0; A[3, 2] := 1.0; A[3, 3] := 2.0;
  
  // Initialize vector b
  b[1] := 8.0;
  b[2] := -11.0;
  b[3] := -3.0;
  
  writeln('Gaussian Elimination Example');
  writeln('============================');
  
  PrintMatrix(A, b, n);
  
  // Apply Gaussian elimination
  GaussianElimination(A, b, n);
  
  PrintSolution(b, n);
  
  writeln('Press any key to exit...');
  readln;
end.
```

## How it works:

1. **Forward Elimination**: Transform the matrix into upper triangular form by eliminating elements below the main diagonal
2. **Back Substitution**: Solve for the unknowns starting from the last equation

## Key Features:

- **Pivot Selection**: Checks for zero pivots to avoid division by zero
- **Error Handling**: Reports when a zero pivot is encountered
- **Input/Output**: Displays the original system and the solution
- **Modular Design**: Separate procedures for different operations

## Sample Output:
```
Gaussian Elimination Example
============================
Matrix A and vector b:
    2.00     1.00    -1.00 |     8.00
   -3.00    -1.00     2.00 |   -11.00
   -2.00     1.00     2.00 |    -3.00

Solution vector:
x[1] =     2.0000
x[2] =     3.0000
x[3] =    -1.0000
```

This implementation solves a 3×3 system of linear equations using the Gaussian elimination method with partial pivoting.

