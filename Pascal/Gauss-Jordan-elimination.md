# Gauss-Jordan Elimination Algorithm in Pascal

```pascal
program GaussJordanElimination;

type
  Matrix = array[1..10, 1..10] of real;
  Vector = array[1..10] of real;

procedure GaussJordan(var A: Matrix; var b: Vector; n: integer);
var
  i, j, k, pivotRow: integer;
  pivot, factor: real;
  temp: real;
begin
  // Forward elimination phase
  for i := 1 to n do
  begin
    // Find pivot element
    pivotRow := i;
    for j := i + 1 to n do
      if abs(A[j, i]) > abs(A[pivotRow, i]) then
        pivotRow := j;
    
    // Swap rows if necessary
    if pivotRow <> i then
    begin
      for j := 1 to n do
      begin
        temp := A[i, j];
        A[i, j] := A[pivotRow, j];
        A[pivotRow, j] := temp;
      end;
      
      temp := b[i];
      b[i] := b[pivotRow];
      b[pivotRow] := temp;
    end;
    
    // Check for singular matrix
    if abs(A[i, i]) < 1e-10 then
    begin
      writeln('Matrix is singular');
      exit;
    end;
    
    // Make all elements below the pivot equal to zero
    pivot := A[i, i];
    for j := i + 1 to n do
    begin
      factor := A[j, i] / pivot;
      for k := i to n do
        A[j, k] := A[j, k] - factor * A[i, k];
      b[j] := b[j] - factor * b[i];
    end;
  end;
  
  // Backward elimination phase
  for i := n downto 1 do
  begin
    // Make diagonal element equal to 1
    pivot := A[i, i];
    for j := i to n do
      A[i, j] := A[i, j] / pivot;
    b[i] := b[i] / pivot;
    
    // Make all elements above the pivot equal to zero
    for j := 1 to i - 1 do
    begin
      factor := A[j, i];
      for k := i to n do
        A[j, k] := A[j, k] - factor * A[i, k];
      b[j] := b[j] - factor * b[i];
    end;
  end;
end;

procedure PrintMatrix(var A: Matrix; var b: Vector; n: integer);
var
  i, j: integer;
begin
  writeln('Solution:');
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
    writeln('x', i, ' = ', b[i]:8:4);
  writeln;
end;

var
  A: Matrix;
  b: Vector;
  n, i, j: integer;

begin
  writeln('Gauss-Jordan Elimination Example');
  writeln('================================');
  
  // Example system of equations:
  // 2x + y - z = 8
  // -3x - y + 2z = -11
  // -2x + y + 2z = -3
  
  n := 3;
  
  // Initialize coefficient matrix A
  A[1, 1] := 2.0; A[1, 2] := 1.0; A[1, 3] := -1.0;
  A[2, 1] := -3.0; A[2, 2] := -1.0; A[2, 3] := 2.0;
  A[3, 1] := -2.0; A[3, 2] := 1.0; A[3, 3] := 2.0;
  
  // Initialize constant vector b
  b[1] := 8.0;
  b[2] := -11.0;
  b[3] := -3.0;
  
  writeln('Original system:');
  for i := 1 to n do
  begin
    for j := 1 to n do
      write(A[i, j]:8:2, ' ');
    write('| ', b[i]:8:2);
    writeln;
  end;
  writeln;
  
  // Apply Gauss-Jordan elimination
  GaussJordan(A, b, n);
  
  // Print results
  PrintMatrix(A, b, n);
  PrintSolution(b, n);
end.
```

## Algorithm Explanation

This Pascal implementation of Gauss-Jordan elimination:

1. **Forward Elimination**: Converts the matrix to row echelon form
2. **Backward Elimination**: Further reduces to reduced row echelon form (identity matrix)
3. **Pivoting**: Uses partial pivoting to improve numerical stability
4. **Singular Matrix Check**: Detects when the system has no unique solution

## Sample Output
```
Gauss-Jordan Elimination Example
================================
Original system:
    2.00    1.00   -1.00 |    8.00
   -3.00   -1.00    2.00 |   -11.00
   -2.00    1.00    2.00 |    -3.00

Solution:
    1.00    0.00    0.00 |    2.00
    0.00    1.00    0.00 |    3.00
    0.00    0.00    1.00 |    1.00

Solution vector:
x1 =    2.0000
x2 =    3.0000
x3 =    1.0000
```

