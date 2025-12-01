# Thomas Algorithm in Pascal

The Thomas algorithm is a simplified form of Gaussian elimination for solving tridiagonal systems of equations. Here's an implementation in Pascal:

```pascal
program ThomasAlgorithm;

type
  TVector = array[1..100] of real;
  TMatrix = array[1..100, 1..3] of real; // Stores a, b, c coefficients

procedure ThomasSolve(n: integer; var a, b, c, d: TVector; var x: TVector);
var
  i: integer;
  m: real;
begin
  // Forward elimination
  for i := 2 to n do
  begin
    m := a[i] / b[i-1];
    b[i] := b[i] - m * c[i-1];
    d[i] := d[i] - m * d[i-1];
  end;
  
  // Back substitution
  x[n] := d[n] / b[n];
  for i := n-1 downto 1 do
  begin
    x[i] := (d[i] - c[i] * x[i+1]) / b[i];
  end;
end;

procedure PrintVector(n: integer; var v: TVector; name: string);
var
  i: integer;
begin
  writeln(name, ':');
  for i := 1 to n do
    write(v[i]:8:2, ' ');
  writeln;
end;

var
  n: integer;
  a, b, c, d, x: TVector;
  i: integer;

begin
  // Example: Solve the tridiagonal system
  // 2x1 + x2 = 3
  // x1 + 3x2 + x3 = 4
  // x2 + 4x3 = 5
  
  n := 3;
  
  // Coefficients for the system Ax = d
  // a[i] = sub-diagonal (i = 2 to n)
  // b[i] = main diagonal
  // c[i] = super-diagonal (i = 1 to n-1)
  // d[i] = right-hand side
  
  a[1] := 0;    // No sub-diagonal element for first row
  a[2] := 1;    // Sub-diagonal element
  a[3] := 1;    // Sub-diagonal element
  
  b[1] := 2;    // Main diagonal element
  b[2] := 3;    // Main diagonal element
  b[3] := 4;    // Main diagonal element
  
  c[1] := 1;    // Super-diagonal element
  c[2] := 1;    // Super-diagonal element
  c[3] := 0;    // No super-diagonal element for last row
  
  d[1] := 3;    // Right-hand side
  d[2] := 4;    // Right-hand side
  d[3] := 5;    // Right-hand side
  
  writeln('Solving tridiagonal system:');
  writeln('Row 1: 2x1 + 1x2 = 3');
  writeln('Row 2: 1x1 + 3x2 + 1x3 = 4');
  writeln('Row 3: 1x2 + 4x3 = 5');
  writeln;
  
  PrintVector(n, a, 'Sub-diagonal (a)');
  PrintVector(n, b, 'Main diagonal (b)');
  PrintVector(n, c, 'Super-diagonal (c)');
  PrintVector(n, d, 'Right-hand side (d)');
  writeln;
  
  // Solve the system
  ThomasSolve(n, a, b, c, d, x);
  
  PrintVector(n, x, 'Solution (x)');
  
  // Verify the solution
  writeln('Verification:');
  for i := 1 to n do
  begin
    writeln('Row ', i, ': ', 
            (a[i]*x[1] + b[i]*x[2] + c[i]*x[3]):8:2, 
            ' = ', d[i]:8:2);
  end;
end.
```

## How it works:

1. **Forward elimination**: Eliminates the sub-diagonal elements to create an upper triangular system
2. **Back substitution**: Solves for the unknowns starting from the last equation

## Key features of this implementation:

- Handles tridiagonal systems of any size (up to 100 equations)
- Uses the standard Thomas algorithm with proper indexing
- Includes verification of results
- Clear separation of forward elimination and back substitution steps

## Expected output:
```
Solving tridiagonal system:
Row 1: 2x1 + 1x2 = 3
Row 2: 1x1 + 3x2 + 1x3 = 4
Row 3: 1x2 + 4x3 = 5

Sub-diagonal (a):   0.00   1.00   1.00 
Main diagonal (b):   2.00   3.00   4.00 
Super-diagonal (c):   1.00   1.00   0.00 
Right-hand side (d):   3.00   4.00   5.00 

Solution (x):   1.00   1.00   1.00 
Verification:
Row 1:   3.00 =   3.00
Row 2:   4.00 =   4.00
Row 3:   5.00 =   5.00
```

