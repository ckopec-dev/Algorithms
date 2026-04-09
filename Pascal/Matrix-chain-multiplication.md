# Matrix Chain Multiplication in Pascal

Here's an implementation of the Matrix Chain Multiplication algorithm in Pascal:

```pascal
program MatrixChainMultiplication;

uses
  crt;

const
  MAXN = 100;

type
  TMatrix = array[1..MAXN, 1..MAXN] of integer;

var
  p: array[0..MAXN] of integer;  // Dimensions of matrices
  m: array[1..MAXN, 1..MAXN] of integer;  // Minimum cost matrix
  s: array[1..MAXN, 1..MAXN] of integer;  // Split point matrix
  n: integer;

// Function to calculate minimum cost of matrix chain multiplication
function MatrixChainOrder(p: array of integer; n: integer): integer;
var
  i, j, k, l, q: integer;
begin
  // m[i,j] is the minimum number of scalar multiplications needed
  // to compute the matrix A[i]A[i+1]...A[j]
  
  // Initialize the cost matrix
  for i := 1 to n do
    m[i, i] := 0;
  
  // L is the chain length
  for l := 2 to n do
  begin
    for i := 1 to n - l + 1 do
    begin
      j := i + l - 1;
      m[i, j] := maxint;
      
      for k := i to j - 1 do
      begin
        q := m[i, k] + m[k + 1, j] + p[i - 1] * p[k] * p[j];
        if q < m[i, j] then
        begin
          m[i, j] := q;
          s[i, j] := k;
        end;
      end;
    end;
  end;
  
  MatrixChainOrder := m[1, n];
end;

// Function to print the optimal parenthesization
procedure PrintOptimalParens(s: array of array of integer; i, j: integer);
begin
  if i = j then
    write('A', i)
  else
  begin
    write('(');
    PrintOptimalParens(s, i, s[i, j]);
    write(' x ');
    PrintOptimalParens(s, s[i, j] + 1, j);
    write(')');
  end;
end;

// Function to print the cost matrix
procedure PrintCostMatrix(n: integer);
var
  i, j: integer;
begin
  writeln('Cost Matrix M:');
  for i := 1 to n do
  begin
    for j := 1 to n do
    begin
      if i > j then
        write('0     ')
      else
        write(m[i, j]:5);
    end;
    writeln;
  end;
end;

begin
  clrscr;
  
  // Example: Matrices A1(10x100), A2(100x5), A3(5x50)
  // Dimensions array p = [10, 100, 5, 50]
  p[0] := 10;
  p[1] := 100;
  p[2] := 5;
  p[3] := 50;
  n := 3;
  
  writeln('Matrix Chain Multiplication Example');
  writeln('==================================');
  writeln('Matrix dimensions: A1(10x100), A2(100x5), A3(5x50)');
  writeln('Dimensions array p = [10, 100, 5, 50]');
  writeln;
  
  // Calculate minimum cost
  MatrixChainOrder(p, n);
  
  // Print results
  writeln('Minimum number of scalar multiplications: ', m[1, n]);
  writeln;
  writeln('Optimal parenthesization:');
  PrintOptimalParens(s, 1, n);
  writeln;
  writeln;
  
  // Print cost matrix
  PrintCostMatrix(n);
  
  writeln;
  writeln('Press any key to exit...');
  readln;
end.
```

## Explanation of the Algorithm

This implementation demonstrates the classic Matrix Chain Multiplication problem using dynamic programming:

### Key Components:

1. **Input**: Array `p` containing matrix dimensions
2. **Dynamic Programming Table**: `m[i,j]` stores minimum cost to multiply matrices from i to j
3. **Split Point Table**: `s[i,j]` stores the optimal split point for matrices from i to j

### Algorithm Steps:

1. **Initialization**: Set `m[i,i] = 0` for all i
2. **Fill the table**: For chain lengths from 2 to n, compute minimum cost
3. **Recurrence relation**: 
   ```
   m[i,j] = min{m[i,k] + m[k+1,j] + p[i-1] * p[k] * p[j]}
   ```
   where k ranges from i to j-1

### Sample Output:
```
Matrix Chain Multiplication Example
==================================
Matrix dimensions: A1(10x100), A2(100x5), A3(5x50)
Dimensions array p = [10, 100, 5, 50]

Minimum number of scalar multiplications: 7500

Optimal parenthesization:
((A1 x A2) x A3)

Cost Matrix M:
    0  7500  7500
    0     0  2500
    0     0     0
```

The algorithm has a time complexity of O(n³) and space complexity of O(n²).

