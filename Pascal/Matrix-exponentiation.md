```pascal
program MatrixExponentiation;

type
  TMatrix = array[1..10, 1..10] of real;
  
procedure MatrixMultiply(A, B: TMatrix; var C: TMatrix; n: integer);
var
  i, j, k: integer;
begin
  for i := 1 to n do
    for j := 1 to n do
    begin
      C[i, j] := 0;
      for k := 1 to n do
        C[i, j] := C[i, j] + A[i, k] * B[k, j];
    end;
end;

procedure MatrixPower(A: TMatrix; var result: TMatrix; n, power: integer);
var
  temp: TMatrix;
  i, j: integer;
begin
  // Initialize result as identity matrix
  for i := 1 to n do
    for j := 1 to n do
    begin
      if i = j then
        result[i, j] := 1
      else
        result[i, j] := 0;
    end;
    
  // If power is 0, return identity matrix
  if power = 0 then
    exit;
    
  // If power is 1, return original matrix
  if power = 1 then
  begin
    for i := 1 to n do
      for j := 1 to n do
        result[i, j] := A[i, j];
    exit;
  end;
  
  // Binary exponentiation
  while power > 0 do
  begin
    if power mod 2 = 1 then
    begin
      MatrixMultiply(result, A, temp, n);
      for i := 1 to n do
        for j := 1 to n do
          result[i, j] := temp[i, j];
    end;
    MatrixMultiply(A, A, temp, n);
    for i := 1 to n do
      for j := 1 to n do
        A[i, j] := temp[i, j];
    power := power div 2;
  end;
end;

procedure PrintMatrix(A: TMatrix; n: integer);
var
  i, j: integer;
begin
  for i := 1 to n do
  begin
    for j := 1 to n do
      write(A[i, j]:8:2);
    writeln;
  end;
  writeln;
end;

var
  matrix: TMatrix;
  result: TMatrix;
  n, power: integer;
  
begin
  // Example: 3x3 matrix
  n := 3;
  power := 4;
  
  // Initialize matrix
  matrix[1, 1] := 1; matrix[1, 2] := 2; matrix[1, 3] := 3;
  matrix[2, 1] := 0; matrix[2, 2] := 1; matrix[2, 3] := 4;
  matrix[3, 1] := 5; matrix[3, 2] := 6; matrix[3, 3] := 0;
  
  writeln('Original matrix:');
  PrintMatrix(matrix, n);
  
  MatrixPower(matrix, result, n, power);
  
  writeln('Matrix raised to power ', power, ':');
  PrintMatrix(result, n);
end.
```

