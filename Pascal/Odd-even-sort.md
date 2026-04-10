# Odd-Even Sort Algorithm in Pascal

Here's an implementation of the Odd-Even sort (also known as Brick Sort) algorithm in Pascal:

```pascal
program OddEvenSort;

{$APPTYPE CONSOLE}

uses
  SysUtils;

procedure OddEvenSort(var arr: array of integer; n: integer);
var
  isSorted: boolean;
  i: integer;
begin
  isSorted := false;
  
  while not isSorted do
  begin
    isSorted := true;
    
    // Odd phase: compare odd indexed elements
    for i := 1 to n - 2 do
    begin
      if arr[i] > arr[i + 1] then
      begin
        // Swap elements
        arr[i] := arr[i] XOR arr[i + 1];
        arr[i + 1] := arr[i] XOR arr[i + 1];
        arr[i] := arr[i] XOR arr[i + 1];
        isSorted := false;
      end;
    end;
    
    // Even phase: compare even indexed elements
    for i := 0 to n - 2 do
    begin
      if arr[i] > arr[i + 1] then
      begin
        // Swap elements
        arr[i] := arr[i] XOR arr[i + 1];
        arr[i + 1] := arr[i] XOR arr[i + 1];
        arr[i] := arr[i] XOR arr[i + 1];
        isSorted := false;
      end;
    end;
  end;
end;

procedure PrintArray(arr: array of integer; n: integer);
var
  i: integer;
begin
  for i := 0 to n - 1 do
    write(arr[i], ' ');
  writeln;
end;

var
  arr: array[0..9] of integer;
  n: integer;
  i: integer;

begin
  // Initialize array with unsorted values
  arr[0] := 64;
  arr[1] := 34;
  arr[2] := 25;
  arr[3] := 12;
  arr[4] := 22;
  arr[5] := 11;
  arr[6] := 90;
  arr[7] := 88;
  arr[8] := 76;
  arr[9] := 50;
  
  n := 10;
  
  writeln('Original array:');
  PrintArray(arr, n);
  
  // Sort the array
  OddEvenSort(arr, n);
  
  writeln('Sorted array:');
  PrintArray(arr, n);
  
  readln;
end.
```

## How the Algorithm Works:

1. **Odd-Even Sorting** is a comparison-based sorting algorithm that works in passes
2. **Two phases** are alternated:
   - **Odd phase**: Compare elements at odd indices (1, 3, 5, ...)
   - **Even phase**: Compare elements at even indices (0, 2, 4, ...)
3. **Swap** elements if they're in the wrong order
4. Continue until no more swaps are needed (array is sorted)

## Output:
```
Original array:
64 34 25 12 22 11 90 88 76 50 
Sorted array:
11 12 22 25 34 50 64 76 88 90 
```

## Key Features:
- **Time Complexity**: O(n²) in worst case, O(n) in best case
- **Space Complexity**: O(1) - sorts in place
- **Stable**: Maintains relative order of equal elements
- **Parallelizable**: Odd and even phases can be executed in parallel

