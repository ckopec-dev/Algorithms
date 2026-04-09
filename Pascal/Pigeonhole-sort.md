# Pigeonhole Sort Algorithm in Pascal

```pascal
program PigeonholeSort;

{$APPTYPE CONSOLE}

uses
  SysUtils;

procedure PigeonholeSort(var arr: array of integer; n: integer);
var
  minVal, maxVal, range, i, j, index: integer;
  holes: array of integer;
begin
  if n = 0 then
    exit;
    
  // Find minimum and maximum values
  minVal := arr[0];
  maxVal := arr[0];
  
  for i := 1 to n - 1 do
  begin
    if arr[i] < minVal then
      minVal := arr[i];
    if arr[i] > maxVal then
      maxVal := arr[i];
  end;
  
  // Calculate range
  range := maxVal - minVal + 1;
  
  // Create holes array
  SetLength(holes, range);
  
  // Initialize holes to 0
  for i := 0 to range - 1 do
    holes[i] := 0;
  
  // Fill holes with counts
  for i := 0 to n - 1 do
    holes[arr[i] - minVal] := holes[arr[i] - minVal] + 1;
  
  // Reconstruct sorted array
  index := 0;
  for i := 0 to range - 1 do
  begin
    for j := 1 to holes[i] do
    begin
      arr[index] := i + minVal;
      index := index + 1;
    end;
  end;
  
  // Free memory
  SetLength(holes, 0);
end;

procedure PrintArray(arr: array of integer; n: integer);
var
  i: integer;
begin
  for i := 0 to n - 1 do
    Write(arr[i], ' ');
  Writeln;
end;

var
  arr: array [0..9] of integer;
  n, i: integer;

begin
  // Example usage
  n := 10;
  arr[0] := 8;
  arr[1] := 3;
  arr[2] := 2;
  arr[3] := 7;
  arr[4] := 4;
  arr[5] := 6;
  arr[6] := 1;
  arr[7] := 9;
  arr[8] := 5;
  arr[9] := 0;
  
  Writeln('Original array:');
  PrintArray(arr, n);
  
  PigeonholeSort(arr, n);
  
  Writeln('Sorted array:');
  PrintArray(arr, n);
  
  Readln;
end.
```

## Output:
```
Original array:
8 3 2 7 4 6 1 9 5 0 
Sorted array:
0 1 2 3 4 5 6 7 8 9 
```

## How it works:

1. **Find Range**: Determine the minimum and maximum values in the array
2. **Create Holes**: Create an array (holes) with size equal to the range of values
3. **Count Occurrences**: Count how many times each value appears and store in corresponding hole
4. **Reconstruct**: Go through the holes and place values back in sorted order

## Time Complexity:
- **Best Case**: O(n + k) where n is the number of elements and k is the range
- **Average Case**: O(n + k)
- **Worst Case**: O(n + k)

## Space Complexity: O(k)

## Notes:
- Pigeonhole sort is most efficient when the range of possible values (k) is not significantly larger than the number of elements (n)
- It's a stable sorting algorithm
- Works well for small ranges of integers
- Not suitable for floating-point numbers or large ranges

