# Counting Sort Algorithm in Pascal

```pascal
program CountingSortExample;

uses crt;

procedure countingSort(var arr: array of integer; n: integer);
var
  i, maxVal, minVal: integer;
  countingArr: array of integer;
  outputArr: array of integer;
  range: integer;
begin
  if n = 0 then
    exit;
  
  // Find maximum and minimum values
  maxVal := arr[0];
  minVal := arr[0];
  
  for i := 1 to n-1 do
  begin
    if arr[i] > maxVal then
      maxVal := arr[i];
    if arr[i] < minVal then
      minVal := arr[i];
  end;
  
  // Calculate range
  range := maxVal - minVal + 1;
  
  // Initialize counting array
  SetLength(countingArr, range);
  SetLength(outputArr, n);
  
  // Initialize counting array to 0
  for i := 0 to range-1 do
    countingArr[i] := 0;
  
  // Count occurrences of each element
  for i := 0 to n-1 do
    countingArr[arr[i] - minVal] := countingArr[arr[i] - minVal] + 1;
  
  // Modify counting array to contain actual positions
  for i := 1 to range-1 do
    countingArr[i] := countingArr[i] + countingArr[i-1];
  
  // Build output array
  for i := n-1 downto 0 do
  begin
    outputArr[countingArr[arr[i] - minVal] - 1] := arr[i];
    countingArr[arr[i] - minVal] := countingArr[arr[i] - minVal] - 1;
  end;
  
  // Copy sorted elements back to original array
  for i := 0 to n-1 do
    arr[i] := outputArr[i];
  
  // Free memory
  SetLength(countingArr, 0);
  SetLength(outputArr, 0);
end;

procedure printArray(arr: array of integer; n: integer);
var
  i: integer;
begin
  for i := 0 to n-1 do
    write(arr[i], ' ');
  writeln;
end;

var
  arr: array[0..9] of integer;
  n, i: integer;

begin
  clrscr;
  
  // Example array to sort
  arr[0] := 4;
  arr[1] := 2;
  arr[2] := 2;
  arr[3] := 8;
  arr[4] := 3;
  arr[5] := 3;
  arr[6] := 1;
  arr[7] := 7;
  arr[8] := 5;
  arr[9] := 6;
  
  n := 10;
  
  writeln('Original array:');
  printArray(arr, n);
  
  countingSort(arr, n);
  
  writeln('Sorted array:');
  printArray(arr, n);
  
  readln;
end.
```

## Output:
```
Original array:
4 2 2 8 3 3 1 7 5 6 

Sorted array:
1 2 2 3 3 4 5 6 7 8 
```

## Key Features of this Implementation:

1. **Handles negative numbers**: Uses minVal to adjust indexing
2. **Stable sorting**: Maintains relative order of equal elements
3. **Time Complexity**: O(n + k) where n is array size and k is range
4. **Space Complexity**: O(k) for the counting array
5. **In-place sorting**: Modifies original array

## How it works:
1. Find the range of input values
2. Create a counting array to store frequencies
3. Count occurrences of each element
4. Modify counting array to store cumulative counts
5. Build output array from right to left to maintain stability
6. Copy sorted elements back to original array

