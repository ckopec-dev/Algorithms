# Counting Sort Algorithm in Pascal

Here's an implementation of the Counting Sort algorithm in Pascal:

```pascal
program CountingSortExample;

{$APPTYPE CONSOLE}

uses
  SysUtils;

procedure CountingSort(var arr: array of integer; n: integer);
var
  i, maxVal, minVal: integer;
  count: array of integer;
  output: array of integer;
begin
  if n = 0 then
    exit;
  
  // Find maximum and minimum values
  maxVal := arr[0];
  minVal := arr[0];
  
  for i := 1 to n - 1 do
  begin
    if arr[i] > maxVal then
      maxVal := arr[i];
    if arr[i] < minVal then
      minVal := arr[i];
  end;
  
  // Create count array with range (max - min + 1)
  SetLength(count, maxVal - minVal + 1);
  SetLength(output, n);
  
  // Initialize count array to 0
  for i := 0 to High(count) do
    count[i] := 0;
  
  // Count occurrences of each element
  for i := 0 to n - 1 do
    count[arr[i] - minVal] := count[arr[i] - minVal] + 1;
  
  // Calculate cumulative count
  for i := 1 to High(count) do
    count[i] := count[i] + count[i - 1];
  
  // Build output array
  for i := n - 1 downto 0 do
  begin
    output[count[arr[i] - minVal] - 1] := arr[i];
    count[arr[i] - minVal] := count[arr[i] - minVal] - 1;
  end;
  
  // Copy sorted elements back to original array
  for i := 0 to n - 1 do
    arr[i] := output[i];
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
  arr: array[0..9] of integer = (4, 2, 2, 8, 3, 3, 1, 7, 5, 6);
  n: integer;

begin
  n := 10;
  
  Writeln('Original array:');
  PrintArray(arr, n);
  
  CountingSort(arr, n);
  
  Writeln('Sorted array:');
  PrintArray(arr, n);
  
  Readln;
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

1. **Handles Negative Numbers**: The algorithm works with negative numbers by using the minimum value as an offset
2. **Stable Sorting**: Maintains the relative order of equal elements
3. **Time Complexity**: O(n + k) where n is the number of elements and k is the range of input
4. **Space Complexity**: O(k) for the count array
5. **Input Validation**: Includes basic error checking for empty arrays

## How it Works:

1. **Find Range**: Determine the minimum and maximum values to establish the range
2. **Count Elements**: Count occurrences of each element using an auxiliary count array
3. **Calculate Cumulative Count**: Modify count array to store actual positions
4. **Build Output**: Place elements in their correct positions in the output array
5. **Copy Back**: Transfer sorted elements back to the original array

This implementation is efficient for sorting integers when the range of possible values is not significantly larger than the number of elements to be sorted.

