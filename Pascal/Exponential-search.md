# Exponential Search Algorithm in Pascal

```pascal
program ExponentialSearchExample;

{$APPTYPE CONSOLE}

uses
  SysUtils;

// Binary search helper function
function BinarySearch(arr: array of Integer; left, right, target: Integer): Integer;
var
  mid: Integer;
begin
  while left <= right do
  begin
    mid := left + (right - left) div 2;
    
    if arr[mid] = target then
    begin
      BinarySearch := mid;
      Exit;
    end;
    
    if arr[mid] < target then
      left := mid + 1
    else
      right := mid - 1;
  end;
  
  BinarySearch := -1; // Element not found
end;

// Exponential search function
function ExponentialSearch(arr: array of Integer; size, target: Integer): Integer;
var
  i: Integer;
begin
  // If target is first element
  if arr[0] = target then
  begin
    ExponentialSearch := 0;
    Exit;
  end;
  
  // Find range where element is present
  i := 1;
  while (i < size) and (arr[i] <= target) do
    i := i * 2;
  
  // Call binary search on the found range
  ExponentialSearch := BinarySearch(arr, i div 2, Min(i, size - 1), target);
end;

// Helper function to print array
procedure PrintArray(arr: array of Integer; size: Integer);
var
  i: Integer;
begin
  Write('Array: [');
  for i := 0 to size - 1 do
  begin
    Write(arr[i]);
    if i < size - 1 then
      Write(', ');
  end;
  Writeln(']');
end;

var
  arr: array[0..9] of Integer = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
  size, target, result: Integer;

begin
  size := 10;
  
  // Print the array
  PrintArray(arr, size);
  
  // Test cases
  target := 7;
  result := ExponentialSearch(arr, size, target);
  if result <> -1 then
    Writeln('Element ', target, ' found at index: ', result)
  else
    Writeln('Element ', target, ' not found');
  
  target := 1;
  result := ExponentialSearch(arr, size, target);
  if result <> -1 then
    Writeln('Element ', target, ' found at index: ', result)
  else
    Writeln('Element ', target, ' not found');
  
  target := 10;
  result := ExponentialSearch(arr, size, target);
  if result <> -1 then
    Writeln('Element ', target, ' found at index: ', result)
  else
    Writeln('Element ', target, ' not found');
  
  target := 15;
  result := ExponentialSearch(arr, size, target);
  if result <> -1 then
    Writeln('Element ', target, ' found at index: ', result)
  else
    Writeln('Element ', target, ' not found');
  
  Readln;
end.
```

## Output:
```
Array: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
Element 7 found at index: 6
Element 1 found at index: 0
Element 10 found at index: 9
Element 15 not found
```

## Algorithm Explanation:

The exponential search algorithm works in two phases:

1. **Finding the range**: Start with index 1 and keep doubling the index until we find an element greater than or equal to the target
2. **Binary search**: Perform binary search in the identified range

## Time Complexity:
- **Best case**: O(1) - when the target is the first element
- **Average case**: O(log n)
- **Worst case**: O(log n)

## Space Complexity:
- O(1) - only using a constant amount of extra space

## Key Features:
- Works well for unbounded or very large arrays
- More efficient than binary search when the target element is closer to the beginning
- Combines the benefits of linear and binary search approaches

