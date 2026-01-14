# Exponential Search Algorithm in Pascal

```pascal
program ExponentialSearchExample;

{$APPTYPE CONSOLE}

uses
  SysUtils;

// Binary search helper function
function BinarySearch(arr: array of Integer; left, right, target: Integer): Integer;
begin
  while left <= right do
  begin
    var mid := left + (right - left) div 2;
    
    if arr[mid] = target then
      Exit(mid)
    else if arr[mid] < target then
      left := mid + 1
    else
      right := mid - 1;
  end;
  
  Exit(-1); // Element not found
end;

// Exponential search function
function ExponentialSearch(arr: array of Integer; target: Integer): Integer;
var
  i: Integer;
  n: Integer;
begin
  n := Length(arr);
  
  // If target is first element
  if (n > 0) and (arr[0] = target) then
  begin
    Exit(0);
  end;
  
  // Find range where element is present
  i := 1;
  while (i < n) and (arr[i] <= target) do
  begin
    i := i * 2;
  end;
  
  // Call binary search on the found range
  Exit(BinarySearch(arr, i div 2, Min(i, n - 1), target));
end;

// Helper function to print array
procedure PrintArray(arr: array of Integer);
var
  i: Integer;
begin
  Write('Array: [');
  for i := 0 to High(arr) do
  begin
    Write(arr[i]);
    if i < High(arr) then
      Write(', ');
  end;
  Write(']');
  Writeln;
end;

// Main program
var
  arr: array[0..9] of Integer = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
  target: Integer;
  result: Integer;
begin
  Writeln('Exponential Search Algorithm Example');
  Writeln('====================================');
  
  PrintArray(arr);
  
  // Test cases
  target := 7;
  result := ExponentialSearch(arr, target);
  if result <> -1 then
    Writeln('Element ', target, ' found at index: ', result)
  else
    Writeln('Element ', target, ' not found');
  
  target := 1;
  result := ExponentialSearch(arr, target);
  if result <> -1 then
    Writeln('Element ', target, ' found at index: ', result)
  else
    Writeln('Element ', target, ' not found');
  
  target := 10;
  result := ExponentialSearch(arr, target);
  if result <> -1 then
    Writeln('Element ', target, ' found at index: ', result)
  else
    Writeln('Element ', target, ' not found');
  
  target := 15;
  result := ExponentialSearch(arr, target);
  if result <> -1 then
    Writeln('Element ', target, ' found at index: ', result)
  else
    Writeln('Element ', target, ' not found');
  
  Readln;
end.
```

## How it works:

1. **Initial Check**: First checks if the target is at index 0
2. **Range Finding**: Expands the search range exponentially (1, 2, 4, 8, ...) until finding a range where the target might be
3. **Binary Search**: Performs binary search within the identified range

## Time Complexity:
- **Best Case**: O(1) - when element is at index 0
- **Average/Worst Case**: O(log n) - where n is the number of elements

## Space Complexity:
- O(1) - only using a constant amount of extra space

## Sample Output:
```
Exponential Search Algorithm Example
====================================
Array: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
Element 7 found at index: 6
Element 1 found at index: 0
Element 10 found at index: 9
Element 15 not found
```

