# Linear Search Algorithm in Pascal

Here's an example of a linear search algorithm implemented in Pascal:

```pascal
program LinearSearchExample;

{$APPTYPE CONSOLE}

uses
  SysUtils;

// Linear search function
function LinearSearch(arr: array of integer; size: integer; target: integer): integer;
var
  i: integer;
begin
  // Search through each element
  for i := 0 to size - 1 do
  begin
    if arr[i] = target then
    begin
      LinearSearch := i;  // Return index if found
      exit;
    end;
  end;
  
  LinearSearch := -1;  // Return -1 if not found
end;

// Procedure to print array elements
procedure PrintArray(arr: array of integer; size: integer);
var
  i: integer;
begin
  for i := 0 to size - 1 do
    write(arr[i], ' ');
  writeln;
end;

var
  numbers: array[0..9] of integer = (10, 25, 3, 47, 15, 8, 92, 33, 56, 12);
  targetNumber, result: integer;
  arraySize: integer;

begin
  arraySize := 10;
  
  writeln('Linear Search Example');
  writeln('====================');
  
  writeln('Array elements:');
  PrintArray(numbers, arraySize);
  
  // Test with existing element
  targetNumber := 47;
  writeln('Searching for: ', targetNumber);
  result := LinearSearch(numbers, arraySize, targetNumber);
  
  if result <> -1 then
    writeln('Element found at index: ', result)
  else
    writeln('Element not found');
  
  // Test with non-existing element
  targetNumber := 100;
  writeln('Searching for: ', targetNumber);
  result := LinearSearch(numbers, arraySize, targetNumber);
  
  if result <> -1 then
    writeln('Element found at index: ', result)
  else
    writeln('Element not found');
  
  writeln;
  writeln('Press any key to exit...');
  readln;
end.
```

## Output:
```
Linear Search Example
====================
Array elements:
10 25 3 47 15 8 92 33 56 12 
Searching for: 47
Element found at index: 3
Searching for: 100
Element not found

Press any key to exit...
```

## How it works:

1. **Function Definition**: `LinearSearch` takes an array, its size, and the target value to search for
2. **Iteration**: The function loops through each element of the array from index 0 to size-1
3. **Comparison**: Each element is compared with the target value
4. **Return Value**: 
   - Returns the index if the element is found
   - Returns -1 if the element is not found
5. **Time Complexity**: O(n) where n is the number of elements in the array

This implementation demonstrates the basic linear search algorithm in Pascal, which checks each element sequentially until the target is found or the entire array is traversed.

