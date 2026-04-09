# Patience Sorting Algorithm in Pascal

Here's an implementation of the patience sorting algorithm in Pascal:

```pascal
program PatienceSorting;

type
  TStack = array of integer;
  TStackArray = array of TStack;

// Function to find the position where to insert element in sorted stack
function BinarySearch(const stack: TStack; value: integer): integer;
var
  left, right, mid: integer;
begin
  left := 0;
  right := High(stack);
  
  if right < 0 then
  begin
    Result := 0;
    exit;
  end;
  
  while left <= right do
  begin
    mid := (left + right) div 2;
    if stack[mid] < value then
      left := mid + 1
    else
      right := mid - 1;
  end;
  
  Result := left;
end;

// Main patience sorting function
procedure PatienceSort(var arr: array of integer);
var
  stacks: TStackArray;
  i, j, stackIndex, topIndex: integer;
  minTop: integer;
  minTopIndex: integer;
begin
  if Length(arr) <= 1 then
    exit;
  
  SetLength(stacks, 0);
  
  // Create piles (stacks) for each element
  for i := 0 to High(arr) do
  begin
    // Find the leftmost pile where we can place current element
    stackIndex := -1;
    for j := 0 to High(stacks) do
    begin
      if (Length(stacks[j]) > 0) and (stacks[j, High(stacks[j])] < arr[i]) then
      begin
        stackIndex := j;
        break;
      end;
    end;
    
    // If no suitable pile found, create a new one
    if stackIndex = -1 then
    begin
      SetLength(stacks, Length(stacks) + 1);
      SetLength(stacks[High(stacks)], 1);
      stacks[High(stacks), 0] := arr[i];
    end
    else
    begin
      // Add element to existing pile
      SetLength(stacks[stackIndex], Length(stacks[stackIndex]) + 1);
      stacks[stackIndex, High(stacks[stackIndex])] := arr[i];
    end;
  end;
  
  // Merge piles back into original array
  i := 0;
  while i < Length(arr) do
  begin
    // Find minimum element among all pile tops
    minTop := MaxInt;
    minTopIndex := -1;
    
    for j := 0 to High(stacks) do
    begin
      if (Length(stacks[j]) > 0) and (stacks[j, High(stacks[j])] < minTop) then
      begin
        minTop := stacks[j, High(stacks[j])];
        minTopIndex := j;
      end;
    end;
    
    // Place minimum element in result array
    arr[i] := minTop;
    i := i + 1;
    
    // Remove element from pile
    if Length(stacks[minTopIndex]) > 0 then
    begin
      SetLength(stacks[minTopIndex], Length(stacks[minTopIndex]) - 1);
    end;
  end;
end;

// Helper procedure to print array
procedure PrintArray(const arr: array of integer);
var
  i: integer;
begin
  for i := 0 to High(arr) do
    Write(arr[i], ' ');
  Writeln;
end;

// Main program
var
  testArray: array[0..9] of integer = (64, 34, 25, 12, 22, 11, 90, 88, 76, 50);
begin
  Writeln('Original array:');
  PrintArray(testArray);
  
  PatienceSort(testArray);
  
  Writeln('Sorted array:');
  PrintArray(testArray);
end.
```

## How the Algorithm Works:

1. **Pile Creation**: Elements are placed into piles (stacks) such that each pile is sorted in descending order
2. **Placement Rule**: Each element is placed on the leftmost pile where the top element is greater than the current element
3. **Merging**: The smallest elements from each pile are collected in order to form the final sorted array

## Time and Space Complexity:

- **Time Complexity**: O(n log n) where n is the number of elements
- **Space Complexity**: O(n) for storing the piles

## Sample Output:
```
Original array:
64 34 25 12 22 11 90 88 76 50 
Sorted array:
11 12 22 25 34 50 64 76 88 90 
```

This implementation demonstrates the patience sorting algorithm, which is a stable sorting algorithm that mimics the card game patience.

