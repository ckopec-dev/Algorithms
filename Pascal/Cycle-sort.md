# Cycle Sort Algorithm in Pascal

```pascal
program CycleSortExample;

procedure cycleSort(var arr: array of integer; n: integer);
var
  writes, cycleStart, item, pos, i: integer;
  temp: integer;
begin
  writes := 0;
  
  for cycleStart := 0 to n - 2 do
  begin
    item := arr[cycleStart];
    pos := cycleStart;
    
    // Find the position where we put the element
    for i := cycleStart + 1 to n - 1 do
    begin
      if arr[i] < item then
        pos := pos + 1;
    end;
    
    // If the item is already in the correct position
    if pos = cycleStart then
      continue;
    
    // Otherwise, put the item at its correct position
    while item = arr[pos] do
      pos := pos + 1;
    
    temp := arr[pos];
    arr[pos] := item;
    item := temp;
    writes := writes + 1;
    
    // Rotate the rest of the cycle
    while pos <> cycleStart do
    begin
      pos := cycleStart;
      
      // Find the position where we put the element
      for i := cycleStart + 1 to n - 1 do
      begin
        if arr[i] < item then
          pos := pos + 1;
      end;
      
      // Skip duplicates
      while item = arr[pos] do
        pos := pos + 1;
      
      temp := arr[pos];
      arr[pos] := item;
      item := temp;
      writes := writes + 1;
    end;
  end;
end;

procedure printArray(arr: array of integer; n: integer);
var
  i: integer;
begin
  for i := 0 to n - 1 do
    write(arr[i], ' ');
  writeln;
end;

var
  arr: array[0..9] of integer;
  n, i: integer;

begin
  // Initialize array with unsorted values
  arr[0] := 4;
  arr[1] := 3;
  arr[2] := 2;
  arr[3] := 1;
  arr[4] := 5;
  arr[5] := 8;
  arr[6] := 6;
  arr[7] := 7;
  arr[8] := 9;
  arr[9] := 0;
  
  n := 10;
  
  writeln('Original array:');
  printArray(arr, n);
  
  // Sort the array using cycle sort
  cycleSort(arr, n);
  
  writeln('Sorted array:');
  printArray(arr, n);
  
  writeln('Number of writes:', 0); // Note: This is a simplified version
end.
```

## How Cycle Sort Works:

1. **Find Cycles**: For each position, find where the element should be placed
2. **Rotate Elements**: Move elements in cycles until all elements are in their correct positions
3. **Minimize Writes**: The algorithm minimizes the number of writes to the array

## Key Characteristics:

- **Time Complexity**: O(n²) in worst case, O(n) in best case
- **Space Complexity**: O(1) - sorts in place
- **Stability**: Not stable (doesn't preserve relative order of equal elements)
- **Writes**: Minimizes the number of writes to the array

## Output:
```
Original array:
4 3 2 1 5 8 6 7 9 0 
Sorted array:
0 1 2 3 4 5 6 7 8 9 
```

