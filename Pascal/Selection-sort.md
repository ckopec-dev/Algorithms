```pascal
program SelectionSortExample;

procedure selectionSort(var arr: array of integer; n: integer);
var
  i, j, minIndex, temp: integer;
begin
  for i := 0 to n - 2 do
  begin
    minIndex := i;
    for j := i + 1 to n - 1 do
    begin
      if arr[j] < arr[minIndex] then
        minIndex := j;
    end;
    
    // Swap elements
    if minIndex <> i then
    begin
      temp := arr[i];
      arr[i] := arr[minIndex];
      arr[minIndex] := temp;
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
  numbers: array[0..9] of integer;
  i, n: integer;

begin
  // Initialize array
  numbers[0] := 64;
  numbers[1] := 34;
  numbers[2] := 25;
  numbers[3] := 12;
  numbers[4] := 22;
  numbers[5] := 11;
  numbers[6] := 90;
  numbers[7] := 88;
  numbers[8] := 76;
  numbers[9] := 50;
  
  n := 10;
  
  writeln('Original array:');
  printArray(numbers, n);
  
  // Sort the array
  selectionSort(numbers, n);
  
  writeln('Sorted array:');
  printArray(numbers, n);
end.
```

**Output:**
```
Original array:
64 34 25 12 22 11 90 88 76 50 
Sorted array:
11 12 22 25 34 50 64 76 88 90 
```

**How Selection Sort works in this example:**

1. **Find the minimum element** in the entire array (11) and swap it with the first element
2. **Find the minimum element** in the remaining unsorted portion (12) and swap it with the second element
3. **Continue this process** until the entire array is sorted
4. **Time complexity:** O(n²)
5. **Space complexity:** O(1) - sorts in place

The algorithm works by repeatedly finding the smallest element from the unsorted portion and placing it at the beginning of the sorted portion.

