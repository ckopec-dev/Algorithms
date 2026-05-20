# Quick Sort Algorithm in Pascal

```pascal
program QuickSortExample;

type
  TArray = array[1..100] of integer;

procedure QuickSort(var arr: TArray; low, high: integer);
var
  i, j, pivot, temp: integer;
begin
  if low < high then
  begin
    // Partition the array
    pivot := arr[high];
    i := low - 1;
    
    for j := low to high - 1 do
    begin
      if arr[j] <= pivot then
      begin
        inc(i);
        // Swap elements
        temp := arr[i];
        arr[i] := arr[j];
        arr[j] := temp;
      end;
    end;
    
    // Place pivot in correct position
    temp := arr[i + 1];
    arr[i + 1] := arr[high];
    arr[high] := temp;
    
    // Recursively sort elements before and after partition
    QuickSort(arr, low, i);
    QuickSort(arr, i + 2, high);
  end;
end;

procedure PrintArray(var arr: TArray; size: integer);
var
  i: integer;
begin
  for i := 1 to size do
    write(arr[i], ' ');
  writeln;
end;

var
  numbers: TArray;
  size, i: integer;

begin
  // Initialize array with unsorted values
  numbers[1] := 64;
  numbers[2] := 34;
  numbers[3] := 25;
  numbers[4] := 12;
  numbers[5] := 22;
  numbers[6] := 11;
  numbers[7] := 90;
  size := 7;
  
  writeln('Original array:');
  PrintArray(numbers, size);
  
  // Sort the array
  QuickSort(numbers, 1, size);
  
  writeln('Sorted array:');
  PrintArray(numbers, size);
end.
```

## Output:
```
Original array:
64 34 25 12 22 11 90 
Sorted array:
11 12 22 25 34 64 90 
```

## How it works:

1. **Partitioning**: Choose a pivot element (in this case, the last element)
2. **Rearrangement**: Move all elements smaller than the pivot to its left, and larger elements to its right
3. **Recursion**: Apply the same process to the sub-arrays on both sides of the pivot
4. **Base case**: When low ≥ high, the sub-array is already sorted

The algorithm has an average time complexity of O(n log n) and worst-case complexity of O(n²).

