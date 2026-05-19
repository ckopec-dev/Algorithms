```pascal
program RandomizedQuickSort;

uses
  SysUtils, Math;

// Function to swap two elements
procedure Swap(var a, b: integer);
var
  temp: integer;
begin
  temp := a;
  a := b;
  b := temp;
end;

// Partition function with random pivot
function RandomizedPartition(var arr: array of integer; low, high: integer): integer;
var
  randomIndex, pivot: integer;
begin
  // Generate random index between low and high
  randomIndex := Random(high - low + 1) + low;
  
  // Swap random element with last element
  Swap(arr[randomIndex], arr[high]);
  
  // Use last element as pivot (now random element is at end)
  pivot := arr[high];
  
  // Partition the array
  var i := low - 1;
  var j := low;
  
  while j < high do
  begin
    if arr[j] <= pivot then
    begin
      inc(i);
      Swap(arr[i], arr[j]);
    end;
    inc(j);
  end;
  
  // Place pivot in correct position
  Swap(arr[i + 1], arr[high]);
  RandomizedPartition := i + 1;
end;

// Randomized QuickSort procedure
procedure RandomizedQuickSort(var arr: array of integer; low, high: integer);
begin
  if low < high then
  begin
    // Get partition index
    var pi := RandomizedPartition(arr, low, high);
    
    // Recursively sort elements before and after partition
    RandomizedQuickSort(arr, low, pi - 1);
    RandomizedQuickSort(arr, pi + 1, high);
  end;
end;

// Helper procedure to print array
procedure PrintArray(arr: array of integer);
var
  i: integer;
begin
  for i := 0 to High(arr) do
    Write(arr[i], ' ');
  Writeln;
end;

// Main program
var
  arr: array[0..9] of integer;
  i: integer;
begin
  // Initialize random number generator
  Randomize;
  
  // Create test array
  arr[0] := 10;
  arr[1] := 7;
  arr[2] := 8;
  arr[3] := 9;
  arr[4] := 1;
  arr[5] := 5;
  arr[6] := 3;
  arr[7] := 6;
  arr[8] := 2;
  arr[9] := 4;
  
  Writeln('Original array:');
  PrintArray(arr);
  
  // Sort the array
  RandomizedQuickSort(arr, 0, High(arr));
  
  Writeln('Sorted array:');
  PrintArray(arr);
end.
```

This Pascal implementation of Randomized QuickSort includes:

1. **Swap procedure** - To exchange two array elements
2. **RandomizedPartition function** - Selects a random pivot element and partitions the array
3. **RandomizedQuickSort procedure** - The main recursive sorting function
4. **PrintArray procedure** - Utility to display array contents
5. **Main program** - Demonstrates usage with a sample array

Key features of this implementation:
- Uses `Randomize` to seed the random number generator
- Selects a random pivot index between `low` and `high`
- Swaps the random element with the last element before partitioning
- Implements standard QuickSort partitioning logic
- Handles edge cases with proper boundary conditions

The algorithm has an average time complexity of O(n log n) and a worst-case complexity of O(n²), but the randomization helps avoid worst-case scenarios in practice.

