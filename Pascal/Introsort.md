# Introsort Algorithm in Pascal

Here's an implementation of the Introsort (Introspective Sort) algorithm in Pascal:

```pascal
program IntrosortExample;

uses
  SysUtils;

type
  TIntArray = array of integer;

// Partition function for quicksort
function Partition(var arr: TIntArray; low, high: integer): integer;
var
  pivot, i, j, temp: integer;
begin
  pivot := arr[high];
  i := low - 1;
  
  for j := low to high - 1 do
  begin
    if arr[j] <= pivot then
    begin
      inc(i);
      temp := arr[i];
      arr[i] := arr[j];
      arr[j] := temp;
    end;
  end;
  
  temp := arr[i + 1];
  arr[i + 1] := arr[high];
  arr[high] := temp;
  
  Partition := i + 1;
end;

// Heapify function for heapsort
procedure Heapify(var arr: TIntArray; n, i: integer);
var
  largest, l, r, temp: integer;
begin
  largest := i;
  l := 2 * i + 1;
  r := 2 * i + 2;
  
  if (l < n) and (arr[l] > arr[largest]) then
    largest := l;
    
  if (r < n) and (arr[r] > arr[largest]) then
    largest := r;
    
  if largest <> i then
  begin
    temp := arr[i];
    arr[i] := arr[largest];
    arr[largest] := temp;
    Heapify(arr, n, largest);
  end;
end;

// Heapsort implementation
procedure HeapSort(var arr: TIntArray; low, high: integer);
var
  i, temp: integer;
begin
  for i := (high - low) div 2 downto 0 do
    Heapify(arr, high - low + 1, i);
    
  for i := high - low downto 1 do
  begin
    temp := arr[low + i];
    arr[low + i] := arr[low];
    arr[low] := temp;
    Heapify(arr, i, 0);
  end;
end;

// Insertion sort for small arrays
procedure InsertionSort(var arr: TIntArray; low, high: integer);
var
  i, j, key: integer;
begin
  for i := low + 1 to high do
  begin
    key := arr[i];
    j := i - 1;
    
    while (j >= low) and (arr[j] > key) do
    begin
      arr[j + 1] := arr[j];
      dec(j);
    end;
    
    arr[j + 1] := key;
  end;
end;

// Calculate maximum depth for introsort
function MaxDepth(n: integer): integer;
var
  depth: integer;
begin
  depth := 0;
  while n > 0 do
  begin
    n := n shr 1;
    inc(depth);
  end;
  MaxDepth := depth * 2;
end;

// Introsort main function
procedure Introsort(var arr: TIntArray; low, high, maxDepth: integer);
var
  pivot: integer;
begin
  // Use insertion sort for small arrays
  if (high - low + 1) <= 10 then
  begin
    InsertionSort(arr, low, high);
    exit;
  end;
  
  // Use heapsort if maxDepth is reached
  if maxDepth = 0 then
  begin
    HeapSort(arr, low, high);
    exit;
  end;
  
  // Use quicksort
  pivot := Partition(arr, low, high);
  Introsort(arr, low, pivot - 1, maxDepth - 1);
  Introsort(arr, pivot + 1, high, maxDepth - 1);
end;

// Wrapper procedure to start introsort
procedure StartIntrosort(var arr: TIntArray);
var
  maxDepth: integer;
begin
  if Length(arr) > 0 then
  begin
    maxDepth := MaxDepth(Length(arr));
    Introsort(arr, 0, Length(arr) - 1, maxDepth);
  end;
end;

// Utility procedure to print array
procedure PrintArray(arr: TIntArray);
var
  i: integer;
begin
  for i := 0 to High(arr) do
    Write(arr[i], ' ');
  Writeln;
end;

// Main program
var
  numbers: TIntArray;
  i: integer;

begin
  // Initialize test array
  SetLength(numbers, 10);
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
  
  Writeln('Original array:');
  PrintArray(numbers);
  
  // Sort the array using Introsort
  StartIntrosort(numbers);
  
  Writeln('Sorted array:');
  PrintArray(numbers);
  
  Readln;
end.
```

## Key Features of this Introsort Implementation:

1. **Hybrid Approach**: Combines quicksort, heapsort, and insertion sort
2. **Depth Control**: Uses a maximum depth limit to switch to heapsort
3. **Insertion Sort**: For small subarrays (≤ 10 elements)
4. **Heap Sort**: Triggered when maximum depth is reached
5. **Quick Sort**: Main sorting algorithm for normal cases

## How it works:

1. **Initial Check**: If array size ≤ 10, use insertion sort
2. **Depth Management**: Calculate maximum recursion depth based on array size
3. **Quick Sort**: Use quicksort with partitioning
4. **Switch to Heapsort**: If recursion depth exceeds limit, switch to heapsort
5. **Final Result**: Efficiently sorts the array with O(n log n) worst-case complexity

The algorithm guarantees O(n log n) time complexity while maintaining good average-case performance.

