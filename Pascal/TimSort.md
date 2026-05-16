# TimSort Algorithm in Pascal

```pascal
program TimSortExample;

{$APPTYPE CONSOLE}

uses
  SysUtils;

const
  MIN_MERGE = 32;

type
  TIntArray = array of integer;

// Merge two sorted subarrays
procedure Merge(var arr: TIntArray; left, mid, right: integer);
var
  i, j, k: integer;
  n1, n2: integer;
  leftArr, rightArr: TIntArray;
begin
  n1 := mid - left + 1;
  n2 := right - mid;
  
  SetLength(leftArr, n1);
  SetLength(rightArr, n2);
  
  // Copy data to temporary arrays
  for i := 0 to n1 - 1 do
    leftArr[i] := arr[left + i];
    
  for j := 0 to n2 - 1 do
    rightArr[j] := arr[mid + 1 + j];
  
  // Merge the temporary arrays back
  i := 0;
  j := 0;
  k := left;
  
  while (i < n1) and (j < n2) do
  begin
    if leftArr[i] <= rightArr[j] then
    begin
      arr[k] := leftArr[i];
      i := i + 1;
    end
    else
    begin
      arr[k] := rightArr[j];
      j := j + 1;
    end;
    k := k + 1;
  end;
  
  // Copy remaining elements
  while i < n1 do
  begin
    arr[k] := leftArr[i];
    i := i + 1;
    k := k + 1;
  end;
  
  while j < n2 do
  begin
    arr[k] := rightArr[j];
    j := j + 1;
    k := k + 1;
  end;
end;

// Insertion sort for small arrays
procedure InsertionSort(var arr: TIntArray; left, right: integer);
var
  i, j, key: integer;
begin
  for i := left + 1 to right do
  begin
    key := arr[i];
    j := i - 1;
    
    while (j >= left) and (arr[j] > key) do
    begin
      arr[j + 1] := arr[j];
      j := j - 1;
    end;
    
    arr[j + 1] := key;
  end;
end;

// TimSort implementation
procedure TimSort(var arr: TIntArray);
var
  n, i, left, mid, right: integer;
begin
  n := Length(arr);
  
  if n < 2 then
    exit;
  
  // Sort small chunks using insertion sort
  for i := 0 to n - 1 do
  begin
    if (i mod MIN_MERGE) = 0 then
    begin
      InsertionSort(arr, i, Min(i + MIN_MERGE - 1, n - 1));
    end;
  end;
  
  // Merge subarrays of increasing sizes
  i := MIN_MERGE;
  while i < n do
  begin
    left := 0;
    while left < n - 1 do
    begin
      mid := left + i - 1;
      right := Min(left + i * 2 - 1, n - 1);
      
      if mid < right then
        Merge(arr, left, mid, right);
      
      left := left + i * 2;
    end;
    i := i * 2;
  end;
end;

// Utility procedure to print array
procedure PrintArray(arr: TIntArray);
var
  i: integer;
begin
  for i := 0 to Length(arr) - 1 do
    Write(arr[i], ' ');
  Writeln;
end;

// Main program
var
  arr: TIntArray;
  i: integer;
begin
  // Example array to sort
  SetLength(arr, 10);
  arr[0] := 5;
  arr[1] := 2;
  arr[2] := 8;
  arr[3] := 1;
  arr[4] := 9;
  arr[5] := 3;
  arr[6] := 7;
  arr[7] := 4;
  arr[8] := 6;
  arr[9] := 0;
  
  Writeln('Original array:');
  PrintArray(arr);
  
  TimSort(arr);
  
  Writeln('Sorted array:');
  PrintArray(arr);
  
  Readln;
end.
```

## How TimSort Works in Pascal

This implementation demonstrates the TimSort algorithm in Pascal with the following key components:

### Key Features:
1. **MIN_MERGE Constant**: Sets the minimum size for chunks (32)
2. **Insertion Sort**: Used for small subarrays (≤ 32 elements)
3. **Merge Function**: Combines two sorted subarrays
4. **TimSort Procedure**: Main sorting algorithm that combines insertion sort and merge

### Algorithm Steps:
1. **Chunk Division**: Divide array into chunks of size MIN_MERGE
2. **Insertion Sort**: Sort each chunk individually using insertion sort
3. **Merge Process**: Merge chunks of increasing sizes (32, 64, 128, etc.)
4. **Final Merge**: Continue merging until entire array is sorted

### Time Complexity:
- **Best Case**: O(n) - when array is already sorted
- **Average Case**: O(n log n)
- **Worst Case**: O(n log n)

### Space Complexity:
- O(n) - due to temporary arrays used in merging

The algorithm is particularly efficient for real-world data that often contains partially sorted sequences, making it one of the most practical sorting algorithms in use today.

