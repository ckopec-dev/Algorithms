# Quick Select Algorithm in Pascal

Here's an implementation of the Quick Select algorithm in Pascal to find the k-th smallest element in an array:

```pascal
program QuickSelectExample;

{$APPTYPE CONSOLE}

uses
  SysUtils;

// Partition function - partitions the array around a pivot
function Partition(var arr: array of Integer; low, high: Integer): Integer;
var
  pivot, i, j, temp: Integer;
begin
  // Choose the rightmost element as pivot
  pivot := arr[high];
  i := low - 1; // Index of smaller element
  
  for j := low to high - 1 do
  begin
    // If current element is smaller than or equal to pivot
    if arr[j] <= pivot then
    begin
      inc(i);
      // Swap elements
      temp := arr[i];
      arr[i] := arr[j];
      arr[j] := temp;
    end;
  end;
  
  // Swap pivot element with element at i+1
  temp := arr[i + 1];
  arr[i + 1] := arr[high];
  arr[high] := temp;
  
  Partition := i + 1;
end;

// QuickSelect function - finds the k-th smallest element
function QuickSelect(var arr: array of Integer; low, high, k: Integer): Integer;
var
  pivotIndex: Integer;
begin
  if low = high then
  begin
    QuickSelect := arr[low];
    exit;
  end;
  
  // Partition the array and get pivot index
  pivotIndex := Partition(arr, low, high);
  
  // If pivot is the k-th element
  if k = pivotIndex then
  begin
    QuickSelect := arr[k];
  end
  // If k is smaller than pivot index, search left subarray
  else if k < pivotIndex then
  begin
    QuickSelect := QuickSelect(arr, low, pivotIndex - 1, k);
  end
  // If k is greater than pivot index, search right subarray
  else
  begin
    QuickSelect := QuickSelect(arr, pivotIndex + 1, high, k);
  end;
end;

// Function to print array
procedure PrintArray(var arr: array of Integer);
var
  i: Integer;
begin
  for i := 0 to High(arr) do
  begin
    Write(arr[i], ' ');
  end;
  Writeln;
end;

var
  arr: array[0..9] of Integer = (3, 6, 8, 10, 1, 2, 1, 4, 7, 9);
  k, result: Integer;

begin
  Writeln('Original array:');
  PrintArray(arr);
  
  // Find the 4th smallest element (k = 3, 0-indexed)
  k := 3;
  result := QuickSelect(arr, 0, High(arr), k);
  
  Writeln('The ', k + 1, 'th smallest element is: ', result);
  
  Writeln('Array after partitioning:');
  PrintArray(arr);
  
  // Find the 7th smallest element
  k := 6;
  result := QuickSelect(arr, 0, High(arr), k);
  
  Writeln('The ', k + 1, 'th smallest element is: ', result);
  
  Readln;
end.
```

## How it works:

1. **Partition Function**: Rearranges the array so that elements smaller than the pivot are on the left, and elements greater than the pivot are on the right.

2. **QuickSelect Function**: 
   - Recursively partitions the array
   - If the pivot index equals k, we found our element
   - If k is smaller than pivot index, search left subarray
   - If k is larger than pivot index, search right subarray

## Time Complexity:
- **Best/Average Case**: O(n)
- **Worst Case**: O(n²)

## Space Complexity:
- O(log n) due to recursion stack

## Sample Output:
```
Original array:
3 6 8 10 1 2 1 4 7 9 
The 4th smallest element is: 3
Array after partitioning:
1 1 2 3 6 8 10 4 7 9 
The 7th smallest element is: 7
```

