# Heap Sort Algorithm in Pascal

Here's a complete implementation of the Heap Sort algorithm in Pascal:

```pascal
program HeapSortExample;

{$APPTYPE CONSOLE}

uses
  SysUtils;

// Function to heapify a subtree rooted at index i
procedure Heapify(var arr: array of integer; n, i: integer);
var
  largest, left, right: integer;
  temp: integer;
begin
  largest := i;        // Initialize largest as root
  left := 2 * i + 1;   // Left child
  right := 2 * i + 2;  // Right child

  // If left child exists and is greater than root
  if (left < n) and (arr[left] > arr[largest]) then
    largest := left;

  // If right child exists and is greater than largest so far
  if (right < n) and (arr[right] > arr[largest]) then
    largest := right;

  // If largest is not root
  if largest <> i then
  begin
    // Swap root with largest element
    temp := arr[i];
    arr[i] := arr[largest];
    arr[largest] := temp;

    // Recursively heapify the affected sub-tree
    Heapify(arr, n, largest);
  end;
end;

// Main heap sort function
procedure HeapSort(var arr: array of integer; n: integer);
var
  i, temp: integer;
begin
  // Build heap (rearrange array)
  for i := n div 2 - 1 downto 0 do
    Heapify(arr, n, i);

  // One by one extract elements from heap
  for i := n - 1 downto 1 do
  begin
    // Move current root to end
    temp := arr[0];
    arr[0] := arr[i];
    arr[i] := temp;

    // Call heapify on the reduced heap
    Heapify(arr, i, 0);
  end;
end;

// Procedure to print array elements
procedure PrintArray(arr: array of integer; n: integer);
var
  i: integer;
begin
  for i := 0 to n - 1 do
    Write(arr[i], ' ');
  Writeln;
end;

// Main program
var
  arr: array[0..9] of integer = (64, 34, 25, 12, 22, 11, 90, 88, 76, 50);
  n: integer;
begin
  n := Length(arr);
  
  Writeln('Original array:');
  PrintArray(arr, n);
  
  HeapSort(arr, n);
  
  Writeln('Sorted array:');
  PrintArray(arr, n);
  
  Readln;
end.
```

## How it works:

1. **Heapify Function**: Maintains the heap property by ensuring that the largest element is at the root
2. **HeapSort Function**: 
   - First builds a max heap from the input array
   - Then repeatedly extracts the maximum element and rebuilds the heap
3. **Time Complexity**: O(n log n)
4. **Space Complexity**: O(1) (in-place sorting)

## Sample Output:
```
Original array:
64 34 25 12 22 11 90 88 76 50 
Sorted array:
11 12 22 25 34 50 64 76 88 90 
```

This implementation demonstrates the classic heap sort algorithm using a max heap structure to efficiently sort an array in ascending order.

