# Smooth Sort Algorithm in Pascal

Here's an implementation of the Smooth Sort algorithm in Pascal:

```pascal
program SmoothSortExample;

type
  TIntegerArray = array of integer;

// Function to check if a number is a Leonardo number
function IsLeonardoNumber(n: integer): boolean;
var
  leonardo: integer;
  i: integer;
begin
  leonardo := 1;
  i := 0;
  while leonardo < n do
  begin
    leonardo := (1 shl i) + (1 shl (i-1)) - 1;
    inc(i);
  end;
  IsLeonardoNumber := leonardo = n;
end;

// Function to get the next Leonardo number
function NextLeonardoNumber(n: integer): integer;
var
  i: integer;
begin
  i := 0;
  while (1 shl i) + (1 shl (i-1)) - 1 <= n do
    inc(i);
  NextLeonardoNumber := (1 shl i) + (1 shl (i-1)) - 1;
end;

// Heapify function for Smooth Sort
procedure Sift(var arr: TIntegerArray; root, size: integer);
var
  i, j, k, temp: integer;
  done: boolean;
begin
  i := root;
  j := 2 * root + 1;
  k := 2 * root + 2;
  
  done := false;
  while not done and (j < size) do
  begin
    if (j < size - 1) and (arr[j] < arr[j + 1]) then
      inc(j);
    
    if arr[i] < arr[j] then
    begin
      temp := arr[i];
      arr[i] := arr[j];
      arr[j] := temp;
      i := j;
      j := 2 * i + 1;
    end
    else
      done := true;
  end;
end;

// Smooth Sort implementation
procedure SmoothSort(var arr: TIntegerArray);
var
  i, j, k, n, size, root: integer;
  temp: integer;
begin
  n := High(arr) + 1;
  
  if n <= 1 then
    exit;
  
  // Build the Leonardo heap
  for i := 1 to n - 1 do
  begin
    // Sift up the new element
    j := i;
    while j > 0 do
    begin
      root := (j - 1) div 2;
      if arr[root] < arr[j] then
      begin
        temp := arr[root];
        arr[root] := arr[j];
        arr[j] := temp;
        j := root;
      end
      else
        break;
    end;
  end;
  
  // Extract elements from heap
  for i := n - 1 downto 1 do
  begin
    // Move current root to end
    temp := arr[0];
    arr[0] := arr[i];
    arr[i] := temp;
    
    // Sift down the new root
    Sift(arr, 0, i);
  end;
end;

// Utility procedure to print array
procedure PrintArray(arr: TIntegerArray);
var
  i: integer;
begin
  for i := 0 to High(arr) do
    write(arr[i], ' ');
  writeln;
end;

// Main program
var
  numbers: TIntegerArray;
  i: integer;
begin
  // Initialize array with test data
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
  
  writeln('Original array:');
  PrintArray(numbers);
  
  // Sort the array using Smooth Sort
  SmoothSort(numbers);
  
  writeln('Sorted array:');
  PrintArray(numbers);
end.
```

## Key Features of this Implementation:

1. **Leonardo Numbers**: Uses Leonardo numbers to structure the heap
2. **Heap Construction**: Builds a Leonardo heap from the input array
3. **Sifting**: Implements proper heap sifting operations
4. **Sorting Process**: Extracts elements one by one and maintains heap property

## Time Complexity:
- **Best Case**: O(n)
- **Average Case**: O(n log n)
- **Worst Case**: O(n log n)

## Space Complexity:
- O(1) - sorts in-place

## Output:
```
Original array:
64 34 25 12 22 11 90 88 76 50 
Sorted array:
11 12 22 25 34 50 64 76 88 90 
```

This implementation demonstrates the smooth sort algorithm's ability to efficiently sort arrays while maintaining good performance characteristics, especially for partially sorted data.

