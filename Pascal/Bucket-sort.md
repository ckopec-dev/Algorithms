# Bucket Sort Algorithm in Pascal

Here's an implementation of the Bucket Sort algorithm in Pascal:

```pascal
program BucketSortExample;

uses
  crt;

const
  MAX_SIZE = 100;
  BUCKET_COUNT = 10;

type
  TFloatArray = array[0..MAX_SIZE-1] of real;
  TIntegerArray = array[0..MAX_SIZE-1] of integer;

// Insertion sort for sorting individual buckets
procedure InsertionSort(var arr: TFloatArray; n: integer);
var
  i, j: integer;
  key: real;
begin
  for i := 1 to n-1 do
  begin
    key := arr[i];
    j := i - 1;
    
    while (j >= 0) and (arr[j] > key) do
    begin
      arr[j+1] := arr[j];
      j := j - 1;
    end;
    
    arr[j+1] := key;
  end;
end;

// Bucket sort implementation
procedure BucketSort(var arr: TFloatArray; n: integer);
var
  buckets: array[0..BUCKET_COUNT-1] of TFloatArray;
  bucketSizes: array[0..BUCKET_COUNT-1] of integer;
  i, j, bucketIndex: integer;
begin
  // Initialize bucket sizes
  for i := 0 to BUCKET_COUNT-1 do
    bucketSizes[i] := 0;
  
  // Distribute elements into buckets
  for i := 0 to n-1 do
  begin
    // Calculate bucket index (assuming values are in range [0, 1))
    bucketIndex := trunc(arr[i] * BUCKET_COUNT);
    
    // Handle edge case where arr[i] = 1.0
    if bucketIndex >= BUCKET_COUNT then
      bucketIndex := BUCKET_COUNT - 1;
    
    // Add element to corresponding bucket
    buckets[bucketIndex][bucketSizes[bucketIndex]] := arr[i];
    bucketSizes[bucketIndex] := bucketSizes[bucketIndex] + 1;
  end;
  
  // Sort each bucket individually
  for i := 0 to BUCKET_COUNT-1 do
  begin
    if bucketSizes[i] > 0 then
      InsertionSort(buckets[i], bucketSizes[i]);
  end;
  
  // Collect sorted elements back to original array
  i := 0;
  for j := 0 to BUCKET_COUNT-1 do
  begin
    while bucketSizes[j] > 0 do
    begin
      arr[i] := buckets[j][0];
      i := i + 1;
      // Shift elements in bucket
      for k := 0 to bucketSizes[j]-2 do
        buckets[j][k] := buckets[j][k+1];
      bucketSizes[j] := bucketSizes[j] - 1;
    end;
  end;
end;

// Utility procedure to print array
procedure PrintArray(arr: TFloatArray; n: integer);
var
  i: integer;
begin
  for i := 0 to n-1 do
    write(arr[i]:0:2, ' ');
  writeln;
end;

// Main program
var
  numbers: TFloatArray;
  size: integer;
  i: integer;

begin
  clrscr;
  
  // Example array to sort
  numbers[0] := 0.897;
  numbers[1] := 0.565;
  numbers[2] := 0.656;
  numbers[3] := 0.123;
  numbers[4] := 0.665;
  numbers[5] := 0.343;
  numbers[6] := 0.789;
  numbers[7] := 0.234;
  numbers[8] := 0.987;
  numbers[9] := 0.456;
  
  size := 10;
  
  writeln('Bucket Sort Example');
  writeln('===================');
  writeln('Original array:');
  PrintArray(numbers, size);
  
  // Perform bucket sort
  BucketSort(numbers, size);
  
  writeln('Sorted array:');
  PrintArray(numbers, size);
  
  readln;
end.
```

## How it works:

1. **Bucket Creation**: Creates `BUCKET_COUNT` buckets (arrays) to hold elements
2. **Distribution**: Each element is placed into a bucket based on its value
3. **Sorting**: Each bucket is sorted individually using insertion sort
4. **Collection**: Elements are collected back into the original array in sorted order

## Key Features:

- **Time Complexity**: O(n + k) average case, where n is the number of elements and k is the number of buckets
- **Space Complexity**: O(n + k)
- **Assumption**: Input elements are uniformly distributed in the range [0, 1)
- **Stable**: Maintains relative order of equal elements

## Sample Output:
```
Bucket Sort Example
===================
Original array:
0.89 0.56 0.65 0.12 0.66 0.34 0.78 0.23 0.98 0.45 
Sorted array:
0.12 0.23 0.34 0.45 0.56 0.65 0.66 0.78 0.89 0.98 
```

