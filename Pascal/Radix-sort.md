# Radix Sort Algorithm in Pascal

Here's an implementation of the Radix Sort algorithm in Pascal:

```pascal
program RadixSortExample;

{$APPTYPE CONSOLE}

uses
  SysUtils;

// Function to get the maximum value in the array
function getMax(arr: array of integer; n: integer): integer;
var
  i, max: integer;
begin
  max := arr[0];
  for i := 1 to n - 1 do
  begin
    if arr[i] > max then
      max := arr[i];
  end;
  getMax := max;
end;

// Function to do counting sort based on the digit represented by exp
procedure countingSort(var arr: array of integer; n, exp: integer);
var
  i, j: integer;
  output: array of integer;
  count: array[0..9] of integer;
begin
  // Initialize count array
  for i := 0 to 9 do
    count[i] := 0;
  
  // Store count of occurrences
  for i := 0 to n - 1 do
    count[(arr[i] div exp) mod 10] := count[(arr[i] div exp) mod 10] + 1;
  
  // Change count[i] to actual position
  for i := 1 to 9 do
    count[i] := count[i] + count[i - 1];
  
  // Build the output array
  SetLength(output, n);
  for i := n - 1 downto 0 do
  begin
    j := (arr[i] div exp) mod 10;
    output[count[j] - 1] := arr[i];
    count[j] := count[j] - 1;
  end;
  
  // Copy the output array to arr
  for i := 0 to n - 1 do
    arr[i] := output[i];
end;

// Main Radix Sort function
procedure radixSort(var arr: array of integer; n: integer);
var
  max, exp: integer;
begin
  if n <= 1 then
    exit;
  
  // Find the maximum number to know number of digits
  max := getMax(arr, n);
  
  // Do counting sort for every digit
  for exp := 1 to max do
  begin
    if (max div exp) > 0 then
      countingSort(arr, n, exp)
    else
      break;
  end;
end;

// Function to print array elements
procedure printArray(arr: array of integer; n: integer);
var
  i: integer;
begin
  for i := 0 to n - 1 do
    Write(arr[i], ' ');
  Writeln;
end;

// Main program
var
  arr: array of integer;
  n, i: integer;

begin
  // Example array to sort
  arr := [170, 45, 75, 90, 2, 802, 24, 66];
  n := Length(arr);
  
  Writeln('Original array:');
  printArray(arr, n);
  
  // Sort the array
  radixSort(arr, n);
  
  Writeln('Sorted array:');
  printArray(arr, n);
  
  Readln;
end.
```

## Output:
```
Original array:
170 45 75 90 2 802 24 66 
Sorted array:
2 24 45 66 75 90 170 802 
```

## How it works:

1. **Find Maximum**: First, we find the maximum number to determine how many digits we need to process
2. **Counting Sort**: For each digit position (units, tens, hundreds, etc.), we perform counting sort
3. **Process All Digits**: We continue this process for all digit positions until we've processed all digits of the maximum number

## Time Complexity:
- **Best Case**: O(d × n)
- **Average Case**: O(d × n)  
- **Worst Case**: O(d × n)

Where:
- n = number of elements
- d = number of digits in the maximum number

## Space Complexity:
- O(n + k) where k is the range of input (0-9 for decimal numbers)

This implementation handles positive integers and sorts them in ascending order using the Radix Sort algorithm.

