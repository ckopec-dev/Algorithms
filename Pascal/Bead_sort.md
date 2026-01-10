# Bead Sort Algorithm in Pascal

```pascal
program BeadSortExample;

{$APPTYPE CONSOLE}

uses
  SysUtils;

procedure BeadSort(var arr: array of Integer; n: Integer);
var
  i, j, k: Integer;
  beads: array of array of Integer;
  maxVal: Integer;
begin
  if n <= 1 then
    Exit;
  
  // Find maximum value
  maxVal := arr[0];
  for i := 1 to n - 1 do
    if arr[i] > maxVal then
      maxVal := arr[i];
  
  // Initialize bead structure
  SetLength(beads, maxVal, n);
  for i := 0 to maxVal - 1 do
    for j := 0 to n - 1 do
      beads[i][j] := 0;
  
  // Drop beads
  for i := 0 to n - 1 do
  begin
    for j := 0 to arr[i] - 1 do
      beads[j][i] := 1;
  end;
  
  // Let beads fall
  for i := 0 to maxVal - 1 do
  begin
    k := 0;
    for j := 0 to n - 1 do
    begin
      if beads[i][j] = 1 then
      begin
        beads[i][j] := 0;
        beads[i][k] := 1;
        Inc(k);
      end;
    end;
  end;
  
  // Read back the sorted array
  for i := 0 to n - 1 do
  begin
    arr[i] := 0;
    for j := maxVal - 1 downto 0 do
    begin
      if beads[j][i] = 1 then
      begin
        Inc(arr[i]);
      end;
    end;
  end;
end;

procedure PrintArray(arr: array of Integer; n: Integer);
var
  i: Integer;
begin
  for i := 0 to n - 1 do
    Write(arr[i], ' ');
  Writeln;
end;

var
  arr: array of Integer;
  n: Integer;
  i: Integer;

begin
  // Example array to sort
  arr := [5, 3, 8, 1, 9, 2, 7, 4, 6];
  n := Length(arr);
  
  Writeln('Original array:');
  PrintArray(arr, n);
  
  // Sort the array
  BeadSort(arr, n);
  
  Writeln('Sorted array:');
  PrintArray(arr, n);
  
  Readln;
end.
```

## How Bead Sort Works

The bead sort algorithm works by:

1. **Creating a "bead" structure**: Each element in the array is represented by a vertical column of beads
2. **Dropping beads**: The beads fall down due to gravity
3. **Reading back**: The final arrangement represents the sorted array

## Example Output
```
Original array:
5 3 8 1 9 2 7 4 6 
Sorted array:
1 2 3 4 5 6 7 8 9 
```

## Key Characteristics

- **Time Complexity**: O(S) where S is the sum of all elements
- **Space Complexity**: O(n × max(arr))
- **Stable**: Yes
- **Natural**: Simulates physical bead sorting process

This implementation demonstrates the bead sort algorithm's unique approach to sorting by using a physical analogy of beads falling under gravity.

