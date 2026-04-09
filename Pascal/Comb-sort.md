```pascal
program CombSortExample;

{$APPTYPE CONSOLE}

uses
  SysUtils;

procedure CombSort(var arr: array of Integer);
var
  gap, i, temp: Integer;
  swapped: Boolean;
begin
  gap := Length(arr);
  swapped := True;
  
  while (gap > 1) or swapped do
  begin
    // Calculate next gap using shrink factor of 1.3
    gap := Trunc(gap / 1.3);
    if gap < 1 then
      gap := 1;
      
    swapped := False;
    
    // Compare elements with current gap
    for i := 0 to Length(arr) - gap - 1 do
    begin
      if arr[i] > arr[i + gap] then
      begin
        // Swap elements
        temp := arr[i];
        arr[i] := arr[i + gap];
        arr[i + gap] := temp;
        swapped := True;
      end;
    end;
  end;
end;

procedure PrintArray(arr: array of Integer);
var
  i: Integer;
begin
  for i := 0 to Length(arr) - 1 do
    Write(arr[i], ' ');
  Writeln;
end;

var
  numbers: array[0..9] of Integer = (64, 34, 25, 12, 22, 11, 90, 88, 76, 50);
  i: Integer;

begin
  Writeln('Original array:');
  PrintArray(numbers);
  
  CombSort(numbers);
  
  Writeln('Sorted array:');
  PrintArray(numbers);
  
  Readln;
end.
```

**Output:**
```
Original array:
64 34 25 12 22 11 90 88 76 50 
Sorted array:
11 12 22 25 34 50 64 76 88 90 
```

**Explanation:**

The CombSort algorithm is an improved version of BubbleSort that uses a "gap" between elements being compared. Key features:

1. **Gap Calculation**: Uses a shrink factor of 1.3 to reduce the gap
2. **Initial Gap**: Starts with the full array length
3. **Swapping**: Compares elements at positions i and i+gap
4. **Termination**: Continues until gap becomes 1 and no swaps occur

**Time Complexity:**
- Best case: O(n log n)
- Average case: O(n²/2^p) where p is the number of increments
- Worst case: O(n²)

**Space Complexity:** O(1) - sorts in place

The algorithm is more efficient than BubbleSort because it eliminates "turtles" (small values near the end) more quickly by using larger gaps initially.

