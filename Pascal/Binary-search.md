```pascal
program BinarySearchExample;

{$APPTYPE CONSOLE}

uses
  SysUtils;

function BinarySearch(arr: array of integer; target: integer): integer;
var
  left, right, middle: integer;
begin
  left := 0;
  right := High(arr);
  
  while left <= right do
  begin
    middle := (left + right) div 2;
    
    if arr[middle] = target then
    begin
      BinarySearch := middle;  // Return index if found
      exit;
    end
    else if arr[middle] < target then
      left := middle + 1
    else
      right := middle - 1;
  end;
  
  BinarySearch := -1;  // Return -1 if not found
end;

procedure PrintArray(arr: array of integer);
var
  i: integer;
begin
  for i := 0 to High(arr) do
    Write(arr[i], ' ');
  Writeln;
end;

var
  numbers: array[0..9] of integer = (1, 3, 5, 7, 9, 11, 13, 15, 17, 19);
  target, result: integer;

begin
  Writeln('Binary Search Example');
  Writeln('====================');
  
  Writeln('Array:');
  PrintArray(numbers);
  
  // Test cases
  target := 7;
  result := BinarySearch(numbers, target);
  if result <> -1 then
    Writeln('Found ', target, ' at index ', result)
  else
    Writeln(target, ' not found in array');
  
  target := 15;
  result := BinarySearch(numbers, target);
  if result <> -1 then
    Writeln('Found ', target, ' at index ', result)
  else
    Writeln(target, ' not found in array');
  
  target := 4;
  result := BinarySearch(numbers, target);
  if result <> -1 then
    Writeln('Found ', target, ' at index ', result)
  else
    Writeln(target, ' not found in array');
  
  Readln;
end.
```

**Output:**
```
Binary Search Example
====================
Array:
1 3 5 7 9 11 13 15 17 19 
Found 7 at index 3
Found 15 at index 7
4 not found in array
```

