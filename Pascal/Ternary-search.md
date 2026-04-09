```pascal
program TernarySearchExample;

function ternarySearch(arr: array of integer; left, right, target: integer): integer;
var
  mid1, mid2: integer;
begin
  if right >= left then
  begin
    // Calculate two mid points
    mid1 := left + (right - left) div 3;
    mid2 := right - (right - left) div 3;
    
    // If target is at either mid point
    if arr[mid1] = target then
    begin
      ternarySearch := mid1;
      exit;
    end;
    
    if arr[mid2] = target then
    begin
      ternarySearch := mid2;
      exit;
    end;
    
    // If target is in the left third
    if target < arr[mid1] then
    begin
      ternarySearch := ternarySearch(arr, left, mid1 - 1, target);
    end
    // If target is in the right third
    else if target > arr[mid2] then
    begin
      ternarySearch := ternarySearch(arr, mid2 + 1, right, target);
    end
    // If target is in the middle third
    else
    begin
      ternarySearch := ternarySearch(arr, mid1 + 1, mid2 - 1, target);
    end;
  end
  else
  begin
    // Target not found
    ternarySearch := -1;
  end;
end;

procedure printArray(arr: array of integer);
var
  i: integer;
begin
  for i := 0 to High(arr) do
    write(arr[i], ' ');
  writeln;
end;

var
  numbers: array[0..9] of integer = (1, 3, 5, 7, 9, 11, 13, 15, 17, 19);
  target, result: integer;
begin
  writeln('Array: ');
  printArray(numbers);
  
  target := 7;
  result := ternarySearch(numbers, 0, High(numbers), target);
  
  if result <> -1 then
    writeln('Element ', target, ' found at index: ', result)
  else
    writeln('Element ', target, ' not found');
  
  target := 12;
  result := ternarySearch(numbers, 0, High(numbers), target);
  
  if result <> -1 then
    writeln('Element ', target, ' found at index: ', result)
  else
    writeln('Element ', target, ' not found');
end.
```

