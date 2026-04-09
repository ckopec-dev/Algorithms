```pascal
program SkipListSearch;

type
  PNode = ^TNode;
  TNode = record
    value: integer;
    forward: array[0..15] of PNode;  // Maximum level of 16
  end;

  TSkipList = record
    header: PNode;
    maxLevel: integer;
  end;

// Function to create a new node
function CreateNode(value: integer; level: integer): PNode;
var
  newNode: PNode;
begin
  new(newNode);
  newNode^.value := value;
  for var i := 0 to level do
    newNode^.forward[i] := nil;
  CreateNode := newNode;
end;

// Function to generate random level for new node
function RandomLevel(maxLevel: integer): integer;
var
  level: integer;
begin
  level := 0;
  while (random(2) = 1) and (level < maxLevel - 1) do
    level := level + 1;
  RandomLevel := level;
end;

// Skip list search algorithm
function SkipListSearch(skipList: TSkipList; searchValue: integer): boolean;
var
  current: PNode;
  level: integer;
begin
  current := skipList.header;
  
  // Start from the highest level and move down
  for level := skipList.maxLevel - 1 downto 0 do
  begin
    // Move forward while the next node's value is less than search value
    while (current^.forward[level] <> nil) and 
          (current^.forward[level]^.value < searchValue) do
    begin
      current := current^.forward[level];
    end;
  end;
  
  // Move one step forward to get the actual node
  current := current^.forward[0];
  
  // Check if we found the value
  if (current <> nil) and (current^.value = searchValue) then
  begin
    SkipListSearch := true;
  end
  else
  begin
    SkipListSearch := false;
  end;
end;

// Function to insert a value into skip list (for demonstration)
procedure SkipListInsert(var skipList: TSkipList; value: integer);
var
  update: array[0..15] of PNode;
  current: PNode;
  level: integer;
  newNode: PNode;
begin
  current := skipList.header;
  
  // Find the position to insert
  for level := skipList.maxLevel - 1 downto 0 do
  begin
    while (current^.forward[level] <> nil) and 
          (current^.forward[level]^.value < value) do
    begin
      current := current^.forward[level];
    end;
    update[level] := current;
  end;
  
  // Create new node
  level := RandomLevel(skipList.maxLevel);
  newNode := CreateNode(value, level);
  
  // Insert the new node
  for level := 0 to level do
  begin
    newNode^.forward[level] := update[level]^.forward[level];
    update[level]^.forward[level] := newNode;
  end;
end;

// Main program
var
  skipList: TSkipList;
  searchValue: integer;
  found: boolean;

begin
  // Initialize skip list
  new(skipList.header);
  skipList.header^.value := -1;  // Sentinel node
  skipList.maxLevel := 16;
  
  // Initialize forward pointers
  for var i := 0 to 15 do
    skipList.header^.forward[i] := nil;
  
  // Insert some values
  SkipListInsert(skipList, 3);
  SkipListInsert(skipList, 6);
  SkipListInsert(skipList, 7);
  SkipListInsert(skipList, 9);
  SkipListInsert(skipList, 12);
  SkipListInsert(skipList, 19);
  SkipListInsert(skipList, 17);
  SkipListInsert(skipList, 26);
  
  // Search for values
  searchValue := 19;
  found := SkipListSearch(skipList, searchValue);
  writeln('Searching for ', searchValue, ': ', 
          ifthen(found, 'Found', 'Not found'));
  
  searchValue := 5;
  found := SkipListSearch(skipList, searchValue);
  writeln('Searching for ', searchValue, ': ', 
          ifthen(found, 'Found', 'Not found'));
  
  searchValue := 26;
  found := SkipListSearch(skipList, searchValue);
  writeln('Searching for ', searchValue, ': ', 
          ifthen(found, 'Found', 'Not found'));
  
  searchValue := 1;
  found := SkipListSearch(skipList, searchValue);
  writeln('Searching for ', searchValue, ': ', 
          ifthen(found, 'Found', 'Not found'));
end.
```

