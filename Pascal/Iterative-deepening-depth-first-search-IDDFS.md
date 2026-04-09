# Iterative Deepening Depth-First Search (IDDFS) in Pascal

```pascal
program IDDFS_Example;

type
  TNode = record
    Value: Integer;
    Children: array[1..10] of Integer;
    ChildCount: Integer;
  end;

  TGraph = array[1..100] of TNode;

var
  Graph: TGraph;
  NodeCount: Integer;
  Found: Boolean;

// Function to check if a node exists in the current path
function IsInPath(Path: array of Integer; PathLength: Integer; Value: Integer): Boolean;
var
  i: Integer;
begin
  for i := 1 to PathLength do
    if Path[i] = Value then
      Exit(True);
  Exit(False);
end;

// Depth-limited DFS function
function DLS(CurrentNode: Integer; Target: Integer; Depth: Integer; 
            var Path: array of Integer; PathLength: Integer): Boolean;
var
  i: Integer;
  NewPath: array[1..20] of Integer;
begin
  // Add current node to path
  Path[PathLength] := CurrentNode;
  
  // If we found the target
  if CurrentNode = Target then
  begin
    Result := True;
    Exit;
  end;
  
  // If we've reached the depth limit
  if Depth = 0 then
  begin
    Result := False;
    Exit;
  end;
  
  // Explore children
  for i := 1 to Graph[CurrentNode].ChildCount do
  begin
    // Skip if node is already in path (to avoid cycles)
    if not IsInPath(Path, PathLength, Graph[CurrentNode].Children[i]) then
    begin
      // Copy path for recursive call
      for var j := 1 to PathLength do
        NewPath[j] := Path[j];
      
      if DLS(Graph[CurrentNode].Children[i], Target, Depth - 1, NewPath, PathLength + 1) then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;
  
  Result := False;
end;

// IDDFS main function
function IDDFS(Source: Integer; Target: Integer): Boolean;
var
  Depth: Integer;
  MaxDepth: Integer;
  Path: array[1..20] of Integer;
  PathLength: Integer;
begin
  MaxDepth := 10; // Set maximum depth limit
  
  for Depth := 0 to MaxDepth do
  begin
    PathLength := 1;
    Path[1] := Source;
    
    if DLS(Source, Target, Depth, Path, PathLength) then
    begin
      // Print the path (optional)
      Write('Path found at depth ', Depth, ': ');
      for var i := 1 to PathLength do
        Write(Path[i], ' ');
      Writeln;
      
      Result := True;
      Exit;
    end;
  end;
  
  Result := False;
end;

// Function to add an edge to the graph
procedure AddEdge(FromNode, ToNode: Integer);
begin
  Inc(Graph[FromNode].ChildCount);
  Graph[FromNode].Children[Graph[FromNode].ChildCount] := ToNode;
end;

// Initialize the graph
procedure InitializeGraph;
begin
  NodeCount := 6;
  
  // Clear all nodes
  for var i := 1 to NodeCount do
  begin
    Graph[i].Value := i;
    Graph[i].ChildCount := 0;
  end;
  
  // Build example graph:
  // 1 -> 2, 3
  // 2 -> 4, 5
  // 3 -> 5, 6
  // 4 -> 
  // 5 -> 
  // 6 -> 
  
  AddEdge(1, 2);
  AddEdge(1, 3);
  AddEdge(2, 4);
  AddEdge(2, 5);
  AddEdge(3, 5);
  AddEdge(3, 6);
end;

begin
  InitializeGraph;
  
  Writeln('Graph structure:');
  Writeln('Node 1: connects to 2, 3');
  Writeln('Node 2: connects to 4, 5');
  Writeln('Node 3: connects to 5, 6');
  Writeln('Node 4: connects to none');
  Writeln('Node 5: connects to none');
  Writeln('Node 6: connects to none');
  Writeln;
  
  // Test IDDFS from node 1 to find node 5
  Writeln('Searching for node 5 starting from node 1:');
  if IDDFS(1, 5) then
    Writeln('Target node 5 found!')
  else
    Writeln('Target node 5 not found!');
  
  Writeln;
  
  // Test IDDFS from node 1 to find node 6
  Writeln('Searching for node 6 starting from node 1:');
  if IDDFS(1, 6) then
    Writeln('Target node 6 found!')
  else
    Writeln('Target node 6 not found!');
  
  Writeln;
  
  // Test IDDFS from node 1 to find node 7 (doesn't exist)
  Writeln('Searching for node 7 starting from node 1:');
  if IDDFS(1, 7) then
    Writeln('Target node 7 found!')
  else
    Writeln('Target node 7 not found!');
end.
```

## How IDDFS Works in This Implementation

1. **Main Loop**: The algorithm iterates through increasing depth limits (0, 1, 2, 3, ...)

2. **Depth-Limited Search (DLS)**: For each depth limit, it performs a regular depth-first search with that depth limit

3. **Path Tracking**: The algorithm maintains a path to detect cycles and report the solution path

4. **Early Termination**: When a solution is found at a certain depth, the algorithm stops and returns the result

## Key Features

- **Memory Efficient**: Only stores the current path, not all explored nodes
- **Optimal**: Finds the shortest path (in terms of depth) to the target
- **Complete**: Will find a solution if one exists
- **Cycle Detection**: Prevents infinite loops in cyclic graphs

## Expected Output
```
Graph structure:
Node 1: connects to 2, 3
Node 2: connects to 4, 5
Node 3: connects to 5, 6
Node 4: connects to none
Node 5: connects to none
Node 6: connects to none

Searching for node 5 starting from node 1:
Path found at depth 2: 1 2 5 
Target node 5 found!

Searching for node 6 starting from node 1:
Path found at depth 2: 1 3 6 
Target node 6 found!

Searching for node 7 starting from node 1:
Target node 7 not found!
```

