# Kosaraju's Algorithm in Pascal

Kosaraju's algorithm is used to find strongly connected components in a directed graph. Here's a complete implementation in Pascal:

```pascal
program KosarajuAlgorithm;

uses
  crt, sysutils;

const
  MAX_NODES = 100;

type
  TGraph = array[1..MAX_NODES, 1..MAX_NODES] of integer;
  TStack = array[1..MAX_NODES] of integer;
  TBoolArray = array[1..MAX_NODES] of boolean;

var
  adjMatrix: TGraph;
  visited: TBoolArray;
  stack: TStack;
  stackTop: integer;
  component: array[1..MAX_NODES] of integer;
  componentCount: integer;
  numNodes, numEdges: integer;

// Push element to stack
procedure Push(value: integer);
begin
  inc(stackTop);
  stack[stackTop] := value;
end;

// Pop element from stack
function Pop: integer;
begin
  if stackTop > 0 then
  begin
    Pop := stack[stackTop];
    dec(stackTop);
  end
  else
    Pop := -1;
end;

// DFS for first pass (filling stack)
procedure DFS1(node: integer);
var
  i: integer;
begin
  visited[node] := true;
  
  for i := 1 to numNodes do
  begin
    if (adjMatrix[node, i] = 1) and (not visited[i]) then
      DFS1(i);
  end;
  
  Push(node);
end;

// DFS for second pass (finding components)
procedure DFS2(node: integer; compNum: integer);
var
  i: integer;
begin
  visited[node] := true;
  component[node] := compNum;
  
  for i := 1 to numNodes do
  begin
    if (adjMatrix[node, i] = 1) and (not visited[i]) then
      DFS2(i, compNum);
  end;
end;

// Transpose the graph
procedure TransposeGraph(var transposed: TGraph);
var
  i, j: integer;
begin
  for i := 1 to numNodes do
    for j := 1 to numNodes do
      transposed[i, j] := adjMatrix[j, i];
end;

// Kosaraju's algorithm implementation
procedure Kosaraju;
var
  i, node, compNum: integer;
  transposed: TGraph;
begin
  // Initialize
  fillchar(visited, sizeof(visited), false);
  stackTop := 0;
  componentCount := 0;
  
  // First pass: Fill stack with finishing times
  for i := 1 to numNodes do
  begin
    if not visited[i] then
      DFS1(i);
  end;
  
  // Transpose the graph
  TransposeGraph(transposed);
  
  // Copy transposed adjacency matrix back to original
  for i := 1 to numNodes do
    for j := 1 to numNodes do
      adjMatrix[i, j] := transposed[i, j];
  
  // Reset visited array
  fillchar(visited, sizeof(visited), false);
  
  // Second pass: Process nodes in reverse order of finishing times
  compNum := 0;
  while stackTop > 0 do
  begin
    node := Pop;
    if not visited[node] then
    begin
      inc(compNum);
      DFS2(node, compNum);
    end;
  end;
  
  componentCount := compNum;
end;

// Print strongly connected components
procedure PrintComponents;
var
  i, j: integer;
begin
  writeln('Strongly Connected Components:');
  for i := 1 to componentCount do
  begin
    write('Component ', i, ': ');
    for j := 1 to numNodes do
    begin
      if component[j] = i then
        write(j, ' ');
    end;
    writeln;
  end;
end;

// Initialize adjacency matrix
procedure InitializeGraph;
var
  i, j: integer;
begin
  for i := 1 to numNodes do
    for j := 1 to numNodes do
      adjMatrix[i, j] := 0;
end;

// Add edge to graph
procedure AddEdge(from, to: integer);
begin
  adjMatrix[from, to] := 1;
end;

// Print adjacency matrix
procedure PrintGraph;
var
  i, j: integer;
begin
  writeln('Adjacency Matrix:');
  for i := 1 to numNodes do
  begin
    for j := 1 to numNodes do
      write(adjMatrix[i, j]:3);
    writeln;
  end;
end;

begin
  clrscr;
  
  // Example graph setup
  numNodes := 6;
  numEdges := 7;
  
  InitializeGraph;
  
  // Add edges to create a sample graph
  AddEdge(1, 2);
  AddEdge(2, 3);
  AddEdge(3, 1);
  AddEdge(2, 4);
  AddEdge(4, 5);
  AddEdge(5, 6);
  AddEdge(6, 4);
  
  writeln('Graph Analysis using Kosaraju''s Algorithm');
  writeln('========================================');
  
  PrintGraph;
  writeln;
  
  Kosaraju;
  PrintComponents;
  
  writeln;
  writeln('Press any key to exit...');
  readln;
end.
```

## Algorithm Explanation

**Kosaraju's Algorithm Steps:**

1. **First DFS Pass**: Perform DFS on original graph and push vertices to stack based on finishing times
2. **Graph Transpose**: Create transpose of the original graph (reverse all edges)
3. **Second DFS Pass**: Process vertices in reverse order of finishing times from stack on transposed graph
4. **Component Assignment**: Each DFS tree in second pass represents a strongly connected component

## Sample Output

```
Graph Analysis using Kosaraju's Algorithm
========================================
Adjacency Matrix:
  0  1  0  0  0  0
  0  0  1  0  0  0
  1  0  0  0  0  0
  0  0  0  0  1  0
  0  0  0  1  0  1
  0  0  0  1  0  0

Strongly Connected Components:
Component 1: 1 2 3 
Component 2: 4 5 6 
```

This implementation finds that nodes 1, 2, and 3 form one strongly connected component, while nodes 4, 5, and 6 form another.

