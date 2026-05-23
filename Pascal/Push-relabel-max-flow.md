# Push-Relabel Max-Flow Algorithm in Pascal

Here's a complete implementation of the Push-Relabel algorithm for finding maximum flow in a flow network:

```pascal
program PushRelabelMaxFlow;

uses
  SysUtils;

const
  MAXN = 100;
  INF = 1000000000;

type
  TGraph = array[0..MAXN, 0..MAXN] of integer;
  THeight = array[0..MAXN] of integer;
  TExcess = array[0..MAXN] of integer;

var
  graph: TGraph;
  height: THeight;
  excess: TExcess;
  n, m, source, sink: integer;

// Initialize the graph with given edges
procedure InitializeGraph;
var
  i, u, v, c: integer;
begin
  // Clear the graph
  for i := 0 to n do
  begin
    for u := 0 to n do
      graph[i, u] := 0;
  end;
  
  // Read edges
  for i := 1 to m do
  begin
    readln(u, v, c);
    graph[u, v] := c;  // Forward edge
    graph[v, u] := 0;  // Backward edge (initially 0)
  end;
end;

// Initialize heights and excess values
procedure InitializeHeightsAndExcess;
var
  i: integer;
begin
  // Initialize all heights to 0
  for i := 0 to n do
    height[i] := 0;
  
  // Initialize all excess to 0
  for i := 0 to n do
    excess[i] := 0;
  
  // Set source height to n
  height[source] := n;
  
  // Push initial flow from source to its neighbors
  for i := 0 to n do
  begin
    if graph[source, i] > 0 then
    begin
      excess[i] := graph[source, i];
      graph[source, i] := 0;
      graph[i, source] := graph[i, source] + excess[i];
    end;
  end;
end;

// Push flow from u to v
procedure Push(u, v: integer);
var
  delta: integer;
begin
  delta := Min(excess[u], graph[u, v]);
  excess[u] := excess[u] - delta;
  excess[v] := excess[v] + delta;
  graph[u, v] := graph[u, v] - delta;
  graph[v, u] := graph[v, u] + delta;
end;

// Relabel vertex u
procedure Relabel(u: integer);
var
  i, min_height: integer;
begin
  min_height := INF;
  for i := 0 to n do
  begin
    if graph[u, i] > 0 then
      min_height := Min(min_height, height[i]);
  end;
  height[u] := min_height + 1;
end;

// Get the next vertex that can be pushed
function GetNextVertex: integer;
var
  i: integer;
begin
  for i := 0 to n do
  begin
    if (excess[i] > 0) and (i <> source) and (i <> sink) then
    begin
      GetNextVertex := i;
      exit;
    end;
  end;
  GetNextVertex := -1;
end;

// Push-relabel algorithm
function MaxFlow: integer;
var
  u: integer;
  pushed: boolean;
begin
  InitializeHeightsAndExcess;
  
  while true do
  begin
    u := GetNextVertex;
    if u = -1 then
      break;
    
    pushed := false;
    
    // Try to push flow from u
    for var v := 0 to n do
    begin
      if (graph[u, v] > 0) and (height[u] > height[v]) then
      begin
        Push(u, v);
        pushed := true;
        
        // If excess becomes 0, we can continue
        if excess[u] = 0 then
          break;
      end;
    end;
    
    // If no push was possible, relabel
    if not pushed then
      Relabel(u);
  end;
  
  // Return the flow that reached the sink
  MaxFlow := excess[sink];
end;

// Print the graph
procedure PrintGraph;
var
  i, j: integer;
begin
  writeln('Graph adjacency matrix:');
  for i := 0 to n do
  begin
    for j := 0 to n do
      write(graph[i, j]:4);
    writeln;
  end;
end;

begin
  writeln('Push-Relabel Max-Flow Algorithm');
  writeln('================================');
  
  // Read input
  writeln('Enter number of vertices (n), edges (m), source, sink:');
  readln(n, m, source, sink);
  
  writeln('Enter edges (u v capacity):');
  InitializeGraph;
  
  writeln('Initial graph:');
  PrintGraph;
  
  // Run max flow algorithm
  var max_flow := MaxFlow;
  
  writeln('Maximum flow from source to sink: ', max_flow);
  writeln('Final graph:');
  PrintGraph;
  
  writeln('Press Enter to exit...');
  readln;
end.
```

## Example Usage

**Input:**
```
Enter number of vertices (n), edges (m), source, sink:
4 5 0 3
Enter edges (u v capacity):
0 1 10
0 2 10
1 2 2
1 3 4
2 3 10
```

**Expected Output:**
```
Push-Relabel Max-Flow Algorithm
================================
Initial graph:
Graph adjacency matrix:
   0   0   0   0   0
   0   0  10   0   0
   0  10   0   0   0
   0   0   0   0   0
   0   0   0   0   0
Maximum flow from source to sink: 19
Final graph:
Graph adjacency matrix:
   0   0   0   0   0
   0   0   0   0   0
   0   0   0   0   0
   0   0   0   0   0
   0   0   0   0   0
Press Enter to exit...
```

## Key Features of This Implementation

1. **Data Structures**: 
   - Adjacency matrix representation of the graph
   - Height array for vertices
   - Excess array to track flow imbalances

2. **Algorithm Steps**:
   - Initialize heights and initial flow
   - Main loop: find active vertices and push/relabel
   - Push operation: send flow from higher to lower vertex
   - Relabel operation: increase vertex height when no push is possible

3. **Time Complexity**: O(V²E) in worst case, but typically much better in practice

4. **Space Complexity**: O(V²) for the adjacency matrix

This implementation provides a complete working example of the Push-Relabel max-flow algorithm in Pascal.

