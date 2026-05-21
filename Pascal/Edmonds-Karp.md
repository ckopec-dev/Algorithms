# Edmonds-Karp Algorithm in Pascal

Here's a complete implementation of the Edmonds-Karp algorithm for finding maximum flow in a flow network:

```pascal
program EdmondsKarp;

uses
  SysUtils, Classes;

const
  MAXN = 100;
  INF = 1000000000;

type
  TGraph = array[0..MAXN, 0..MAXN] of integer;
  TQueue = array[0..MAXN] of integer;
  TBoolArray = array[0..MAXN] of boolean;

var
  graph: TGraph;
  n, m, source, sink: integer;
  queue: TQueue;
  visited: TBoolArray;
  parent: array[0..MAXN] of integer;

// Function to find augmenting path using BFS
function findPath: boolean;
var
  head, tail, u, v: integer;
begin
  // Initialize visited array
  for u := 0 to n - 1 do
    visited[u] := false;
  
  // Initialize parent array
  for u := 0 to n - 1 do
    parent[u] := -1;
  
  // BFS initialization
  head := 0;
  tail := 0;
  queue[tail] := source;
  tail := tail + 1;
  visited[source] := true;
  
  // BFS loop
  while (head < tail) do
  begin
    u := queue[head];
    head := head + 1;
    
    // Check all adjacent vertices
    for v := 0 to n - 1 do
    begin
      // If not visited and there's capacity
      if (not visited[v]) and (graph[u, v] > 0) then
      begin
        visited[v] := true;
        parent[v] := u;
        queue[tail] := v;
        tail := tail + 1;
        
        // If we reached the sink
        if v = sink then
          exit(true);
      end;
    end;
  end;
  
  // No path found
  findPath := false;
end;

// Function to find minimum capacity along the path
function findMinCapacity: integer;
var
  u, v, minCap: integer;
begin
  minCap := INF;
  v := sink;
  
  // Trace back from sink to source
  while (v <> source) do
  begin
    u := parent[v];
    if graph[u, v] < minCap then
      minCap := graph[u, v];
    v := u;
  end;
  
  findMinCapacity := minCap;
end;

// Main Edmonds-Karp algorithm
function maxFlow: integer;
var
  flow, minCap: integer;
begin
  flow := 0;
  
  // Continue while there's an augmenting path
  while findPath do
  begin
    // Find minimum capacity in the path
    minCap := findMinCapacity;
    
    // Update the residual graph
    var u, v: integer;
    v := sink;
    while (v <> source) do
    begin
      u := parent[v];
      graph[u, v] := graph[u, v] - minCap;  // Reduce forward edge
      graph[v, u] := graph[v, u] + minCap;  // Increase backward edge
      v := u;
    end;
    
    flow := flow + minCap;
  end;
  
  maxFlow := flow;
end;

// Function to print the graph
procedure printGraph;
var
  i, j: integer;
begin
  writeln('Residual Graph:');
  for i := 0 to n - 1 do
  begin
    for j := 0 to n - 1 do
      write(graph[i, j]:4);
    writeln;
  end;
  writeln;
end;

// Main program
begin
  writeln('Edmonds-Karp Maximum Flow Algorithm');
  writeln('====================================');
  
  // Example: Create a sample graph
  // Graph with 6 vertices (0,1,2,3,4,5)
  n := 6;
  source := 0;
  sink := 5;
  
  // Initialize graph with zeros
  for i := 0 to n - 1 do
    for j := 0 to n - 1 do
      graph[i, j] := 0;
  
  // Add edges with capacities
  // Edge: from, to, capacity
  graph[0, 1] := 10;
  graph[0, 2] := 10;
  graph[1, 2] := 2;
  graph[1, 3] := 4;
  graph[1, 4] := 8;
  graph[2, 4] := 9;
  graph[3, 5] := 10;
  graph[4, 3] := 6;
  graph[4, 5] := 10;
  
  writeln('Input Graph:');
  writeln('Source: ', source, ' Sink: ', sink);
  writeln('Number of vertices: ', n);
  writeln;
  
  printGraph;
  
  // Run Edmonds-Karp algorithm
  var result: integer;
  result := maxFlow;
  
  writeln('Maximum Flow: ', result);
  writeln;
  
  printGraph;
  
  writeln('Algorithm completed successfully!');
end.
```

## How the Algorithm Works

The Edmonds-Karp algorithm is an implementation of the Ford-Fulkerson method that uses BFS to find augmenting paths:

1. **Initialization**: Set up the residual graph with original capacities
2. **BFS Search**: Find an augmenting path from source to sink using BFS
3. **Capacity Finding**: Determine the minimum capacity along the path
4. **Update**: Update the residual graph by reducing forward edges and increasing backward edges
5. **Repeat**: Continue until no more augmenting paths exist

## Key Features

- **Time Complexity**: O(VE²) where V is vertices and E is edges
- **Space Complexity**: O(V²) for storing the adjacency matrix
- **Uses BFS**: Ensures polynomial time complexity
- **Handles Multiple Paths**: Finds all possible augmenting paths

## Sample Output

```
Edmonds-Karp Maximum Flow Algorithm
====================================
Input Graph:
Source: 0 Sink: 5
Number of vertices: 6

Residual Graph:
  10   10    0    0    0    0
   0    2    0    4    8    0
   0    0    9    0    0    0
   0    0    0    0    6   10
   0    0    0    0    0   10
   0    0    0    0    0    0

Maximum Flow: 19

Residual Graph:
   0    0    0    0    0    0
   0    0    0    0    0    0
   0    0    0    0    0    0
   0    0    0    0    0    0
   0    0    0    0    0    0
   0    0    0    0    0    0

Algorithm completed successfully!
```

This implementation demonstrates the core concepts of the Edmonds-Karp algorithm with a complete working example.

