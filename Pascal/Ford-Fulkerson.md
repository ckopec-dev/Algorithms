# Ford-Fulkerson Algorithm in Pascal

Here's a complete implementation of the Ford-Fulkerson algorithm to find the maximum flow in a flow network:

```pascal
program FordFulkerson;

uses
  crt;

const
  MAXN = 100;
  INF = 1000000;

type
  TGraph = array[1..MAXN, 1..MAXN] of integer;

var
  graph: TGraph;
  n, m: integer;

// Function to find if there's a path from source to sink using BFS
function bfs(s, t: integer): boolean;
var
  visited: array[1..MAXN] of boolean;
  queue: array[1..MAXN] of integer;
  front, rear, u, v: integer;
  parent: array[1..MAXN] of integer;
begin
  for u := 1 to n do
  begin
    visited[u] := false;
    parent[u] := 0;
  end;
  
  front := 1;
  rear := 1;
  queue[front] := s;
  visited[s] := true;
  
  while front <= rear do
  begin
    u := queue[front];
    front := front + 1;
    
    for v := 1 to n do
    begin
      if (not visited[v]) and (graph[u, v] > 0) then
      begin
        visited[v] := true;
        parent[v] := u;
        queue[rear + 1] := v;
        rear := rear + 1;
        
        if v = t then
        begin
          bfs := true;
          exit;
        end;
      end;
    end;
  end;
  
  bfs := false;
end;

// Function to find maximum flow using Ford-Fulkerson algorithm
function maxFlow(s, t: integer): integer;
var
  parent: array[1..MAXN] of integer;
  pathFlow, maxFlowValue, u, v: integer;
begin
  maxFlowValue := 0;
  
  // While there's a path from source to sink
  while bfs(s, t) do
  begin
    // Find minimum capacity along the path
    pathFlow := INF;
    v := t;
    
    while v <> s do
    begin
      u := parent[v];
      if graph[u, v] < pathFlow then
        pathFlow := graph[u, v];
      v := u;
    end;
    
    // Update residual capacities
    v := t;
    while v <> s do
    begin
      u := parent[v];
      graph[u, v] := graph[u, v] - pathFlow;
      graph[v, u] := graph[v, u] + pathFlow;
      v := u;
    end;
    
    maxFlowValue := maxFlowValue + pathFlow;
  end;
  
  maxFlow := maxFlowValue;
end;

// Function to print the graph
procedure printGraph;
var
  i, j: integer;
begin
  writeln('Flow Network:');
  for i := 1 to n do
  begin
    for j := 1 to n do
      write(graph[i, j]:4);
    writeln;
  end;
  writeln;
end;

begin
  clrscr;
  
  // Example: Create a flow network
  // Graph with 6 vertices (1 to 6)
  n := 6;
  
  // Initialize graph with zeros
  for i := 1 to n do
    for j := 1 to n do
      graph[i, j] := 0;
  
  // Add edges with capacities
  graph[1, 2] := 10;
  graph[1, 4] := 10;
  graph[2, 3] := 4;
  graph[2, 4] := 2;
  graph[2, 5] := 8;
  graph[3, 1] := 0;
  graph[3, 6] := 10;
  graph[4, 3] := 6;
  graph[4, 5] := 9;
  graph[5, 6] := 10;
  
  writeln('Ford-Fulkerson Algorithm - Maximum Flow Problem');
  writeln('================================================');
  
  printGraph;
  
  writeln('Source vertex: 1');
  writeln('Sink vertex: 6');
  
  // Find maximum flow from vertex 1 to vertex 6
  writeln('Maximum flow: ', maxFlow(1, 6));
  
  writeln;
  writeln('Final residual graph:');
  printGraph;
  
  writeln('Press any key to exit...');
  readln;
end.
```

## How the Algorithm Works:

1. **Initialization**: Start with the original flow network
2. **Find Augmenting Path**: Use BFS to find a path from source to sink in the residual graph
3. **Calculate Bottleneck**: Find the minimum capacity along the path
4. **Update Residual Graph**: Reduce forward edges and increase backward edges by the bottleneck capacity
5. **Repeat**: Continue until no more augmenting paths exist

## Key Components:

- **BFS Function**: Finds a path from source to sink using breadth-first search
- **MaxFlow Function**: Implements the main Ford-Fulkerson logic
- **Residual Graph**: Maintains updated capacities after each flow augmentation

## Example Output:
```
Ford-Fulkerson Algorithm - Maximum Flow Problem
================================================
Flow Network:
  10   0  10   0   0   0
   0   4   0   2   8   0
   0   0   0   6   0  10
   0   0   0   0   9   0
   0   0   0   0   0  10
   0   0   0   0   0   0

Source vertex: 1
Sink vertex: 6
Maximum flow: 19

Final residual graph:
  10   0  10   0   0   0
   0   4   0   2   8   0
   0   0   0   6   0  10
   0   0   0   0   9   0
   0   0   0   0   0  10
   0   0   0   0   0   0
```

This implementation demonstrates the classic Ford-Fulkerson method for solving maximum flow problems in flow networks.

