# Dinic's Algorithm in Pascal

Here's an implementation of Dinic's algorithm for finding the maximum flow in a flow network using Pascal:

```pascal
program DinicMaxFlow;

uses
  SysUtils, Classes;

const
  MAXN = 1000;
  INF = 1000000000;

type
  TEdge = record
    toVertex, capacity, reverseEdge: integer;
  end;

  TGraph = array[1..MAXN] of array of TEdge;
  TLevel = array[1..MAXN] of integer;
  TPointer = array[1..MAXN] of integer;

var
  graph: TGraph;
  level: TLevel;
  ptr: TPointer;
  n, m: integer;
  source, sink: integer;

// Function to add edge to the graph
procedure AddEdge(from, to, cap: integer);
begin
  SetLength(graph[from], Length(graph[from]) + 1);
  graph[from][High(graph[from])].toVertex := to;
  graph[from][High(graph[from])].capacity := cap;
  graph[from][High(graph[from])].reverseEdge := Length(graph[to]) + 1;
  
  SetLength(graph[to], Length(graph[to]) + 1);
  graph[to][High(graph[to])].toVertex := from;
  graph[to][High(graph[to])].capacity := 0;
  graph[to][High(graph[to])].reverseEdge := Length(graph[from]);
end;

// BFS to check if path exists from source to sink and build level graph
function BFS: boolean;
var
  queue: array[1..MAXN] of integer;
  head, tail, u, i: integer;
  v: integer;
begin
  for i := 1 to n do
    level[i] := 0;
    
  head := 1;
  tail := 1;
  queue[1] := source;
  level[source] := 1;
  
  while head <= tail do
  begin
    u := queue[head];
    inc(head);
    
    for i := 0 to High(graph[u]) do
    begin
      v := graph[u][i].toVertex;
      if (level[v] = 0) and (graph[u][i].capacity > 0) then
      begin
        level[v] := level[u] + 1;
        inc(tail);
        queue[tail] := v;
      end;
    end;
  end;
  
  BFS := level[sink] <> 0;
end;

// DFS to find blocking flow
function DFS(u, flow: integer): integer;
var
  i, pushed, v: integer;
begin
  if u = sink then
  begin
    DFS := flow;
    exit;
  end;
  
  for i := ptr[u] to High(graph[u]) do
  begin
    v := graph[u][i].toVertex;
    
    if (level[v] = level[u] + 1) and (graph[u][i].capacity > 0) then
    begin
      ptr[u] := i;
      
      pushed := DFS(v, min(flow, graph[u][i].capacity));
      
      if pushed <> 0 then
      begin
        graph[u][i].capacity := graph[u][i].capacity - pushed;
        graph[v][graph[u][i].reverseEdge - 1].capacity := 
          graph[v][graph[u][i].reverseEdge - 1].capacity + pushed;
        DFS := pushed;
        exit;
      end;
    end;
  end;
  
  DFS := 0;
end;

// Main Dinic's algorithm
function Dinic: integer;
var
  totalFlow, flow: integer;
begin
  totalFlow := 0;
  
  while BFS do
  begin
    for i := 1 to n do
      ptr[i] := 0;
      
    repeat
      flow := DFS(source, INF);
      totalFlow := totalFlow + flow;
    until flow = 0;
  end;
  
  Dinic := totalFlow;
end;

// Example usage
procedure Example;
begin
  n := 6;
  source := 1;
  sink := 6;
  
  // Clear the graph
  for i := 1 to n do
    SetLength(graph[i], 0);
  
  // Add edges to the graph
  AddEdge(1, 2, 10);
  AddEdge(1, 4, 10);
  AddEdge(2, 3, 4);
  AddEdge(2, 4, 2);
  AddEdge(2, 5, 8);
  AddEdge(3, 6, 10);
  AddEdge(4, 3, 6);
  AddEdge(4, 5, 9);
  AddEdge(5, 6, 10);
  
  writeln('Maximum flow from source to sink: ', Dinic);
end;

begin
  Example;
end.
```

## How it works:

1. **Graph Representation**: Uses an adjacency list representation where each edge stores:
   - Destination vertex (`toVertex`)
   - Capacity (`capacity`)
   - Reverse edge index (`reverseEdge`)

2. **Key Components**:
   - **BFS**: Builds level graph to determine if a path exists from source to sink
   - **DFS**: Finds blocking flow using the level graph
   - **Dinic's Algorithm**: Repeatedly calls BFS and DFS until no more augmenting paths exist

3. **Time Complexity**: O(V²E) in worst case, but typically much better in practice

4. **Example Graph**: 
   ```
   1 → 2 (10) → 3 (4) → 6 (10)
   |    ↘      ↘     ↗
   |     2 (2)  6 (6) 
   |      ↘     ↗
   4 (10)  → 5 (8) → 6 (10)
   |         ↘     ↗
   |          9 (9)
   |           ↗
   6 (10)
   ```

The algorithm will output the maximum flow from vertex 1 to vertex 6.

