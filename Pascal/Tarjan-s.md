# Tarjan's Algorithm in Pascal

Here's an implementation of Tarjan's algorithm for finding strongly connected components in a directed graph using Pascal:

```pascal
program TarjanSCC;

uses
  SysUtils, Classes;

type
  TNode = record
    Index: Integer;
    LowLink: Integer;
    OnStack: Boolean;
  end;

  TGraph = record
    Vertices: Integer;
    AdjList: array of array of Integer;
    Nodes: array of TNode;
    Stack: array of Integer;
    StackPointer: Integer;
    SccCount: Integer;
    SccList: array of array of Integer;
  end;

var
  Graph: TGraph;
  IndexCounter: Integer;

procedure InitializeGraph(Vertices: Integer);
begin
  Graph.Vertices := Vertices;
  SetLength(Graph.AdjList, Vertices);
  SetLength(Graph.Nodes, Vertices);
  SetLength(Graph.Stack, Vertices);
  Graph.StackPointer := 0;
  Graph.SccCount := 0;
  SetLength(Graph.SccList, Vertices);
  IndexCounter := 0;
end;

procedure AddEdge(From, To: Integer);
begin
  SetLength(Graph.AdjList[From], Length(Graph.AdjList[From]) + 1);
  Graph.AdjList[From][High(Graph.AdjList[From])] := To;
end;

procedure StrongConnect(Vertex: Integer);
var
  W: Integer;
  Scc: array of Integer;
  SccSize: Integer;
begin
  // Set the depth index for v
  Graph.Nodes[Vertex].Index := IndexCounter;
  Graph.Nodes[Vertex].LowLink := IndexCounter;
  IndexCounter := IndexCounter + 1;
  
  // Push vertex onto stack
  Graph.Stack[Graph.StackPointer] := Vertex;
  Graph.StackPointer := Graph.StackPointer + 1;
  Graph.Nodes[Vertex].OnStack := True;
  
  // Consider successors of v
  for W in Graph.AdjList[Vertex] do
  begin
    if Graph.Nodes[W].Index = -1 then
    begin
      // Successor w has not yet been visited
      StrongConnect(W);
      Graph.Nodes[Vertex].LowLink := Min(Graph.Nodes[Vertex].LowLink, Graph.Nodes[W].LowLink);
    end
    else if Graph.Nodes[W].OnStack then
    begin
      // Successor w is in stack S and hence in the current SCC
      Graph.Nodes[Vertex].LowLink := Min(Graph.Nodes[Vertex].LowLink, Graph.Nodes[W].Index);
    end;
  end;
  
  // If v is a root node, pop the stack and create a strongly connected component
  if Graph.Nodes[Vertex].LowLink = Graph.Nodes[Vertex].Index then
  begin
    // Create new SCC
    SetLength(Graph.SccList[Graph.SccCount], 0);
    SccSize := 0;
    
    repeat
      Graph.StackPointer := Graph.StackPointer - 1;
      W := Graph.Stack[Graph.StackPointer];
      Graph.Nodes[W].OnStack := False;
      
      // Add vertex to current SCC
      SetLength(Graph.SccList[Graph.SccCount], SccSize + 1);
      Graph.SccList[Graph.SccCount][SccSize] := W;
      SccSize := SccSize + 1;
    until W = Vertex;
    
    // Increment SCC count
    Graph.SccCount := Graph.SccCount + 1;
  end;
end;

procedure FindStronglyConnectedComponents;
var
  I: Integer;
begin
  // Initialize all nodes
  for I := 0 to Graph.Vertices - 1 do
  begin
    Graph.Nodes[I].Index := -1;  // -1 means unvisited
    Graph.Nodes[I].LowLink := -1;
    Graph.Nodes[I].OnStack := False;
  end;
  
  // Run StrongConnect for each unvisited vertex
  for I := 0 to Graph.Vertices - 1 do
  begin
    if Graph.Nodes[I].Index = -1 then
      StrongConnect(I);
  end;
end;

procedure PrintResults;
var
  I, J: Integer;
begin
  Writeln('Strongly Connected Components:');
  Writeln('============================');
  
  for I := 0 to Graph.SccCount - 1 do
  begin
    Write('SCC ', I + 1, ': ');
    for J := 0 to High(Graph.SccList[I]) do
    begin
      Write(Graph.SccList[I][J], ' ');
    end;
    Writeln;
  end;
end;

begin
  // Example graph construction
  InitializeGraph(8);
  
  // Add edges to create a sample graph
  AddEdge(0, 1);
  AddEdge(1, 2);
  AddEdge(2, 0);
  AddEdge(1, 3);
  AddEdge(3, 4);
  AddEdge(4, 5);
  AddEdge(5, 3);
  AddEdge(6, 7);
  AddEdge(7, 6);
  
  // Find strongly connected components
  FindStronglyConnectedComponents;
  
  // Print results
  PrintResults;
  
  Readln;
end.
```

## Algorithm Explanation

This implementation of Tarjan's algorithm:

1. **Uses a stack** to keep track of vertices in the current path
2. **Assigns indices** to vertices during the depth-first search
3. **Maintains low-link values** to identify the root of each SCC
4. **Creates strongly connected components** when a root node is found

## Key Components

- **TNode record**: Stores index, low-link value, and stack status for each vertex
- **TGraph record**: Contains the graph structure, stack, and SCC results
- **StrongConnect procedure**: The core recursive algorithm implementation
- **FindStronglyConnectedComponents**: Main function to initiate the algorithm

## Time Complexity
- **Time**: O(V + E) where V is vertices and E is edges
- **Space**: O(V) for the stack and auxiliary arrays

The algorithm correctly identifies all strongly connected components in the directed graph, which are maximal subgraphs where every pair of vertices is reachable from each other.

