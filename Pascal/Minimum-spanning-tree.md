# Minimum Spanning Tree Algorithm in Pascal

Here's an implementation of Kruskal's algorithm for finding the Minimum Spanning Tree in Pascal:

```pascal
program MinimumSpanningTree;

type
  // Edge structure
  TEdge = record
    src, dest, weight: integer;
  end;
  
  // Array of edges
  TEdgeArray = array of TEdge;
  
  // Union-Find structure
  TUnionFind = record
    parent: array of integer;
    rank: array of integer;
  end;

// Function to find root with path compression
function Find(var uf: TUnionFind; x: integer): integer;
begin
  if uf.parent[x] <> x then
    uf.parent[x] := Find(uf, uf.parent[x]);
  Find := uf.parent[x];
end;

// Function to unite two sets
procedure Union(var uf: TUnionFind; x, y: integer);
var
  xRoot, yRoot: integer;
begin
  xRoot := Find(uf, x);
  yRoot := Find(uf, y);
  
  if xRoot <> yRoot then
  begin
    if uf.rank[xRoot] < uf.rank[yRoot] then
      uf.parent[xRoot] := yRoot
    else if uf.rank[xRoot] > uf.rank[yRoot] then
      uf.parent[yRoot] := xRoot
    else
    begin
      uf.parent[yRoot] := xRoot;
      Inc(uf.rank[xRoot]);
    end;
  end;
end;

// Comparison function for sorting edges by weight
function CompareEdges(const edge1, edge2: TEdge): integer;
begin
  if edge1.weight < edge2.weight then
    CompareEdges := -1
  else if edge1.weight > edge2.weight then
    CompareEdges := 1
  else
    CompareEdges := 0;
end;

// Quick sort implementation for edges
procedure QuickSort(var edges: TEdgeArray; left, right: integer);
var
  i, j: integer;
  pivot, temp: TEdge;
begin
  if left < right then
  begin
    i := left;
    j := right;
    pivot := edges[(left + right) div 2];
    
    while i <= j do
    begin
      while CompareEdges(edges[i], pivot) < 0 do
        Inc(i);
      while CompareEdges(edges[j], pivot) > 0 do
        Dec(j);
      
      if i <= j then
      begin
        temp := edges[i];
        edges[i] := edges[j];
        edges[j] := temp;
        Inc(i);
        Dec(j);
      end;
    end;
    
    QuickSort(edges, left, j);
    QuickSort(edges, i, right);
  end;
end;

// Main Kruskal's algorithm implementation
procedure KruskalMST(var edges: TEdgeArray; numVertices, numEdges: integer);
var
  uf: TUnionFind;
  i, edgeCount: integer;
  totalWeight: integer;
begin
  // Initialize Union-Find structure
  SetLength(uf.parent, numVertices);
  SetLength(uf.rank, numVertices);
  
  for i := 0 to numVertices - 1 do
  begin
    uf.parent[i] := i;
    uf.rank[i] := 0;
  end;
  
  // Sort edges by weight
  QuickSort(edges, 0, numEdges - 1);
  
  writeln('Edges in Minimum Spanning Tree:');
  edgeCount := 0;
  totalWeight := 0;
  
  for i := 0 to numEdges - 1 do
  begin
    if edgeCount < numVertices - 1 then
    begin
      if Find(uf, edges[i].src) <> Find(uf, edges[i].dest) then
      begin
        writeln('Edge ', edges[i].src, ' - ', edges[i].dest, ' (weight: ', edges[i].weight, ')');
        Union(uf, edges[i].src, edges[i].dest);
        Inc(totalWeight, edges[i].weight);
        Inc(edgeCount);
      end;
    end
    else
      break;
  end;
  
  writeln('Total weight of MST: ', totalWeight);
end;

// Main program
var
  edges: TEdgeArray;
  numVertices, numEdges, i: integer;
begin
  writeln('Minimum Spanning Tree - Kruskal''s Algorithm');
  writeln('==========================================');
  
  // Example graph with 6 vertices and 10 edges
  numVertices := 6;
  numEdges := 10;
  
  SetLength(edges, numEdges);
  
  // Initialize edges
  edges[0] := (src: 0; dest: 1; weight: 4);
  edges[1] := (src: 0; dest: 2; weight: 3);
  edges[2] := (src: 1; dest: 2; weight: 1);
  edges[3] := (src: 1; dest: 3; weight: 2);
  edges[4] := (src: 2; dest: 3; weight: 4);
  edges[5] := (src: 3; dest: 4; weight: 2);
  edges[6] := (src: 3; dest: 5; weight: 6);
  edges[7] := (src: 4; dest: 5; weight: 3);
  edges[8] := (src: 0; dest: 4; weight: 5);
  edges[9] := (src: 1; dest: 5; weight: 7);
  
  writeln('Input edges:');
  for i := 0 to numEdges - 1 do
    writeln('Edge ', edges[i].src, ' - ', edges[i].dest, ' (weight: ', edges[i].weight, ')');
  
  writeln();
  
  // Find Minimum Spanning Tree
  KruskalMST(edges, numVertices, numEdges);
  
  writeln();
  writeln('Program completed successfully.');
end.
```

## Output Example:
```
Minimum Spanning Tree - Kruskal's Algorithm
==========================================
Input edges:
Edge 0 - 1 (weight: 4)
Edge 0 - 2 (weight: 3)
Edge 1 - 2 (weight: 1)
Edge 1 - 3 (weight: 2)
Edge 2 - 3 (weight: 4)
Edge 3 - 4 (weight: 2)
Edge 3 - 5 (weight: 6)
Edge 4 - 5 (weight: 3)
Edge 0 - 4 (weight: 5)
Edge 1 - 5 (weight: 7)

Edges in Minimum Spanning Tree:
Edge 1 - 2 (weight: 1)
Edge 1 - 3 (weight: 2)
Edge 3 - 4 (weight: 2)
Edge 0 - 2 (weight: 3)
Edge 4 - 5 (weight: 3)
Total weight of MST: 11

Program completed successfully.
```

## Key Features of this Implementation:

1. **Kruskal's Algorithm**: Uses Union-Find data structure for efficient cycle detection
2. **Edge Sorting**: Sorts edges by weight to process lighter edges first
3. **Union-Find**: Implements path compression and union by rank for optimal performance
4. **Memory Management**: Uses dynamic arrays for flexible memory allocation
5. **Clear Output**: Shows the selected edges and total MST weight

This implementation demonstrates the classic greedy approach to finding minimum spanning trees with O(E log E) time complexity.

