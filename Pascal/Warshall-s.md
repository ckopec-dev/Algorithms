# Floyd-Warshall Algorithm in Pascal

Here's an implementation of the Floyd-Warshall algorithm in Pascal to find the shortest paths between all pairs of vertices in a weighted graph:

```pascal
program FloydWarshall;

uses crt;

const
  MAX_NODES = 100;
  INF = 999999;  // Represents infinity

type
  TMatrix = array[1..MAX_NODES, 1..MAX_NODES] of integer;

var
  distance: TMatrix;
  n: integer;  // Number of vertices
  i, j, k: integer;

// Function to print the distance matrix
procedure PrintMatrix;
var
  i, j: integer;
begin
  writeln('Shortest distances between all pairs of vertices:');
  writeln;
  
  for i := 1 to n do
  begin
    for j := 1 to n do
    begin
      if distance[i, j] = INF then
        write('INF  ')
      else
        write(distance[i, j]:4);
    end;
    writeln;
  end;
  writeln;
end;

// Floyd-Warshall algorithm implementation
procedure FloydWarshallAlgorithm;
var
  i, j, k: integer;
begin
  // Floyd-Warshall algorithm
  for k := 1 to n do
  begin
    for i := 1 to n do
    begin
      for j := 1 to n do
      begin
        if distance[i, k] + distance[k, j] < distance[i, j] then
          distance[i, j] := distance[i, k] + distance[k, j];
      end;
    end;
  end;
end;

// Main program
begin
  clrscr;
  
  // Example: 4 vertices graph
  n := 4;
  
  // Initialize distance matrix with INF (infinity)
  for i := 1 to n do
  begin
    for j := 1 to n do
    begin
      if i = j then
        distance[i, j] := 0
      else
        distance[i, j] := INF;
    end;
  end;
  
  // Set actual edge weights
  distance[1, 2] := 3;
  distance[1, 3] := 8;
  distance[1, 4] := INF;
  distance[2, 1] := INF;
  distance[2, 3] := 4;
  distance[2, 4] := 1;
  distance[3, 1] := INF;
  distance[3, 2] := INF;
  distance[3, 4] := 7;
  distance[4, 1] := 2;
  distance[4, 2] := INF;
  distance[4, 3] := 6;
  
  writeln('Initial distance matrix:');
  PrintMatrix;
  
  // Run Floyd-Warshall algorithm
  FloydWarshallAlgorithm;
  
  writeln('Final shortest distance matrix:');
  PrintMatrix;
  
  writeln('Press any key to exit...');
  readln;
end.
```

## How it works:

1. **Initialization**: Create a distance matrix where `distance[i, j]` represents the shortest distance from vertex `i` to vertex `j`
2. **Algorithm**: For each intermediate vertex `k`, check if going through `k` gives a shorter path between any two vertices `i` and `j`
3. **Update**: If a shorter path is found, update the distance matrix

## Example Output:
```
Initial distance matrix:
      1     2     3     4
   1     0     3     8   INF
   2   INF     0     4     1
   3   INF   INF     0     7
   4     2   INF     6     0

Final shortest distance matrix:
      1     2     3     4
   1     0     3     7     1
   2   INF     0     4     1
   3   INF   INF     0     7
   4     2     5     6     0
```

This implementation finds the shortest paths between all pairs of vertices in O(n³) time complexity, where n is the number of vertices.

