# PageRank Algorithm in Pascal

Here's an implementation of the PageRank algorithm in Pascal:

```pascal
program PageRankAlgorithm;

uses
  SysUtils, Math;

const
  MAX_NODES = 100;
  MAX_ITERATIONS = 100;
  DAMPING_FACTOR = 0.85;
  TOLERANCE = 1e-6;

type
  TMatrix = array[1..MAX_NODES, 1..MAX_NODES] of double;
  TVector = array[1..MAX_NODES] of double;

var
  adjacencyMatrix: TMatrix;
  pageRank: TVector;
  newPageRank: TVector;
  nodeCount: integer;
  iteration: integer;
  diff: double;

// Function to initialize the adjacency matrix
procedure InitializeMatrix;
var
  i, j: integer;
begin
  for i := 1 to MAX_NODES do
    for j := 1 to MAX_NODES do
      adjacencyMatrix[i, j] := 0.0;
end;

// Function to add an edge from node i to node j
procedure AddEdge(i, j: integer);
begin
  adjacencyMatrix[i, j] := 1.0;
end;

// Function to normalize the adjacency matrix (create transition matrix)
procedure NormalizeMatrix;
var
  i, j: integer;
  sum: double;
begin
  for j := 1 to nodeCount do
  begin
    sum := 0.0;
    for i := 1 to nodeCount do
      sum := sum + adjacencyMatrix[i, j];
    
    if sum > 0.0 then
      for i := 1 to nodeCount do
        adjacencyMatrix[i, j] := adjacencyMatrix[i, j] / sum
    else
      // Handle dangling nodes
      for i := 1 to nodeCount do
        adjacencyMatrix[i, j] := 1.0 / nodeCount;
  end;
end;

// Function to calculate PageRank
procedure CalculatePageRank;
var
  i, j: integer;
  sum: double;
begin
  // Initialize page ranks
  for i := 1 to nodeCount do
    pageRank[i] := 1.0 / nodeCount;
  
  // Iterate until convergence or maximum iterations
  for iteration := 1 to MAX_ITERATIONS do
  begin
    // Calculate new page ranks
    for i := 1 to nodeCount do
    begin
      sum := 0.0;
      for j := 1 to nodeCount do
        sum := sum + pageRank[j] * adjacencyMatrix[i, j];
      
      newPageRank[i] := (1 - DAMPING_FACTOR) / nodeCount + DAMPING_FACTOR * sum;
    end;
    
    // Check for convergence
    diff := 0.0;
    for i := 1 to nodeCount do
    begin
      diff := diff + abs(newPageRank[i] - pageRank[i]);
      pageRank[i] := newPageRank[i];
    end;
    
    if diff < TOLERANCE then
      break;
  end;
end;

// Function to print results
procedure PrintResults;
var
  i: integer;
begin
  writeln('PageRank Results:');
  writeln('================');
  for i := 1 to nodeCount do
    writeln('Node ', i, ': ', pageRank[i]:0:6);
  writeln('Iterations: ', iteration);
end;

// Main program
begin
  writeln('PageRank Algorithm Implementation');
  writeln('=================================');
  
  // Initialize
  InitializeMatrix;
  nodeCount := 4; // Example with 4 nodes
  
  // Create example web graph
  // Node 1 links to nodes 2 and 3
  // Node 2 links to node 3
  // Node 3 links to node 1 and 4
  // Node 4 links to node 1
  
  AddEdge(1, 2);
  AddEdge(1, 3);
  AddEdge(2, 3);
  AddEdge(3, 1);
  AddEdge(3, 4);
  AddEdge(4, 1);
  
  // Normalize the matrix
  NormalizeMatrix;
  
  // Calculate PageRank
  CalculatePageRank;
  
  // Print results
  PrintResults;
  
  writeln;
  writeln('Adjacency Matrix:');
  writeln('=================');
  for i := 1 to nodeCount do
  begin
    for j := 1 to nodeCount do
      write(adjacencyMatrix[i, j]:0:3, ' ');
    writeln;
  end;
end.
```

## Explanation

This Pascal implementation of PageRank includes:

1. **Data Structures**:
   - `TMatrix`: 2D array for adjacency matrix
   - `TVector`: 1D array for page rank values

2. **Key Functions**:
   - `InitializeMatrix`: Sets up the adjacency matrix
   - `AddEdge`: Adds connections between nodes
   - `NormalizeMatrix`: Converts adjacency matrix to transition matrix
   - `CalculatePageRank`: Implements the iterative PageRank algorithm
   - `PrintResults`: Displays the final page rank values

3. **Algorithm Features**:
   - Uses damping factor (0.85) as per original PageRank
   - Handles convergence checking
   - Manages dangling nodes
   - Iterative calculation until convergence

4. **Example Graph**:
   - 4 nodes with directed edges
   - Demonstrates the algorithm's working with a simple web graph

The program outputs both the final PageRank values for each node and the normalized transition matrix used in the calculation.

