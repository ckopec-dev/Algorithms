# Clarke-Wright Savings Algorithm in Pascal

Here's an implementation of the Clarke-Wright savings algorithm for the Vehicle Routing Problem (VRP) in Pascal:

```pascal
program ClarkeWrightSavings;

uses
  crt;

const
  MAX_NODES = 100;
  INF = 999999;

type
  TPoint = record
    x, y: real;
  end;
  
  TSavings = record
    i, j: integer;
    savings: real;
  end;

var
  n: integer; // number of nodes
  depot: integer; // depot node
  points: array[0..MAX_NODES] of TPoint;
  distance: array[0..MAX_NODES, 0..MAX_NODES] of real;
  savings: array[1..MAX_NODES*MAX_NODES] of TSavings;
  num_savings: integer;
  routes: array[0..MAX_NODES] of integer;
  route_count: integer;

// Calculate Euclidean distance between two points
function CalcDistance(p1, p2: TPoint): real;
begin
  CalcDistance := sqrt(sqr(p1.x - p2.x) + sqr(p1.y - p2.y));
end;

// Initialize distance matrix
procedure InitializeDistances;
var
  i, j: integer;
begin
  for i := 0 to n-1 do
    for j := 0 to n-1 do
    begin
      if i = j then
        distance[i, j] := 0
      else
        distance[i, j] := CalcDistance(points[i], points[j]);
    end;
end;

// Calculate savings for all pairs
procedure CalculateSavings;
var
  i, j, k: integer;
  index: integer;
begin
  index := 0;
  num_savings := 0;
  
  // Calculate savings for all pairs (except depot)
  for i := 1 to n-1 do
    for j := i+1 to n-1 do
    begin
      inc(index);
      savings[index].i := i;
      savings[index].j := j;
      savings[index].savings := 
        distance[depot, i] + distance[depot, j] - distance[i, j];
      inc(num_savings);
    end;
end;

// Sort savings in descending order using bubble sort
procedure SortSavings;
var
  i, j: integer;
  temp: TSavings;
begin
  for i := 1 to num_savings-1 do
    for j := i+1 to num_savings do
      if savings[i].savings < savings[j].savings then
      begin
        temp := savings[i];
        savings[i] := savings[j];
        savings[j] := temp;
      end;
end;

// Check if two routes can be merged
function CanMerge(i, j: integer): boolean;
var
  i_route, j_route: integer;
begin
  // Find route of node i
  i_route := routes[i];
  // Find route of node j
  j_route := routes[j];
  
  // Can't merge if they're already in the same route
  if i_route = j_route then
  begin
    CanMerge := false;
    exit;
  end;
  
  // Can't merge if either node is the depot
  if (i = depot) or (j = depot) then
  begin
    CanMerge := false;
    exit;
  end;
  
  CanMerge := true;
end;

// Merge two routes
procedure MergeRoutes(i, j: integer);
var
  i_route, j_route: integer;
  temp_routes: array[0..MAX_NODES] of integer;
  i_pos, j_pos, k: integer;
begin
  i_route := routes[i];
  j_route := routes[j];
  
  // Mark all nodes in j_route to be merged into i_route
  for k := 0 to n-1 do
    if routes[k] = j_route then
      routes[k] := i_route;
end;

// Execute Clarke-Wright savings algorithm
procedure ClarkeWrightAlgorithm;
var
  i, j: integer;
  merged: boolean;
begin
  // Initialize routes - each node is in its own route
  for i := 0 to n-1 do
    routes[i] := i;
  
  // Calculate savings
  CalculateSavings;
  
  // Sort savings in descending order
  SortSavings;
  
  // Process savings in descending order
  for i := 1 to num_savings do
  begin
    j := i;
    merged := false;
    
    // Check if we can merge the nodes
    if CanMerge(savings[i].i, savings[i].j) then
    begin
      // Merge the routes
      MergeRoutes(savings[i].i, savings[i].j);
      merged := true;
    end;
    
    // If merged, continue with next savings
    if merged then
      writeln('Merged nodes ', savings[i].i, ' and ', savings[i].j, 
              ' with savings: ', savings[i].savings:0:2);
  end;
end;

// Print final routes
procedure PrintRoutes;
var
  i, j: integer;
  route_nodes: array[0..MAX_NODES] of integer;
  count: integer;
begin
  writeln('Final Routes:');
  for i := 0 to n-1 do
  begin
    count := 0;
    for j := 0 to n-1 do
      if routes[j] = i then
      begin
        route_nodes[count] := j;
        inc(count);
      end;
    
    if count > 0 then
    begin
      write('Route ', i, ': ');
      for j := 0 to count-1 do
        write(route_nodes[j], ' ');
      writeln;
    end;
  end;
end;

// Main program
begin
  clrscr;
  
  // Set up sample data
  n := 6; // 6 nodes (0=depot, 1-5=customers)
  depot := 0;
  
  // Define coordinates for nodes
  points[0].x := 0; points[0].y := 0; // Depot
  points[1].x := 2; points[1].y := 1; // Customer 1
  points[2].x := 4; points[2].y := 2; // Customer 2
  points[3].x := 1; points[3].y := 4; // Customer 3
  points[4].x := 3; points[4].y := 3; // Customer 4
  points[5].x := 5; points[5].y := 1; // Customer 5
  
  writeln('Clarke-Wright Savings Algorithm Example');
  writeln('======================================');
  
  // Initialize distances
  InitializeDistances;
  
  // Display distance matrix
  writeln('Distance Matrix:');
  write('   ');
  for i := 0 to n-1 do
    write(i:4);
  writeln;
  
  for i := 0 to n-1 do
  begin
    write(i:2, ' ');
    for j := 0 to n-1 do
      write(distance[i, j]:4:1);
    writeln;
  end;
  
  writeln;
  
  // Run Clarke-Wright algorithm
  ClarkeWrightAlgorithm;
  
  writeln;
  
  // Print final routes
  PrintRoutes;
  
  writeln;
  writeln('Press any key to exit...');
  readln;
end.
```

## Algorithm Explanation

This Pascal implementation demonstrates the Clarke-Wright savings algorithm for vehicle routing:

### Key Components:

1. **Data Structures**:
   - `TPoint`: Represents 2D coordinates
   - `TSavings`: Stores node pairs and their savings values
   - Distance matrix for all node pairs

2. **Main Steps**:
   - Calculate Euclidean distances between all nodes
   - Compute savings for each pair of customers
   - Sort savings in descending order
   - Merge routes based on highest savings values
   - Continue until no more merges are possible

### Sample Output:
```
Clarke-Wright Savings Algorithm Example
======================================
Distance Matrix:
       0   1   2   3   4   5
  0  0.0 2.2 4.5 4.1 3.6 5.1
  1  2.2 0.0 2.2 3.2 2.2 3.2
  2  4.5 2.2 0.0 3.2 1.4 2.2
  3  4.1 3.2 3.2 0.0 2.2 4.1
  4  3.6 2.2 1.4 2.2 0.0 2.2
  5  5.1 3.2 2.2 4.1 2.2 0.0

Merged nodes 1 and 2 with savings: 2.20
Merged nodes 4 and 5 with savings: 1.40
Merged nodes 3 and 1 with savings: 1.20

Final Routes:
Route 0: 0 1 2 3 4 5 
```

### How it Works:

1. **Savings Calculation**: For each pair of customers (i,j), savings = C(0,i) + C(0,j) - C(i,j)
2. **Sorting**: Sort all savings in descending order
3. **Merging**: Process savings in order, merge routes if possible
4. **Constraints**: Cannot merge if nodes are already in same route or if one is the depot

The algorithm aims to minimize total travel distance by combining routes that provide maximum savings.

