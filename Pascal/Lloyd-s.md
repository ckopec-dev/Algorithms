# Lloyd's Algorithm in Pascal

Lloyd's algorithm is used for k-means clustering. Here's an implementation in Pascal:

```pascal
program LloydAlgorithm;

uses
  crt, math;

const
  MAX_POINTS = 100;
  MAX_DIMENSIONS = 10;
  MAX_CLUSTERS = 5;

type
  TPoint = array[1..MAX_DIMENSIONS] of real;
  TCluster = array[1..MAX_DIMENSIONS] of real;
  TPoints = array[1..MAX_POINTS] of TPoint;
  TClusters = array[1..MAX_CLUSTERS] of TCluster;

var
  points: TPoints;
  clusters: TClusters;
  assignments: array[1..MAX_POINTS] of integer;
  numPoints, numDimensions, numClusters: integer;
  maxIterations: integer;
  tolerance: real;

// Calculate Euclidean distance between two points
function Distance(p1, p2: TPoint): real;
var
  i: integer;
  sum: real;
begin
  sum := 0;
  for i := 1 to numDimensions do
    sum := sum + sqr(p1[i] - p2[i]);
  Distance := sqrt(sum);
end;

// Initialize clusters randomly
procedure InitializeClusters;
var
  i, j: integer;
begin
  for i := 1 to numClusters do
  begin
    for j := 1 to numDimensions do
    begin
      clusters[i][j] := random * 100; // Random values between 0-100
    end;
  end;
end;

// Assign points to nearest cluster
procedure AssignPoints;
var
  i, j, k: integer;
  minDist, dist: real;
begin
  for i := 1 to numPoints do
  begin
    minDist := Distance(points[i], clusters[1]);
    assignments[i] := 1;
    
    for j := 2 to numClusters do
    begin
      dist := Distance(points[i], clusters[j]);
      if dist < minDist then
      begin
        minDist := dist;
        assignments[i] := j;
      end;
    end;
  end;
end;

// Update cluster centers
procedure UpdateClusters;
var
  i, j, k: integer;
  count: array[1..MAX_CLUSTERS] of integer;
begin
  // Initialize cluster centers to zero
  for i := 1 to numClusters do
  begin
    for j := 1 to numDimensions do
    begin
      clusters[i][j] := 0;
    end;
    count[i] := 0;
  end;
  
  // Sum up all points assigned to each cluster
  for i := 1 to numPoints do
  begin
    k := assignments[i];
    count[k] := count[k] + 1;
    for j := 1 to numDimensions do
    begin
      clusters[k][j] := clusters[k][j] + points[i][j];
    end;
  end;
  
  // Calculate average for each cluster
  for i := 1 to numClusters do
  begin
    if count[i] > 0 then
    begin
      for j := 1 to numDimensions do
      begin
        clusters[i][j] := clusters[i][j] / count[i];
      end;
    end;
  end;
end;

// Check if clusters have converged
function HasConverged(oldClusters, newClusters: TClusters): boolean;
var
  i, j: integer;
  diff: real;
begin
  for i := 1 to numClusters do
  begin
    for j := 1 to numDimensions do
    begin
      diff := abs(oldClusters[i][j] - newClusters[i][j]);
      if diff > tolerance then
      begin
        HasConverged := false;
        exit;
      end;
    end;
  end;
  HasConverged := true;
end;

// Main Lloyd's algorithm
procedure LloydAlgorithm;
var
  i, iteration: integer;
  oldClusters: TClusters;
  changed: boolean;
begin
  writeln('Starting Lloyd''s Algorithm...');
  writeln('Number of points: ', numPoints);
  writeln('Number of dimensions: ', numDimensions);
  writeln('Number of clusters: ', numClusters);
  writeln;
  
  InitializeClusters;
  
  for iteration := 1 to maxIterations do
  begin
    // Store old cluster positions
    for i := 1 to numClusters do
      oldClusters[i] := clusters[i];
    
    // Assign points to clusters
    AssignPoints;
    
    // Update cluster centers
    UpdateClusters;
    
    // Check for convergence
    if HasConverged(oldClusters, clusters) then
    begin
      writeln('Converged after ', iteration, ' iterations');
      break;
    end;
  end;
  
  // Display final results
  writeln('Final cluster centers:');
  for i := 1 to numClusters do
  begin
    write('Cluster ', i, ': (');
    for j := 1 to numDimensions do
    begin
      write(clusters[i][j]:0:2);
      if j < numDimensions then write(', ');
    end;
    writeln(')');
  end;
  
  writeln('Point assignments:');
  for i := 1 to numPoints do
  begin
    write('Point ', i, ' -> Cluster ', assignments[i]);
    write(' (');
    for j := 1 to numDimensions do
    begin
      write(points[i][j]:0:2);
      if j < numDimensions then write(', ');
    end;
    writeln(')');
  end;
end;

// Initialize sample data
procedure InitializeTestData;
begin
  numPoints := 10;
  numDimensions := 2;
  numClusters := 3;
  maxIterations := 100;
  tolerance := 0.001;
  
  // Sample 2D points
  points[1][1] := 1.0; points[1][2] := 2.0;
  points[2][1] := 1.5; points[2][2] := 1.8;
  points[3][1] := 5.0; points[3][2] := 8.0;
  points[4][1] := 8.0; points[4][2] := 8.0;
  points[5][1] := 1.0; points[5][2] := 0.6;
  points[6][1] := 9.0; points[6][2] := 11.0;
  points[7][1] := 8.0; points[7][2] := 2.0;
  points[8][1] := 10.0; points[8][2] := 2.0;
  points[9][1] := 9.0; points[9][2] := 3.0;
  points[10][1] := 2.0; points[10][2] := 1.0;
end;

begin
  randomize;
  InitializeTestData;
  LloydAlgorithm;
  writeln;
  writeln('Press any key to exit...');
  readln;
end.
```

## Key Features of this Implementation:

1. **Data Structures**: Uses arrays for points, clusters, and assignments
2. **Distance Calculation**: Implements Euclidean distance formula
3. **Initialization**: Randomly initializes cluster centers
4. **Assignment Step**: Assigns each point to the nearest cluster
5. **Update Step**: Recalculates cluster centers as centroids
6. **Convergence Check**: Stops when clusters don't change significantly
7. **Sample Data**: Includes sample 2D points for demonstration

## How it Works:

1. Initialize cluster centers randomly
2. Assign each point to the nearest cluster
3. Update cluster centers to be the mean of assigned points
4. Repeat steps 2-3 until convergence or maximum iterations reached

This implementation demonstrates the core iterative process of Lloyd's k-means clustering algorithm in Pascal.

