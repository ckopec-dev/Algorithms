# K-Means Clustering in Pascal

Here's a complete implementation of the K-means clustering algorithm in Pascal:

```pascal
program KMeansClustering;

uses
  crt, math;

const
  MAX_POINTS = 1000;
  MAX_DIMENSIONS = 10;
  MAX_CLUSTERS = 5;

type
  TPoint = array[1..MAX_DIMENSIONS] of real;
  TPoints = array[1..MAX_POINTS] of TPoint;
  TCentroids = array[1..MAX_CLUSTERS] of TPoint;
  TCluster = array[1..MAX_POINTS] of integer;

var
  points: TPoints;
  centroids: TCentroids;
  clusters: TCluster;
  numPoints, numDimensions, numClusters: integer;
  maxIterations: integer;
  tolerance: real;

// Calculate Euclidean distance between two points
function EuclideanDistance(const p1, p2: TPoint): real;
var
  i: integer;
  sum: real;
begin
  sum := 0;
  for i := 1 to numDimensions do
    sum := sum + sqr(p1[i] - p2[i]);
  EuclideanDistance := sqrt(sum);
end;

// Initialize centroids randomly
procedure InitializeCentroids;
var
  i, j: integer;
  minVal, maxVal: real;
begin
  for i := 1 to numClusters do
  begin
    for j := 1 to numDimensions do
    begin
      // Get min and max values for this dimension
      minVal := points[1][j];
      maxVal := points[1][j];
      for var k := 2 to numPoints do
      begin
        if points[k][j] < minVal then minVal := points[k][j];
        if points[k][j] > maxVal then maxVal := points[k][j];
      end;
      // Set centroid to random value in range
      centroids[i][j] := minVal + random * (maxVal - minVal);
    end;
  end;
end;

// Assign points to nearest centroid
procedure AssignPointsToClusters;
var
  i, j, closestCluster: integer;
  distance, minDistance: real;
begin
  for i := 1 to numPoints do
  begin
    minDistance := EuclideanDistance(points[i], centroids[1]);
    closestCluster := 1;
    
    for j := 2 to numClusters do
    begin
      distance := EuclideanDistance(points[i], centroids[j]);
      if distance < minDistance then
      begin
        minDistance := distance;
        closestCluster := j;
      end;
    end;
    clusters[i] := closestCluster;
  end;
end;

// Update centroids based on current cluster assignments
procedure UpdateCentroids;
var
  i, j, clusterSize: integer;
  sum: real;
begin
  for i := 1 to numClusters do
  begin
    for j := 1 to numDimensions do
    begin
      sum := 0;
      clusterSize := 0;
      
      for var k := 1 to numPoints do
      begin
        if clusters[k] = i then
        begin
          sum := sum + points[k][j];
          clusterSize := clusterSize + 1;
        end;
      end;
      
      if clusterSize > 0 then
        centroids[i][j] := sum / clusterSize
      else
        // If cluster is empty, keep old centroid
        centroids[i][j] := centroids[i][j];
    end;
  end;
end;

// Check if centroids have converged
function HasConverged: boolean;
var
  i, j: integer;
  distance: real;
begin
  HasConverged := true;
  for i := 1 to numClusters do
  begin
    distance := EuclideanDistance(centroids[i], centroids[i]);
    if distance > tolerance then
    begin
      HasConverged := false;
      exit;
    end;
  end;
end;

// Main K-means algorithm
procedure KMeans;
var
  iteration: integer;
  changed: boolean;
begin
  writeln('Starting K-means clustering...');
  
  InitializeCentroids;
  
  for iteration := 1 to maxIterations do
  begin
    AssignPointsToClusters;
    UpdateCentroids;
    
    writeln('Iteration ', iteration, ': Centroids updated');
    
    // Check for convergence
    if iteration > 1 then
    begin
      changed := false;
      for var i := 1 to numClusters do
      begin
        if EuclideanDistance(centroids[i], centroids[i]) > tolerance then
        begin
          changed := true;
          break;
        end;
      end;
      
      if not changed then
      begin
        writeln('Converged after ', iteration, ' iterations');
        exit;
      end;
    end;
  end;
  
  writeln('Max iterations reached');
end;

// Display results
procedure DisplayResults;
var
  i, j: integer;
begin
  writeln('Final Clusters:');
  writeln('----------------');
  
  for i := 1 to numClusters do
  begin
    writeln('Cluster ', i, ':');
    for j := 1 to numPoints do
    begin
      if clusters[j] = i then
      begin
        write('  Point (');
        for var k := 1 to numDimensions do
          write(points[j][k]:0:2, ' ');
        writeln(') -> Cluster ', i);
      end;
    end;
    writeln('Centroid: (');
    for j := 1 to numDimensions do
      write(centroids[i][j]:0:2, ' ');
    writeln(')');
    writeln;
  end;
end;

// Generate sample data
procedure GenerateSampleData;
begin
  numPoints := 20;
  numDimensions := 2;
  numClusters := 3;
  maxIterations := 100;
  tolerance := 0.001;
  
  // Sample 2D points
  points[1][1] := 1.0; points[1][2] := 2.0;
  points[2][1] := 1.5; points[2][2] := 1.8;
  points[3][1] := 1.2; points[3][2] := 1.1;
  points[4][1] := 5.0; points[4][2] := 8.0;
  points[5][1] := 6.0; points[5][2] := 8.5;
  points[6][1] := 5.5; points[6][2] := 7.5;
  points[7][1] := 1.0; points[7][2] := 0.5;
  points[8][1] := 2.0; points[8][2] := 1.0;
  points[9][1] := 3.0; points[9][2] := 1.5;
  points[10][1] := 4.0; points[10][2] := 4.0;
  points[11][1] := 5.0; points[11][2] := 5.0;
  points[12][1] := 6.0; points[12][2] := 6.0;
  points[13][1] := 7.0; points[13][2] := 7.0;
  points[14][1] := 8.0; points[14][2] := 8.0;
  points[15][1] := 2.0; points[15][2] := 2.0;
  points[16][1] := 3.0; points[16][2] := 3.0;
  points[17][1] := 4.0; points[17][2] := 5.0;
  points[18][1] := 5.0; points[18][2] := 6.0;
  points[19][1] := 6.0; points[19][2] := 7.0;
  points[20][1] := 7.0; points[20][2] := 8.0;
end;

begin
  randomize;
  
  GenerateSampleData;
  
  writeln('K-Means Clustering Algorithm');
  writeln('============================');
  writeln('Points: ', numPoints);
  writeln('Dimensions: ', numDimensions);
  writeln('Clusters: ', numClusters);
  writeln;
  
  KMeans;
  DisplayResults;
  
  writeln('Press any key to exit...');
  readln;
end.
```

## Key Features of this Implementation:

1. **Data Structures**:
   - `TPoint`: Array of real numbers representing a point in n-dimensional space
   - `TPoints`: Array of points
   - `TCentroids`: Array of cluster centroids
   - `TCluster`: Array mapping points to clusters

2. **Core Functions**:
   - `EuclideanDistance`: Calculates distance between two points
   - `InitializeCentroids`: Randomly initializes cluster centroids
   - `AssignPointsToClusters`: Assigns each point to the nearest centroid
   - `UpdateCentroids`: Recalculates centroids based on current assignments
   - `HasConverged`: Checks if the algorithm has converged

3. **Algorithm Steps**:
   - Initialize random centroids
   - Assign points to nearest centroids
   - Update centroids based on point assignments
   - Repeat until convergence or maximum iterations

4. **Sample Data**:
   - Generates 20 2D points for demonstration
   - Creates 3 clusters to group similar points

This implementation provides a complete working example of K-means clustering in Pascal, demonstrating the fundamental concepts of the algorithm in a structured, readable format.

