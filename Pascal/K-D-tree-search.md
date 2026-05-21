# K-D Tree Search Algorithm in Pascal

Here's an implementation of a K-D tree search algorithm in Pascal:

```pascal
program KDTreeSearch;

type
  // Point structure for k-dimensional space
  TPoint = array[1..2] of real;  // 2D points for example
  
  // K-D Tree node structure
  TKDNode = record
    Point: TPoint;
    Left: ^TKDNode;
    Right: ^TKDNode;
    Depth: integer;
  end;
  
  PKDNode = ^TKDNode;

// Function to create a new node
function CreateNode(const Point: TPoint; Depth: integer): PKDNode;
begin
  New(CreateNode);
  CreateNode^.Point := Point;
  CreateNode^.Left := nil;
  CreateNode^.Right := nil;
  CreateNode^.Depth := Depth;
end;

// Function to insert a point into the K-D tree
procedure InsertNode(var Root: PKDNode; const Point: TPoint; Depth: integer);
var
  Axis: integer;
begin
  if Root = nil then
  begin
    Root := CreateNode(Point, Depth);
    exit;
  end;
  
  // Determine the axis based on depth
  Axis := Depth mod 2 + 1;  // 1 for x-axis, 2 for y-axis
  
  // Recursively insert based on axis comparison
  if Point[Axis] < Root^.Point[Axis] then
    InsertNode(Root^.Left, Point, Depth + 1)
  else
    InsertNode(Root^.Right, Point, Depth + 1);
end;

// Function to find the minimum of two values
function Min(a, b: real): real;
begin
  if a < b then
    Min := a
  else
    Min := b;
end;

// Function to calculate Euclidean distance between two points
function Distance(const Point1, Point2: TPoint): real;
var
  i: integer;
  sum: real;
begin
  sum := 0;
  for i := 1 to 2 do
    sum := sum + Sqr(Point1[i] - Point2[i]);
  Distance := Sqrt(sum);
end;

// K-D Tree nearest neighbor search
function NearestNeighborSearch(Root: PKDNode; const Target: TPoint): TPoint;
var
  Best: TPoint;
  BestDistance: real;
  TempDistance: real;
  
  // Recursive helper function
  procedure Search(Node: PKDNode; var CurrentBest: TPoint; var CurrentBestDistance: real);
  var
    Axis: integer;
    DistanceToSplit: real;
    TempPoint: TPoint;
    TempDistance: real;
    BestLeft, BestRight: TPoint;
    BestLeftDist, BestRightDist: real;
  begin
    if Node = nil then
      exit;
    
    // Calculate distance to current node
    TempDistance := Distance(Target, Node^.Point);
    if TempDistance < CurrentBestDistance then
    begin
      CurrentBest := Node^.Point;
      CurrentBestDistance := TempDistance;
    end;
    
    // Determine the axis
    Axis := Node^.Depth mod 2 + 1;
    
    // Calculate distance to the splitting hyperplane
    DistanceToSplit := Target[Axis] - Node^.Point[Axis];
    
    // Search the side that contains the target point first
    if Target[Axis] < Node^.Point[Axis] then
    begin
      Search(Node^.Left, CurrentBest, CurrentBestDistance);
      
      // Check if we need to search the other side
      if Sqr(DistanceToSplit) < CurrentBestDistance then
        Search(Node^.Right, CurrentBest, CurrentBestDistance);
    end
    else
    begin
      Search(Node^.Right, CurrentBest, CurrentBestDistance);
      
      // Check if we need to search the other side
      if Sqr(DistanceToSplit) < CurrentBestDistance then
        Search(Node^.Left, CurrentBest, CurrentBestDistance);
    end;
  end;
  
begin
  // Initialize with a large distance
  BestDistance := MaxReal;
  Best := Target;  // Initialize with target point
  
  Search(Root, Best, BestDistance);
  NearestNeighborSearch := Best;
end;

// Function to print a point
procedure PrintPoint(const Point: TPoint);
begin
  Write('(', Point[1]:0:2, ', ', Point[2]:0:2, ')');
end;

// Main program
var
  Root: PKDNode;
  TargetPoint: TPoint;
  NearestPoint: TPoint;
  Points: array[1..6] of TPoint;
  i: integer;
begin
  Root := nil;
  
  // Initialize sample points
  Points[1] := [1.0, 3.0];
  Points[2] := [4.0, 7.0];
  Points[3] := [2.0, 9.0];
  Points[4] := [8.0, 1.0];
  Points[5] := [7.0, 2.0];
  Points[6] := [5.0, 6.0];
  
  // Build the K-D tree
  for i := 1 to 6 do
    InsertNode(Root, Points[i], 0);
  
  // Search for nearest neighbor
  TargetPoint := [5.0, 5.0];
  
  Writeln('K-D Tree Nearest Neighbor Search');
  Writeln('================================');
  Writeln('Sample points:');
  for i := 1 to 6 do
  begin
    Write('Point ', i, ': ');
    PrintPoint(Points[i]);
    Writeln;
  end;
  
  Writeln;
  Writeln('Target point: ');
  PrintPoint(TargetPoint);
  Writeln;
  
  NearestPoint := NearestNeighborSearch(Root, TargetPoint);
  
  Writeln('Nearest neighbor: ');
  PrintPoint(NearestPoint);
  Writeln;
  
  // Clean up memory
  // Note: In a complete implementation, you would need proper memory deallocation
end.
```

## Key Features of this Implementation:

1. **TPoint Structure**: Represents points in 2D space (can be extended for higher dimensions)
2. **TKDNode Structure**: Contains point data, left/right child pointers, and depth information
3. **InsertNode Procedure**: Builds the K-D tree by recursively inserting points
4. **NearestNeighborSearch Function**: Implements the core K-D tree search algorithm
5. **Distance Calculation**: Uses Euclidean distance metric
6. **Optimized Search**: Only searches branches that could contain closer points

## Algorithm Complexity:
- **Construction**: O(n log n) average case
- **Search**: O(log n) average case, O(n) worst case
- **Space**: O(n)

This implementation demonstrates the fundamental K-D tree search algorithm with proper axis selection and pruning of search branches based on distance calculations.

