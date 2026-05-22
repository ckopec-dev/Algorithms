# Convex Hull Algorithm in Pascal

Here's an implementation of the Graham Scan algorithm for finding the convex hull of a set of points in Pascal:

```pascal
program ConvexHull;

uses crt;

type
  Point = record
    x, y: real;
  end;
  
  PointArray = array of Point;

// Function to calculate cross product of three points
function CrossProduct(p1, p2, p3: Point): real;
begin
  CrossProduct := (p2.x - p1.x) * (p3.y - p1.y) - (p2.y - p1.y) * (p3.x - p1.x);
end;

// Function to calculate distance between two points
function Distance(p1, p2: Point): real;
begin
  Distance := sqrt(sqr(p2.x - p1.x) + sqr(p2.y - p1.y));
end;

// Function to find the bottom-most point
function FindBottomMostPoint(points: PointArray): integer;
var
  i, bottomIndex: integer;
  bottomY: real;
begin
  bottomIndex := 0;
  bottomY := points[0].y;
  
  for i := 1 to High(points) do
  begin
    if (points[i].y < bottomY) or 
       ((points[i].y = bottomY) and (points[i].x < points[bottomIndex].x)) then
    begin
      bottomY := points[i].y;
      bottomIndex := i;
    end;
  end;
  
  FindBottomMostPoint := bottomIndex;
end;

// Function to sort points by polar angle
procedure SortByPolarAngle(var points: PointArray; basePoint: Point);
var
  i, j: integer;
  temp: Point;
  angle1, angle2: real;
begin
  for i := 0 to High(points) - 1 do
  begin
    for j := i + 1 to High(points) do
    begin
      // Calculate angles from base point
      angle1 := arctan2(points[i].y - basePoint.y, points[i].x - basePoint.x);
      angle2 := arctan2(points[j].y - basePoint.y, points[j].x - basePoint.x);
      
      if angle1 > angle2 then
      begin
        temp := points[i];
        points[i] := points[j];
        points[j] := temp;
      end;
    end;
  end;
end;

// Graham Scan algorithm for convex hull
function GrahamScan(points: PointArray): PointArray;
var
  hull: PointArray;
  i, j: integer;
  n: integer;
begin
  n := Length(points);
  
  // Handle edge cases
  if n < 3 then
  begin
    SetLength(hull, n);
    for i := 0 to n - 1 do
      hull[i] := points[i];
    GrahamScan := hull;
    exit;
  end;
  
  // Find bottom-most point
  i := FindBottomMostPoint(points);
  
  // Swap bottom-most point to first position
  if i <> 0 then
  begin
    points[0] := points[0] + points[i];
    points[i] := points[0] - points[i];
    points[0] := points[0] - points[i];
  end;
  
  // Sort points by polar angle
  SortByPolarAngle(points, points[0]);
  
  // Initialize hull with first three points
  SetLength(hull, 3);
  for i := 0 to 2 do
    hull[i] := points[i];
  
  // Process remaining points
  for i := 3 to n - 1 do
  begin
    // Remove points that make clockwise turn
    while (Length(hull) >= 2) and 
          (CrossProduct(hull[Length(hull)-2], hull[Length(hull)-1], points[i]) <= 0) do
    begin
      SetLength(hull, Length(hull) - 1);
    end;
    
    SetLength(hull, Length(hull) + 1);
    hull[Length(hull)-1] := points[i];
  end;
  
  GrahamScan := hull;
end;

// Procedure to print points
procedure PrintPoints(points: PointArray);
var
  i: integer;
begin
  writeln('Convex Hull Points:');
  for i := 0 to High(points) do
  begin
    writeln('(', points[i].x:0:2, ', ', points[i].y:0:2, ')');
  end;
end;

var
  points: PointArray;
  hull: PointArray;
  i: integer;

begin
  // Initialize sample points
  SetLength(points, 6);
  points[0].x := 0; points[0].y := 3;
  points[1].x := 1; points[1].y := 1;
  points[2].x := 2; points[2].y := 2;
  points[3].x := 4; points[3].y := 4;
  points[4].x := 0; points[4].y := 0;
  points[5].x := 1; points[5].y := 2;
  
  writeln('Input Points:');
  for i := 0 to High(points) do
  begin
    writeln('(', points[i].x:0:2, ', ', points[i].y:0:2, ')');
  end;
  writeln;
  
  // Find convex hull
  hull := GrahamScan(points);
  
  // Print result
  PrintPoints(hull);
  
  readln;
end.
```

## Algorithm Explanation

This implementation uses the **Graham Scan algorithm** which works as follows:

1. **Find the bottom-most point** - This point is always part of the convex hull
2. **Sort all points** by polar angle with respect to the bottom-most point
3. **Process points in sorted order** and use a stack to maintain the convex hull
4. **Remove points** that create clockwise turns (using cross product)
5. **Return the final hull points**

## Key Features

- **Cross Product Calculation**: Used to determine the orientation of three points
- **Polar Angle Sorting**: Points are sorted by their angle from the base point
- **Stack-based Processing**: Efficiently builds the convex hull
- **Edge Case Handling**: Properly handles cases with fewer than 3 points

## Time Complexity
- **Time**: O(n log n) due to sorting
- **Space**: O(n) for storing points and hull

The algorithm correctly identifies the smallest convex polygon that contains all input points.

