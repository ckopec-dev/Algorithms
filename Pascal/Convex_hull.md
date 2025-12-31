# Convex Hull Algorithm in Pascal

Here's an implementation of the Graham Scan algorithm for finding the convex hull of a set of points in Pascal:

```pascal
program ConvexHull;

uses
  crt;

type
  Point = record
    x, y: real;
  end;
  
  PointArray = array of Point;

// Function to calculate cross product of three points
function CrossProduct(p1, p2, p3: Point): real;
begin
  CrossProduct := (p2.x - p1.x) * (p3.y - p2.y) - (p2.y - p1.y) * (p3.x - p2.x);
end;

// Function to calculate distance between two points
function Distance(p1, p2: Point): real;
begin
  Distance := sqrt(sqr(p2.x - p1.x) + sqr(p2.y - p1.y));
end;

// Function to find the point with minimum y-coordinate (and minimum x if tie)
function FindBottomPoint(points: PointArray): integer;
var
  i, minIndex: integer;
  minY, minX: real;
begin
  minIndex := 0;
  minY := points[0].y;
  minX := points[0].x;
  
  for i := 1 to High(points) do
  begin
    if (points[i].y < minY) or ((points[i].y = minY) and (points[i].x < minX)) then
    begin
      minY := points[i].y;
      minX := points[i].x;
      minIndex := i;
    end;
  end;
  
  FindBottomPoint := minIndex;
end;

// Function to sort points by polar angle with respect to the bottom point
procedure SortByPolarAngle(var points: PointArray; bottomIndex: integer);
var
  i, j: integer;
  temp: Point;
  angle: array of real;
  n: integer;
begin
  n := Length(points);
  SetLength(angle, n);
  
  // Calculate angles for all points
  for i := 0 to n - 1 do
  begin
    if i = bottomIndex then
      angle[i] := -1  // Special case for bottom point
    else
      angle[i] := arctan2(points[i].y - points[bottomIndex].y, 
                         points[i].x - points[bottomIndex].x);
  end;
  
  // Simple bubble sort by angle
  for i := 0 to n - 2 do
  begin
    for j := i + 1 to n - 1 do
    begin
      if angle[i] > angle[j] then
      begin
        // Swap angles
        temp := points[i];
        points[i] := points[j];
        points[j] := temp;
        
        // Swap corresponding angles
        temp := points[i];
        points[i] := points[j];
        points[j] := temp;
      end;
    end;
  end;
end;

// Graham Scan algorithm
procedure GrahamScan(points: PointArray; var hull: PointArray);
var
  i, n, top: integer;
  stack: array of Point;
begin
  n := Length(points);
  if n < 3 then
  begin
    hull := points;
    exit;
  end;
  
  SetLength(stack, n + 5);  // Extra space for safety
  top := 0;
  
  // Add first three points to stack
  stack[0] := points[0];
  stack[1] := points[1];
  stack[2] := points[2];
  top := 2;
  
  // Process remaining points
  for i := 3 to n - 1 do
  begin
    // Remove points from stack while the turn is not counterclockwise
    while (top > 1) and (CrossProduct(stack[top-2], stack[top-1], points[i]) <= 0) do
      dec(top);
    
    // Add current point to stack
    inc(top);
    stack[top] := points[i];
  end;
  
  // Copy result to hull array
  SetLength(hull, top + 1);
  for i := 0 to top do
    hull[i] := stack[i];
end;

// Procedure to print points
procedure PrintPoints(points: PointArray);
var
  i: integer;
begin
  for i := 0 to High(points) do
    writeln('(', points[i].x:0:2, ', ', points[i].y:0:2, ')');
end;

var
  points: PointArray;
  hull: PointArray;
  i, n: integer;

begin
  clrscr;
  
  // Sample points
  n := 6;
  SetLength(points, n);
  
  points[0].x := 0; points[0].y := 3;
  points[1].x := 1; points[1].y := 1;
  points[2].x := 2; points[2].y := 2;
  points[3].x := 4; points[3].y := 4;
  points[4].x := 0; points[4].y := 0;
  points[5].x := 1; points[5].y := 2;
  
  writeln('Input points:');
  PrintPoints(points);
  
  writeln('Convex Hull points:');
  GrahamScan(points, hull);
  PrintPoints(hull);
  
  readln;
end.
```

## Key Features of this Implementation:

1. **Point Structure**: Defines a Point record with x and y coordinates
2. **Cross Product Function**: Used to determine the orientation of three points
3. **Graham Scan Algorithm**: Implements the standard convex hull algorithm
4. **Polar Angle Sorting**: Sorts points by their polar angle relative to the bottom point
5. **Stack-based Processing**: Uses a stack to build the convex hull incrementally

## Algorithm Steps:

1. Find the bottom-most point
2. Sort all points by polar angle with respect to the bottom point
3. Process points in order, maintaining a stack of hull points
4. Remove points that make clockwise turns (using cross product)
5. The remaining points form the convex hull

This implementation demonstrates the classic Graham Scan approach for computing convex hulls in 2D space.

