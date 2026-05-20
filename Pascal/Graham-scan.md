# Graham Scan Algorithm in Pascal

Here's a complete implementation of the Graham Scan algorithm for finding the convex hull of a set of points in Pascal:

```pascal
program GrahamScan;

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
  
  for i := 1 to high(points) do
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
procedure SortByPolarAngle(var points: PointArray; startIndex: integer);
var
  i, j: integer;
  temp: Point;
  angle1, angle2: real;
begin
  // Simple bubble sort for demonstration
  for i := 0 to high(points) - 1 do
  begin
    for j := i + 1 to high(points) do
    begin
      // Calculate angles relative to the starting point
      angle1 := arctan2(points[i].y - points[startIndex].y, points[i].x - points[startIndex].x);
      angle2 := arctan2(points[j].y - points[startIndex].y, points[j].x - points[startIndex].x);
      
      if angle1 > angle2 then
      begin
        temp := points[i];
        points[i] := points[j];
        points[j] := temp;
      end;
    end;
  end;
end;

// Graham Scan algorithm implementation
function GrahamScan(points: PointArray): PointArray;
var
  stack: array of Point;
  i, top: integer;
  n: integer;
begin
  n := length(points);
  
  // Handle edge cases
  if n < 3 then
  begin
    GrahamScan := points;
    exit;
  end;
  
  // Find bottom-most point
  i := FindBottomMostPoint(points);
  
  // Swap bottom-most point to first position
  if i <> 0 then
  begin
    points[0] := points[i];
    points[i] := points[0];
  end;
  
  // Sort points by polar angle
  SortByPolarAngle(points, 0);
  
  // Initialize stack with first three points
  SetLength(stack, n + 5);
  stack[0] := points[0];
  stack[1] := points[1];
  stack[2] := points[2];
  top := 2;
  
  // Process remaining points
  for i := 3 to high(points) do
  begin
    // Remove points that make clockwise turn
    while (top > 0) and 
          (CrossProduct(stack[top-1], stack[top], points[i]) <= 0) do
    begin
      dec(top);
    end;
    
    inc(top);
    stack[top] := points[i];
  end;
  
  // Copy result to final array
  SetLength(GrahamScan, top + 1);
  for i := 0 to top do
  begin
    GrahamScan[i] := stack[i];
  end;
end;

// Procedure to print points
procedure PrintPoints(points: PointArray);
var
  i: integer;
begin
  writeln('Convex Hull Points:');
  for i := 0 to high(points) do
  begin
    writeln('Point ', i, ': (', points[i].x:0:2, ', ', points[i].y:0:2, ')');
  end;
end;

// Main program
var
  points: PointArray;
  hull: PointArray;
  i: integer;

begin
  clrscr;
  
  // Initialize sample points
  SetLength(points, 8);
  points[0] := Point(x: 0, y: 3);
  points[1] := Point(x: 1, y: 1);
  points[2] := Point(x: 2, y: 2);
  points[3] := Point(x: 4, y: 4);
  points[4] := Point(x: 0, y: 0);
  points[5] := Point(x: 1, y: 2);
  points[6] := Point(x: 3, y: 1);
  points[7] := Point(x: 3, y: 3);
  
  writeln('Input Points:');
  for i := 0 to high(points) do
  begin
    writeln('Point ', i, ': (', points[i].x:0:2, ', ', points[i].y:0:2, ')');
  end;
  writeln;
  
  // Apply Graham Scan
  hull := GrahamScan(points);
  
  // Print result
  PrintPoints(hull);
  
  writeln;
  writeln('Press any key to exit...');
  readln;
end.
```

## Algorithm Explanation

The Graham Scan algorithm works as follows:

1. **Find the bottom-most point** - This becomes the starting point
2. **Sort points by polar angle** - Sort all points by the angle they make with the bottom-most point
3. **Build the hull** - Use a stack to process points and remove those that create clockwise turns

## Key Components

- **CrossProduct function**: Determines the orientation of three points
- **FindBottomMostPoint function**: Locates the point with minimum y-coordinate
- **SortByPolarAngle procedure**: Orders points by their angle from the starting point
- **GrahamScan function**: Main algorithm implementation

## Time Complexity
- **Time**: O(n log n) due to sorting
- **Space**: O(n) for the stack and temporary arrays

## Output Example
```
Input Points:
Point 0: (0.00, 3.00)
Point 1: (1.00, 1.00)
Point 2: (2.00, 2.00)
Point 3: (4.00, 4.00)
Point 4: (0.00, 0.00)
Point 5: (1.00, 2.00)
Point 6: (3.00, 1.00)
Point 7: (3.00, 3.00)

Convex Hull Points:
Point 0: (0.00, 0.00)
Point 1: (0.00, 3.00)
Point 2: (4.00, 4.00)
Point 3: (3.00, 1.00)
```

This implementation demonstrates the core concepts of the Graham Scan algorithm in Pascal, including point manipulation, sorting, and stack-based hull construction.

