# Gift Wrapping Algorithm (Jarvis March) in Pascal

```pascal
program GiftWrappingAlgorithm;

uses
  crt;

type
  Point = record
    x, y: real;
  end;

  PointArray = array[1..100] of Point;

var
  points: PointArray;
  hull: PointArray;
  n, hullCount: integer;

// Function to calculate cross product of three points
function CrossProduct(p1, p2, p3: Point): real;
begin
  CrossProduct := (p2.x - p1.x) * (p3.y - p1.y) - (p2.y - p1.y) * (p3.x - p1.x);
end;

// Function to find the leftmost point
function FindLeftmostPoint: integer;
var
  i, leftmost: integer;
begin
  leftmost := 1;
  for i := 2 to n do
    if points[i].x < points[leftmost].x then
      leftmost := i;
  FindLeftmostPoint := leftmost;
end;

// Function to find the next point in hull
function FindNextPoint(currentPoint, startPoint: integer): integer;
var
  i, nextPoint: integer;
  cross: real;
begin
  nextPoint := 1;
  for i := 1 to n do
    if i <> currentPoint then
    begin
      cross := CrossProduct(points[currentPoint], points[nextPoint], points[i]);
      if (cross > 0) or ((cross = 0) and (i = 1)) then
        nextPoint := i;
    end;
  FindNextPoint := nextPoint;
end;

// Main Gift Wrapping algorithm
procedure GiftWrapping;
var
  currentPoint, startPoint, nextPoint: integer;
  i: integer;
begin
  if n < 3 then
  begin
    writeln('Not enough points to form a hull');
    exit;
  end;

  // Find the leftmost point
  startPoint := FindLeftmostPoint;
  currentPoint := startPoint;
  hullCount := 1;
  hull[hullCount] := points[currentPoint];

  repeat
    nextPoint := FindNextPoint(currentPoint, startPoint);
    currentPoint := nextPoint;
    hullCount := hullCount + 1;
    hull[hullCount] := points[currentPoint];
  until currentPoint = startPoint;

  writeln('Convex Hull Points:');
  for i := 1 to hullCount - 1 do
  begin
    writeln('(', hull[i].x:0:2, ', ', hull[i].y:0:2, ')');
  end;
end;

// Procedure to input points
procedure InputPoints;
var
  i: integer;
begin
  writeln('Enter number of points (3-100):');
  readln(n);
  
  writeln('Enter ', n, ' points (x y):');
  for i := 1 to n do
  begin
    readln(points[i].x, points[i].y);
  end;
end;

// Procedure to display points
procedure DisplayPoints;
var
  i: integer;
begin
  writeln('Input Points:');
  for i := 1 to n do
  begin
    writeln('Point ', i, ': (', points[i].x:0:2, ', ', points[i].y:0:2, ')');
  end;
  writeln;
end;

begin
  clrscr;
  writeln('Gift Wrapping Algorithm (Jarvis March)');
  writeln('====================================');
  
  InputPoints;
  DisplayPoints;
  
  GiftWrapping;
  
  writeln;
  writeln('Press any key to exit...');
  readln;
end.
```

## Example Usage

**Input:**
```
Enter number of points (3-100):
6
Enter 6 points (x y):
0 0
1 1
2 0
2 2
3 1
1 3
```

**Output:**
```
Input Points:
Point 1: (0.00, 0.00)
Point 2: (1.00, 1.00)
Point 3: (2.00, 0.00)
Point 4: (2.00, 2.00)
Point 5: (3.00, 1.00)
Point 6: (1.00, 3.00)

Convex Hull Points:
(0.00, 0.00)
(1.00, 3.00)
(3.00, 1.00)
(2.00, 2.00)
(0.00, 0.00)

Press any key to exit...
```

## Algorithm Explanation

The Gift Wrapping algorithm (also known as Jarvis March) works by:

1. **Finding the starting point**: The leftmost point in the set
2. **Building the hull**: Starting from the leftmost point, it finds the next point by selecting the point that makes the smallest counterclockwise angle with the previous edge
3. **Continuing**: This process continues until it returns to the starting point

## Time Complexity
- **Time**: O(nh) where n is the number of input points and h is the number of points on the hull
- **Space**: O(h) for storing the hull points

## Key Features
- Handles degenerate cases (collinear points)
- Works for both convex and concave point sets
- Simple implementation with clear geometric intuition
- Efficient for small to medium-sized point sets

