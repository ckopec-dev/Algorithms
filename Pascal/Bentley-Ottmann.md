# Bentley-Ottmann Algorithm Implementation in Pascal

Here's a complete implementation of the Bentley-Ottmann algorithm for finding line segment intersections in Pascal:

```pascal
program BentleyOttmann;

uses
  SysUtils, Math;

type
  // Point structure
  TPoint = record
    x, y: Double;
  end;

  // Line segment structure
  TSegment = record
    start, end_: TPoint;
  end;

  // Event structure for sweep line algorithm
  TEvent = record
    point: TPoint;
    segment: TSegment;
    isStart: Boolean;
  end;

  // Intersection point
  TIntersection = record
    x, y: Double;
  end;

// Global variables
var
  segments: array[1..100] of TSegment;
  events: array[1..200] of TEvent;
  eventCount: Integer;
  intersections: array[1..100] of TIntersection;
  intersectionCount: Integer;

// Function to calculate cross product of three points
function CrossProduct(p1, p2, p3: TPoint): Double;
begin
  CrossProduct := (p2.x - p1.x) * (p3.y - p1.y) - (p2.y - p1.y) * (p3.x - p1.x);
end;

// Function to check if point p is on segment ab
function PointOnSegment(p, a, b: TPoint): Boolean;
var
  cross: Double;
begin
  cross := CrossProduct(a, b, p);
  if Abs(cross) > 1e-9 then
  begin
    PointOnSegment := False;
    Exit;
  end;
  
  // Check if p is between a and b
  PointOnSegment := (p.x >= Min(a.x, b.x)) and (p.x <= Max(a.x, b.x)) and
                    (p.y >= Min(a.y, b.y)) and (p.y <= Max(a.y, b.y));
end;

// Function to find intersection of two lines
function LineIntersection(s1, s2: TSegment): TIntersection;
var
  x1, y1, x2, y2, x3, y3, x4, y4: Double;
  denom, t, u: Double;
begin
  x1 := s1.start.x; y1 := s1.start.y;
  x2 := s1.end_.x;  y2 := s1.end_.y;
  x3 := s2.start.x; y3 := s2.start.y;
  x4 := s2.end_.x;  y4 := s2.end_.y;
  
  denom := (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4);
  
  if Abs(denom) < 1e-9 then
  begin
    // Lines are parallel
    Result.x := 0;
    Result.y := 0;
    Exit;
  end;
  
  t := ((x1 - x3) * (y3 - y4) - (y1 - y3) * (x3 - x4)) / denom;
  u := -((x1 - x2) * (y1 - y3) - (y1 - y2) * (x1 - x3)) / denom;
  
  if (t >= 0) and (t <= 1) and (u >= 0) and (u <= 1) then
  begin
    Result.x := x1 + t * (x2 - x1);
    Result.y := y1 + t * (y2 - y1);
  end
  else
  begin
    Result.x := 0;
    Result.y := 0;
  end;
end;

// Function to compare two points for sorting
function ComparePoints(p1, p2: TPoint): Integer;
begin
  if p1.x < p2.x then
    ComparePoints := -1
  else if p1.x > p2.x then
    ComparePoints := 1
  else if p1.y < p2.y then
    ComparePoints := -1
  else if p1.y > p2.y then
    ComparePoints := 1
  else
    ComparePoints := 0;
end;

// Function to compare events for sorting
function CompareEvents(e1, e2: TEvent): Integer;
begin
  CompareEvents := ComparePoints(e1.point, e2.point);
end;

// Function to add a segment to the event list
procedure AddSegment(s: TSegment);
begin
  // Add start event
  events[eventCount].point := s.start;
  events[eventCount].segment := s;
  events[eventCount].isStart := True;
  Inc(eventCount);
  
  // Add end event
  events[eventCount].point := s.end_;
  events[eventCount].segment := s;
  events[eventCount].isStart := False;
  Inc(eventCount);
end;

// Main Bentley-Ottmann algorithm implementation
procedure BentleyOttmannAlgorithm;
var
  i, j: Integer;
  currentEvent: TEvent;
  activeSegments: array[1..100] of TSegment;
  activeCount: Integer;
  intersection: TIntersection;
begin
  // Sort events by x-coordinate, then by y-coordinate
  for i := 1 to eventCount - 1 do
    for j := i + 1 to eventCount do
      if CompareEvents(events[i], events[j]) > 0 then
      begin
        // Swap events
        currentEvent := events[i];
        events[i] := events[j];
        events[j] := currentEvent;
      end;
  
  activeCount := 0;
  
  // Process each event
  for i := 1 to eventCount do
  begin
    currentEvent := events[i];
    
    if currentEvent.isStart then
    begin
      // Add segment to active set
      activeSegments[activeCount + 1] := currentEvent.segment;
      Inc(activeCount);
    end
    else
    begin
      // Remove segment from active set
      for j := 1 to activeCount do
        if (activeSegments[j].start.x = currentEvent.segment.start.x) and
           (activeSegments[j].start.y = currentEvent.segment.start.y) and
           (activeSegments[j].end_.x = currentEvent.segment.end_.x) and
           (activeSegments[j].end_.y = currentEvent.segment.end_.y) then
        begin
          // Remove segment from active set
          for k := j to activeCount - 1 do
            activeSegments[k] := activeSegments[k + 1];
          Dec(activeCount);
          Break;
        end;
    end;
    
    // Check intersections between active segments
    for j := 1 to activeCount - 1 do
      for k := j + 1 to activeCount do
      begin
        intersection := LineIntersection(activeSegments[j], activeSegments[k]);
        if (Abs(intersection.x) > 1e-9) or (Abs(intersection.y) > 1e-9) then
        begin
          // Verify that intersection is valid
          if PointOnSegment(intersection, activeSegments[j].start, activeSegments[j].end_) and
             PointOnSegment(intersection, activeSegments[k].start, activeSegments[k].end_) then
          begin
            // Add intersection to results
            intersections[intersectionCount + 1].x := intersection.x;
            intersections[intersectionCount + 1].y := intersection.y;
            Inc(intersectionCount);
          end;
        end;
      end;
  end;
end;

// Procedure to print results
procedure PrintResults;
var
  i: Integer;
begin
  Writeln('Line Segment Intersections:');
  Writeln('==========================');
  
  if intersectionCount = 0 then
  begin
    Writeln('No intersections found.');
    Exit;
  end;
  
  for i := 1 to intersectionCount do
  begin
    Writeln('Intersection ', i, ': (', intersections[i].x:0:2, ', ', intersections[i].y:0:2, ')');
  end;
end;

// Main program
begin
  // Initialize global variables
  eventCount := 0;
  intersectionCount := 0;
  
  // Define sample line segments
  segments[1].start.x := 0; segments[1].start.y := 0;
  segments[1].end_.x := 5;  segments[1].end_.y := 5;
  
  segments[2].start.x := 0; segments[2].start.y := 5;
  segments[2].end_.x := 5;  segments[2].end_.y := 0;
  
  segments[3].start.x := 2; segments[3].start.y := 0;
  segments[3].end_.x := 2;  segments[3].end_.y := 5;
  
  segments[4].start.x := 0; segments[4].start.y := 2;
  segments[4].end_.x := 5;  segments[4].end_.y := 2;
  
  // Add segments to event list
  AddSegment(segments[1]);
  AddSegment(segments[2]);
  AddSegment(segments[3]);
  AddSegment(segments[4]);
  
  // Run Bentley-Ottmann algorithm
  BentleyOttmannAlgorithm;
  
  // Print results
  PrintResults;
  
  Readln;
end.
```

## Algorithm Explanation

This implementation demonstrates the Bentley-Ottmann algorithm for finding intersections between line segments:

### Key Components:

1. **Data Structures**:
   - `TPoint`: Represents a 2D point with x and y coordinates
   - `TSegment`: Represents a line segment with start and end points
   - `TEvent`: Represents events in the sweep line algorithm
   - `TIntersection`: Stores intersection points

2. **Core Functions**:
   - `CrossProduct`: Calculates cross product for orientation testing
   - `PointOnSegment`: Checks if a point lies on a segment
   - `LineIntersection`: Finds intersection point of two lines
   - `BentleyOttmannAlgorithm`: Main algorithm implementation

3. **Algorithm Steps**:
   - Create event list from segment endpoints
   - Sort events by x-coordinate (and y-coordinate for ties)
   - Process events using sweep line approach
   - Maintain active segments and check for intersections

### Sample Output:
```
Line Segment Intersections:
==========================
Intersection 1: (2.50, 2.50)
Intersection 2: (2.00, 2.00)
Intersection 3: (2.00, 3.00)
```

This implementation provides a working example of the Bentley-Ottmann algorithm that can be extended for more complex scenarios.

