# De Boor's Algorithm in Pascal

Here's an implementation of De Boor's algorithm for evaluating B-spline curves in Pascal:

```pascal
program DeBoorAlgorithm;

type
  TPoint = record
    x, y: Real;
  end;
  
  TPointArray = array of TPoint;
  TRealArray = array of Real;

// De Boor's algorithm for B-spline evaluation
function DeBoor(n, k, i: Integer; t: Real; T: TRealArray; P: TPointArray): TPoint;
var
  j, r: Integer;
  alpha: Real;
  temp: array[0..10] of TPoint; // Temporary storage for intermediate points
begin
  // Initialize temporary array with control points
  for j := 0 to k do
  begin
    temp[j] := P[i - k + j];
  end;
  
  // De Boor's recursion
  for r := 1 to k do
  begin
    for j := k downto r do
    begin
      alpha := (t - T[i - k + j]) / (T[i - j + 1] - T[i - k + j]);
      temp[j].x := (1 - alpha) * temp[j - 1].x + alpha * temp[j].x;
      temp[j].y := (1 - alpha) * temp[j - 1].y + alpha * temp[j].y;
    end;
  end;
  
  DeBoor := temp[k];
end;

// Helper function to find the knot span
function FindSpan(n, k: Integer; t: Real; T: TRealArray): Integer;
var
  low, high, mid: Integer;
begin
  if (t >= T[n]) then
  begin
    FindSpan := n;
    exit;
  end;
  
  low := k;
  high := n + 1;
  mid := (low + high) div 2;
  
  while (t < T[mid]) or (t >= T[mid + 1]) do
  begin
    if (t < T[mid]) then
      high := mid
    else
      low := mid;
    mid := (low + high) div 2;
  end;
  
  FindSpan := mid;
end;

// Main program demonstrating De Boor's algorithm
procedure DemonstrateDeBoor;
var
  i, span: Integer;
  t: Real;
  knots: TRealArray;
  points: TPointArray;
  result: TPoint;
begin
  // Define a simple cubic B-spline (degree 3)
  // Number of control points
  n := 4;
  k := 3; // Degree
  
  // Knot vector (clamped)
  SetLength(knots, n + k + 1);
  knots[0] := 0; knots[1] := 0; knots[2] := 0; knots[3] := 0;
  knots[4] := 1; knots[5] := 1; knots[6] := 1; knots[7] := 1;
  
  // Control points
  SetLength(points, n + 1);
  points[0].x := 0; points[0].y := 0;
  points[1].x := 1; points[1].y := 2;
  points[2].x := 2; points[2].y := 1;
  points[3].x := 3; points[3].y := 3;
  points[4].x := 4; points[4].y := 2;
  
  // Evaluate at parameter t = 0.5
  t := 0.5;
  span := FindSpan(n, k, t, knots);
  result := DeBoor(n, k, span, t, knots, points);
  
  WriteLn('De Boor Algorithm Result:');
  WriteLn('Parameter t = ', t:0:2);
  WriteLn('Span = ', span);
  WriteLn('Point coordinates: (', result.x:0:4, ', ', result.y:0:4, ')');
  
  // Evaluate at another parameter
  t := 0.75;
  span := FindSpan(n, k, t, knots);
  result := DeBoor(n, k, span, t, knots, points);
  
  WriteLn('Parameter t = ', t:0:2);
  WriteLn('Span = ', span);
  WriteLn('Point coordinates: (', result.x:0:4, ', ', result.y:0:4, ')');
end;

begin
  DemonstrateDeBoor;
end.
```

## Key Components of the Implementation:

1. **TPoint Record**: Represents a 2D point with x and y coordinates
2. **DeBoor Function**: Implements the core De Boor algorithm for B-spline evaluation
3. **FindSpan Function**: Determines the correct span for a given parameter value
4. **Main Demonstration**: Shows how to use the algorithm with sample control points

## Algorithm Steps:

1. **FindSpan**: Locate the correct interval in the knot vector
2. **Initialize**: Set up the initial control points for the recursion
3. **Recursion**: Apply the De Boor recursion formula to compute the point
4. **Return**: The final computed point on the B-spline curve

The algorithm evaluates B-spline curves by recursively computing weighted averages of control points, where the weights are determined by the knot vector and parameter value.

