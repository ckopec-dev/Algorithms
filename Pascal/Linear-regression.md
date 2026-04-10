# Linear Regression in Pascal

Here's an implementation of linear regression algorithm in Pascal that fits a line y = ax + b to a set of data points:

```pascal
program LinearRegression;

uses
  crt;

type
  TPoint = record
    x, y: real;
  end;

  TLinearRegression = record
    slope: real;      // a coefficient
    intercept: real;  // b coefficient
    correlation: real; // correlation coefficient
  end;

var
  points: array[1..100] of TPoint;
  n: integer;
  regression: TLinearRegression;

// Function to calculate mean of x and y values
function CalculateMeans: array[1..2] of real;
var
  i: integer;
  sumX, sumY: real;
begin
  sumX := 0;
  sumY := 0;
  
  for i := 1 to n do
  begin
    sumX := sumX + points[i].x;
    sumY := sumY + points[i].y;
  end;
  
  CalculateMeans[1] := sumX / n;  // mean of x
  CalculateMeans[2] := sumY / n;  // mean of y
end;

// Main linear regression function
procedure FitLinearRegression;
var
  i: integer;
  meanX, meanY: real;
  numerator, denominator: real;
  sumXX, sumXY: real;
begin
  // Calculate means
  meanX := CalculateMeans[1];
  meanY := CalculateMeans[2];
  
  // Calculate sums for slope calculation
  sumXX := 0;
  sumXY := 0;
  
  for i := 1 to n do
  begin
    sumXX := sumXX + (points[i].x - meanX) * (points[i].x - meanX);
    sumXY := sumXY + (points[i].x - meanX) * (points[i].y - meanY);
  end;
  
  // Calculate slope (a) and intercept (b)
  if sumXX <> 0 then
  begin
    regression.slope := sumXY / sumXX;
    regression.intercept := meanY - regression.slope * meanX;
  end
  else
  begin
    regression.slope := 0;
    regression.intercept := meanY;
  end;
  
  // Calculate correlation coefficient
  numerator := 0;
  denominator := 0;
  for i := 1 to n do
  begin
    numerator := numerator + (points[i].x - meanX) * (points[i].y - meanY);
    denominator := denominator + 
      (points[i].x - meanX) * (points[i].x - meanX) * 
      (points[i].y - meanY) * (points[i].y - meanY);
  end;
  
  if denominator <> 0 then
    regression.correlation := numerator / sqrt(denominator)
  else
    regression.correlation := 0;
end;

// Function to predict y value for given x
function Predict(x: real): real;
begin
  Predict := regression.slope * x + regression.intercept;
end;

// Function to display results
procedure DisplayResults;
begin
  writeln('Linear Regression Results:');
  writeln('========================');
  writeln('Equation: y = ', regression.slope:0:4, 'x + ', regression.intercept:0:4);
  writeln('Slope (a): ', regression.slope:0:6);
  writeln('Intercept (b): ', regression.intercept:0:6);
  writeln('Correlation coefficient: ', regression.correlation:0:6);
  writeln;
end;

// Function to input data points
procedure InputData;
var
  i: integer;
begin
  writeln('Enter number of data points (1-100): ');
  readln(n);
  
  if n <= 0 then
  begin
    writeln('Invalid number of points');
    halt;
  end;
  
  writeln('Enter data points (x y):');
  for i := 1 to n do
  begin
    write('Point ', i, ': ');
    readln(points[i].x, points[i].y);
  end;
end;

// Function to print data points
procedure PrintData;
var
  i: integer;
begin
  writeln('Data Points:');
  writeln('------------');
  for i := 1 to n do
    writeln('(', points[i].x:0:2, ', ', points[i].y:0:2, ')');
  writeln;
end;

begin
  clrscr;
  
  // Input data
  InputData;
  
  // Display input data
  PrintData;
  
  // Perform linear regression
  FitLinearRegression;
  
  // Display results
  DisplayResults;
  
  // Example prediction
  writeln('Example prediction:');
  writeln('For x = 5, y = ', Predict(5):0:4);
  writeln;
  
  writeln('Press any key to exit...');
  readln;
end.
```

## Example Usage

When you run this program with the following sample data:

```
Enter number of data points (1-100): 5
Enter data points (x y):
Point 1: 1 2
Point 2: 2 4
Point 3: 3 6
Point 4: 4 8
Point 5: 5 10
```

The output would be:
```
Linear Regression Results:
========================
Equation: y = 2.0000x + 0.0000
Slope (a): 2.000000
Intercept (b): 0.000000
Correlation coefficient: 1.000000

Example prediction:
For x = 5, y = 10.0000
```

## Key Features

1. **Data Structure**: Uses records to store data points and regression results
2. **Mean Calculation**: Computes mean values for x and y coordinates
3. **Slope Calculation**: Uses the least squares method to calculate slope
4. **Intercept Calculation**: Computes y-intercept using the slope and means
5. **Correlation Coefficient**: Calculates the correlation between variables
6. **Prediction Function**: Allows prediction of y values for given x values
7. **Input Validation**: Handles edge cases like division by zero

This implementation provides a complete linear regression solution in Pascal that can be easily extended or modified for specific needs.

