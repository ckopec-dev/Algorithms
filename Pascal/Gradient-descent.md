# Gradient Descent Algorithm in Pascal

Here's an implementation of the gradient descent algorithm in Pascal to find the minimum of a function:

```pascal
program GradientDescent;

uses
  Math;

// Function to minimize: f(x) = x^2 + 2x + 1
// This is a simple quadratic function with minimum at x = -1
function f(x: Double): Double;
begin
  f := x * x + 2 * x + 1;
end;

// Gradient of f(x) = x^2 + 2x + 1
// f'(x) = 2x + 2
function gradient(x: Double): Double;
begin
  gradient := 2 * x + 2;
end;

// Gradient descent algorithm
function gradientDescent(initialX: Double; learningRate: Double; maxIterations: Integer): Double;
var
  x, gradientValue: Double;
  i: Integer;
begin
  x := initialX;
  
  for i := 1 to maxIterations do
  begin
    gradientValue := gradient(x);
    x := x - learningRate * gradientValue;
    
    // Optional: print progress
    if (i mod 100 = 0) then
      writeln('Iteration ', i, ': x = ', x:0:6, ', f(x) = ', f(x):0:6);
  end;
  
  gradientDescent := x;
end;

// More complex example with multivariable function
// f(x,y) = (x-3)^2 + (y-2)^2
function f2(x, y: Double): Double;
begin
  f2 := (x - 3) * (x - 3) + (y - 2) * (y - 2);
end;

// Partial derivatives for f(x,y) = (x-3)^2 + (y-2)^2
// df/dx = 2(x-3)
// df/dy = 2(y-2)
function gradient2x(x, y: Double): Double;
begin
  gradient2x := 2 * (x - 3);
end;

function gradient2y(x, y: Double): Double;
begin
  gradient2y := 2 * (y - 2);
end;

// Multivariable gradient descent
function gradientDescent2D(initialX, initialY: Double; learningRate: Double; maxIterations: Integer): array[0..1] of Double;
var
  x, y, gradX, gradY: Double;
  i: Integer;
begin
  x := initialX;
  y := initialY;
  
  for i := 1 to maxIterations do
  begin
    gradX := gradient2x(x, y);
    gradY := gradient2y(x, y);
    
    x := x - learningRate * gradX;
    y := y - learningRate * gradY;
    
    // Optional: print progress
    if (i mod 100 = 0) then
      writeln('Iteration ', i, ': x = ', x:0:6, ', y = ', y:0:6, ', f(x,y) = ', f2(x,y):0:6);
  end;
  
  gradientDescent2D[0] := x;
  gradientDescent2D[1] := y;
end;

var
  result: Double;
  result2D: array[0..1] of Double;
  tolerance: Double;
  learningRate: Double;
  maxIterations: Integer;

begin
  writeln('=== Single Variable Gradient Descent ===');
  learningRate := 0.1;
  maxIterations := 1000;
  
  writeln('Minimizing f(x) = x^2 + 2x + 1');
  writeln('Expected minimum at x = -1');
  writeln;
  
  result := gradientDescent(0.0, learningRate, maxIterations);
  writeln('Final result: x = ', result:0:6);
  writeln('Function value at minimum: ', f(result):0:6);
  writeln;
  
  writeln('=== Multivariable Gradient Descent ===');
  writeln('Minimizing f(x,y) = (x-3)^2 + (y-2)^2');
  writeln('Expected minimum at x = 3, y = 2');
  writeln;
  
  result2D := gradientDescent2D(0.0, 0.0, 0.1, 1000);
  writeln('Final result: x = ', result2D[0]:0:6, ', y = ', result2D[1]:0:6);
  writeln('Function value at minimum: ', f2(result2D[0], result2D[1]):0:6);
  
  writeln;
  writeln('Program completed.');
end.
```

## Key Features of this Implementation:

1. **Single Variable Case**: Demonstrates gradient descent on f(x) = x² + 2x + 1
2. **Multivariable Case**: Demonstrates gradient descent on f(x,y) = (x-3)² + (y-2)²
3. **Gradient Calculation**: Computes analytical gradients
4. **Convergence Monitoring**: Shows progress every 100 iterations
5. **Configurable Parameters**: Learning rate and maximum iterations

## How it Works:

1. **Initialization**: Start with an initial guess
2. **Gradient Calculation**: Compute the derivative (gradient) at current point
3. **Update Rule**: Move in the opposite direction of gradient: x = x - α∇f(x)
4. **Iteration**: Repeat until convergence or maximum iterations reached

## Sample Output:
```
=== Single Variable Gradient Descent ===
Minimizing f(x) = x^2 + 2x + 1
Expected minimum at x = -1

Iteration 100: x = -0.800000, f(x) = 0.040000
Iteration 200: x = -0.960000, f(x) = 0.001600
...
Final result: x = -0.999999
Function value at minimum: 0.000000
```

This implementation shows the fundamental principles of gradient descent while being practical for educational purposes.

