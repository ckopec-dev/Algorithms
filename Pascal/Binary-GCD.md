```pascal
program BinaryGCD;

function BinaryGCD(a, b: integer): integer;
var
  shift: integer;
begin
  // Handle edge cases
  if (a = 0) then
  begin
    BinaryGCD := b;
    exit;
  end;
  
  if (b = 0) then
  begin
    BinaryGCD := a;
    exit;
  end;
  
  // Find the common factors of 2
  shift := 0;
  while ((a or b) and 1) = 0 do
  begin
    a := a shr 1;
    b := b shr 1;
    inc(shift);
  end;
  
  // Remove remaining factors of 2 from a
  while (a and 1) = 0 do
    a := a shr 1;
  
  repeat
    // Remove factors of 2 from b
    while (b and 1) = 0 do
      b := b shr 1;
    
    // Ensure a <= b
    if (a > b) then
    begin
      a := a xor b;
      b := a xor b;
      a := a xor b;
    end;
    
    // Subtract
    b := b - a;
    
  until (b = 0);
  
  // Restore common factors of 2
  BinaryGCD := a shl shift;
end;

// Test the function
var
  num1, num2, result: integer;

begin
  writeln('Binary GCD Algorithm Example');
  writeln('============================');
  
  // Test cases
  num1 := 48;
  num2 := 18;
  result := BinaryGCD(num1, num2);
  writeln('GCD(', num1, ', ', num2, ') = ', result);
  
  num1 := 100;
  num2 := 25;
  result := BinaryGCD(num1, num2);
  writeln('GCD(', num1, ', ', num2, ') = ', result);
  
  num1 := 17;
  num2 := 13;
  result := BinaryGCD(num1, num2);
  writeln('GCD(', num1, ', ', num2, ') = ', result);
  
  num1 := 0;
  num2 := 5;
  result := BinaryGCD(num1, num2);
  writeln('GCD(', num1, ', ', num2, ') = ', result);
end.
```

This Pascal implementation of the Binary GCD algorithm (also known as Stein's algorithm) works by:

1. **Handling edge cases**: When either number is zero, return the other number
2. **Finding common factors of 2**: Shift both numbers right until at least one becomes odd
3. **Removing factors of 2 from the first number**: Ensure the first number is odd
4. **Iterative subtraction**: Keep subtracting the smaller number from the larger one until one becomes zero
5. **Restoring factors of 2**: Multiply the result by the common factors of 2 that were removed

The algorithm is more efficient than the traditional Euclidean algorithm for large numbers because it uses bit operations instead of division.

