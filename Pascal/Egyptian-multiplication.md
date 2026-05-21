# Egyptian Multiplication Algorithm in Pascal

The Egyptian multiplication algorithm, also known as binary multiplication or Russian peasant multiplication, is an ancient method of multiplying two numbers using repeated doubling and addition.

```pascal
program EgyptianMultiplication;

function egyptianMultiply(a, b: integer): integer;
var
  result, multiplier, multiplicand: integer;
  isEven: boolean;
begin
  // Handle edge cases
  if (a = 0) or (b = 0) then
  begin
    egyptianMultiply := 0;
    exit;
  end;
  
  // Ensure a is the smaller number for efficiency
  if a > b then
  begin
    multiplier := b;
    multiplicand := a;
  end
  else
  begin
    multiplier := a;
    multiplicand := b;
  end;
  
  result := 0;
  
  // Main algorithm
  while multiplier > 0 do
  begin
    // If multiplier is odd, add the multiplicand to result
    if (multiplier mod 2) = 1 then
      result := result + multiplicand;
    
    // Double the multiplicand and halve the multiplier
    multiplicand := multiplicand * 2;
    multiplier := multiplier div 2;
  end;
  
  egyptianMultiply := result;
end;

procedure showProcess(a, b: integer);
var
  multiplier, multiplicand, result: integer;
  step: integer;
begin
  writeln('Egyptian multiplication of ', a, ' × ', b);
  writeln('----------------------------------------');
  
  if a > b then
  begin
    multiplier := b;
    multiplicand := a;
  end
  else
  begin
    multiplier := a;
    multiplicand := b;
  end;
  
  result := 0;
  step := 1;
  
  while multiplier > 0 do
  begin
    if (multiplier mod 2) = 1 then
    begin
      writeln('Step ', step, ': ', multiplier, ' is odd → add ', multiplicand);
      result := result + multiplicand;
    end
    else
    begin
      writeln('Step ', step, ': ', multiplier, ' is even → skip');
    end;
    
    writeln('         ', multiplier, ' → ', multiplier div 2);
    writeln('         ', multiplicand, ' → ', multiplicand * 2);
    writeln('         Current result: ', result);
    writeln;
    
    multiplicand := multiplicand * 2;
    multiplier := multiplier div 2;
    step := step + 1;
  end;
  
  writeln('Final result: ', result);
end;

var
  num1, num2: integer;

begin
  writeln('Egyptian Multiplication Algorithm Demo');
  writeln;
  
  // Example 1
  num1 := 17;
  num2 := 23;
  writeln('Example 1:');
  showProcess(num1, num2);
  writeln('Verification: ', num1, ' × ', num2, ' = ', egyptianMultiply(num1, num2));
  writeln;
  
  // Example 2
  num1 := 13;
  num2 := 9;
  writeln('Example 2:');
  showProcess(num1, num2);
  writeln('Verification: ', num1, ' × ', num2, ' = ', egyptianMultiply(num1, num2));
  writeln;
  
  // Example 3 - Simple case
  num1 := 5;
  num2 := 6;
  writeln('Example 3:');
  showProcess(num1, num2);
  writeln('Verification: ', num1, ' × ', num2, ' = ', egyptianMultiply(num1, num2));
end.
```

## How it works:

1. **Initialize**: Start with two numbers `a` and `b`
2. **Loop**: While the multiplier is greater than 0:
   - If the multiplier is odd, add the current multiplicand to the result
   - Double the multiplicand and halve the multiplier
3. **Return**: The accumulated result

## Example trace for 17 × 23:

```
Step 1: 17 is odd → add 23
         17 → 8
         23 → 46
         Current result: 23

Step 2: 8 is even → skip
         8 → 4
         46 → 92
         Current result: 23

Step 3: 4 is even → skip
         4 → 2
         92 → 184
         Current result: 23

Step 4: 2 is even → skip
         2 → 1
         184 → 368
         Current result: 23

Step 5: 1 is odd → add 368
         1 → 0
         368 → 736
         Current result: 391

Final result: 391
```

This algorithm works by expressing one number in binary and using the distributive property of multiplication.

