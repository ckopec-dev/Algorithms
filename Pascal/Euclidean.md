```pascal
program EuclideanAlgorithm;

function gcd(a, b: integer): integer;
begin
  while b <> 0 do
  begin
    gcd := a mod b;
    a := b;
    b := gcd;
  end;
  gcd := a;
end;

var
  num1, num2, result: integer;

begin
  writeln('Euclidean Algorithm - Greatest Common Divisor');
  writeln('============================================');
  
  writeln('Enter first number: ');
  readln(num1);
  
  writeln('Enter second number: ');
  readln(num2);
  
  if (num1 < 0) or (num2 < 0) then
  begin
    writeln('Please enter positive integers only.');
    exit;
  end;
  
  result := gcd(num1, num2);
  
  writeln('GCD of ', num1, ' and ', num2, ' is: ', result);
  
  // Example with specific values
  writeln;
  writeln('Example with predefined values:');
  writeln('GCD of 48 and 18 is: ', gcd(48, 18));
end.
```

**Output:**
```
Euclidean Algorithm - Greatest Common Divisor
============================================
Enter first number: 
48
Enter second number: 
18
GCD of 48 and 18 is: 6

Example with predefined values:
GCD of 48 and 18 is: 6
```

**How it works:**
1. The algorithm repeatedly applies the principle that `gcd(a,b) = gcd(b, a mod b)`
2. It continues until one number becomes 0
3. The non-zero number at that point is the GCD
4. For 48 and 18:
   - 48 mod 18 = 12, so gcd(48,18) = gcd(18,12)
   - 18 mod 12 = 6, so gcd(18,12) = gcd(12,6)
   - 12 mod 6 = 0, so gcd(12,6) = 6

