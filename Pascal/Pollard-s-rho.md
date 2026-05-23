# Pollard's Rho Algorithm in Pascal

Here's an implementation of Pollard's Rho algorithm for integer factorization in Pascal:

```pascal
program PollardsRho;

uses
  crt;

// Function to compute greatest common divisor using Euclidean algorithm
function gcd(a, b: int64): int64;
begin
  while b <> 0 do
  begin
    a := a mod b;
    a := a xor b;
    b := a xor b;
    a := a xor b;
  end;
  gcd := a;
end;

// Function to compute (a * b) mod m to avoid overflow
function mulmod(a, b, m: int64): int64;
var
  result: int64;
begin
  if a = 0 then
    mulmod := 0
  else
  begin
    result := 0;
    while b > 0 do
    begin
      if (b and 1) = 1 then
        result := (result + a) mod m;
      a := (a * 2) mod m;
      b := b shr 1;
    end;
    mulmod := result;
  end;
end;

// Pollard's Rho algorithm
function pollard_rho(n: int64): int64;
var
  x, y, c, g: int64;
  f: function(x: int64): int64;
begin
  // Simple quadratic function f(x) = x^2 + 1
  f := function(x: int64): int64
  begin
    f := (mulmod(x, x, n) + 1) mod n;
  end;
  
  x := 2;
  y := 2;
  c := 1;
  
  while true do
  begin
    x := f(x);
    y := f(f(y));
    g := gcd(abs(x - y), n);
    
    if g <> 1 then
    begin
      pollard_rho := g;
      exit;
    end;
    
    // Prevent infinite loop for small numbers
    if x = y then
    begin
      pollard_rho := n;
      exit;
    end;
  end;
end;

// Function to check if a number is prime
function is_prime(n: int64): boolean;
var
  i: int64;
begin
  if n <= 1 then
    is_prime := false
  else if n <= 3 then
    is_prime := true
  else if (n mod 2 = 0) or (n mod 3 = 0) then
    is_prime := false
  else
  begin
    i := 5;
    while (i * i) <= n do
    begin
      if (n mod i = 0) or (n mod (i + 2) = 0) then
      begin
        is_prime := false;
        exit;
      end;
      i := i + 6;
    end;
    is_prime := true;
  end;
end;

// Main program
var
  n, factor: int64;
  i: integer;

begin
  writeln('Pollard''s Rho Factorization Algorithm');
  writeln('====================================');
  
  // Test cases
  writeln('Testing with various numbers:');
  writeln;
  
  // Test with a composite number
  n := 13407807929;
  writeln('Number to factor: ', n);
  factor := pollard_rho(n);
  writeln('Factor found: ', factor);
  writeln('Other factor: ', n div factor);
  writeln('Verification: ', factor * (n div factor));
  writeln;
  
  // Test with another number
  n := 1537228672809;
  writeln('Number to factor: ', n);
  factor := pollard_rho(n);
  writeln('Factor found: ', factor);
  writeln('Other factor: ', n div factor);
  writeln('Verification: ', factor * (n div factor));
  writeln;
  
  // Test with a large number
  n := 1000000007 * 1000000009; // Product of two primes
  writeln('Number to factor: ', n);
  factor := pollard_rho(n);
  writeln('Factor found: ', factor);
  writeln('Other factor: ', n div factor);
  writeln('Verification: ', factor * (n div factor));
  writeln;
  
  // Interactive input
  writeln('Enter a number to factor (0 to exit): ');
  readln(n);
  
  while n <> 0 do
  begin
    if n < 0 then
    begin
      writeln('Please enter a positive number');
    end
    else if is_prime(n) then
    begin
      writeln(n, ' is prime');
    end
    else
    begin
      factor := pollard_rho(n);
      if factor = n then
        writeln('No non-trivial factor found')
      else
      begin
        writeln('Factor found: ', factor);
        writeln('Other factor: ', n div factor);
        writeln('Verification: ', factor * (n div factor));
      end;
    end;
    
    writeln('Enter another number (0 to exit): ');
    readln(n);
  end;
  
  writeln('Program terminated.');
end.
```

## How the Algorithm Works:

1. **Function `gcd`**: Computes the greatest common divisor using the Euclidean algorithm
2. **Function `mulmod`**: Performs modular multiplication to prevent integer overflow
3. **Function `pollard_rho`**: Implements the core Pollard's Rho algorithm:
   - Uses the function f(x) = x² + 1
   - Applies Floyd's cycle detection algorithm
   - Uses the GCD to find non-trivial factors
4. **Main program**: Tests the algorithm with various numbers and allows interactive input

## Key Features:

- **Cycle Detection**: Uses Floyd's cycle-finding algorithm to detect cycles in the sequence
- **Modular Arithmetic**: Prevents integer overflow using modular multiplication
- **Multiple Test Cases**: Includes several test numbers with different factorization difficulties
- **Interactive Mode**: Allows users to input their own numbers to factor

## Time Complexity:
- Average case: O(n^(1/4)) for finding a factor
- Space complexity: O(1)

This implementation demonstrates the practical application of Pollard's Rho algorithm for integer factorization, which is particularly effective for numbers with small prime factors.

