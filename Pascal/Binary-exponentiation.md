```pascal
program BinaryExponentiation;

function binaryExponentiation(base: integer; exponent: integer): integer;
var
  result, currentBase: integer;
begin
  // Handle edge cases
  if exponent = 0 then
  begin
    binaryExponentiation := 1;
    exit;
  end;
  
  if exponent < 0 then
  begin
    // For negative exponents, we would typically return a real number
    // but this implementation assumes integer result
    binaryExponentiation := 0;
    exit;
  end;
  
  result := 1;
  currentBase := base;
  
  // Binary exponentiation algorithm
  while exponent > 0 do
  begin
    // If exponent is odd, multiply result by current base
    if exponent mod 2 = 1 then
    begin
      result := result * currentBase;
    end;
    
    // Square the base and halve the exponent
    currentBase := currentBase * currentBase;
    exponent := exponent div 2;
  end;
  
  binaryExponentiation := result;
end;

procedure testBinaryExponentiation;
begin
  writeln('Binary Exponentiation Examples:');
  writeln('2^10 = ', binaryExponentiation(2, 10));
  writeln('3^5 = ', binaryExponentiation(3, 5));
  writeln('5^3 = ', binaryExponentiation(5, 3));
  writeln('2^0 = ', binaryExponentiation(2, 0));
  writeln('10^2 = ', binaryExponentiation(10, 2));
end;

begin
  testBinaryExponentiation;
end.
```

**Algorithm Explanation:**

The binary exponentiation algorithm works by:
1. **Binary representation**: Instead of multiplying the base by itself `n` times, we use the binary representation of the exponent
2. **Bit-by-bit processing**: For each bit in the exponent (from right to left):
   - If the bit is 1, multiply the result by the current base
   - Square the base and move to the next bit
3. **Time complexity**: O(log n) instead of O(n)

**Example trace for 2^10:**
- 10 in binary = 1010
- Process bits from right to left:
  - Bit 0 (even): skip, square base (2² = 4)
  - Bit 1 (odd): multiply result by current base (result = 1 × 4 = 4)
  - Bit 2 (even): skip, square base (4² = 16)
  - Bit 3 (odd): multiply result by current base (result = 4 × 16 = 64)

**Output:**
```
Binary Exponentiation Examples:
2^10 = 1024
3^5 = 243
5^3 = 125
2^0 = 1
10^2 = 100
```

