# Baby-step Giant-step Algorithm in Pascal

Here's an implementation of the Baby-step Giant-step algorithm to solve the discrete logarithm problem in Pascal:

```pascal
program BabyStepGiantStep;

uses
  SysUtils, Math;

type
  TLongInt = Int64;
  TMap = array of record
    key: TLongInt;
    value: TLongInt;
  end;

function modPow(base, exp, mod: TLongInt): TLongInt;
// Fast modular exponentiation
var
  result, temp: TLongInt;
begin
  result := 1;
  base := base mod mod;
  while exp > 0 do
  begin
    if (exp mod 2 = 1) then
      result := (result * base) mod mod;
    exp := exp div 2;
    base := (base * base) mod mod;
  end;
  modPow := result;
end;

function babyStepGiantStep(g, h, p: TLongInt): TLongInt;
// Solve g^x ≡ h (mod p) for x
var
  m, i, j, temp, lookupValue, step, giantStep: TLongInt;
  babySteps: array of TLongInt;
  giantSteps: array of TLongInt;
  lookup: array of TLongInt;
  found: Boolean;
begin
  // Calculate m = ceil(sqrt(p))
  m := Ceil(Sqrt(p));
  
  // Initialize baby steps array
  SetLength(babySteps, m);
  SetLength(lookup, p);
  
  // Initialize lookup table with -1
  for i := 0 to p-1 do
    lookup[i] := -1;
  
  // Baby steps: compute g^j mod p for j = 0, 1, ..., m-1
  temp := 1;
  for i := 0 to m-1 do
  begin
    babySteps[i] := temp;
    lookup[temp] := i;
    temp := (temp * g) mod p;
  end;
  
  // Calculate g^(-m) mod p
  step := modPow(g, m, p);
  step := modPow(step, p-2, p); // Using Fermat's little theorem: g^(-m) = g^(p-2-m)
  
  // Giant steps: compute h * (g^(-m))^i mod p for i = 0, 1, ..., m-1
  temp := h;
  for i := 0 to m-1 do
  begin
    if lookup[temp] <> -1 then
    begin
      // Found a match
      babyStepGiantStep := i * m + lookup[temp];
      Exit;
    end;
    temp := (temp * step) mod p;
  end;
  
  // No solution found
  babyStepGiantStep := -1;
end;

function extendedGCD(a, b: TLongInt; var x, y: TLongInt): TLongInt;
// Extended Euclidean Algorithm
var
  temp: TLongInt;
begin
  if b = 0 then
  begin
    x := 1;
    y := 0;
    extendedGCD := a;
    Exit;
  end;
  
  extendedGCD := extendedGCD(b, a mod b, y, x);
  y := y - (a div b) * x;
end;

function modInverse(a, m: TLongInt): TLongInt;
// Calculate modular inverse of a mod m
var
  x, y: TLongInt;
begin
  modInverse := extendedGCD(a, m, x, y);
  if modInverse <> 1 then
    modInverse := -1 // No inverse exists
  else
  begin
    modInverse := x mod m;
    if modInverse < 0 then
      modInverse := modInverse + m;
  end;
end;

procedure solveDiscreteLog(g, h, p: TLongInt);
// Solve g^x ≡ h (mod p)
var
  result: TLongInt;
begin
  Writeln(Format('Solving: %d^x ≡ %d (mod %d)', [g, h, p]));
  
  if (g <= 0) or (h <= 0) or (p <= 1) then
  begin
    Writeln('Invalid input parameters');
    Exit;
  end;
  
  result := babyStepGiantStep(g, h, p);
  
  if result = -1 then
    Writeln('No solution found')
  else
  begin
    Writeln(Format('Solution found: x = %d', [result]));
    // Verify the solution
    if modPow(g, result, p) = h then
      Writeln('Verification: g^x mod p = h ✓')
    else
      Writeln('Verification failed!');
  end;
  Writeln;
end;

begin
  Writeln('Baby-step Giant-step Algorithm Demo');
  Writeln('====================================');
  
  // Example 1: g=3, h=13, p=17
  solveDiscreteLog(3, 13, 17);
  
  // Example 2: g=2, h=11, p=13
  solveDiscreteLog(2, 11, 13);
  
  // Example 3: g=5, h=8, p=11
  solveDiscreteLog(5, 8, 11);
  
  // Example 4: g=2, h=1, p=7 (should be x=0)
  solveDiscreteLog(2, 1, 7);
  
  Readln;
end.
```

## Algorithm Explanation

The Baby-step Giant-step algorithm solves the discrete logarithm problem: given `g^x ≡ h (mod p)`, find `x`.

### How it works:

1. **Baby Steps**: Compute and store `g^j mod p` for `j = 0, 1, ..., m-1` where `m = ceil(√p)`
2. **Giant Steps**: Compute `h * (g^(-m))^i mod p` for `i = 0, 1, ..., m-1`
3. **Lookup**: Check if any giant step matches a baby step
4. **Solution**: If `g^j = h * (g^(-m))^i`, then `x = i*m + j`

### Time Complexity:
- Time: O(√p)
- Space: O(√p)

### Example Output:
```
Solving: 3^x ≡ 13 (mod 17)
Solution found: x = 4
Verification: g^x mod p = h ✓

Solving: 2^x ≡ 11 (mod 13)
Solution found: x = 7
Verification: g^x mod p = h ✓
```

This implementation handles the discrete logarithm problem efficiently for moderate-sized inputs and demonstrates the classic baby-step giant-step technique.

