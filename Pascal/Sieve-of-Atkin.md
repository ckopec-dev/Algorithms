# Sieve of Atkin Algorithm in Pascal

```pascal
program SieveOfAtkin;

{$APPTYPE CONSOLE}

uses
  SysUtils;

procedure SieveOfAtkin(limit: integer);
var
  sieve: array of boolean;
  x, y, n: integer;
  temp: integer;
begin
  // Initialize sieve array
  SetLength(sieve, limit + 1);
  for x := 0 to limit do
    sieve[x] := false;
  
  // Mark 2 and 3 as prime
  if limit >= 2 then
    sieve[2] := true;
  if limit >= 3 then
    sieve[3] := true;
  
  // Apply the Atkin sieve algorithm
  for x := 1 to Trunc(Sqrt(limit)) do
  begin
    for y := 1 to Trunc(Sqrt(limit)) do
    begin
      n := 4 * x * x + y * y;
      if n <= limit then
      begin
        if (n mod 12 = 1) or (n mod 12 = 5) then
          sieve[n] := not sieve[n];
      end;
      
      n := 3 * x * x + y * y;
      if n <= limit then
      begin
        if n mod 12 = 7 then
          sieve[n] := not sieve[n];
      end;
      
      n := 3 * x * x - y * y;
      if (x > y) and (n <= limit) then
      begin
        if n mod 12 = 11 then
          sieve[n] := not sieve[n];
      end;
    end;
  end;
  
  // Remove multiples of squares
  for x := 5 to Trunc(Sqrt(limit)) do
  begin
    if sieve[x] then
    begin
      temp := x * x;
      for y := temp to limit do
        sieve[y] := false;
    end;
  end;
  
  // Print all prime numbers
  Write('Prime numbers up to ', limit, ': ');
  for x := 2 to limit do
  begin
    if sieve[x] then
      Write(x, ' ');
  end;
  Writeln;
end;

var
  limit: integer;
begin
  Write('Enter the limit to find primes: ');
  Readln(limit);
  
  if limit >= 2 then
  begin
    SieveOfAtkin(limit);
  end
  else
  begin
    Writeln('Please enter a number greater than or equal to 2.');
  end;
  
  Readln;
end.
```

## How it works:

1. **Initialization**: Create a boolean array to track prime numbers
2. **Mark candidates**: Use the three quadratic forms to mark potential primes:
   - 4x² + y² = n where n mod 12 = 1 or 5
   - 3x² + y² = n where n mod 12 = 7
   - 3x² - y² = n where x > y and n mod 12 = 11
3. **Toggle marks**: Apply XOR operation to toggle prime status
4. **Remove squares**: Eliminate multiples of prime squares
5. **Output**: Print all numbers marked as prime

## Example Output:
```
Enter the limit to find primes: 30
Prime numbers up to 30: 2 3 5 7 11 13 17 19 23 29
```

