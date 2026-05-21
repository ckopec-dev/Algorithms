# Sieve of Eratosthenes in Pascal

Here's an implementation of the Sieve of Eratosthenes algorithm in Pascal to find all prime numbers up to a given limit:

```pascal
program SieveOfEratosthenes;

uses crt;

procedure SieveOfEratosthenes(limit: integer);
var
  i, j: integer;
  isPrime: array[0..10000] of boolean;
begin
  // Initialize all numbers as prime (true)
  for i := 0 to limit do
    isPrime[i] := true;
  
  // 0 and 1 are not prime numbers
  isPrime[0] := false;
  isPrime[1] := false;
  
  // Sieve process
  i := 2;
  while i * i <= limit do
  begin
    if isPrime[i] then
    begin
      // Mark all multiples of i as not prime
      j := i * i;
      while j <= limit do
      begin
        isPrime[j] := false;
        j := j + i;
      end;
    end;
    i := i + 1;
  end;
  
  // Print all prime numbers
  writeln('Prime numbers up to ', limit, ':');
  for i := 2 to limit do
  begin
    if isPrime[i] then
      write(i, ' ');
  end;
  writeln;
end;

var
  n: integer;
begin
  writeln('Sieve of Eratosthenes - Prime Number Generator');
  writeln('==============================================');
  
  write('Enter the limit: ');
  readln(n);
  
  if n >= 2 then
  begin
    SieveOfEratosthenes(n);
  end
  else
  begin
    writeln('Please enter a number greater than or equal to 2.');
  end;
  
  writeln('Press any key to exit...');
  readln;
end.
```

## How it works:

1. **Initialization**: Create a boolean array `isPrime` where each index represents a number, initially all set to `true`
2. **Mark non-primes**: Starting from 2, for each prime number found, mark all its multiples as `false`
3. **Optimization**: Only check up to √n since larger factors would have been found already
4. **Output**: Print all numbers that remain marked as `true`

## Sample Output:
```
Sieve of Eratosthenes - Prime Number Generator
==============================================
Enter the limit: 30
Prime numbers up to 30:
2 3 5 7 11 13 17 19 23 29 
Press any key to exit...
```

This implementation efficiently finds all prime numbers up to the specified limit using the classic Sieve of Eratosthenes algorithm.

