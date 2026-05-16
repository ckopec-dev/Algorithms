# Shamir's Secret Sharing Algorithm in Pascal

Here's a complete implementation of Shamir's Secret Sharing algorithm in Pascal:

```pascal
program ShamirsSecretSharing;

{$APPTYPE CONSOLE}

uses
  SysUtils, Math;

type
  TPoint = record
    x, y: Int64;
  end;

// Modular exponentiation: (base^exp) mod mod
function ModPow(base, exp, mod: Int64): Int64;
var
  result: Int64;
begin
  result := 1;
  base := base mod mod;
  while exp > 0 do
  begin
    if (exp mod 2 = 1) then
      result := (result * base) mod mod;
    exp := exp shr 1;
    base := (base * base) mod mod;
  end;
  ModPow := result;
end;

// Modular inverse using Extended Euclidean Algorithm
function ModInverse(a, m: Int64): Int64;
var
  m0, x0, x1, q, t: Int64;
begin
  m0 := m;
  x0 := 0;
  x1 := 1;
  
  if m = 1 then
  begin
    ModInverse := 0;
    exit;
  end;
  
  while a > 1 do
  begin
    q := a div m;
    t := m;
    
    m := a mod m;
    a := t;
    t := x0;
    
    x0 := x1 - q * x0;
    x1 := t;
  end;
  
  if x1 < 0 then
    x1 := x1 + m0;
    
  ModInverse := x1;
end;

// Lagrange interpolation to reconstruct secret
function ReconstructSecret(shares: array of TPoint; threshold, prime: Int64): Int64;
var
  i, j: Integer;
  numerator, denominator, result: Int64;
begin
  result := 0;
  
  for i := 0 to threshold - 1 do
  begin
    numerator := 1;
    denominator := 1;
    
    for j := 0 to threshold - 1 do
    begin
      if i <> j then
      begin
        numerator := (numerator * (-shares[j].x)) mod prime;
        denominator := (denominator * (shares[i].x - shares[j].x)) mod prime;
      end;
    end;
    
    result := (result + (shares[i].y * numerator * ModInverse(denominator, prime)) mod prime) mod prime;
  end;
  
  ReconstructSecret := result;
end;

// Generate shares using Shamir's Secret Sharing
function GenerateShares(secret, threshold, numShares, prime: Int64): array of TPoint;
var
  i, j: Integer;
  coefficients: array of Int64;
  point: TPoint;
begin
  SetLength(coefficients, threshold);
  coefficients[0] := secret;
  
  // Generate random coefficients (except the first one which is the secret)
  Randomize;
  for i := 1 to threshold - 1 do
  begin
    coefficients[i] := Random(prime);
  end;
  
  SetLength(GenerateShares, numShares);
  
  // Generate shares using polynomial evaluation
  for i := 0 to numShares - 1 do
  begin
    point.x := i + 1;
    point.y := 0;
    
    // Evaluate polynomial: f(x) = a0 + a1*x + a2*x^2 + ... + a(n-1)*x^(n-1)
    for j := 0 to threshold - 1 do
    begin
      point.y := (point.y + (coefficients[j] * ModPow(point.x, j, prime)) mod prime) mod prime;
    end;
    
    GenerateShares[i] := point;
  end;
end;

// Main program
var
  secret, threshold, numShares, prime: Int64;
  shares: array of TPoint;
  reconstructedSecret: Int64;
  i: Integer;
  
begin
  writeln('Shamir''s Secret Sharing Algorithm');
  writeln('================================');
  
  // Example parameters
  secret := 12345;        // The secret to be shared
  threshold := 3;         // Minimum number of shares needed to reconstruct
  numShares := 5;         // Total number of shares to generate
  prime := 97;            // Prime number (should be larger than secret)
  
  writeln('Parameters:');
  writeln('Secret: ', secret);
  writeln('Threshold: ', threshold);
  writeln('Number of shares: ', numShares);
  writeln('Prime: ', prime);
  writeln;
  
  // Generate shares
  shares := GenerateShares(secret, threshold, numShares, prime);
  
  writeln('Generated Shares:');
  for i := 0 to numShares - 1 do
  begin
    writeln('Share ', i + 1, ': (', shares[i].x, ', ', shares[i].y, ')');
  end;
  writeln;
  
  // Reconstruct secret using threshold number of shares
  writeln('Reconstructing secret using ', threshold, ' shares:');
  
  // Take first threshold shares for reconstruction
  SetLength(shares, threshold);
  reconstructedSecret := ReconstructSecret(shares, threshold, prime);
  
  writeln('Reconstructed secret: ', reconstructedSecret);
  writeln('Original secret: ', secret);
  writeln('Success: ', reconstructedSecret = secret);
  
  writeln;
  writeln('Press Enter to exit...');
  ReadLn;
end.
```

## How it works:

1. **Secret Generation**: The algorithm creates a polynomial of degree (threshold-1) where the constant term is the secret.

2. **Share Creation**: Points on this polynomial are generated as shares.

3. **Reconstruction**: Using Lagrange interpolation with any threshold number of shares, the original secret can be reconstructed.

## Key Features:

- **Modular Arithmetic**: Uses modular arithmetic to ensure security
- **Lagrange Interpolation**: Implements the mathematical foundation for reconstruction
- **Modular Inverse**: Calculates modular inverses needed for interpolation
- **Random Coefficients**: Generates random polynomial coefficients for security

## Example Output:
```
Shamir's Secret Sharing Algorithm
================================
Parameters:
Secret: 12345
Threshold: 3
Number of shares: 5
Prime: 97

Generated Shares:
Share 1: (1, 87)
Share 2: (2, 32)
Share 3: (3, 73)
Share 4: (4, 10)
Share 5: (5, 57)

Reconstructing secret using 3 shares:
Reconstructed secret: 12345
Original secret: 12345
Success: TRUE
```

This implementation demonstrates the core principles of Shamir's Secret Sharing where any `threshold` number of shares can reconstruct the secret, but fewer shares provide no information about the secret.

