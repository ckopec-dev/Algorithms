# Diffie-Hellman Key Exchange in Pascal

```pascal
program DiffieHellmanKeyExchange;

uses
  crt, math;

type
  TBigInteger = array[1..100] of integer;

var
  p, g, a, b, A, B, s, s2: integer;
  secretA, secretB: integer;

// Function to perform modular exponentiation
function ModExp(base, exp, mod: integer): integer;
var
  result, temp: integer;
begin
  result := 1;
  base := base mod mod;
  
  while exp > 0 do
  begin
    if (exp mod 2) = 1 then
      result := (result * base) mod mod;
    
    exp := exp div 2;
    base := (base * base) mod mod;
  end;
  
  ModExp := result;
end;

// Function to generate a random prime number (simplified)
function GeneratePrime: integer;
begin
  // Using a small prime number for demonstration
  GeneratePrime := 23; // Small prime number
end;

// Function to generate a random private key
function GeneratePrivateKey: integer;
begin
  GeneratePrivateKey := random(100) + 1; // Random number between 1-100
end;

begin
  clrscr;
  writeln('Diffie-Hellman Key Exchange Algorithm');
  writeln('====================================');
  
  // Initialize random number generator
  randomize;
  
  // Step 1: Choose public parameters
  p := GeneratePrime;  // Large prime number
  g := 5;              // Primitive root modulo p
  
  writeln('Public Parameters:');
  writeln('Prime number p = ', p);
  writeln('Primitive root g = ', g);
  writeln;
  
  // Step 2: Each party generates private key
  a := GeneratePrivateKey;  // Alice's private key
  b := GeneratePrivateKey;  // Bob's private key
  
  writeln('Private Keys:');
  writeln('Alice''s private key a = ', a);
  writeln('Bob''s private key b = ', b);
  writeln;
  
  // Step 3: Each party calculates public key
  A := ModExp(g, a, p);  // Alice's public key
  B := ModExp(g, b, p);  // Bob's public key
  
  writeln('Public Keys:');
  writeln('Alice''s public key A = ', A);
  writeln('Bob''s public key B = ', B);
  writeln;
  
  // Step 4: Each party calculates shared secret
  s := ModExp(B, a, p);  // Alice calculates shared secret
  s2 := ModExp(A, b, p); // Bob calculates shared secret
  
  writeln('Shared Secrets:');
  writeln('Alice''s shared secret = ', s);
  writeln('Bob''s shared secret = ', s2);
  writeln;
  
  // Verify that both parties have the same secret
  if s = s2 then
    writeln('SUCCESS: Both parties have the same shared secret!')
  else
    writeln('ERROR: Shared secrets do not match!');
  
  writeln;
  writeln('Press any key to exit...');
  readln;
end.
```

## How it works:

1. **Public Parameters**: Both parties agree on a large prime number `p` and a primitive root `g`
2. **Private Keys**: Each party generates a random private key (`a` for Alice, `b` for Bob)
3. **Public Keys**: Each party calculates their public key using modular exponentiation:
   - Alice: `A = g^a mod p`
   - Bob: `B = g^b mod p`
4. **Shared Secret**: Each party calculates the shared secret:
   - Alice: `s = B^a mod p`
   - Bob: `s2 = A^b mod p`

## Key Features:

- **Modular Exponentiation**: Efficiently computes large powers modulo a number
- **Random Generation**: Uses Pascal's `random` function for key generation
- **Security**: Demonstrates the mathematical principle behind DH key exchange
- **Verification**: Checks that both parties arrive at the same shared secret

## Note:

This is a simplified implementation for educational purposes. In real-world applications, you would need:
- Much larger prime numbers (at least 2048 bits)
- Cryptographically secure random number generation
- Proper error handling
- Additional security measures

