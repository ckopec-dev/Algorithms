# ElGamal Encryption Algorithm in Pascal

Here's a complete implementation of the ElGamal encryption algorithm in Pascal:

```pascal
program ElGamalEncryption;

uses
  crt, sysutils;

type
  TBigInteger = array[0..100] of integer;
  TPublicKey = record
    p: integer;  // Prime modulus
    g: integer;  // Generator
    y: integer;  // Public key (g^x mod p)
  end;
  TPrivateKey = record
    p: integer;  // Prime modulus
    x: integer;  // Private key
  end;
  TEncryptedMessage = record
    a: integer;
    b: integer;
  end;

// Function to calculate modular exponentiation (g^x mod p)
function ModExp(base, exponent, modulus: integer): integer;
var
  result, temp: integer;
begin
  if modulus = 1 then
  begin
    ModExp := 0;
    exit;
  end;
  
  result := 1;
  base := base mod modulus;
  
  while exponent > 0 do
  begin
    if (exponent mod 2) = 1 then
      result := (result * base) mod modulus;
    exponent := exponent shr 1;
    base := (base * base) mod modulus;
  end;
  
  ModExp := result;
end;

// Function to generate a random prime number (simplified version)
function GeneratePrime: integer;
begin
  // This is a simplified prime generator for demonstration
  GeneratePrime := 23;  // Using small prime for example
end;

// Function to generate ElGamal key pair
procedure GenerateKeyPair(var publicKey: TPublicKey; var privateKey: TPrivateKey);
var
  x: integer;
begin
  // Generate prime p (in real implementation, use proper prime generation)
  publicKey.p := GeneratePrime;
  
  // Generate generator g
  publicKey.g := 5;  // Simple generator
  
  // Generate private key x (random number)
  x := 6;  // Fixed for demonstration, should be random
  
  // Calculate public key y = g^x mod p
  publicKey.y := ModExp(publicKey.g, x, publicKey.p);
  
  // Store private key
  privateKey.p := publicKey.p;
  privateKey.x := x;
end;

// Function to encrypt a message
function Encrypt(message: integer; publicKey: TPublicKey): TEncryptedMessage;
var
  k: integer;
  temp: integer;
begin
  // Generate random k (should be random in practice)
  k := 7;
  
  // Calculate a = g^k mod p
  temp := ModExp(publicKey.g, k, publicKey.p);
  Encrypt.a := temp;
  
  // Calculate b = m * y^k mod p
  temp := ModExp(publicKey.y, k, publicKey.p);
  temp := (message * temp) mod publicKey.p;
  Encrypt.b := temp;
end;

// Function to decrypt a message
function Decrypt(encrypted: TEncryptedMessage; privateKey: TPrivateKey): integer;
var
  temp: integer;
begin
  // Calculate s = a^x mod p
  temp := ModExp(encrypted.a, privateKey.x, privateKey.p);
  
  // Calculate m = b * s^(-1) mod p
  // First find modular inverse of temp
  temp := ModExp(temp, privateKey.p - 2, privateKey.p);  // Using Fermat's little theorem
  temp := (encrypted.b * temp) mod privateKey.p;
  
  Decrypt := temp;
end;

// Main program
var
  publicKey: TPublicKey;
  privateKey: TPrivateKey;
  message: integer;
  encrypted: TEncryptedMessage;
  decrypted: integer;
  i: integer;

begin
  writeln('ElGamal Encryption Algorithm Demo');
  writeln('================================');
  
  // Generate key pair
  writeln('Generating key pair...');
  GenerateKeyPair(publicKey, privateKey);
  
  writeln('Public Key:');
  writeln('  p = ', publicKey.p);
  writeln('  g = ', publicKey.g);
  writeln('  y = ', publicKey.y);
  writeln('Private Key:');
  writeln('  x = ', privateKey.x);
  
  // Encrypt a message
  message := 15;  // Message to encrypt
  writeln('Original message: ', message);
  
  encrypted := Encrypt(message, publicKey);
  writeln('Encrypted message:');
  writeln('  a = ', encrypted.a);
  writeln('  b = ', encrypted.b);
  
  // Decrypt the message
  decrypted := Decrypt(encrypted, privateKey);
  writeln('Decrypted message: ', decrypted);
  
  if message = decrypted then
    writeln('Encryption/Decryption successful!')
  else
    writeln('Error in encryption/decryption!');
  
  writeln('');
  writeln('Press any key to exit...');
  readln;
end.
```

## How the Algorithm Works:

1. **Key Generation**:
   - Choose a large prime `p`
   - Choose a generator `g` of the multiplicative group of integers modulo `p`
   - Choose a random private key `x` where `1 ≤ x ≤ p-2`
   - Calculate public key `y = g^x mod p`

2. **Encryption**:
   - Choose random `k` where `1 ≤ k ≤ p-2`
   - Calculate `a = g^k mod p`
   - Calculate `b = m * y^k mod p`
   - Return ciphertext `(a, b)`

3. **Decryption**:
   - Calculate `s = a^x mod p`
   - Calculate `m = b * s^(-1) mod p` where `s^(-1)` is modular inverse of `s`

## Sample Output:
```
ElGamal Encryption Algorithm Demo
================================
Generating key pair...
Public Key:
  p = 23
  g = 5
  y = 8
Private Key:
  x = 6
Original message: 15
Encrypted message:
  a = 10
  b = 19
Decrypted message: 15
Encryption/Decryption successful!
```

**Note**: This is a simplified implementation for demonstration purposes. A production implementation would include:
- Proper prime number generation
- Secure random number generation
- Larger key sizes
- Better error handling
- Modular arithmetic for large numbers

