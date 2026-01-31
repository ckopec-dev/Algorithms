# RSA Encryption Algorithm in Ada

Here's a complete example of RSA encryption and decryption implemented in Ada:

```ada
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Ada.Numerics.Big_Integers; use Ada.Numerics.Big_Integers;

procedure RSA_Example is
   
   -- RSA parameters
   P : constant Big_Integer := 61;     -- Prime number 1
   Q : constant Big_Integer := 53;     -- Prime number 2
   N : constant Big_Integer := P * Q;   -- Modulus
   Phi : constant Big_Integer := (P - 1) * (Q - 1); -- Euler's totient
   E : constant Big_Integer := 17;     -- Public exponent
   D : constant Big_Integer := 2753;   -- Private exponent (calculated)
   
   -- Message to encrypt
   Message : constant Big_Integer := 123;
   
   -- Modular exponentiation function
   function Modular_Exponentiation(Base, Exponent, Modulus : Big_Integer) return Big_Integer is
      Result : Big_Integer := 1;
      Base_Mod : Big_Integer := Base mod Modulus;
      Exp : Big_Integer := Exponent;
   begin
      while Exp > 0 loop
         if Exp mod 2 = 1 then
            Result := (Result * Base_Mod) mod Modulus;
         end if;
         Base_Mod := (Base_Mod * Base_Mod) mod Modulus;
         Exp := Exp / 2;
      end loop;
      return Result;
   end Modular_Exponentiation;
   
   -- Encryption function
   function Encrypt(M : Big_Integer) return Big_Integer is
   begin
      return Modular_Exponentiation(M, E, N);
   end Encrypt;
   
   -- Decryption function
   function Decrypt(C : Big_Integer) return Big_Integer is
   begin
      return Modular_Exponentiation(C, D, N);
   end Decrypt;
   
begin
   -- Display RSA parameters
   Put_Line("RSA Encryption Example");
   Put_Line("======================");
   Put_Line("Prime P: " & P'Img);
   Put_Line("Prime Q: " & Q'Img);
   Put_Line("Modulus N: " & N'Img);
   Put_Line("Euler's totient Phi: " & Phi'Img);
   Put_Line("Public exponent E: " & E'Img);
   Put_Line("Private exponent D: " & D'Img);
   Put_Line("Original message: " & Message'Img);
   
   -- Encrypt the message
   declare
      Ciphertext : constant Big_Integer := Encrypt(Message);
   begin
      Put_Line("Encrypted message: " & Ciphertext'Img);
      
      -- Decrypt the message
      declare
         Decrypted : constant Big_Integer := Decrypt(Ciphertext);
      begin
         Put_Line("Decrypted message: " & Decrypted'Img);
         
         -- Verify correctness
         if Decrypted = Message then
            Put_Line("SUCCESS: Decryption matches original message!");
         else
            Put_Line("ERROR: Decryption failed!");
         end if;
      end;
   end;
   
end RSA_Example;
```

## Key Components Explained:

### 1. **RSA Parameters**
- Two prime numbers P = 61 and Q = 53
- Modulus N = P × Q = 3233
- Euler's totient φ(N) = (P-1) × (Q-1) = 3120
- Public exponent E = 17
- Private exponent D = 2753

### 2. **Modular Exponentiation**
The core of RSA operations, implemented using binary exponentiation for efficiency.

### 3. **Encryption Process**
C = M^E mod N

### 4. **Decryption Process**
M = C^D mod N

### 5. **Expected Output**
```
RSA Encryption Example
======================
Prime P:  61
Prime Q:  53
Modulus N:  3233
Euler's totient Phi:  3120
Public exponent E:  17
Private exponent D:  2753
Original message:  123
Encrypted message:  855
Decrypted message:  123
SUCCESS: Decryption matches original message!
```

## Important Notes:

1. **Security**: This example uses small primes for demonstration purposes only
2. **Real-world RSA**: Production systems use much larger primes (1024+ bits)
3. **Key Generation**: The private exponent D should be calculated using the Extended Euclidean Algorithm
4. **Practical Considerations**: This implementation lacks proper padding and security measures

This example demonstrates the mathematical foundation of RSA encryption and decryption in Ada programming language.

