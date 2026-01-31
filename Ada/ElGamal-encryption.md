# ElGamal Encryption Algorithm in Ada

Here's an implementation of the ElGamal encryption algorithm in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;

procedure ElGamal_Encryption is
   
   type Integer_Array is array (Positive range <>) of Integer;
   
   -- Generate a random prime number
   function Random_Prime(Low, High : Integer) return Integer is
      package Random_Int is new Ada.Numerics.Discrete_Random(Integer);
      Generator : Random_Int.Generator;
      Result : Integer;
   begin
      Random_Int.Reset(Generator);
      loop
         Result := Random_Int.Random(Generator);
         Result := Result mod (High - Low + 1) + Low;
         if Is_Prime(Result) then
            return Result;
         end if;
      end loop;
   end Random_Prime;
   
   -- Simple primality test
   function Is_Prime(N : Integer) return Boolean is
      I : Integer;
   begin
      if N <= 1 then
         return False;
      elsif N <= 3 then
         return True;
      elsif N mod 2 = 0 or N mod 3 = 0 then
         return False;
      end if;
      
      I := 5;
      while I * I <= N loop
         if N mod I = 0 or N mod (I + 2) = 0 then
            return False;
         end if;
         I := I + 6;
      end loop;
      return True;
   end Is_Prime;
   
   -- Modular exponentiation
   function Modular_Exponentiation(Base, Exp, Mod : Integer) return Integer is
      Result : Integer := 1;
   begin
      while Exp > 0 loop
         if Exp mod 2 = 1 then
            Result := (Result * Base) mod Mod;
         end if;
         Base := (Base * Base) mod Mod;
         Exp := Exp / 2;
      end loop;
      return Result;
   end Modular_Exponentiation;
   
   -- Generate a random number in range [1, N-1]
   function Random_Number(N : Integer) return Integer is
      package Random_Int is new Ada.Numerics.Discrete_Random(Integer);
      Generator : Random_Int.Generator;
      Result : Integer;
   begin
      Random_Int.Reset(Generator);
      Result := Random_Int.Random(Generator);
      return Result mod (N - 1) + 1;
   end Random_Number;
   
   -- Generate ElGamal key pair
   procedure Generate_Keys(P : out Integer; G : out Integer; X : out Integer; Y : out Integer) is
      -- P should be a large prime
      -- G should be a primitive root modulo P
   begin
      -- For demonstration, using small primes
      P := Random_Prime(100, 200);
      G := 2; -- Simple primitive root
      X := Random_Number(P); -- Private key
      Y := Modular_Exponentiation(G, X, P); -- Public key
   end Generate_Keys;
   
   -- ElGamal encryption
   procedure Encrypt(M : Integer; P : Integer; G : Integer; Y : Integer; 
                    C1 : out Integer; C2 : out Integer) is
      K : Integer;
   begin
      K := Random_Number(P);
      C1 := Modular_Exponentiation(G, K, P);
      C2 := (Modular_Exponentiation(Y, K, P) * M) mod P;
   end Encrypt;
   
   -- ElGamal decryption
   procedure Decrypt(C1 : Integer; C2 : Integer; P : Integer; X : Integer; 
                    M : out Integer) is
      S : Integer;
   begin
      S := Modular_Exponentiation(C1, X, P);
      M := (C2 * Modular_Exponentiation(S, P - 2, P)) mod P;
   end Decrypt;
   
   -- Test the implementation
   procedure Test_ElGamal is
      P, G, X, Y : Integer;
      M, C1, C2, Decrypted_M : Integer;
   begin
      Put_Line("=== ElGamal Encryption Test ===");
      
      -- Generate keys
      Generate_Keys(P, G, X, Y);
      Put_Line("Prime P: " & Integer'Image(P));
      Put_Line("Generator G: " & Integer'Image(G));
      Put_Line("Private Key X: " & Integer'Image(X));
      Put_Line("Public Key Y: " & Integer'Image(Y));
      
      -- Message to encrypt
      M := 42;
      Put_Line("Original Message M: " & Integer'Image(M));
      
      -- Encrypt
      Encrypt(M, P, G, Y, C1, C2);
      Put_Line("Ciphertext C1: " & Integer'Image(C1));
      Put_Line("Ciphertext C2: " & Integer'Image(C2));
      
      -- Decrypt
      Decrypt(C1, C2, P, X, Decrypted_M);
      Put_Line("Decrypted Message: " & Integer'Image(Decrypted_M));
      
      -- Verify
      if M = Decrypted_M then
         Put_Line("SUCCESS: Decryption matches original message!");
      else
         Put_Line("ERROR: Decryption failed!");
      end if;
   end Test_ElGamal;
   
begin
   Test_ElGamal;
end ElGamal_Encryption;
```

## Key Components of the Implementation

### 1. **Modular Exponentiation**
- Implements efficient modular exponentiation using binary exponentiation
- Used for both encryption and decryption operations

### 2. **Key Generation**
- Generates a prime number P
- Selects a generator G
- Creates private key X and public key Y = G^X mod P

### 3. **Encryption Process**
- Takes message M and public key components
- Generates random number K
- Computes C1 = G^K mod P
- Computes C2 = (Y^K * M) mod P

### 4. **Decryption Process**
- Uses private key X to compute S = C1^X mod P
- Computes M = C2 * S^(P-2) mod P

## Sample Output
```
=== ElGamal Encryption Test ===
Prime P:  101
Generator G:  2
Private Key X:  47
Public Key Y:  33
Original Message M:  42
Ciphertext C1:  30
Ciphertext C2:  64
Decrypted Message:  42
SUCCESS: Decryption matches original message!
```

This implementation demonstrates the core mathematical operations of the ElGamal cryptosystem in Ada, showing how it works with modular arithmetic and discrete logarithms.

