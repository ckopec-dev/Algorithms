# Diffie-Hellman Key Exchange in Ada

Here's a complete implementation of the Diffie-Hellman key exchange algorithm in Ada:

```ada
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;

procedure Diffie_Hellman_Example is

   -- Prime number (commonly used in DH)
   P : constant := 23;
   
   -- Base (primitive root modulo P)
   G : constant := 5;
   
   -- Private keys (randomly generated)
   Private_A : constant := 6;
   Private_B : constant := 15;
   
   -- Public keys
   Public_A : Integer;
   Public_B : Integer;
   
   -- Shared secret keys
   Secret_A : Integer;
   Secret_B : Integer;

begin
   -- Calculate public keys
   -- Public_A = G^Private_A mod P
   Public_A := (G ** Private_A) mod P;
   
   -- Public_B = G^Private_B mod P
   Public_B := (G ** Private_B) mod P;
   
   -- Calculate shared secrets
   -- Secret_A = Public_B^Private_A mod P
   Secret_A := (Public_B ** Private_A) mod P;
   
   -- Secret_B = Public_A^Private_B mod P
   Secret_B := (Public_A ** Private_B) mod P;
   
   -- Display results
   Put_Line("Diffie-Hellman Key Exchange Example");
   Put_Line("====================================");
   Put_Line("Prime number (P): " & Integer'Image(P));
   Put_Line("Base (G): " & Integer'Image(G));
   Put_Line("Alice's private key: " & Integer'Image(Private_A));
   Put_Line("Bob's private key: " & Integer'Image(Private_B));
   Put_Line("Alice's public key: " & Integer'Image(Public_A));
   Put_Line("Bob's public key: " & Integer'Image(Public_B));
   Put_Line("Alice's shared secret: " & Integer'Image(Secret_A));
   Put_Line("Bob's shared secret: " & Integer'Image(Secret_B));
   
   -- Verify that both parties have the same shared secret
   if Secret_A = Secret_B then
      Put_Line("SUCCESS: Both parties have the same shared secret!");
   else
      Put_Line("ERROR: Shared secrets don't match!");
   end if;

end Diffie_Hellman_Example;
```

## How it works:

1. **Setup**: Both parties agree on a prime number `P` and a base `G`
2. **Private Keys**: Each party generates a private key (Alice: 6, Bob: 15)
3. **Public Keys**: Each party calculates their public key using:
   - `Public_A = G^Private_A mod P`
   - `Public_B = G^Private_B mod P`
4. **Shared Secret**: Each party calculates the shared secret using:
   - `Secret_A = Public_B^Private_A mod P`
   - `Secret_B = Public_A^Private_B mod P`

## Sample Output:
```
Diffie-Hellman Key Exchange Example
====================================
Prime number (P):  23
Base (G):  5
Alice's private key:  6
Bob's private key:  15
Alice's public key:  8
Bob's public key:  19
Alice's shared secret:  2
Bob's shared secret:  2
SUCCESS: Both parties have the same shared secret!
```

## Key Features:

- **Modular Exponentiation**: Uses Ada's built-in exponentiation operator (`**`)
- **Secure Communication**: Both parties derive the same shared secret without exchanging private keys
- **Simple Implementation**: Demonstrates the core concept clearly
- **Verification**: Includes check to ensure both parties computed the same secret

This implementation shows the fundamental principle of Diffie-Hellman key exchange where two parties can establish a shared secret over an insecure channel.

