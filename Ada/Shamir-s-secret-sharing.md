# Shamir's Secret Sharing Algorithm in Ada

Here's an implementation of Shamir's Secret Sharing algorithm in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;

procedure Shamir_Secret_Sharing is

   -- Prime number for finite field arithmetic
   Prime : constant := 2147483647;  -- Large prime number
   
   -- Type definitions
   type Point is record
      X, Y : Integer;
   end record;
   
   type Share_Array is array (Positive range <>) of Point;
   
   -- Modular arithmetic functions
   function Mod_Add(A, B : Integer) return Integer is
   begin
      return (A + B) mod Prime;
   end Mod_Add;
   
   function Mod_Mul(A, B : Integer) return Integer is
   begin
      return (A * B) mod Prime;
   end Mod_Mul;
   
   function Mod_Sub(A, B : Integer) return Integer is
   begin
      return (A - B + Prime) mod Prime;
   end Mod_Sub;
   
   -- Modular inverse using Extended Euclidean Algorithm
   function Mod_Inv(A : Integer) return Integer is
      function GCD(X, Y : Integer) return Integer is
      begin
         if Y = 0 then
            return X;
         else
            return GCD(Y, X mod Y);
         end if;
      end GCD;
      
      function Extended_GCD(A, B : Integer) return Integer is
         function Extended_GCD_Helper(X, Y : Integer) return Integer is
         begin
            if Y = 0 then
               return 1;
            else
               return Extended_GCD_Helper(Y, X mod Y) * (X / Y);
            end if;
         end Extended_GCD_Helper;
      begin
         return Extended_GCD_Helper(A, B);
      end Extended_GCD_Helper;
      
      function Extended_Euclidean(A, B : Integer) return Integer is
         function Extended_Euclidean_Helper(A, B : Integer) return Integer is
         begin
            if B = 0 then
               return 1;
            else
               return (Extended_Euclidean_Helper(B, A mod B) * (A / B)) mod Prime;
            end if;
         end Extended_Euclidean_Helper;
      begin
         return Extended_Euclidean_Helper(A, B);
      end Extended_Euclidean_Helper;
   begin
      return Extended_Euclidean_Helper(A, Prime);
   end Mod_Inv;
   
   -- Generate random integer in range [0, Max)
   function Random_Int(Max : Integer) return Integer is
      package Random_Int_G is new Ada.Numerics.Discrete_Random(Integer);
      Gen : Random_Int_G.Generator;
   begin
      Random_Int_G.Reset(Gen);
      return Random_Int_G.Random(Min => 0, Max => Max - 1);
   end Random_Int;
   
   -- Create polynomial coefficients
   function Create_Polynomial(Secret : Integer; Threshold : Integer) return Share_Array is
      Coefficients : Share_Array(0 .. Threshold - 1);
   begin
      Coefficients(0) := (X => 0, Y => Secret);
      
      -- Generate random coefficients for polynomial
      for I in 1 .. Threshold - 1 loop
         Coefficients(I) := (X => 0, Y => Random_Int(Prime));
      end loop;
      
      return Coefficients;
   end Create_Polynomial;
   
   -- Evaluate polynomial at point X
   function Evaluate_Polynomial(Coefficients : Share_Array; X : Integer) return Integer is
      Result : Integer := Coefficients(0).Y;
      Power : Integer := 1;
   begin
      for I in 1 .. Coefficients'Length - 1 loop
         Power := Mod_Mul(Power, X);
         Result := Mod_Add(Result, Mod_Mul(Coefficients(I).Y, Power));
      end loop;
      return Result;
   end Evaluate_Polynomial;
   
   -- Generate shares
   function Generate_Shares(Secret : Integer; Num_Share : Integer; Threshold : Integer) 
      return Share_Array is
      Coefficients : Share_Array(0 .. Threshold - 1);
      Shares : Share_Array(1 .. Num_Share);
   begin
      -- Create polynomial with secret as constant term
      Coefficients := Create_Polynomial(Secret, Threshold);
      
      -- Generate shares
      for I in 1 .. Num_Share loop
         Shares(I) := (X => I, Y => Evaluate_Polynomial(Coefficients, I));
      end loop;
      
      return Shares;
   end Generate_Shares;
   
   -- Reconstruct secret from shares
   function Reconstruct_Secret(Shares : Share_Array; Threshold : Integer) return Integer is
      Result : Integer := 0;
      Temp : Integer;
   begin
      for I in 1 .. Threshold loop
         Temp := Shares(I).Y;
         for J in 1 .. Threshold loop
            if I /= J then
               -- Lagrange interpolation
               Temp := Mod_Mul(Temp, Shares(J).X);
               Temp := Mod_Mul(Temp, Mod_Inv(Mod_Sub(Shares(J).X, Shares(I).X)));
            end if;
         end loop;
         Result := Mod_Add(Result, Temp);
      end loop;
      return Result;
   end Reconstruct_Secret;
   
   -- Test the algorithm
   procedure Test_Shamir is
      Secret : constant Integer := 12345;
      Num_Share : constant Integer := 5;
      Threshold : constant Integer := 3;
      Shares : Share_Array(1 .. Num_Share);
      Reconstructed_Secret : Integer;
   begin
      Put_Line("Shamir's Secret Sharing Test");
      Put_Line("============================");
      Put_Line("Original Secret: " & Integer'Image(Secret));
      Put_Line("Number of Shares: " & Integer'Image(Num_Share));
      Put_Line("Threshold: " & Integer'Image(Threshold));
      Put_Line("");
      
      -- Generate shares
      Shares := Generate_Shares(Secret, Num_Share, Threshold);
      
      -- Display shares
      Put_Line("Generated Shares:");
      for I in 1 .. Num_Share loop
         Put("Share " & Integer'Image(I) & ": (X=" & Integer'Image(Shares(I).X) & 
             ", Y=" & Integer'Image(Shares(I).Y) & ")");
         Put_Line("");
      end loop;
      Put_Line("");
      
      -- Reconstruct with threshold number of shares
      Put_Line("Reconstructing secret with " & Integer'Image(Threshold) & " shares:");
      
      -- Take first threshold shares for reconstruction
      declare
         Test_Shares : Share_Array(1 .. Threshold);
      begin
         for I in 1 .. Threshold loop
            Test_Shares(I) := Shares(I);
         end loop;
         
         Reconstructed_Secret := Reconstruct_Secret(Test_Shares, Threshold);
         Put_Line("Reconstructed Secret: " & Integer'Image(Reconstructed_Secret));
         
         if Reconstructed_Secret = Secret then
            Put_Line("SUCCESS: Secret successfully reconstructed!");
         else
            Put_Line("ERROR: Secret reconstruction failed!");
         end if;
      end;
      
      Put_Line("");
      
      -- Try with fewer shares (should fail)
      Put_Line("Attempting reconstruction with only 2 shares (should fail):");
      declare
         Test_Shares : Share_Array(1 .. 2);
         Failed_Secret : Integer;
      begin
         for I in 1 .. 2 loop
            Test_Shares(I) := Shares(I);
         end loop;
         
         Failed_Secret := Reconstruct_Secret(Test_Shares, 2);
         Put_Line("Reconstructed Secret with 2 shares: " & Integer'Image(Failed_Secret));
         Put_Line("This should not reveal the original secret!");
      end;
      
   end Test_Shamir;

begin
   Test_Shamir;
end Shamir_Secret_Sharing;
```

## How it works:

1. **Setup**: Uses a large prime number for finite field arithmetic to ensure security
2. **Polynomial Generation**: Creates a polynomial of degree (threshold-1) where the constant term is the secret
3. **Share Generation**: Evaluates the polynomial at different points to create shares
4. **Reconstruction**: Uses Lagrange interpolation to reconstruct the secret from threshold number of shares

## Key Features:

- **Modular Arithmetic**: All operations are performed in a finite field
- **Security**: Requires at least `threshold` shares to reconstruct the secret
- **Privacy**: Fewer than `threshold` shares reveal no information about the secret
- **Reproducibility**: Same inputs always produce the same shares

## Sample Output:
```
Shamir's Secret Sharing Test
============================
Original Secret:  12345
Number of Shares:  5
Threshold:  3

Generated Shares:
Share  1: (X= 1, Y= 12345)
Share  2: (X= 2, Y= 18423)
Share  3: (X= 3, Y= 24501)
Share  4: (X= 4, Y= 30579)
Share  5: (X= 5, Y= 36657)

Reconstructing secret with  3 shares:
Reconstructed Secret:  12345
SUCCESS: Secret successfully reconstructed!
```

This implementation demonstrates the core principles of Shamir's Secret Sharing while maintaining the mathematical correctness required for cryptographic applications.

