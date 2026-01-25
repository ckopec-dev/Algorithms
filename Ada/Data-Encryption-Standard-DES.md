# Data Encryption Standard (DES) Algorithm in Ada

Here's an example implementation of the DES algorithm in Ada:

```ada
-- DES Algorithm Implementation in Ada
-- This is a simplified version for demonstration purposes

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure DES_Example is
   
   -- DES constants
   type Bit_Array is array (1..64) of Boolean;
   type Byte_Array is array (1..8) of Integer;
   
   -- Initial Permutation Table
   Initial_Permutation : constant array (1..64) of Integer := (
      58, 50, 42, 34, 26, 18, 10, 2,
      60, 52, 44, 36, 28, 20, 12, 4,
      62, 54, 46, 38, 30, 22, 14, 6,
      64, 56, 48, 40, 32, 24, 16, 8,
      57, 49, 41, 33, 25, 17, 9, 1,
      59, 51, 43, 35, 27, 19, 11, 3,
      61, 53, 45, 37, 29, 21, 13, 5,
      63, 55, 47, 39, 31, 23, 15, 7
   );
   
   -- Subkey Generation Constants
   PC1 : constant array (1..56) of Integer := (
      57, 49, 41, 33, 25, 17, 9,
      1, 58, 50, 42, 34, 26, 18,
      10, 2, 59, 51, 43, 35, 27,
      19, 11, 3, 60, 52, 44, 36,
      63, 55, 47, 39, 31, 23, 15,
      7, 62, 54, 46, 38, 30, 22,
      14, 6, 61, 53, 45, 37, 29,
      21, 13, 5, 28, 20, 12, 4
   );
   
   -- Function to convert byte array to bit array
   function Bytes_To_Bits(Bytes : Byte_Array) return Bit_Array is
      Bits : Bit_Array;
   begin
      for I in 1..8 loop
         for J in 1..8 loop
            Bits((I-1)*8 + J) := (Bytes(I) and (2**(8-J))) /= 0;
         end loop;
      end loop;
      return Bits;
   end Bytes_To_Bits;
   
   -- Function to convert bit array to byte array
   function Bits_To_Bytes(Bits : Bit_Array) return Byte_Array is
      Bytes : Byte_Array;
   begin
      for I in 1..8 loop
         Bytes(I) := 0;
         for J in 1..8 loop
            if Bits((I-1)*8 + J) then
               Bytes(I) := Bytes(I) or (2**(8-J));
            end if;
         end loop;
      end loop;
      return Bytes;
   end Bits_To_Bytes;
   
   -- Initial Permutation
   function Initial_Permute(Bits : Bit_Array) return Bit_Array is
      Permuted : Bit_Array;
   begin
      for I in 1..64 loop
         Permuted(I) := Bits(Initial_Permutation(I));
      end loop;
      return Permuted;
   end Initial_Permute;
   
   -- Simple DES Round function (simplified)
   function DES_Round(Left, Right : Bit_Array; Round_Key : Bit_Array) return Bit_Array is
      -- This is a simplified version - real DES would be much more complex
      Combined : Bit_Array;
   begin
      -- Simple XOR operation for demonstration
      for I in 1..32 loop
         Combined(I) := Left(I) xor Right(I);
      end loop;
      
      -- Add key (simplified)
      for I in 1..32 loop
         Combined(I) := Combined(I) xor Round_Key(I);
      end loop;
      
      return Combined;
   end DES_Round;
   
   -- Main DES encryption function (simplified)
   procedure DES_Encrypt(Plaintext : in Byte_Array; 
                        Ciphertext : out Byte_Array;
                        Key : in Byte_Array) is
      -- Convert to bit arrays
      Plain_Bits : Bit_Array := Bytes_To_Bits(Plaintext);
      Key_Bits : Bit_Array := Bytes_To_Bits(Key);
      Encrypted_Bits : Bit_Array;
      
      -- Initial permutation
      Permuted_Bits : Bit_Array := Initial_Permute(Plain_Bits);
      
      -- Simplified round operations
      Left_Part : Bit_Array;
      Right_Part : Bit_Array;
      
   begin
      -- Split into left and right parts (32 bits each)
      for I in 1..32 loop
         Left_Part(I) := Permuted_Bits(I);
         Right_Part(I) := Permuted_Bits(I+32);
      end loop;
      
      -- Simple round processing (in real DES, this would be 16 rounds)
      for Round in 1..1 loop
         -- Simple substitution
         for I in 1..32 loop
            Right_Part(I) := Right_Part(I) xor (Key_Bits(I) and Left_Part(I));
         end loop;
      end loop;
      
      -- Combine back
      for I in 1..32 loop
         Encrypted_Bits(I) := Right_Part(I);
         Encrypted_Bits(I+32) := Left_Part(I);
      end loop;
      
      -- Convert back to bytes
      Ciphertext := Bits_To_Bytes(Encrypted_Bits);
   end DES_Encrypt;
   
   -- Test data
   Plaintext : constant Byte_Array := (16#3A#, 16#2D#, 16#4B#, 16#5E#, 16#6F#, 16#7C#, 16#8D#, 16#9A#);
   Key : constant Byte_Array := (16#12#, 16#34#, 16#56#, 16#78#, 16#90#, 16#AB#, 16#CD#, 16#EF#);
   Ciphertext : Byte_Array;
   
begin
   -- Display input data
   Put_Line("DES Encryption Example");
   Put_Line("=====================");
   Put_Line("Plaintext (hex): ");
   for I in 1..8 loop
      Put(Key(I), Width => 2);
      Put(" ");
   end loop;
   New_Line;
   
   Put_Line("Key (hex): ");
   for I in 1..8 loop
      Put(Key(I), Width => 2);
      Put(" ");
   end loop;
   New_Line;
   
   -- Perform encryption
   DES_Encrypt(Plaintext, Ciphertext, Key);
   
   -- Display results
   Put_Line("Ciphertext (hex): ");
   for I in 1..8 loop
      Put(Ciphertext(I), Width => 2);
      Put(" ");
   end loop;
   New_Line;
   
end DES_Example;
```

## Key Features of This Implementation

This Ada implementation demonstrates:

1. **Bit Manipulation**: Uses Boolean arrays to represent binary data
2. **Data Types**: Defines custom types for bit arrays and byte arrays
3. **Permutation Functions**: Implements initial permutation table
4. **Key Handling**: Processes 64-bit keys for encryption
5. **Simplified Rounds**: Shows the basic structure of DES rounds
6. **Input/Output**: Displays plaintext, key, and ciphertext in hexadecimal

## Important Notes

- This is a **simplified educational example** of DES
- The actual DES algorithm is much more complex with:
  - 16 rounds of processing
  - Complex permutation tables
  - S-box substitutions
  - Key scheduling algorithms
- For production use, consider using established cryptographic libraries
- DES is considered insecure for modern applications due to its 56-bit key length

## Compilation and Execution

To compile and run this Ada program:

```bash
gnatmake des_example.adb
./des_example
```

This example provides a foundation for understanding how DES works in Ada, though a full implementation would require implementing all the complex components of the actual DES algorithm.

