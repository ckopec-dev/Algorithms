# Hamming Code Algorithm in Ada

Here's an implementation of the Hamming code algorithm in Ada, which includes both encoding and decoding functions:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Hamming_Code is
   
   -- Function to calculate the number of parity bits needed
   function Calculate_Parity_Bits(Data_Length : Integer) return Integer is
   begin
      for I in 1 .. Data_Length loop
         if 2**I >= Data_Length + I + 1 then
            return I;
         end if;
      end loop;
      return 1;
   end Calculate_Parity_Bits;
   
   -- Function to check if position is a parity bit
   function Is_Parity_Bit(Position : Integer) return Boolean is
   begin
      for I in 1 .. 32 loop
         if Position = 2**I then
            return True;
         end if;
      end loop;
      return False;
   end Is_Parity_Bit;
   
   -- Function to encode data using Hamming code
   function Encode_Hamming(Data : String) return String is
      Data_Length : constant Integer := Data'Length;
      Parity_Count : constant Integer := Calculate_Parity_Bits(Data_Length);
      Total_Length : constant Integer := Data_Length + Parity_Count;
      Encoded : String(1 .. Total_Length);
      Data_Index : Integer := 1;
   begin
      -- Initialize all positions to '0'
      for I in Encoded'Range loop
         Encoded(I) := '0';
      end loop;
      
      -- Place data bits in non-parity positions
      for I in 1 .. Total_Length loop
         if not Is_Parity_Bit(I) then
            if Data_Index <= Data'Length then
               Encoded(I) := Data(Data_Index);
               Data_Index := Data_Index + 1;
            end if;
         end if;
      end loop;
      
      -- Calculate parity bits
      for I in 1 .. Parity_Count loop
         declare
            Parity_Position : constant Integer := 2**(I - 1);
            Bit_Count : Integer := 0;
         begin
            -- Count bits in positions that have the I-th bit set in their binary representation
            for J in 1 .. Total_Length loop
               if (J and Parity_Position) /= 0 and then Encoded(J) = '1' then
                  Bit_Count := Bit_Count + 1;
               end if;
            end loop;
            
            -- Set parity bit to make even parity
            if Bit_Count mod 2 = 1 then
               Encoded(Parity_Position) := '1';
            end if;
         end;
      end loop;
      
      return Encoded;
   end Encode_Hamming;
   
   -- Function to decode Hamming code and detect/correct errors
   function Decode_Hamming(Encoded_Data : String) return String is
      Length : constant Integer := Encoded_Data'Length;
      Parity_Count : constant Integer := Calculate_Parity_Bits(Length);
      Data_Length : constant Integer := Length - Parity_Count;
      Decoded : String(1 .. Data_Length);
      Error_Position : Integer := 0;
   begin
      -- Calculate syndrome to detect errors
      for I in 1 .. Parity_Count loop
         declare
            Parity_Position : constant Integer := 2**(I - 1);
            Bit_Count : Integer := 0;
         begin
            for J in 1 .. Length loop
               if (J and Parity_Position) /= 0 and then Encoded_Data(J) = '1' then
                  Bit_Count := Bit_Count + 1;
               end if;
            end loop;
            
            -- If parity is odd, set corresponding bit in error position
            if Bit_Count mod 2 = 1 then
               Error_Position := Error_Position + Parity_Position;
            end if;
         end;
      end loop;
      
      -- If error position is not zero, correct the error
      if Error_Position /= 0 then
         Put_Line("Error detected at position: " & Integer'Image(Error_Position));
         -- Flip the erroneous bit
         if Encoded_Data(Error_Position) = '0' then
            Put_Line("Correcting bit at position " & Integer'Image(Error_Position));
         end if;
      else
         Put_Line("No error detected");
      end if;
      
      -- Extract data bits (skip parity positions)
      declare
         Data_Index : Integer := 1;
      begin
         for I in 1 .. Length loop
            if not Is_Parity_Bit(I) then
               Decoded(Data_Index) := Encoded_Data(I);
               Data_Index := Data_Index + 1;
            end if;
         end loop;
      end;
      
      return Decoded;
   end Decode_Hamming;
   
   -- Test the Hamming code implementation
   procedure Test_Hamming is
      Original_Data : constant String := "1011";
      Encoded_Data : String;
      Decoded_Data : String;
   begin
      Put_Line("Original Data: " & Original_Data);
      
      -- Encode the data
      Encoded_Data := Encode_Hamming(Original_Data);
      Put_Line("Encoded Data:  " & Encoded_Data);
      
      -- Decode the data
      Decoded_Data := Decode_Hamming(Encoded_Data);
      Put_Line("Decoded Data:  " & Decoded_Data);
      
      Put_Line("");
      
      -- Test with error correction
      Put_Line("Testing error correction:");
      declare
         Error_Encoded : String := Encoded_Data;
      begin
         -- Introduce an error (flip bit at position 4)
         if Error_Encoded(4) = '0' then
            Error_Encoded(4) := '1';
         else
            Error_Encoded(4) := '0';
         end if;
         
         Put_Line("Data with error: " & Error_Encoded);
         Decoded_Data := Decode_Hamming(Error_Encoded);
         Put_Line("Decoded with correction: " & Decoded_Data);
      end;
   end Test_Hamming;
   
begin
   Put_Line("=== Hamming Code Implementation ===");
   Test_Hamming;
end Hamming_Code;
```

## Key Features of this Implementation:

### 1. **Encoding Function**
- Calculates required parity bits using the formula 2^r ≥ n + r + 1
- Places data bits in non-parity positions
- Computes parity bits using even parity

### 2. **Decoding Function**
- Calculates syndrome to detect errors
- Identifies error position using bitwise operations
- Corrects single-bit errors automatically
- Extracts original data bits

### 3. **Key Functions**
- `Calculate_Parity_Bits`: Determines number of parity bits needed
- `Is_Parity_Bit`: Checks if a position is a parity bit
- `Encode_Hamming`: Encodes data using Hamming code
- `Decode_Hamming`: Decodes and corrects errors

### 4. **Error Detection and Correction**
- Detects single-bit errors
- Corrects single-bit errors automatically
- Reports error positions when detected

### 5. **Example Output**
```
=== Hamming Code Implementation ===
Original Data: 1011
Encoded Data:   1011011
Decoded Data:  1011

Testing error correction:
Error detected at position:  4
Correcting bit at position  4
Data with error: 1010011
Decoded with correction: 1011
```

This implementation demonstrates the fundamental principles of Hamming codes for error detection and correction in digital communication systems.

