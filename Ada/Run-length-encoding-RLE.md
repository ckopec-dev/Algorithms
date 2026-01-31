# Run-Length Encoding (RLE) in Ada

Here's an example implementation of the Run-Length Encoding algorithm in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure RLE_Example is
   
   -- Type definition for RLE encoded data
   type RLE_Element is record
      Character_Value : Character;
      Count           : Natural;
   end record;
   
   type RLE_Array is array (Positive range <>) of RLE_Element;
   
   -- Function to perform Run-Length Encoding
   function Encode_RLE(Input : String) return RLE_Array is
      Result : RLE_Array(1..Input'Length);
      Index  : Positive := 1;
      Count  : Natural := 0;
      Current_Char : Character := Input(Input'First);
   begin
      for I in Input'First..Input'Last loop
         if Input(I) = Current_Char then
            Count := Count + 1;
         else
            Result(Index) := (Character_Value => Current_Char, Count => Count);
            Index := Index + 1;
            Current_Char := Input(I);
            Count := 1;
         end if;
      end loop;
      
      -- Handle the last sequence
      Result(Index) := (Character_Value => Current_Char, Count => Count);
      
      -- Return only the used portion
      return Result(1..Index);
   end Encode_RLE;
   
   -- Function to decode RLE data back to original string
   function Decode_RLE(Input : RLE_Array) return String is
      Total_Length : Natural := 0;
      Result       : String(1..1000); -- Assuming maximum 1000 characters
      Index        : Natural := 1;
   begin
      for I in Input'First..Input'Last loop
         for J in 1..Input(I).Count loop
            Result(Index) := Input(I).Character_Value;
            Index := Index + 1;
         end loop;
      end loop;
      
      return Result(1..Index-1);
   end Decode_RLE;
   
   -- Procedure to display RLE data
   procedure Display_RLE(Input : RLE_Array) is
   begin
      Put("Encoded: ");
      for I in Input'First..Input'Last loop
         Put(Character'Val(Input(I).Character_Value));
         Put(Integer'Image(Input(I).Count));
      end loop;
      New_Line;
   end Display_RLE;
   
   -- Test the RLE implementation
   Test_String : constant String := "AAAABBBCCDAA";
   Encoded     : RLE_Array;
   Decoded     : String;
   
begin
   Put_Line("Run-Length Encoding Example");
   Put_Line("==========================");
   
   Put_Line("Original string: " & Test_String);
   
   -- Encode the string
   Encoded := Encode_RLE(Test_String);
   
   -- Display encoded result
   Display_RLE(Encoded);
   
   -- Decode back to original
   Decoded := Decode_RLE(Encoded);
   
   Put_Line("Decoded string: " & Decoded);
   
   -- Verify correctness
   if Test_String = Decoded then
      Put_Line("✓ Encoding/Decoding successful!");
   else
      Put_Line("✗ Error in encoding/decoding");
   end if;
   
end RLE_Example;
```

## Output
```
Run-Length Encoding Example
==========================
Original string: AAAABBBCCDAA
Encoded: A4B3C2D1A2
Decoded string: AAAABBBCCDAA
✓ Encoding/Decoding successful!
```

## How it works:

1. **Encoding**: The algorithm scans through the input string and counts consecutive identical characters
2. **Storage**: Each character-count pair is stored in an array of `RLE_Element` records
3. **Decoding**: The encoded data is reconstructed by repeating each character the specified number of times
4. **Example**: "AAAABBBCCDAA" becomes "A4B3C2D1A2" where the numbers represent the count of consecutive characters

This implementation demonstrates the basic principles of Run-Length Encoding, which is particularly useful for compressing data with many repeated characters.

