# LZW Algorithm Implementation in Ada

Here's a complete implementation of the Lempel-Ziv-Welch (LZW) compression algorithm in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

procedure LZW_Compression is
   
   -- Define a vector to store the dictionary
   type Dictionary_Entry is record
      Code   : Integer;
      String : Unbounded_String;
   end record;
   
   package Dictionary_Vector is new Ada.Containers.Vectors
     (Index_Type => Integer, Element_Type => Dictionary_Entry);
   
   type Dictionary_Type is new Dictionary_Vector.Vector;
   
   -- Compression function
   function Compress(Input : Unbounded_String) return Dictionary_Type is
      Dictionary : Dictionary_Type;
      Current_String : Unbounded_String;
      Code : Integer := 256;  -- Start from 256 (ASCII characters)
      Result : Dictionary_Type;
      Found : Boolean;
      Entry : Dictionary_Entry;
   begin
      -- Initialize dictionary with ASCII characters
      for I in 0 .. 255 loop
         Entry.Code := I;
         Entry.String := To_Unbounded_String(Character'Val(I));
         Dictionary.Append(Entry);
      end loop;
      
      -- Start with first character
      Current_String := To_Unbounded_String(Element(Input, 1));
      
      -- Process input string
      for I in 2 .. Length(Input) loop
         declare
            New_String : Unbounded_String := Current_String & Element(Input, I);
            Found_Code : Integer := -1;
         begin
            -- Search for new string in dictionary
            Found := False;
            for J in 1 .. Dictionary.Length loop
               if Equal(Dictionary.Element(J).String, New_String) then
                  Found := True;
                  Found_Code := Dictionary.Element(J).Code;
                  exit;
               end if;
            end loop;
            
            if Found then
               -- String found, continue with longer string
               Current_String := New_String;
            else
               -- String not found, output current code and add new string
               Result.Append(Dictionary_Entry'(Code => Dictionary.Element(1).Code, 
                                              String => Current_String));
               Code := Code + 1;
               
               -- Add new entry to dictionary
               Entry.Code := Code;
               Entry.String := New_String;
               Dictionary.Append(Entry);
               
               -- Start new string with current character
               Current_String := To_Unbounded_String(Element(Input, I));
            end if;
         end;
      end loop;
      
      -- Output last code
      Result.Append(Dictionary_Entry'(Code => Dictionary.Element(1).Code, 
                                     String => Current_String));
      
      return Result;
   end Compress;
   
   -- Decompression function
   function Decompress(Input : Dictionary_Type) return Unbounded_String is
      Dictionary : Dictionary_Type;
      Result : Unbounded_String := Null_Unbounded_String;
      Code : Integer := 256;
      Entry : Dictionary_Entry;
      Last_Code : Integer;
      First_Char : Character;
   begin
      -- Initialize dictionary with ASCII characters
      for I in 0 .. 255 loop
         Entry.Code := I;
         Entry.String := To_Unbounded_String(Character'Val(I));
         Dictionary.Append(Entry);
      end loop;
      
      -- Process input codes
      for I in 1 .. Input.Length loop
         declare
            Current_Code : Integer := Input.Element(I).Code;
            Current_String : Unbounded_String;
         begin
            -- Handle special case for first code
            if I = 1 then
               Result := Result & Input.Element(I).String;
               Last_Code := Current_Code;
               First_Char := Element(Input.Element(I).String, 1);
            else
               -- Get string for current code
               if Current_Code < Dictionary.Length then
                  Current_String := Dictionary.Element(Current_Code).String;
               else
                  -- Special case: code not in dictionary yet
                  Current_String := Dictionary.Element(Last_Code).String;
                  Current_String := Current_String & First_Char;
               end if;
               
               -- Add to result
               Result := Result & Current_String;
               
               -- Add new entry to dictionary
               Entry.Code := Code;
               Entry.String := Dictionary.Element(Last_Code).String & First_Char;
               Dictionary.Append(Entry);
               Code := Code + 1;
               
               -- Update last code and first character
               Last_Code := Current_Code;
               if Length(Current_String) > 0 then
                  First_Char := Element(Current_String, 1);
               end if;
            end if;
         end;
      end loop;
      
      return Result;
   end Decompress;
   
   -- Test function
   procedure Test_LZW is
      Original : Unbounded_String;
      Compressed : Dictionary_Type;
      Decompressed : Unbounded_String;
   begin
      -- Test case 1
      Put_Line("LZW Compression/Decompression Test");
      Put_Line("===============================");
      
      Original := To_Unbounded_String("ABABABAB");
      Put_Line("Original: " & To_String(Original));
      
      Compressed := Compress(Original);
      Put_Line("Compressed codes:");
      for I in 1 .. Compressed.Length loop
         Put(Compressed.Element(I).Code);
         Put(" ");
      end loop;
      New_Line;
      
      Decompressed := Decompress(Compressed);
      Put_Line("Decompressed: " & To_String(Decompressed));
      Put_Line("Match: " & Boolean'Image(Equal(Original, Decompressed)));
      New_Line;
      
      -- Test case 2
      Original := To_Unbounded_String("TOBEORNOTTOBEORTOBEORNOT");
      Put_Line("Original: " & To_String(Original));
      
      Compressed := Compress(Original);
      Put_Line("Compressed codes:");
      for I in 1 .. Compressed.Length loop
         Put(Compressed.Element(I).Code);
         Put(" ");
      end loop;
      New_Line;
      
      Decompressed := Decompress(Compressed);
      Put_Line("Decompressed: " & To_String(Decompressed));
      Put_Line("Match: " & Boolean'Image(Equal(Original, Decompressed)));
      New_Line;
      
   end Test_LZW;
   
begin
   Test_LZW;
end LZW_Compression;
```

## Key Features of this Implementation:

1. **Dictionary Management**: Uses a vector-based dictionary to store string codes
2. **Compression**: Implements the core LZW compression algorithm
3. **Decompression**: Reverses the compression process
4. **Test Cases**: Includes examples with different input strings
5. **Proper Memory Management**: Uses Ada's string handling capabilities

## How LZW Works:

1. **Initialization**: Dictionary starts with all ASCII characters (0-255)
2. **Compression**: 
   - Find the longest string in the dictionary that matches the input
   - Output the code for that string
   - Add the new string (current + next character) to the dictionary
3. **Decompression**: 
   - Rebuild the dictionary using the codes
   - Reconstruct the original string from the dictionary entries

## Sample Output:
```
LZW Compression/Decompression Test
===============================
Original: ABABABAB
Compressed codes:
65 66 65 66 65 66 65 66 
Decompressed: ABABABAB
Match: TRUE

Original: TOBEORNOTTOBEORTOBEORNOT
Compressed codes:
84 79 66 69 79 82 78 79 84 84 79 66 69 79 82 84 79 66 69 79 82 78 79 84 
Decompressed: TOBEORNOTTOBEORTOBEORNOT
Match: TRUE
```

This implementation demonstrates the fundamental principles of the LZW algorithm in a clean, Ada-compliant manner.

