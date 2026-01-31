# Lempel-Ziv Algorithm Implementation in Ada

Here's a simple implementation of the Lempel-Ziv compression algorithm in Ada:

```ada
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;

procedure Lempel_Ziv_Compression is
   
   type Dictionary_Entry is record
      Index : Integer;
      Value : Unbounded_String;
   end record;
   
   type Dictionary_Array is array (Positive range <>) of Dictionary_Entry;
   
   -- Simple LZ compression function
   function Compress_Text(Text : String) return String is
      Dictionary : Dictionary_Array(1..256);
      Dict_Size : Integer := 0;
      Result : Unbounded_String := Null_Unbounded_String;
      I : Integer := 1;
      Current_Window : Unbounded_String;
      Match_Index : Integer;
      Found : Boolean;
      
      -- Helper function to find string in dictionary
      function Find_In_Dictionary(Search_String : String) return Integer is
      begin
         for J in 1..Dict_Size loop
            if To_String(Dictionary(J).Value) = Search_String then
               return Dictionary(J).Index;
            end if;
         end loop;
         return -1;
      end Find_In_Dictionary;
      
   begin
      -- Initialize dictionary with single characters
      for J in 0..255 loop
         Dict_Size := Dict_Size + 1;
         Dictionary(Dict_Size).Index := J;
         Dictionary(Dict_Size).Value := To_Unbounded_String(Character'Val(J));
      end loop;
      
      -- Main compression loop
      while I <= Text'Length loop
         Current_Window := Null_Unbounded_String;
         Match_Index := -1;
         Found := False;
         
         -- Try to find longest match in dictionary
         for Length in reverse 1..Text'Length - I + 1 loop
            declare
               Test_String : String(1..Length);
            begin
               for K in 1..Length loop
                  Test_String(K) := Text(I + K - 1);
               end loop;
               
               Match_Index := Find_In_Dictionary(Test_String);
               if Match_Index /= -1 then
                  Found := True;
                  exit;
               end if;
            end;
         end loop;
         
         if Found then
            -- Add match index to result
            Put(Result, Character'Val(0)); -- Separator
            Put(Result, Match_Index);
            Put(Result, Character'Val(0));
            I := I + Length;
         else
            -- Add new character to dictionary
            Dict_Size := Dict_Size + 1;
            Dictionary(Dict_Size).Index := Dict_Size;
            Dictionary(Dict_Size).Value := To_Unbounded_String(Text(I));
            Put(Result, Character'Val(0));
            Put(Result, 0); -- New character
            Put(Result, Character'Val(0));
            Put(Result, Text(I));
            I := I + 1;
         end if;
      end loop;
      
      return To_String(Result);
   end Compress_Text;
   
   -- Simple LZ decompression function
   function Decompress_Text(Compressed : String) return String is
      Result : Unbounded_String := Null_Unbounded_String;
      I : Integer := 1;
      Dict_Size : Integer := 0;
      Dictionary : Dictionary_Array(1..256);
      
   begin
      -- Initialize dictionary with single characters
      for J in 0..255 loop
         Dict_Size := Dict_Size + 1;
         Dictionary(Dict_Size).Index := J;
         Dictionary(Dict_Size).Value := To_Unbounded_String(Character'Val(J));
      end loop;
      
      while I <= Compressed'Length loop
         if Compressed(I) = Character'Val(0) then
            I := I + 1;
            if I > Compressed'Length then exit;
            declare
               Index : Integer;
            begin
               -- Parse index
               Index := 0;
               while I <= Compressed'Length and Compressed(I) /= Character'Val(0) loop
                  Index := Index * 10 + Integer'Value(Character'Val(Compressed(I)));
                  I := I + 1;
               end loop;
               I := I + 1; -- Skip separator
               
               if Index = 0 then
                  -- New character
                  if I <= Compressed'Length then
                     Put(Result, Compressed(I));
                     Dict_Size := Dict_Size + 1;
                     Dictionary(Dict_Size).Index := Dict_Size;
                     Dictionary(Dict_Size).Value := To_Unbounded_String(Compressed(I));
                     I := I + 1;
                  end if;
               else
                  -- Existing pattern
                  Put(Result, To_String(Dictionary(Index).Value));
                  -- Add to dictionary (simplified)
               end if;
            end;
         else
            Put(Result, Compressed(I));
            I := I + 1;
         end if;
      end loop;
      
      return To_String(Result);
   end Decompress_Text;
   
   -- Test the implementation
   Input_Text : constant String := "ABABABABABABABABABABABABABABABAB";
   Compressed : String;
   Decompressed : String;
   
begin
   Put_Line("Original Text:");
   Put_Line(Input_Text);
   Put_Line("Length: " & Integer'Image(Input_Text'Length));
   
   Put_Line("Compressing...");
   Compressed := Compress_Text(Input_Text);
   Put_Line("Compressed:");
   Put_Line(Compressed);
   Put_Line("Length: " & Integer'Image(Compressed'Length));
   
   Put_Line("Decompressing...");
   Decompressed := Decompress_Text(Compressed);
   Put_Line("Decompressed:");
   Put_Line(Decompressed);
   
   if Input_Text = Decompressed then
      Put_Line("Success: Original and decompressed text match!");
   else
      Put_Line("Error: Text mismatch!");
   end if;
   
end Lempel_Ziv_Compression;
```

## Key Features of this Implementation:

1. **Dictionary-based approach**: Uses a dictionary to store previously seen patterns
2. **Simple compression algorithm**: Finds longest matches in the dictionary
3. **Basic encoding**: Uses separator characters to distinguish between indices and literal characters
4. **Compression/decompression functions**: Separate functions for both operations
5. **Error handling**: Basic checks for bounds and valid characters

## How it works:

1. **Initialization**: Dictionary starts with all single characters (0-255)
2. **Compression**: 
   - Scan input for longest match in dictionary
   - Output match index if found
   - Add new patterns to dictionary if not found
3. **Decompression**: 
   - Parse indices and literal characters
   - Reconstruct original text using dictionary

This is a simplified version of LZ algorithms. Real-world implementations like LZ77 or LZ78 would be more sophisticated but this demonstrates the core principles of Lempel-Ziv compression in Ada.

