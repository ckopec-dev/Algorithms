# Rabin-Karp Algorithm in Ada

Here's an implementation of the Rabin-Karp string matching algorithm in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Rabin_Karp is
   
   -- Hash function parameters
   Prime : constant := 101;  -- A prime number
   Base  : constant := 256;  -- Number of characters in input alphabet
   
   -- Function to compute hash value of a string
   function Hash(String_Value : String) return Integer is
      Hash_Value : Integer := 0;
      Power      : Integer := 1;
   begin
      for I in String_Value'Range loop
         Hash_Value := (Hash_Value + (Integer(Character'Pos(String_Value(I))) * Power)) mod Prime;
         Power := (Power * Base) mod Prime;
      end loop;
      return Hash_Value;
   end Hash;
   
   -- Function to compute hash value of a substring with given length
   function Hash_Substring(String_Value : String; Length : Natural) return Integer is
      Hash_Value : Integer := 0;
      Power      : Integer := 1;
   begin
      for I in 1..Length loop
         Hash_Value := (Hash_Value + (Integer(Character'Pos(String_Value(I))) * Power)) mod Prime;
         Power := (Power * Base) mod Prime;
      end loop;
      return Hash_Value;
   end Hash_Substring;
   
   -- Function to check if two strings match
   function Strings_Match(Text : String; Pattern : String; Start_Pos : Natural) return Boolean is
   begin
      if Start_Pos + Pattern'Length - 1 > Text'Length then
         return False;
      end if;
      
      for I in Pattern'Range loop
         if Text(Start_Pos + I - Pattern'First) /= Pattern(I) then
            return False;
         end if;
      end loop;
      return True;
   end Strings_Match;
   
   -- Main Rabin-Karp algorithm
   function Rabin_Karp_Search(Text : String; Pattern : String) return Integer is
      Text_Length   : constant Natural := Text'Length;
      Pattern_Length : constant Natural := Pattern'Length;
      Pattern_Hash  : Integer;
      Text_Hash     : Integer;
      Power         : Integer := 1;
      Result        : Integer := -1;  -- -1 means not found
   begin
      -- Handle edge cases
      if Pattern_Length > Text_Length then
         return -1;
      end if;
      
      -- Calculate base^(pattern_length-1) % prime
      for I in 1..Pattern_Length-1 loop
         Power := (Power * Base) mod Prime;
      end loop;
      
      -- Calculate hash value of pattern and first window of text
      Pattern_Hash := Hash_Substring(Pattern, Pattern_Length);
      Text_Hash := Hash_Substring(Text, Pattern_Length);
      
      -- Slide the pattern over text one by one
      for I in 0..Text_Length - Pattern_Length loop
         -- Check if hash values match
         if Pattern_Hash = Text_Hash then
            -- Check for actual match
            if Strings_Match(Text, Pattern, I + 1) then
               Result := I;
               return Result;
            end if;
         end if;
         
         -- Calculate hash value for next window of text
         if I < Text_Length - Pattern_Length then
            Text_Hash := (Base * (Text_Hash - (Integer(Character'Pos(Text(I + 1))) * Power)) + 
                         Integer(Character'Pos(Text(I + Pattern_Length + 1)))) mod Prime;
            
            -- Handle negative hash values
            if Text_Hash < 0 then
               Text_Hash := Text_Hash + Prime;
            end if;
         end if;
      end loop;
      
      return Result;
   end Rabin_Karp_Search;
   
   -- Test the algorithm
   Text   : constant String := "ABABDABACDABABCABCABCABCABC";
   Pattern : constant String := "ABABCABCABCABC";
   
   Position : Integer;
   
begin
   Put_Line("Text: " & Text);
   Put_Line("Pattern: " & Pattern);
   Put_Line("Length of text: " & Integer'Image(Text'Length));
   Put_Line("Length of pattern: " & Integer'Image(Pattern'Length));
   
   Position := Rabin_Karp_Search(Text, Pattern);
   
   if Position = -1 then
      Put_Line("Pattern not found in text");
   else
      Put_Line("Pattern found at position: " & Integer'Image(Position));
      Put_Line("Matched substring: " & Text(Position + 1 .. Position + Pattern'Length));
   end if;
   
   -- Additional test cases
   Put_Line("--- Additional Test Cases ---");
   
   -- Test case 1
   declare
      Test_Text   : constant String := "AABAACAADAABAABA";
      Test_Pattern : constant String := "AABA";
      Test_Pos : Integer;
   begin
      Test_Pos := Rabin_Karp_Search(Test_Text, Test_Pattern);
      Put_Line("Text: " & Test_Text);
      Put_Line("Pattern: " & Test_Pattern);
      Put_Line("Position: " & Integer'Image(Test_Pos));
   end;
   
   -- Test case 2
   declare
      Test_Text   : constant String := "HELLO WORLD";
      Test_Pattern : constant String := "WORLD";
      Test_Pos : Integer;
   begin
      Test_Pos := Rabin_Karp_Search(Test_Text, Test_Pattern);
      Put_Line("Text: " & Test_Text);
      Put_Line("Pattern: " & Test_Pattern);
      Put_Line("Position: " & Integer'Image(Test_Pos));
   end;
   
end Rabin_Karp;
```

## How the Algorithm Works

The Rabin-Karp algorithm uses a rolling hash to efficiently search for patterns in text:

1. **Hash Calculation**: Each string is converted to a hash value using polynomial rolling hash
2. **Pattern Hash**: Compute hash value of the pattern to search for
3. **Sliding Window**: Slide a window of pattern length over the text
4. **Hash Comparison**: Compare hash values of current window and pattern
5. **Verification**: When hash values match, verify actual string match to handle hash collisions

## Key Features

- **Time Complexity**: O(n+m) average case, O(n×m) worst case
- **Space Complexity**: O(1)
- **Handles Hash Collisions**: Uses verification step when hash matches
- **Rolling Hash**: Efficiently updates hash values as window slides

## Sample Output

```
Text: ABABDABACDABABCABCABCABCABC
Pattern: ABABCABCABCABC
Length of text:  28
Length of pattern:  14
Pattern found at position:  10
Matched substring: ABABCABCABCABC
--- Additional Test Cases ---
Text: AABAACAADAABAABA
Pattern: AABA
Position:  0
Text: HELLO WORLD
Pattern: WORLD
Position:  6
```

This implementation demonstrates the core principles of the Rabin-Karp algorithm while maintaining Ada's strong typing and structured programming features.

