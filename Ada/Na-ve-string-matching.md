# Naïve String Matching Algorithm in Ada

Here's an implementation of the naïve string matching algorithm (also known as brute-force string matching) in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Naive_String_Match is
   
   -- Function to perform naive string matching
   function Naive_Search(Text : String; Pattern : String) return Integer is
      Text_Length : constant Integer := Text'Length;
      Pattern_Length : constant Integer := Pattern'Length;
      i, j : Integer;
   begin
      -- Handle edge cases
      if Pattern_Length = 0 then
         return 0;  -- Empty pattern found at position 0
      end if;
      
      if Text_Length < Pattern_Length then
         return -1;  -- Pattern longer than text
      end if;
      
      -- Search for pattern in text
      for i in 1 .. Text_Length - Pattern_Length + 1 loop
         j := 1;
         -- Check if pattern matches at position i
         while j <= Pattern_Length loop
            if Text(i + j - 1) /= Pattern(j) then
               exit;
            end if;
            j := j + 1;
         end loop;
         
         -- If we've matched all characters, we found a match
         if j > Pattern_Length then
            return i - 1;  -- Return 0-based index
         end if;
      end loop;
      
      return -1;  -- Pattern not found
   end Naive_Search;
   
   -- Test procedure
   procedure Test_Search is
      Text : constant String := "ABABCABABDABABCABCABCABCABC";
      Pattern : constant String := "ABABCABCABCABC";
      Result : Integer;
   begin
      Put_Line("Text: " & Text);
      Put_Line("Pattern: " & Pattern);
      
      Result := Naive_Search(Text, Pattern);
      
      if Result >= 0 then
         Put_Line("Pattern found at position: " & Integer'Image(Result));
      else
         Put_Line("Pattern not found in text");
      end if;
   end Test_Search;
   
begin
   Test_Search;
   
   -- Additional test cases
   Put_Line("--- Additional Test Cases ---");
   
   -- Test case 1: Pattern found at beginning
   declare
      Text1 : constant String := "hello world";
      Pattern1 : constant String := "hello";
      Result1 : Integer := Naive_Search(Text1, Pattern1);
   begin
      Put_Line("Text: " & Text1);
      Put_Line("Pattern: " & Pattern1);
      Put_Line("Result: " & Integer'Image(Result1));
   end;
   
   -- Test case 2: Pattern not found
   declare
      Text2 : constant String := "abcdefg";
      Pattern2 : constant String := "xyz";
      Result2 : Integer := Naive_Search(Text2, Pattern2);
   begin
      Put_Line("Text: " & Text2);
      Put_Line("Pattern: " & Pattern2);
      Put_Line("Result: " & Integer'Image(Result2));
   end;
   
end Naive_String_Match;
```

## How it works:

1. **Function `Naive_Search`**: Takes a text string and a pattern string as input
2. **Edge case handling**: Checks for empty pattern or pattern longer than text
3. **Main loop**: Iterates through each possible starting position in the text
4. **Character comparison**: For each position, compares characters one by one
5. **Match found**: If all characters match, returns the starting position
6. **No match**: If no match is found after checking all positions, returns -1

## Time Complexity:
- **Best case**: O(n) when pattern is at the beginning
- **Average case**: O(n×m) where n is text length and m is pattern length
- **Worst case**: O(n×m) when pattern is at the end or not found

## Output Example:
```
Text: ABABCABABDABABCABCABCABCABC
Pattern: ABABCABCABCABC
Pattern found at position: 10

--- Additional Test Cases ---
Text: hello world
Pattern: hello
Result: 0

Text: abcdefg
Pattern: xyz
Result: -1
```

This implementation demonstrates the basic brute-force approach where we check every possible position in the text for a match with the pattern.

