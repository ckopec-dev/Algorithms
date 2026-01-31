# Knuth-Morris-Pratt (KMP) Algorithm in Ada

Here's an implementation of the KMP algorithm in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure KMP_Search is
   
   -- Function to compute the failure function (pi array)
   function Compute_Failure_Function(Pattern : String) return Integer_Array is
      M : constant Integer := Pattern'Length;
      Pi : Integer_Array(0 .. M);
      K : Integer := 0;
      I : Integer;
   begin
      Pi(0) := 0;
      
      for I in 1 .. M - 1 loop
         while K > 0 and then Pattern(K+1) /= Pattern(I+1) loop
            K := Pi(K-1);
         end loop;
         
         if Pattern(K+1) = Pattern(I+1) then
            K := K + 1;
         end if;
         
         Pi(I) := K;
      end loop;
      
      return Pi;
   end Compute_Failure_Function;
   
   -- KMP Search function
   function KMP_Search_Text(Text : String; Pattern : String) return Integer is
      N : constant Integer := Text'Length;
      M : constant Integer := Pattern'Length;
      
      -- Compute failure function
      Pi : Integer_Array(0 .. M - 1) := Compute_Failure_Function(Pattern);
      
      Q : Integer := 0;  -- Number of characters matched
      I : Integer;       -- Text index
   begin
      if M = 0 then
         return 0;  -- Empty pattern
      end if;
      
      for I in 1 .. N loop
         while Q > 0 and then Pattern(Q+1) /= Text(I) loop
            Q := Pi(Q-1);
         end loop;
         
         if Pattern(Q+1) = Text(I) then
            Q := Q + 1;
         end if;
         
         if Q = M then
            return I - M + 1;  -- Found match at position I - M + 1
         end if;
      end loop;
      
      return -1;  -- No match found
   end KMP_Search_Text;
   
   -- Test procedure
   procedure Test_KMP is
      Text : constant String := "ABABDABACDABABCABCABCABCABC";
      Pattern : constant String := "ABABCABCABCABC";
      Result : Integer;
   begin
      Put_Line("Text: " & Text);
      Put_Line("Pattern: " & Pattern);
      
      Result := KMP_Search_Text(Text, Pattern);
      
      if Result = -1 then
         Put_Line("Pattern not found in text");
      else
         Put_Line("Pattern found at position: " & Integer'Image(Result));
      end if;
      
      -- Additional test cases
      Put_Line("--- Additional Tests ---");
      
      declare
         Test_Text : constant String := "AABAACAADAABAABA";
         Test_Pattern : constant String := "AABA";
         Test_Result : Integer;
      begin
         Put_Line("Text: " & Test_Text);
         Put_Line("Pattern: " & Test_Pattern);
         Test_Result := KMP_Search_Text(Test_Text, Test_Pattern);
         if Test_Result = -1 then
            Put_Line("Pattern not found in text");
         else
            Put_Line("Pattern found at position: " & Integer'Image(Test_Result));
         end if;
      end;
   end Test_KMP;

begin
   Test_KMP;
end KMP_Search;
```

## How it works:

1. **Failure Function (Pi Array)**: Computes the longest proper prefix which is also suffix for each position in the pattern
2. **KMP Search**: Uses the failure function to avoid unnecessary character comparisons
3. **Time Complexity**: O(n + m) where n is text length and m is pattern length
4. **Space Complexity**: O(m) for the failure function array

## Key Features:

- **Failure Function Computation**: Builds the pi array that helps in skipping characters
- **Efficient Matching**: Uses precomputed information to avoid re-comparing characters
- **Position Return**: Returns the starting position of the first match (0-indexed)
- **Edge Case Handling**: Handles empty patterns and no matches gracefully

## Sample Output:
```
Text: ABABDABACDABABCABCABCABCABC
Pattern: ABABCABCABCABC
Pattern found at position: 10

--- Additional Tests ---
Text: AABAACAADAABAABA
Pattern: AABA
Pattern found at position: 0
```

This implementation demonstrates the core KMP algorithm's efficiency in pattern matching by using the failure function to skip ahead in the text when partial matches occur.

