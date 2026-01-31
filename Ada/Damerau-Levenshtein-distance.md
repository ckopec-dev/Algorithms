# Damerau-Levenshtein Distance in Ada

Here's an implementation of the Damerau-Levenshtein distance algorithm in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Damerau_Levenshtein is
   
   function Damerau_Levenshtein_Distance(S1, S2 : String) return Integer is
      L1 : constant Integer := S1'Length;
      L2 : constant Integer := S2'Length;
      
      -- Create a 2D array for dynamic programming
      type Matrix is array (0 .. L1, 0 .. L2) of Integer;
      D : Matrix;
      
      I, J : Integer;
   begin
      -- Initialize base cases
      for I in 0 .. L1 loop
         D(I, 0) := I;
      end loop;
      
      for J in 0 .. L2 loop
         D(0, J) := J;
      end loop;
      
      -- Fill the matrix
      for I in 1 .. L1 loop
         for J in 1 .. L2 loop
            if S1(I) = S2(J) then
               -- Characters match, no operation needed
               D(I, J) := D(I-1, J-1);
            else
               -- Take minimum of three operations
               D(I, J) := Integer'Min(
                  Integer'Min(
                     D(I-1, J) + 1,     -- deletion
                     D(I, J-1) + 1      -- insertion
                  ),
                  D(I-1, J-1) + 1       -- substitution
               );
            end if;
            
            -- Check for transposition (Damerau-Levenshtein only)
            if I > 1 and J > 1 and S1(I) = S2(J-1) and S1(I-1) = S2(J) then
               D(I, J) := Integer'Min(
                  D(I, J),
                  D(I-2, J-2) + 1  -- transposition
               );
            end if;
         end loop;
      end loop;
      
      return D(L1, L2);
   end Damerau_Levenshtein_Distance;
   
   -- Test cases
   procedure Test_Distance(S1, S2 : String) is
      Distance : constant Integer := Damerau_Levenshtein_Distance(S1, S2);
   begin
      Put_Line("Distance between """ & S1 & """ and """ & S2 & """ is: " & Integer'Image(Distance));
   end Test_Distance;
   
begin
   Put_Line("Damerau-Levenshtein Distance Examples:");
   Put_Line("------------------------------------");
   
   Test_Distance("kitten", "sitting");
   Test_Distance("saturday", "sunday");
   Test_Distance("abc", "acb");
   Test_Distance("abc", "def");
   Test_Distance("hello", "hallo");
   Test_Distance("", "test");
   Test_Distance("test", "");
   
end Damerau_Levenshtein;
```

## Expected Output:
```
Damerau-Levenshtein Distance Examples:
------------------------------------
Distance between "kitten" and "sitting" is:  3
Distance between "saturday" and "sunday" is:  3
Distance between "abc" and "acb" is:  1
Distance between "abc" and "def" is:  3
Distance between "hello" and "hallo" is:  1
Distance between "" and "test" is:  4
Distance between "test" and "" is:  4
```

## Key Features of this Implementation:

1. **Dynamic Programming Approach**: Uses a 2D matrix to store intermediate results
2. **Damerau-Levenshtein Specific**: Includes transposition operations in addition to insertions, deletions, and substitutions
3. **Proper Initialization**: Handles edge cases like empty strings
4. **Clear Structure**: Well-commented code with descriptive variable names
5. **Test Cases**: Includes various examples to demonstrate functionality

## Algorithm Explanation:

The Damerau-Levenshtein distance extends the classic Levenshtein distance by allowing transpositions (swapping adjacent characters) as a single operation. The algorithm computes the minimum number of operations needed to transform one string into another, where operations include:
- Insertion of a character
- Deletion of a character  
- Substitution of a character
- Transposition of two adjacent characters

The time complexity is O(m×n) where m and n are the lengths of the input strings.

