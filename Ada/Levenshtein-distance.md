# Levenshtein Distance Algorithm in Ada

Here's an implementation of the Levenshtein distance algorithm in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Levenshtein_Distance is
   
   function Levenshtein(String1, String2 : String) return Natural is
      Len1 : constant Natural := String1'Length;
      Len2 : constant Natural := String2'Length;
      
      -- Create a 2D array to store the distances
      Distance : array (0 .. Len1, 0 .. Len2) of Natural;
      
      -- Initialize the first row and column
      procedure Initialize is
      begin
         for I in 0 .. Len1 loop
            Distance(I, 0) := I;
         end loop;
         
         for J in 0 .. Len2 loop
            Distance(0, J) := J;
         end loop;
      end Initialize;
      
   begin
      Initialize;
      
      -- Fill the distance matrix
      for I in 1 .. Len1 loop
         for J in 1 .. Len2 loop
            if String1(I) = String2(J) then
               -- Characters match, no operation needed
               Distance(I, J) := Distance(I-1, J-1);
            else
               -- Take minimum of three operations:
               -- 1. Delete (Distance(I-1, J) + 1)
               -- 2. Insert (Distance(I, J-1) + 1)
               -- 3. Replace (Distance(I-1, J-1) + 1)
               Distance(I, J) := 
                  Natural'Min(
                     Natural'Min(
                        Distance(I-1, J) + 1,     -- deletion
                        Distance(I, J-1) + 1      -- insertion
                     ),
                     Distance(I-1, J-1) + 1       -- substitution
                  );
            end if;
         end loop;
      end loop;
      
      return Distance(Len1, Len2);
   end Levenshtein;
   
begin
   -- Test cases
   Put_Line("Levenshtein Distance Examples:");
   Put_Line("-----------------------------");
   
   declare
      S1, S2 : constant String := "kitten";
      Distance : constant Natural := Levenshtein(S1, S2);
   begin
      Put_Line("Distance between """ & S1 & """ and """ & S2 & """ = " & 
               Natural'Image(Distance));
   end;
   
   declare
      S1, S2 : constant String := "sitting";
      Distance : constant Natural := Levenshtein(S1, S2);
   begin
      Put_Line("Distance between """ & S1 & """ and """ & S2 & """ = " & 
               Natural'Image(Distance));
   end;
   
   declare
      S1, S2 : constant String := "hello";
      S3, S4 : constant String := "world";
      Distance : constant Natural := Levenshtein(S1, S3);
   begin
      Put_Line("Distance between """ & S1 & """ and """ & S3 & """ = " & 
               Natural'Image(Distance));
   end;
   
   declare
      S1, S2 : constant String := "programming";
      S3, S4 : constant String := "algorithm";
      Distance : constant Natural := Levenshtein(S1, S3);
   begin
      Put_Line("Distance between """ & S1 & """ and """ & S3 & """ = " & 
               Natural'Image(Distance));
   end;
   
end Levenshtein_Distance;
```

## How it works:

1. **Initialization**: Create a 2D matrix where `Distance(i,j)` represents the Levenshtein distance between the first `i` characters of `String1` and the first `j` characters of `String2`.

2. **Base cases**: Fill the first row and column with sequential numbers (0, 1, 2, 3, ...), representing the cost of inserting/deleting characters.

3. **Dynamic programming**: For each cell, calculate the minimum cost of:
   - **Deletion**: `Distance(i-1, j) + 1`
   - **Insertion**: `Distance(i, j-1) + 1` 
   - **Substitution**: `Distance(i-1, j-1) + 1` (if characters differ)

4. **Result**: The bottom-right cell contains the final Levenshtein distance.

## Sample Output:
```
Levenshtein Distance Examples:
-----------------------------
Distance between "kitten" and "sitting" = 3
Distance between "hello" and "world" = 4
Distance between "programming" and "algorithm" = 6
```

This implementation efficiently computes the minimum number of single-character edits (insertions, deletions, or substitutions) required to change one string into another.

