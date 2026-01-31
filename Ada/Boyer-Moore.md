# Boyer-Moore Algorithm in Ada

Here's an implementation of the Boyer-Moore string search algorithm in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Boyer_Moore_Example is
   
   -- Function to build the bad character table
   function Build_Bad_Character_Table(Pattern : String) return array (Character) of Integer is
      Bad_Table : array (Character) of Integer;
   begin
      -- Initialize all entries to -1
      for I in Character'First .. Character'Last loop
         Bad_Table(I) := -1;
      end loop;
      
      -- Fill in the positions of characters in pattern
      for I in Pattern'Range loop
         Bad_Table(Pattern(I)) := I;
      end loop;
      
      return Bad_Table;
   end Build_Bad_Character_Table;
   
   -- Boyer-Moore search algorithm
   function Boyer_Moore_Search(Text : String; Pattern : String) return Integer is
      Bad_Table : array (Character) of Integer;
      Text_Length : constant Integer := Text'Length;
      Pattern_Length : constant Integer := Pattern'Length;
      i, j : Integer;
   begin
      -- Handle edge cases
      if Pattern_Length = 0 then
         return 0;
      end if;
      
      if Text_Length < Pattern_Length then
         return -1;
      end if;
      
      -- Build bad character table
      Bad_Table := Build_Bad_Character_Table(Pattern);
      
      i := Pattern_Length;
      
      while i <= Text_Length loop
         j := Pattern_Length;
         
         -- Compare from right to left
         while j > 0 and Text(i) = Pattern(j) loop
            i := i - 1;
            j := j - 1;
         end loop;
         
         -- If pattern is found
         if j = 0 then
            return i + 1;  -- Return 0-based index
         end if;
         
         -- Calculate shift using bad character rule
         declare
            Shift : constant Integer := j - Bad_Table(Text(i));
         begin
            if Shift > 0 then
               i := i + Shift;
            else
               i := i + 1;
            end if;
         end;
      end loop;
      
      return -1;  -- Pattern not found
   end Boyer_Moore_Search;
   
   -- Test the algorithm
   Text_String : constant String := "ABAAABCDABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABC

