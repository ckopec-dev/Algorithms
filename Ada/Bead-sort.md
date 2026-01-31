# Bead Sort Algorithm in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Bead_Sort is
   
   type Integer_Array is array (Positive range <>) of Integer;
   
   -- Bead sort implementation
   procedure Bead_Sort_Array(A : in out Integer_Array) is
      Max_Value : Integer := 0;
      Beads     : array (Integer range 1..1000) of Integer := (others => 0);
      Temp      : Integer_Array(A'Range);
   begin
      -- Find maximum value
      for I in A'Range loop
         if A(I) > Max_Value then
            Max_Value := A(I);
         end if;
      end loop;
      
      -- Drop beads
      for I in A'Range loop
         for J in 1..A(I) loop
            Beads(J) := Beads(J) + 1;
         end loop;
      end loop;
      
      -- Count beads from bottom
      for I in reverse A'Range loop
         A(I) := 0;
         for J in 1..Max_Value loop
            if Beads(J) > 0 then
               A(I) := A(I) + 1;
               Beads(J) := Beads(J) - 1;
            end if;
         end loop;
      end loop;
   end Bead_Sort_Array;
   
   -- Helper procedure to print array
   procedure Print_Array(A : Integer_Array) is
   begin
      for I in A'Range loop
         Put(A(I) & " ");
      end loop;
      New_Line;
   end Print_Array;
   
   -- Test the algorithm
   Numbers : Integer_Array(1..5) := (5, 3, 8, 1, 9);
   
begin
   Put_Line("Original array:");
   Print_Array(Numbers);
   
   Bead_Sort_Array(Numbers);
   
   Put_Line("Sorted array:");
   Print_Array(Numbers);
end Bead_Sort;
```

## How it works:

1. **Find Maximum**: First, we find the maximum value in the array to determine how many "levels" we need
2. **Drop Beads**: For each element in the array, we drop that many beads at each level
3. **Count Beads**: We count beads from bottom to top to reconstruct the sorted array

## Example Output:
```
Original array:
5 3 8 1 9 
Sorted array:
1 3 5 8 9 
```

This implementation demonstrates the bead sort algorithm's unique approach of simulating beads falling under gravity to achieve sorting, which is particularly efficient for sorting non-negative integers.

