```ada
-- Comb Sort Algorithm in Ada

with Ada.Text_IO; use Ada.Text_IO;

procedure Comb_Sort is
   
   -- Define array type
   type Array_Type is array (Positive range <>) of Integer;
   
   -- Comb Sort procedure
   procedure Comb_Sort_Array(Arr : in out Array_Type) is
      Gap     : Integer;
      Swapped : Boolean;
      Temp    : Integer;
      N       : constant Integer := Arr'Length;
   begin
      if N <= 1 then
         return;
      end if;
      
      -- Initial gap
      Gap := N;
      Swapped := True;
      
      while Gap > 1 or Swapped loop
         -- Update gap using shrink factor
         Gap := Gap * 10 / 13;
         
         -- If gap becomes 0, set it to 1
         if Gap = 0 then
            Gap := 1;
         end if;
         
         Swapped := False;
         
         -- Compare elements with current gap
         for I in 1 .. N - Gap loop
            if Arr(I) > Arr(I + Gap) then
               -- Swap elements
               Temp := Arr(I);
               Arr(I) := Arr(I + Gap);
               Arr(I + Gap) := Temp;
               Swapped := True;
            end if;
         end loop;
      end loop;
   end Comb_Sort_Array;
   
   -- Procedure to print array
   procedure Print_Array(Arr : Array_Type) is
   begin
      for I in Arr'First .. Arr'Last loop
         Put(Integer'Image(Arr(I)) & " ");
      end loop;
      New_Line;
   end Print_Array;
   
   -- Test the algorithm
   Test_Array : Array_Type(1 .. 10) := (8, 4, 1, 56, 32, 5, 12, 9, 2, 7);
   
begin
   Put_Line("Original array:");
   Print_Array(Test_Array);
   
   Comb_Sort_Array(Test_Array);
   
   Put_Line("Sorted array:");
   Print_Array(Test_Array);
end Comb_Sort;
```

This Ada implementation of Comb Sort includes:

1. **Array Type Definition**: Uses a generic array type for flexibility
2. **Comb_Sort_Array Procedure**: Implements the core comb sort algorithm with:
   - Initial gap calculation using shrink factor of 10/13
   - Gap reduction in each iteration
   - Element swapping when necessary
   - Proper termination conditions
3. **Helper Procedures**: 
   - `Print_Array`: Displays array contents
   - Main procedure with test data
4. **Key Features**:
   - Uses Ada's array bounds checking
   - Handles edge cases (empty or single-element arrays)
   - Follows Ada programming conventions
   - Includes proper variable declarations and scoping

The algorithm works by comparing elements separated by a gap that decreases with each iteration, making it more efficient than bubble sort while being simpler to implement than other O(n log n) algorithms.

