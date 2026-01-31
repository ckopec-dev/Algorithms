# Cocktail Shaker Sort in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Cocktail_Shaker_Sort is
   
   type Integer_Array is array (Positive range <>) of Integer;
   
   procedure Cocktail_Shaker (A : in out Integer_Array) is
      Left_Index  : Positive := A'First;
      Right_Index : Positive := A'Last;
      Swapped     : Boolean;
      Temp        : Integer;
   begin
      loop
         -- Forward pass (left to right)
         Swapped := False;
         for I in Left_Index .. Right_Index - 1 loop
            if A(I) > A(I + 1) then
               Temp := A(I);
               A(I) := A(I + 1);
               A(I + 1) := Temp;
               Swapped := True;
            end if;
         end loop;
         
         -- If no swaps occurred, array is sorted
         exit when not Swapped;
         
         -- Move the right boundary one position to the left
         Right_Index := Right_Index - 1;
         
         -- Backward pass (right to left)
         for I in reverse Left_Index + 1 .. Right_Index loop
            if A(I) < A(I - 1) then
               Temp := A(I);
               A(I) := A(I - 1);
               A(I - 1) := Temp;
               Swapped := True;
            end if;
         end loop;
         
         -- Move the left boundary one position to the right
         Left_Index := Left_Index + 1;
         
         -- If no swaps occurred, array is sorted
         exit when not Swapped;
      end loop;
   end Cocktail_Shaker;
   
   -- Procedure to print array elements
   procedure Print_Array (A : Integer_Array) is
   begin
      for I in A'Range loop
         Put(Integer'Image(A(I)) & " ");
      end loop;
      New_Line;
   end Print_Array;
   
   -- Test the algorithm
   Test_Array : Integer_Array(1..8) := (64, 34, 25, 12, 22, 11, 90, 5);
   
begin
   Put_Line("Original array:");
   Print_Array(Test_Array);
   
   Cocktail_Shaker(Test_Array);
   
   Put_Line("Sorted array:");
   Print_Array(Test_Array);
end Cocktail_Shaker_Sort;
```

## How it works:

The Cocktail Shaker Sort (also known as Bidirectional Bubble Sort) is a variation of bubble sort that sorts in both directions on each pass through the list. It works by:

1. **Forward pass**: Comparing adjacent elements from left to right and moving larger elements toward the right
2. **Backward pass**: Comparing adjacent elements from right to left and moving smaller elements toward the left
3. **Boundary adjustment**: After each pass, the sorted boundaries are adjusted
4. **Early termination**: If no swaps occur during a complete pass, the array is sorted

## Output:
```
Original array:
 64  34  25  12  22  11  90   5 
Sorted array:
  5  11  12  22  25  34  64  90 
```

This implementation uses Ada's array bounds checking and provides a clear, readable version of the cocktail shaker sort algorithm.

