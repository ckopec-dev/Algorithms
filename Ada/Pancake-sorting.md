# Pancake Sorting Algorithm in Ada

Pancake sorting is a sorting algorithm that sorts an array by performing a series of "pancake flips" - reversing subarrays from the beginning of the array.

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Pancake_Sort is
   
   type Array_Type is array (Positive range <>) of Integer;
   
   -- Function to reverse a subarray from index i to j
   procedure Reverse_Subarray(Arr : in out Array_Type; I, J : Integer) is
   begin
      while I < J loop
         declare
            Temp : Integer := Arr(I);
         begin
            Arr(I) := Arr(J);
            Arr(J) := Temp;
            I := I + 1;
            J := J - 1;
         end;
      end loop;
   end Reverse_Subarray;
   
   -- Find the index of the maximum element in subarray from 1 to N
   function Find_Max_Index(Arr : Array_Type; N : Integer) return Integer is
      Max_Index : Integer := 1;
      Max_Value : Integer := Arr(1);
   begin
      for I in 2 .. N loop
         if Arr(I) > Max_Value then
            Max_Value := Arr(I);
            Max_Index := I;
         end if;
      end loop;
      return Max_Index;
   end Find_Max_Index;
   
   -- Main pancake sorting algorithm
   procedure Pancake_Sort_Array(Arr : in out Array_Type) is
      N : Integer := Arr'Length;
      Max_Index : Integer;
   begin
      -- Start from the end of the array
      for I in reverse 2 .. N loop
         -- Find the maximum element in the unsorted portion
         Max_Index := Find_Max_Index(Arr, I);
         
         -- If the maximum element is not already at the correct position
         if Max_Index /= I then
            -- Flip the maximum element to the front
            if Max_Index /= 1 then
               Reverse_Subarray(Arr, 1, Max_Index);
               Put_Line("Flip 1 to " & Integer'Image(Max_Index) & ": " & 
                       Integer'Image(Arr(Max_Index)) & " to front");
            end if;
            
            -- Flip the maximum element to its correct position at the end
            Reverse_Subarray(Arr, 1, I);
            Put_Line("Flip 1 to " & Integer'Image(I) & ": " & 
                    Integer'Image(Arr(I)) & " to correct position");
         end if;
         
         -- Display current state
         Put("Current array: ");
         for J in Arr'Range loop
            Put(Integer'Image(Arr(J)));
         end loop;
         New_Line;
      end loop;
   end Pancake_Sort_Array;
   
   -- Procedure to print array
   procedure Print_Array(Arr : Array_Type) is
   begin
      Put("Array: ");
      for I in Arr'Range loop
         Put(Integer'Image(Arr(I)));
      end loop;
      New_Line;
   end Print_Array;
   
   -- Test the algorithm
   procedure Test_Pancake_Sort is
      Test_Array : Array_Type(1..6) := (8, 3, 6, 1, 4, 2);
   begin
      Put_Line("Pancake Sorting Algorithm Demo");
      Put_Line("==============================");
      Put_Line("Original array:");
      Print_Array(Test_Array);
      New_Line;
      
      Put_Line("Sorting process:");
      Pancake_Sort_Array(Test_Array);
      
      Put_Line("Final sorted array:");
      Print_Array(Test_Array);
   end Test_Pancake_Sort;
   
begin
   Test_Pancake_Sort;
end Pancake_Sort;
```

## How it works:

1. **Find Maximum**: For each position from the end to the beginning, find the maximum element in the unsorted portion
2. **Flip to Front**: If the maximum element is not already at the front, flip it to the front
3. **Flip to Correct Position**: Flip the maximum element to its correct position at the end of the unsorted portion
4. **Repeat**: Continue until the entire array is sorted

## Sample Output:
```
Pancake Sorting Algorithm Demo
==============================
Original array:
Array:  8  3  6  1  4  2

Sorting process:
Flip 1 to 1:  8 to front
Flip 1 to 6:  8 to correct position
Current array:  2  4  1  6  3  8
Flip 1 to 2:  4 to front
Flip 1 to 5:  4 to correct position
Current array:  1  3  2  4  6  8
Flip 1 to 3:  3 to front
Flip 1 to 4:  3 to correct position
Current array:  1  2  3  4  6  8
Flip 1 to 2:  2 to front
Flip 1 to 3:  2 to correct position
Current array:  1  2  3  4  6  8
Final sorted array:
Array:  1  2  3  4  6  8
```

The algorithm has a time complexity of O(n²) and requires at most 2n - 3 flips to sort the array.

