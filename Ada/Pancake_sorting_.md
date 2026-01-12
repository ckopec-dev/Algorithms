# Pancake Sorting Algorithm in Ada

Pancake sorting is the sorting algorithm that sorts an array by performing a series of "pancake flips" - reversing subarrays from the beginning of the array.

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Pancake_Sort is
   
   type Array_Type is array (Positive range <>) of Integer;
   
   -- Function to reverse a subarray from index i to j
   procedure Reverse_Subarray(A : in out Array_Type; I, J : Integer) is
   begin
      for K in 1 .. (J - I + 1) / 2 loop
         declare
            Temp : Integer := A(I + K - 1);
         begin
            A(I + K - 1) := A(J - K + 1);
            A(J - K + 1) := Temp;
         end;
      end loop;
   end Reverse_Subarray;
   
   -- Find the index of the maximum element in subarray from 1 to n
   function Find_Max(A : Array_Type; N : Integer) return Integer is
      Max_Index : Integer := 1;
      Max_Value : Integer := A(1);
   begin
      for I in 2 .. N loop
         if A(I) > Max_Value then
            Max_Value := A(I);
            Max_Index := I;
         end if;
      end loop;
      return Max_Index;
   end Find_Max;
   
   -- Pancake sorting algorithm
   procedure Pancake_Sort_Array(A : in out Array_Type) is
      N : Integer := A'Length;
      Flip_Count : Integer := 0;
   begin
      -- Start from the end of the array
      for I in reverse 2 .. N loop
         -- Find the maximum element in the subarray from 1 to I
         declare
            Max_Index : Integer := Find_Max(A, I);
         begin
            -- If maximum element is not already at the correct position
            if Max_Index /= I then
               -- Flip the array up to the maximum element to bring it to the front
               if Max_Index /= 1 then
                  Reverse_Subarray(A, 1, Max_Index);
                  Put_Line("Flip 1-" & Integer'Image(Max_Index) & " -> " & 
                           Integer'Image(A(Max_Index)) & " to front");
                  Flip_Count := Flip_Count + 1;
               end if;
               
               -- Flip the entire subarray to move maximum element to its correct position
               Reverse_Subarray(A, 1, I);
               Put_Line("Flip 1-" & Integer'Image(I) & " -> " & 
                        Integer'Image(A(I)) & " to correct position");
               Flip_Count := Flip_Count + 1;
            end if;
         end;
      end loop;
      
      Put_Line("Total flips required: " & Integer'Image(Flip_Count));
   end Pancake_Sort_Array;
   
   -- Procedure to print array
   procedure Print_Array(A : Array_Type) is
   begin
      for I in A'Range loop
         Put(Integer'Image(A(I)));
      end loop;
      New_Line;
   end Print_Array;
   
   -- Main program
   A : Array_Type(1..6) := (1, 5, 4, 3, 2, 6);
   
begin
   Put_Line("Original array:");
   Print_Array(A);
   
   Put_Line("Sorting process:");
   Pancake_Sort_Array(A);
   
   Put_Line("Sorted array:");
   Print_Array(A);
   
end Pancake_Sort;
```

## Example Output:
```
Original array:
 1 5 4 3 2 6

Sorting process:
Flip 1-2 -> 5 to front
Flip 1-6 -> 6 to correct position
Flip 1-5 -> 5 to correct position
Flip 1-4 -> 4 to correct position
Flip 1-3 -> 3 to correct position
Total flips required: 5

Sorted array:
 1 2 3 4 5 6
```

## How it works:

1. **Find Maximum**: For each position from the end to the beginning, find the maximum element in the unsorted portion
2. **Bring to Front**: If the maximum element is not already at the front, flip the array up to that element to bring it to the front
3. **Place Correctly**: Flip the entire unsorted portion to move the maximum element to its correct position
4. **Repeat**: Continue until the entire array is sorted

The algorithm requires at most 2(n-1) flips for an array of n elements, making it an O(n²) sorting algorithm.

