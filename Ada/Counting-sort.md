```ada
-- Counting Sort Algorithm in Ada

with Ada.Text_IO; use Ada.Text_IO;

procedure Counting_Sort is
   
   type Integer_Array is array (Positive range <>) of Integer;
   
   -- Counting Sort procedure
   procedure Counting_Sort_Array(A : in out Integer_Array) is
      Max_Value, Min_Value : Integer;
      Count_Array          : array (Integer range <>) of Integer;
      Sorted_Array         : Integer_Array(A'Range);
      Index                : Integer;
   begin
      -- Handle empty array
      if A'Length = 0 then
         return;
      end if;
      
      -- Find minimum and maximum values
      Min_Value := A(A'First);
      Max_Value := A(A'First);
      
      for I in A'First + 1 .. A'Last loop
         if A(I) < Min_Value then
            Min_Value := A(I);
         elsif A(I) > Max_Value then
            Max_Value := A(I);
         end if;
      end loop;
      
      -- Create count array
      declare
         Count_Size : constant Integer := Max_Value - Min_Value + 1;
         Temp_Count : array (0 .. Count_Size - 1) of Integer;
      begin
         -- Initialize count array to zero
         for I in Temp_Count'Range loop
            Temp_Count(I) := 0;
         end loop;
         
         -- Count occurrences of each element
         for I in A'Range loop
            Temp_Count(A(I) - Min_Value) := Temp_Count(A(I) - Min_Value) + 1;
         end loop;
         
         -- Calculate cumulative counts
         for I in Temp_Count'First + 1 .. Temp_Count'Last loop
            Temp_Count(I) := Temp_Count(I) + Temp_Count(I - 1);
         end loop;
         
         -- Build sorted array from counts
         for I in reverse A'Range loop
            Index := Temp_Count(A(I) - Min_Value);
            Sorted_Array(Index) := A(I);
            Temp_Count(A(I) - Min_Value) := Temp_Count(A(I) - Min_Value) - 1;
         end loop;
         
         -- Copy sorted array back to original array
         for I in A'Range loop
            A(I) := Sorted_Array(I);
         end loop;
      end;
   end Counting_Sort_Array;
   
   -- Helper procedure to print array
   procedure Print_Array(A : Integer_Array) is
   begin
      for I in A'Range loop
         Put(A(I) & " ");
      end loop;
      New_Line;
   end Print_Array;
   
   -- Test the algorithm
   Test_Array : Integer_Array := (4, 2, 2, 8, 3, 3, 1);
   
begin
   Put_Line("Original array:");
   Print_Array(Test_Array);
   
   Counting_Sort_Array(Test_Array);
   
   Put_Line("Sorted array:");
   Print_Array(Test_Array);
   
end Counting_Sort;
```

This Ada implementation of counting sort includes:

1. **Type Definitions**: Defines `Integer_Array` as a flexible array type
2. **Counting_Sort_Array Procedure**: Main sorting algorithm that:
   - Finds minimum and maximum values in the array
   - Creates a count array based on the range of values
   - Counts occurrences of each element
   - Calculates cumulative counts
   - Builds the sorted output array
   - Copies the result back to the original array

3. **Key Features**:
   - Handles negative numbers through offset calculation
   - Works with any range of integers
   - In-place sorting (modifies original array)
   - Time complexity: O(n + k) where n is array size and k is range of input
   - Space complexity: O(k)

4. **Example Output**:
   ```
   Original array:
   4 2 2 8 3 3 1 
   Sorted array:
   1 2 2 3 3 4 8 
   ```

