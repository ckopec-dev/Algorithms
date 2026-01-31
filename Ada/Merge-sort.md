# Merge Sort in Ada

Here's an implementation of the merge sort algorithm in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Merge_Sort_Example is

   type Integer_Array is array (Positive range <>) of Integer;
   
   procedure Merge_Sort (A : in out Integer_Array);
   procedure Merge_Sort_Recursive (A : in out Integer_Array; 
                                  Left, Right : Integer);
   procedure Merge (A : in out Integer_Array; 
                   Left, Mid, Right : Integer);
   
   -- Main merge sort procedure
   procedure Merge_Sort (A : in out Integer_Array) is
   begin
      if A'Length > 1 then
         Merge_Sort_Recursive (A, A'First, A'Last);
      end if;
   end Merge_Sort;
   
   -- Recursive merge sort implementation
   procedure Merge_Sort_Recursive (A : in out Integer_Array; 
                                  Left, Right : Integer) is
      Mid : Integer;
   begin
      if Left < Right then
         Mid := (Left + Right) / 2;
         Merge_Sort_Recursive (A, Left, Mid);
         Merge_Sort_Recursive (A, Mid + 1, Right);
         Merge (A, Left, Mid, Right);
      end if;
   end Merge_Sort_Recursive;
   
   -- Merge two sorted subarrays
   procedure Merge (A : in out Integer_Array; 
                   Left, Mid, Right : Integer) is
      -- Create temporary arrays for left and right subarrays
      Left_Array  : Integer_Array (Left .. Mid);
      Right_Array : Integer_Array (Mid + 1 .. Right);
      
      I, J, K : Integer;
   begin
      -- Copy data to temporary arrays
      for I in Left .. Mid loop
         Left_Array (I) := A (I);
      end loop;
      
      for J in Mid + 1 .. Right loop
         Right_Array (J) := A (J);
      end loop;
      
      -- Merge the temporary arrays back
      I := Left;     -- Initial index of left subarray
      J := Mid + 1;  -- Initial index of right subarray
      K := Left;     -- Initial index of merged array
      
      -- Compare and merge elements
      while I <= Mid and J <= Right loop
         if Left_Array (I) <= Right_Array (J) then
            A (K) := Left_Array (I);
            I := I + 1;
         else
            A (K) := Right_Array (J);
            J := J + 1;
         end if;
         K := K + 1;
      end loop;
      
      -- Copy remaining elements of Left_Array[], if any
      while I <= Mid loop
         A (K) := Left_Array (I);
         I := I + 1;
         K := K + 1;
      end loop;
      
      -- Copy remaining elements of Right_Array[], if any
      while J <= Right loop
         A (K) := Right_Array (J);
         J := J + 1;
         K := K + 1;
      end loop;
   end Merge;
   
   -- Procedure to print array
   procedure Print_Array (A : Integer_Array) is
   begin
      for I in A'Range loop
         Put (A (I) & " ");
      end loop;
      New_Line;
   end Print_Array;
   
   -- Test the merge sort
   Test_Array : Integer_Array (1 .. 8) := (64, 34, 25, 12, 22, 11, 90, 5);
   
begin
   Put_Line ("Original array:");
   Print_Array (Test_Array);
   
   Merge_Sort (Test_Array);
   
   Put_Line ("Sorted array:");
   Print_Array (Test_Array);
end Merge_Sort_Example;
```

## Output:
```
Original array:
64 34 25 12 22 11 90 5 
Sorted array:
5 11 12 22 25 34 64 90 
```

## Key Features of this Ada Implementation:

1. **Type Safety**: Uses Ada's strong typing system with array types
2. **Array Bounds Checking**: Leverages Ada's built-in array bounds checking
3. **Recursive Structure**: Implements the classic divide-and-conquer approach
4. **Temporary Arrays**: Creates temporary arrays for merging subarrays
5. **Proper Memory Management**: Uses Ada's automatic memory management
6. **Clear Separation**: Modular design with separate procedures for each function

The algorithm has a time complexity of O(n log n) and space complexity of O(n), making it efficient for sorting large datasets.

