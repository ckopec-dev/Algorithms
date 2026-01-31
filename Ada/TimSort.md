# TimSort Algorithm in Ada

Here's an implementation of the TimSort algorithm in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure TimSort_Example is
   
   type Integer_Array is array (Positive range <>) of Integer;
   
   -- Minimum run size for TimSort
   MIN_MERGE : constant := 32;
   
   -- Insertion sort for small arrays
   procedure Insertion_Sort(Arr : in out Integer_Array; Left, Right : Integer) is
      I, J, Key : Integer;
   begin
      for I in Left+1 .. Right loop
         Key := Arr(I);
         J := I - 1;
         
         while J >= Left and then Arr(J) > Key loop
            Arr(J+1) := Arr(J);
            J := J - 1;
         end loop;
         
         Arr(J+1) := Key;
      end loop;
   end Insertion_Sort;
   
   -- Merge two sorted subarrays
   procedure Merge(Arr : in out Integer_Array; Left, Mid, Right : Integer) is
      Left_Size  : constant := Mid - Left + 1;
      Right_Size : constant := Right - Mid;
      
      -- Create temporary arrays
      Left_Array  : Integer_Array(1 .. Left_Size);
      Right_Array : Integer_Array(1 .. Right_Size);
      
      I, J, K : Integer;
   begin
      -- Copy data to temporary arrays
      for I in 1 .. Left_Size loop
         Left_Array(I) := Arr(Left + I - 1);
      end loop;
      
      for J in 1 .. Right_Size loop
         Right_Array(J) := Arr(Mid + J);
      end loop;
      
      -- Merge the temporary arrays back
      I := 1;
      J := 1;
      K := Left;
      
      while I <= Left_Size and then J <= Right_Size loop
         if Left_Array(I) <= Right_Array(J) then
            Arr(K) := Left_Array(I);
            I := I + 1;
         else
            Arr(K) := Right_Array(J);
            J := J + 1;
         end if;
         K := K + 1;
      end loop;
      
      -- Copy remaining elements
      while I <= Left_Size loop
         Arr(K) := Left_Array(I);
         I := I + 1;
         K := K + 1;
      end loop;
      
      while J <= Right_Size loop
         Arr(K) := Right_Array(J);
         J := J + 1;
         K := K + 1;
      end loop;
   end Merge;
   
   -- TimSort implementation
   procedure TimSort(Arr : in out Integer_Array) is
      N : constant := Arr'Length;
      Run_Size : Integer;
      Left, Mid, Right : Integer;
   begin
      if N < 2 then
         return;
      end if;
      
      -- Sort small runs using insertion sort
      for Left in 0 .. N - MIN_MERGE loop
         if Left + MIN_MERGE - 1 < N then
            Insertion_Sort(Arr, Left, Left + MIN_MERGE - 1);
         else
            Insertion_Sort(Arr, Left, N - 1);
         end if;
      end loop;
      
      -- Merge runs of increasing size
      Run_Size := MIN_MERGE;
      while Run_Size < N loop
         Left := 0;
         while Left < N - Run_Size loop
            Mid := Left + Run_Size - 1;
            Right := Math.Min(N - 1, Mid + Run_Size);
            
            if Mid < Right then
               Merge(Arr, Left, Mid, Right);
            end if;
            
            Left := Left + Run_Size * 2;
         end loop;
         
         Run_Size := Run_Size * 2;
      end loop;
   end TimSort;
   
   -- Helper function to find minimum of two integers
   function Math_Min(A, B : Integer) return Integer is
   begin
      if A < B then
         return A;
      else
         return B;
      end if;
   end Math_Min;
   
   -- Print array
   procedure Print_Array(Arr : Integer_Array) is
   begin
      for I in Arr'Range loop
         Put(Arr(I), Width => 4);
      end loop;
      New_Line;
   end Print_Array;
   
   -- Test the algorithm
   Test_Array : Integer_Array(1 .. 10) := (5, 2, 8, 1, 9, 3, 7, 4, 6, 0);
   
begin
   Put_Line("Original array:");
   Print_Array(Test_Array);
   
   TimSort(Test_Array);
   
   Put_Line("Sorted array:");
   Print_Array(Test_Array);
   
end TimSort_Example;
```

## Key Features of this TimSort Implementation:

1. **Hybrid Approach**: Combines insertion sort for small arrays with merge sort for larger ones
2. **Minimum Run Size**: Uses a minimum run size of 32 (standard for TimSort)
3. **Natural Runs**: Identifies and merges existing sorted sequences
4. **Stable Sorting**: Maintains relative order of equal elements
5. **Adaptive**: Performs well on partially sorted data

## How it works:

1. **Initial Sorting**: Sorts small runs (chunks) using insertion sort
2. **Merging**: Gradually merges runs of increasing size using merge sort
3. **Efficiency**: Takes advantage of existing order in the data

## Time Complexity:
- **Best Case**: O(n) - when array is already sorted
- **Average Case**: O(n log n)
- **Worst Case**: O(n log n)

## Space Complexity: O(n)

This implementation demonstrates the core principles of TimSort while maintaining the stability and efficiency characteristics that make it one of the most practical sorting algorithms.

