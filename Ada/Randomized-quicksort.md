# Randomized Quicksort in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Discrete_Random;

procedure Randomized_Quicksort is
   
   type Integer_Array is array (Positive range <>) of Integer;
   
   -- Generate random number in range 1..N
   package Random_Int is new Ada.Numerics.Discrete_Random(Integer);
   Gen : Random_Int.Generator;
   
   -- Partition function with random pivot
   function Partition(A : in out Integer_Array; Low, High : Integer) return Integer is
      Pivot_Index : Integer;
      Temp        : Integer;
   begin
      -- Randomly select pivot index
      Pivot_Index := Low + Random_Int.Random(Gen) mod (High - Low + 1);
      
      -- Move pivot to end
      Temp := A(Pivot_Index);
      A(Pivot_Index) := A(High);
      A(High) := Temp;
      
      -- Standard partitioning
      declare
         i : Integer := Low - 1;
         j : Integer;
      begin
         for j in Low..High-1 loop
            if A(j) <= A(High) then
               i := i + 1;
               Temp := A(i);
               A(i) := A(j);
               A(j) := Temp;
            end if;
         end loop;
         
         -- Move pivot to correct position
         Temp := A(i + 1);
         A(i + 1) := A(High);
         A(High) := Temp;
         
         return i + 1;
      end;
   end Partition;
   
   -- Randomized quicksort recursive function
   procedure QuickSort(A : in out Integer_Array; Low, High : Integer) is
   begin
      if Low < High then
         declare
            Pivot_Index : Integer;
         begin
            Pivot_Index := Partition(A, Low, High);
            QuickSort(A, Low, Pivot_Index - 1);
            QuickSort(A, Pivot_Index + 1, High);
         end;
      end if;
   end QuickSort;
   
   -- Wrapper procedure to initialize random generator and sort array
   procedure Sort_Array(A : in out Integer_Array) is
   begin
      Random_Int.Reset(Gen);
      QuickSort(A, A'First, A'Last);
   end Sort_Array;
   
   -- Print array function
   procedure Print_Array(A : in Integer_Array) is
   begin
      for I in A'Range loop
         Put(A(I), 4);
      end loop;
      New_Line;
   end Print_Array;
   
   -- Test the algorithm
   Test_Array : Integer_Array(1..10) := (5, 2, 8, 1, 9, 3, 7, 4, 6, 0);
   
begin
   Put_Line("Original array:");
   Print_Array(Test_Array);
   
   Sort_Array(Test_Array);
   
   Put_Line("Sorted array:");
   Print_Array(Test_Array);
   
end Randomized_Quicksort;
```

## Key Features of this Implementation:

1. **Random Pivot Selection**: Uses `Ada.Numerics.Discrete_Random` to select a random pivot element
2. **Standard Partitioning**: Implements the classic Lomuto partition scheme
3. **Recursive Sorting**: Uses recursion to sort subarrays
4. **Proper Array Handling**: Uses Ada's array bounds checking and indexing
5. **Modular Design**: Separates concerns into different procedures

## How it Works:

1. **Random Pivot**: A random index is selected from the current subarray
2. **Pivot Movement**: The random pivot is moved to the end of the subarray
3. **Partitioning**: Elements are rearranged so that smaller elements are on the left and larger on the right
4. **Recursion**: The algorithm recursively sorts the left and right subarrays

This implementation demonstrates how to incorporate randomness into the quicksort algorithm to avoid worst-case O(n²) performance on already sorted arrays.

