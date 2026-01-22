# Quick Sort Algorithm in Ada

Here's an implementation of the Quick Sort algorithm in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Quick_Sort_Example is
   
   type Integer_Array is array (Positive range <>) of Integer;
   
   -- Partition function
   function Partition(A : in out Integer_Array; Low, High : Integer) return Integer is
      Pivot : Integer := A(High);
      I     : Integer := Low - 1;
      J     : Integer;
      Temp  : Integer;
   begin
      for J in Low .. High - 1 loop
         if A(J) <= Pivot then
            I := I + 1;
            -- Swap A(I) and A(J)
            Temp := A(I);
            A(I) := A(J);
            A(J) := Temp;
         end if;
      end loop;
      
      -- Swap A(I+1) and A(High)
      Temp := A(I + 1);
      A(I + 1) := A(High);
      A(High) := Temp;
      
      return I + 1;
   end Partition;
   
   -- Quick Sort procedure
   procedure Quick_Sort(A : in out Integer_Array; Low, High : Integer) is
      Pivot_Index : Integer;
   begin
      if Low < High then
         Pivot_Index := Partition(A, Low, High);
         Quick_Sort(A, Low, Pivot_Index - 1);
         Quick_Sort(A, Pivot_Index + 1, High);
      end if;
   end Quick_Sort;
   
   -- Helper procedure to print array
   procedure Print_Array(A : in Integer_Array) is
   begin
      for I in A'Range loop
         Put(A(I) & " ");
      end loop;
      New_Line;
   end Print_Array;
   
   -- Main program
   A : Integer_Array(1..7) := (64, 34, 25, 12, 22, 11, 90);
   
begin
   Put_Line("Original array:");
   Print_Array(A);
   
   Quick_Sort(A, A'First, A'Last);
   
   Put_Line("Sorted array:");
   Print_Array(A);
end Quick_Sort_Example;
```

## Output:
```
Original array:
64 34 25 12 22 11 90 
Sorted array:
11 12 22 25 34 64 90 
```

## Key Features of this Implementation:

1. **Generic Structure**: Uses Ada's array types and bounds checking
2. **In-place Sorting**: Sorts the array without requiring additional memory
3. **Recursive Approach**: Implements the classic divide-and-conquer quicksort algorithm
4. **Partition Function**: Places the pivot element in its correct position
5. **Boundary Handling**: Properly handles array bounds using Ada's range checking

## How it Works:

1. **Partition**: Rearranges the array so elements smaller than the pivot come before it, and larger elements come after it
2. **Recursion**: Recursively applies the same process to the sub-arrays on both sides of the pivot
3. **Base Case**: When the sub-array has one or zero elements, it's already sorted

The algorithm has an average time complexity of O(n log n) and worst-case complexity of O(n²).

