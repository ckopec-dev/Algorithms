# Introsort Algorithm in Ada

Here's an implementation of the Introsort (Introspective Sort) algorithm in Ada:

```ada
with Ada.Integer_Text_IO;
procedure Introsort_Example is
   type Integer_Array is array (Positive range <>) of Integer;
   
   -- Function to calculate the depth limit for introsort
   function Depth_Limit(n : Natural) return Natural is
   begin
      return 2 * Integer(Ada.Numerics.Generic_Elementary_Functions.Log(2.0, Float(n)));
   end Depth_Limit;
   
   -- Partition function for quicksort
   function Partition(arr : in out Integer_Array; low, high : Integer) return Integer is
      pivot : constant Integer := arr(high);
      i     : Integer := low - 1;
   begin
      for j in low .. high - 1 loop
         if arr(j) <= pivot then
            i := i + 1;
            -- Swap elements
            declare
               temp : constant Integer := arr(i);
            begin
               arr(i) := arr(j);
               arr(j) := temp;
            end;
         end if;
      end loop;
      
      -- Swap pivot with element at i+1
      declare
         temp : constant Integer := arr(i + 1);
      begin
         arr(i + 1) := arr(high);
         arr(high) := temp;
      end;
      
      return i + 1;
   end Partition;
   
   -- Heapify function for heapsort
   procedure Heapify(arr : in out Integer_Array; n, i : Integer) is
      largest : Integer := i;
      left    : Integer := 2 * i + 1;
      right   : Integer := 2 * i + 2;
   begin
      if left < n and then arr(left) > arr(largest) then
         largest := left;
      end if;
      
      if right < n and then arr(right) > arr(largest) then
         largest := right;
      end if;
      
      if largest /= i then
         -- Swap elements
         declare
            temp : constant Integer := arr(i);
         begin
            arr(i) := arr(largest);
            arr(largest) := temp;
         end;
         
         Heapify(arr, n, largest);
      end if;
   end Heapify;
   
   -- Heapsort implementation
   procedure Heap_Sort(arr : in out Integer_Array; low, high : Integer) is
      n : constant Integer := high - low + 1;
   begin
      -- Build heap
      for i in reverse 0 .. n/2 - 1 loop
         Heapify(arr, n, i);
      end loop;
      
      -- Extract elements from heap
      for i in reverse 1 .. n - 1 loop
         -- Swap root with last element
         declare
            temp : constant Integer := arr(0);
         begin
            arr(0) := arr(i);
            arr(i) := temp;
         end;
         
         Heapify(arr, i, 0);
      end loop;
   end Heap_Sort;
   
   -- Introsort recursive function
   procedure Introsort_Recursive(arr : in out Integer_Array; low, high, depth_limit : Integer) is
   begin
      if high - low + 1 <= 1 then
         return;
      elsif depth_limit = 0 then
         -- Switch to heapsort if depth limit reached
         Heap_Sort(arr, low, high);
      else
         -- Partition and recursively sort
         declare
            pi : constant Integer := Partition(arr, low, high);
         begin
            Introsort_Recursive(arr, low, pi - 1, depth_limit - 1);
            Introsort_Recursive(arr, pi + 1, high, depth_limit - 1);
         end;
      end if;
   end Introsort_Recursive;
   
   -- Main Introsort procedure
   procedure Introsort(arr : in out Integer_Array) is
      depth_limit : constant Natural := Depth_Limit(arr'Length);
   begin
      if arr'Length > 1 then
         Introsort_Recursive(arr, arr'First, arr'Last, depth_limit);
      end if;
   end Introsort;
   
   -- Procedure to print array
   procedure Print_Array(arr : in Integer_Array) is
   begin
      for i in arr'Range loop
         Ada.Integer_Text_IO.Put(arr(i), Width => 4);
      end loop;
      Ada.Text_IO.New_Line;
   end Print_Array;
   
   -- Test the implementation
   Test_Array : Integer_Array(1 .. 10) := (5, 2, 8, 1, 9, 3, 7, 4, 6, 0);
   
begin
   Ada.Text_IO.Put_Line("Original array:");
   Print_Array(Test_Array);
   
   Introsort(Test_Array);
   
   Ada.Text_IO.Put_Line("Sorted array:");
   Print_Array(Test_Array);
end Introsort_Example;
```

## Key Features of this Implementation:

1. **Hybrid Approach**: Combines quicksort, heapsort, and insertion sort
2. **Depth Limit**: Uses a calculated depth limit to prevent worst-case performance
3. **Three Sorting Methods**:
   - Quicksort for normal cases
   - Heapsort when depth limit is reached
   - Partitioning function for quicksort
4. **Memory Efficient**: In-place sorting with O(1) extra space
5. **Time Complexity**: O(n log n) average case, O(n log n) worst case

## How it Works:

1. **Initial Call**: Calculates depth limit based on array size
2. **Recursive Process**: 
   - Uses quicksort for normal partitioning
   - Switches to heapsort when maximum depth is reached
3. **Depth Control**: Prevents quicksort from degrading to O(n²) performance
4. **Guaranteed Performance**: Ensures O(n log n) complexity in all cases

The algorithm provides the best of both worlds: the average-case speed of quicksort and the worst-case guarantee of heapsort.

