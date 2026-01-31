# Heap Sort Algorithm in Ada

Here's an implementation of the Heap Sort algorithm in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Heap_Sort is
   
   -- Define array type
   type Array_Type is array (Integer range <>) of Integer;
   
   -- Heapify procedure to maintain the heap property
   procedure Heapify(A : in out Array_Type; n, i : Integer) is
      largest : Integer := i;
      left    : Integer := 2 * i + 1;
      right   : Integer := 2 * i + 2;
   begin
      -- If left child exists and is greater than root
      if left < n and then A(left) > A(largest) then
         largest := left;
      end if;
      
      -- If right child exists and is greater than largest so far
      if right < n and then A(right) > A(largest) then
         largest := right;
      end if;
      
      -- If largest is not root
      if largest /= i then
         -- Swap root with largest
         declare
            temp : Integer := A(i);
         begin
            A(i) := A(largest);
            A(largest) := temp;
         end;
         
         -- Recursively heapify the affected sub-tree
         Heapify(A, n, largest);
      end if;
   end Heapify;
   
   -- Main heap sort procedure
   procedure Heap_Sort_Proc(A : in out Array_Type) is
      n : Integer := A'Last - A'First + 1;
   begin
      -- Build heap (rearrange array)
      for i in reverse 0 .. n/2 - 1 loop
         Heapify(A, n, i);
      end loop;
      
      -- One by one extract elements from heap
      for i in reverse 1 .. n - 1 loop
         -- Move current root to end
         declare
            temp : Integer := A(0);
         begin
            A(0) := A(i);
            A(i) := temp;
         end;
         
         -- Call heapify on the reduced heap
         Heapify(A, i, 0);
      end loop;
   end Heap_Sort_Proc;
   
   -- Procedure to print array
   procedure Print_Array(A : Array_Type) is
   begin
      for i in A'First .. A'Last loop
         Put(Integer'Image(A(i)));
      end loop;
      New_Line;
   end Print_Array;
   
   -- Test the algorithm
   Test_Array : Array_Type(0 .. 9) := (12, 11, 13, 5, 6, 7, 1, 8, 9, 10);
   
begin
   Put_Line("Original array:");
   Print_Array(Test_Array);
   
   Heap_Sort_Proc(Test_Array);
   
   Put_Line("Sorted array:");
   Print_Array(Test_Array);
end Heap_Sort;
```

## How it works:

1. **Heapify Procedure**: Maintains the heap property by ensuring that the largest element is at the root
2. **Build Heap**: Converts the array into a max heap by calling heapify on all non-leaf nodes
3. **Sort**: Repeatedly extracts the maximum element from the heap and rebuilds the heap

## Output:
```
Original array:
 12 11 13 5 6 7 1 8 9 10
Sorted array:
 1 5 6 7 8 9 10 11 12 13
```

## Key Features:

- **Time Complexity**: O(n log n)
- **Space Complexity**: O(1) - sorts in place
- **Stable**: No, but can be made stable with modifications
- **In-place**: Yes, requires only O(1) extra memory

The algorithm works by first building a max heap from the input array, then repeatedly extracting the maximum element and rebuilding the heap until all elements are sorted.

