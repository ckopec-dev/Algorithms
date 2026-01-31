# Insertion Sort in Ada

Here's an implementation of the insertion sort algorithm in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Insertion_Sort is
   
   type Integer_Array is array (Positive range <>) of Integer;
   
   -- Insertion Sort procedure
   procedure Insertion_Sort_Array(Arr : in out Integer_Array) is
      I, J, Key : Integer;
   begin
      for I in Arr'First + 1 .. Arr'Last loop
         Key := Arr(I);
         J := I - 1;
         
         -- Move elements greater than key one position ahead
         while J >= Arr'First and then Arr(J) > Key loop
            Arr(J + 1) := Arr(J);
            J := J - 1;
         end loop;
         
         Arr(J + 1) := Key;
      end loop;
   end Insertion_Sort_Array;
   
   -- Procedure to print array
   procedure Print_Array(Arr : Integer_Array) is
   begin
      for I in Arr'Range loop
         Put(Arr(I) & " ");
      end loop;
      New_Line;
   end Print_Array;
   
   -- Example usage
   Test_Array : constant Integer_Array := (64, 34, 25, 12, 22, 11, 90);
   
begin
   Put_Line("Original array:");
   Print_Array(Test_Array);
   
   -- Sort the array
   Insertion_Sort_Array(Test_Array);
   
   Put_Line("Sorted array:");
   Print_Array(Test_Array);
end Insertion_Sort;
```

## Output:
```
Original array:
64 34 25 12 22 11 90 
Sorted array:
11 12 22 25 34 64 90 
```

## How it works:

1. **Outer loop**: Iterates through each element starting from the second element (index 2)
2. **Key selection**: Stores the current element to be inserted
3. **Inner loop**: Shifts elements greater than the key one position to the right
4. **Insertion**: Places the key in its correct position

## Key Features:

- **In-place sorting**: Only requires O(1) extra memory
- **Stable**: Maintains relative order of equal elements
- **Adaptive**: Performs well on nearly sorted arrays
- **Online algorithm**: Can sort elements as they are received

The time complexity is O(n²) in the worst case and O(n) in the best case (already sorted array).

