# Selection Sort Algorithm in Ada

Here's an example of the selection sort algorithm implemented in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Selection_Sort is
   
   -- Define array type
   type Integer_Array is array (Positive range <>) of Integer;
   
   -- Selection sort procedure
   procedure Sort_Array(Arr : in out Integer_Array) is
      Min_Index : Integer;
      Temp      : Integer;
   begin
      for I in Arr'First .. Arr'Last - 1 loop
         Min_Index := I;
         
         -- Find the minimum element in remaining unsorted array
         for J in I + 1 .. Arr'Last loop
            if Arr(J) < Arr(Min_Index) then
               Min_Index := J;
            end if;
         end loop;
         
         -- Swap the found minimum element with the first element
         if Min_Index /= I then
            Temp := Arr(I);
            Arr(I) := Arr(Min_Index);
            Arr(Min_Index) := Temp;
         end if;
      end loop;
   end Sort_Array;
   
   -- Procedure to print array
   procedure Print_Array(Arr : Integer_Array) is
   begin
      for I in Arr'Range loop
         Put(Arr(I) & " ");
      end loop;
      New_Line;
   end Print_Array;
   
   -- Test the algorithm
   Test_Array : Integer_Array(1..6) := (64, 34, 25, 12, 22, 11);
   
begin
   Put_Line("Original array:");
   Print_Array(Test_Array);
   
   Sort_Array(Test_Array);
   
   Put_Line("Sorted array:");
   Print_Array(Test_Array);
end Selection_Sort;
```

## Output:
```
Original array:
64 34 25 12 22 11 
Sorted array:
11 12 22 25 34 64 
```

## How it works:

1. **Find Minimum**: For each position in the array, find the smallest element in the remaining unsorted portion
2. **Swap**: Swap the found minimum element with the element at the current position
3. **Repeat**: Continue this process until the entire array is sorted

## Key Features:

- **Time Complexity**: O(n²) in all cases
- **Space Complexity**: O(1) - sorts in place
- **Stability**: Not stable (doesn't preserve relative order of equal elements)
- **In-place**: Requires only O(1) additional memory space

The algorithm works by repeatedly finding the minimum element from the unsorted part and putting it at the beginning of the sorted part.

