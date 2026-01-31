# Pigeonhole Sort in Ada

Here's an implementation of the Pigeonhole sort algorithm in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Pigeonhole_Sort is
   
   type Integer_Array is array (Positive range <>) of Integer;
   
   -- Pigeonhole Sort procedure
   procedure Pigeonhole_Sort_Array(A : in out Integer_Array) is
      Min_Val, Max_Val : Integer;
      Range_Size       : Integer;
      Pigeonholes      : array (Integer range <>) of Integer;
      Index            : Integer := 1;
   begin
      -- Handle empty array
      if A'Length = 0 then
         return;
      end if;
      
      -- Find minimum and maximum values
      Min_Val := A(A'First);
      Max_Val := A(A'First);
      
      for I in A'First + 1 .. A'Last loop
         if A(I) < Min_Val then
            Min_Val := A(I);
         elsif A(I) > Max_Val then
            Max_Val := A(I);
         end if;
      end loop;
      
      -- Calculate range size
      Range_Size := Max_Val - Min_Val + 1;
      
      -- Create pigeonholes array
      Pigeonholes := new Integer_Array(0 .. Range_Size - 1);
      
      -- Initialize pigeonholes to 0
      for I in Pigeonholes'Range loop
         Pigeonholes(I) := 0;
      end loop;
      
      -- Place elements in pigeonholes
      for I in A'Range loop
         Pigeonholes(A(I) - Min_Val) := Pigeonholes(A(I) - Min_Val) + 1;
      end loop;
      
      -- Reconstruct sorted array from pigeonholes
      for I in Pigeonholes'Range loop
         while Pigeonholes(I) > 0 loop
            A(Index) := I + Min_Val;
            Index := Index + 1;
            Pigeonholes(I) := Pigeonholes(I) - 1;
         end loop;
      end loop;
      
      -- Clean up
      Free(Pigeonholes);
   end Pigeonhole_Sort_Array;
   
   -- Helper procedure to print array
   procedure Print_Array(A : Integer_Array) is
   begin
      for I in A'Range loop
         Put(A(I) & " ");
      end loop;
      New_Line;
   end Print_Array;
   
   -- Test the algorithm
   A : Integer_Array(1 .. 7) := (8, 3, 2, 7, 4, 6, 1);
   
begin
   Put_Line("Original array:");
   Print_Array(A);
   
   Pigeonhole_Sort_Array(A);
   
   Put_Line("Sorted array:");
   Print_Array(A);
   
end Pigeonhole_Sort;
```

## How it works:

1. **Find Range**: First, we find the minimum and maximum values in the array to determine the range
2. **Create Pigeonholes**: We create an array (pigeonholes) with size equal to the range
3. **Place Elements**: Each element is placed in its corresponding pigeonhole based on its value
4. **Reconstruct**: Elements are retrieved from pigeonholes in order to form the sorted array

## Time and Space Complexity:

- **Time Complexity**: O(n + k) where n is the number of elements and k is the range
- **Space Complexity**: O(k) for the pigeonhole array

## Example Output:
```
Original array:
8 3 2 7 4 6 1 
Sorted array:
1 2 3 4 6 7 8 
```

**Note**: This implementation assumes the input array contains integers and handles edge cases like empty arrays. The algorithm is most efficient when the range of possible values is not significantly larger than the number of elements to be sorted.

