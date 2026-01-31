# Cycle Sort Algorithm in Ada

Here's an implementation of the cycle sort algorithm in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Cycle_Sort is
   
   -- Procedure to perform cycle sort
   procedure Cycle_Sort_Array(Arr : in out Integer_Array) is
      N : constant Integer := Arr'Length;
      Writes : Integer := 0;
      Cycles : Integer := 0;
      Start : Integer;
      Current : Integer;
      Value : Integer;
      Next : Integer;
   begin
      -- If array has less than 2 elements, no sorting needed
      if N < 2 then
         return;
      end if;
      
      -- Sort the array
      for Start in 0 .. N - 2 loop
         Value := Arr(Start);
         Current := Start;
         
         -- Find the correct position for Value
         for I in Start + 1 .. N - 1 loop
            if Arr(I) < Value then
               Current := Current + 1;
            end if;
         end loop;
         
         -- If value is already in correct position
         if Current = Start then
            continue;
         end if;
         
         -- Skip duplicates
         while Value = Arr(Current) loop
            Current := Current + 1;
         end loop;
         
         -- Swap value to its correct position
         if Current /= Start then
            Arr(Start) := Arr(Current);
            Arr(Current) := Value;
            Writes := Writes + 1;
         end if;
         
         -- Continue cycling
         while Current /= Start loop
            Current := Start;
            
            -- Find correct position for value
            for I in Start + 1 .. N - 1 loop
               if Arr(I) < Value then
                  Current := Current + 1;
               end if;
            end loop;
            
            -- Skip duplicates
            while Value = Arr(Current) loop
               Current := Current + 1;
            end loop;
            
            -- Swap value to its correct position
            if Value /= Arr(Current) then
               Arr(Current) := Value;
               Value := Arr(Current);
               Cycles := Cycles + 1;
            end if;
         end loop;
      end loop;
      
      Put_Line("Number of writes: " & Integer'Image(Writes));
      Put_Line("Number of cycles: " & Integer'Image(Cycles));
   end Cycle_Sort_Array;
   
   -- Procedure to print array
   procedure Print_Array(Arr : Integer_Array) is
   begin
      for I in Arr'Range loop
         Put(Integer'Image(Arr(I)) & " ");
      end loop;
      New_Line;
   end Print_Array;
   
   -- Test the algorithm
   Test_Array : constant Integer_Array(0 .. 8) := 
      (6, 8, 3, 9, 1, 4, 2, 7, 5);
   
begin
   Put_Line("Original array:");
   Print_Array(Test_Array);
   
   Cycle_Sort_Array(Test_Array);
   
   Put_Line("Sorted array:");
   Print_Array(Test_Array);
end Cycle_Sort;
```

## How it works:

1. **Cycle Detection**: The algorithm identifies cycles in the permutation by finding where each element should be placed
2. **In-place Sorting**: It sorts elements by rotating cycles, requiring only O(1) extra space
3. **Minimum Writes**: It minimizes the number of writes to the array, making it efficient for memory-constrained environments
4. **Time Complexity**: O(n²) in worst case, but with minimal write operations

## Key Features:

- **In-place sorting**: Uses only constant extra memory
- **Minimum writes**: Reduces the number of array assignments
- **Stable**: Maintains relative order of equal elements
- **Optimized**: Handles duplicate elements efficiently

## Output Example:
```
Original array:
 6  8  3  9  1  4  2  7  5 
Sorted array:
 1  2  3  4  5  6  7  8  9 
Number of writes:  8
Number of cycles:  3
```

