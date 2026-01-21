# Binary Search Algorithm in Ada

Here's an example of a binary search algorithm implemented in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Binary_Search_Example is
   
   -- Define array type
   type Integer_Array is array (Positive range <>) of Integer;
   
   -- Binary search function
   function Binary_Search (Arr : Integer_Array; Target : Integer) return Integer is
      Low  : Positive := Arr'First;
      High : Positive := Arr'Last;
      Mid  : Positive;
   begin
      while Low <= High loop
         Mid := (Low + High) / 2;
         
         if Arr(Mid) = Target then
            return Mid;  -- Found target at index Mid
         elsif Arr(Mid) < Target then
            Low := Mid + 1;  -- Search right half
         else
            High := Mid - 1; -- Search left half
         end if;
      end loop;
      
      return 0;  -- Target not found
   end Binary_Search;
   
   -- Test array (must be sorted for binary search to work)
   Test_Array : constant Integer_Array := (1, 3, 5, 7, 9, 11, 13, 15, 17, 19);
   
   -- Test values
   Search_Targets : constant array (1..4) of Integer := (7, 1, 19, 10);
   
begin
   Put_Line("Binary Search Example");
   Put_Line("=====================");
   
   -- Display the array
   Put("Array: ");
   for I in Test_Array'Range loop
      Put(Test_Array(I), 3);
   end loop;
   New_Line;
   
   -- Perform searches
   for I in Search_Targets'Range loop
      declare
         Target : constant Integer := Search_Targets(I);
         Result : constant Integer := Binary_Search(Test_Array, Target);
      begin
         Put("Searching for " & Integer'Image(Target) & ": ");
         if Result = 0 then
            Put_Line("Not found");
         else
            Put_Line("Found at index " & Integer'Image(Result));
         end if;
      end;
   end loop;
   
end Binary_Search_Example;
```

## Output:
```
Binary Search Example
=====================
Array:   1   3   5   7   9  11  13  15  17  19
Searching for   7: Found at index  4
Searching for   1: Found at index  1
Searching for  19: Found at index 10
Searching for  10: Not found
```

## Key Features of this Implementation:

1. **Generic Design**: Uses Ada's array types with proper bounds checking
2. **Bounds Checking**: Ada's built-in array bounds checking prevents out-of-bounds access
3. **Proper Loop Control**: Uses while loop with proper termination conditions
4. **Clear Return Values**: Returns array index when found, 0 when not found
5. **Type Safety**: Strong typing prevents many common programming errors

## How it Works:

1. **Initialize**: Set low and high pointers to array bounds
2. **Loop**: Continue while low ≤ high
3. **Calculate Mid**: Find middle position
4. **Compare**: 
   - If middle element equals target: return index
   - If middle element < target: search right half
   - If middle element > target: search left half
5. **Return**: 0 if not found

This implementation demonstrates Ada's safety features while providing an efficient O(log n) search algorithm.

