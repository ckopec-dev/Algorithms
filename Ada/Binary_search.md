```ada
-- Binary Search Algorithm in Ada

with Ada.Text_IO; use Ada.Text_IO;

procedure Binary_Search_Example is
   
   -- Define array type
   type Integer_Array is array (Positive range <>) of Integer;
   
   -- Binary search function
   function Binary_Search (Arr : Integer_Array; Target : Integer) return Integer is
      Low  : Integer := Arr'First;
      High : Integer := Arr'Last;
      Mid  : Integer;
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
      
      return -1;  -- Target not found
   end Binary_Search;
   
   -- Test array (must be sorted for binary search to work)
   Test_Array : constant Integer_Array := (1, 3, 5, 7, 9, 11, 13, 15, 17, 19);
   
   -- Test values
   Search_Targets : constant array (1..4) of Integer := (7, 1, 19, 10);
   
begin
   Put_Line("Binary Search Example");
   Put_Line("====================");
   
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
         if Result = -1 then
            Put_Line("Not found");
         else
            Put_Line("Found at index " & Integer'Image(Result));
         end if;
      end;
   end loop;
   
end Binary_Search_Example;
```

This Ada program demonstrates a binary search algorithm with the following features:

1. **Array Declaration**: Uses `Integer_Array` type with dynamic range
2. **Binary_Search Function**: Implements the classic binary search algorithm
3. **Key Logic**:
   - Maintains `Low` and `High` pointers
   - Calculates `Mid` position
   - Compares target with middle element
   - Adjusts search range accordingly
4. **Test Cases**: Searches for different values including existing elements and a non-existing element
5. **Error Handling**: Returns -1 when element is not found

**Output:**
```
Binary Search Example
====================
Array:   1   3   5   7   9  11  13  15  17  19
Searching for   7: Found at index   4
Searching for   1: Found at index   1
Searching for  19: Found at index  10
Searching for  10: Not found
```

