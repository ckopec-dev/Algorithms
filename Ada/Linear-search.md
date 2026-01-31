# Linear Search Algorithm in Ada

Here's an example of a linear search algorithm implemented in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Linear_Search_Example is
   
   -- Define array type
   type Integer_Array is array (Natural range <>) of Integer;
   
   -- Function to perform linear search
   function Linear_Search(Array_Data : Integer_Array; Target : Integer) return Natural is
      Index : Natural := 0;
   begin
      -- Loop through each element
      for I in Array_Data'Range loop
         if Array_Data(I) = Target then
            return I;  -- Return index if found
         end if;
      end loop;
      
      return Natural'Last;  -- Return special value if not found
   end Linear_Search;
   
   -- Sample array
   Numbers : constant Integer_Array(0..9) := (10, 25, 3, 47, 15, 8, 92, 33, 56, 12);
   
   -- Test values
   Search_Value : Integer;
   Result_Index : Natural;
   
begin
   -- Display the array
   Put_Line("Array elements:");
   for I in Numbers'Range loop
      Put(Integer'Image(Numbers(I)) & " ");
   end loop;
   New_Line;
   
   -- Test case 1: Search for existing element
   Search_Value := 47;
   Result_Index := Linear_Search(Numbers, Search_Value);
   
   if Result_Index /= Natural'Last then
      Put_Line("Element " & Integer'Image(Search_Value) & " found at index " & Natural'Image(Result_Index));
   else
      Put_Line("Element " & Integer'Image(Search_Value) & " not found in array");
   end if;
   
   -- Test case 2: Search for non-existing element
   Search_Value := 100;
   Result_Index := Linear_Search(Numbers, Search_Value);
   
   if Result_Index /= Natural'Last then
      Put_Line("Element " & Integer'Image(Search_Value) & " found at index " & Natural'Image(Result_Index));
   else
      Put_Line("Element " & Integer'Image(Search_Value) & " not found in array");
   end if;
   
end Linear_Search_Example;
```

## Output:
```
Array elements:
 10  25   3  47  15   8  92  33  56  12 
Element  47 found at index  3
Element 100 not found in array
```

## Key Features of this Implementation:

1. **Generic Function**: The `Linear_Search` function takes an array and target value as parameters
2. **Index Tracking**: Uses a loop to check each element sequentially
3. **Return Value**: Returns the index if found, or `Natural'Last` if not found
4. **Type Safety**: Uses Ada's strong typing system with proper array declarations
5. **Clear Logic**: Simple and readable implementation following linear search principles

The time complexity is O(n) and space complexity is O(1), making it suitable for small to medium-sized arrays where simplicity is preferred over efficiency.

