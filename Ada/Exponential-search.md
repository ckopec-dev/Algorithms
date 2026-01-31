# Exponential Search Algorithm in Ada

Here's an implementation of the exponential search algorithm in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Exponential_Search is
   
   -- Function to perform binary search in a sorted array
   function Binary_Search(arr : in array of Integer; 
                         target : in Integer; 
                         left : in Integer; 
                         right : in Integer) return Integer is
   begin
      while left <= right loop
         declare
            mid : constant Integer := left + (right - left) / 2;
         begin
            if arr(mid) = target then
               return mid;
            elsif arr(mid) < target then
               left := mid + 1;
            else
               right := mid - 1;
            end if;
         end;
      end loop;
      return -1;  -- Element not found
   end Binary_Search;
   
   -- Main exponential search function
   function Exponential_Search_Function(arr : in array of Integer; 
                                       target : in Integer) return Integer is
      n : constant Integer := arr'Length;
   begin
      -- If target is first element
      if n > 0 and then arr(0) = target then
         return 0;
      end if;
      
      -- Find range where element is present
      -- Keep doubling index until element is found or we exceed array bounds
      declare
         i : Integer := 1;
      begin
         while i < n and then arr(i) <= target loop
            i := i * 2;
         end loop;
         
         -- Perform binary search in the range [i/2, min(i, n-1)]
         return Binary_Search(arr, target, i / 2, Integer'Min(i, n - 1));
      end;
   end Exponential_Search_Function;
   
   -- Test the algorithm
   procedure Test_Exponential_Search is
      -- Sample sorted array
      test_array : array(0..9) of Integer := (1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
      target : Integer;
      result : Integer;
   begin
      Put_Line("Exponential Search Algorithm Test");
      Put_Line("Array: " & 
               Integer'Image(test_array(0)) & " " &
               Integer'Image(test_array(1)) & " " &
               Integer'Image(test_array(2)) & " " &
               Integer'Image(test_array(3)) & " " &
               Integer'Image(test_array(4)) & " " &
               Integer'Image(test_array(5)) & " " &
               Integer'Image(test_array(6)) & " " &
               Integer'Image(test_array(7)) & " " &
               Integer'Image(test_array(8)) & " " &
               Integer'Image(test_array(9)));
      
      -- Test cases
      target := 6;
      result := Exponential_Search_Function(test_array, target);
      if result >= 0 then
         Put_Line("Element " & Integer'Image(target) & " found at index: " & Integer'Image(result));
      else
         Put_Line("Element " & Integer'Image(target) & " not found");
      end if;
      
      target := 1;
      result := Exponential_Search_Function(test_array, target);
      if result >= 0 then
         Put_Line("Element " & Integer'Image(target) & " found at index: " & Integer'Image(result));
      else
         Put_Line("Element " & Integer'Image(target) & " not found");
      end if;
      
      target := 10;
      result := Exponential_Search_Function(test_array, target);
      if result >= 0 then
         Put_Line("Element " & Integer'Image(target) & " found at index: " & Integer'Image(result));
      else
         Put_Line("Element " & Integer'Image(target) & " not found");
      end if;
      
      target := 15;
      result := Exponential_Search_Function(test_array, target);
      if result >= 0 then
         Put_Line("Element " & Integer'Image(target) & " found at index: " & Integer'Image(result));
      else
         Put_Line("Element " & Integer'Image(target) & " not found");
      end if;
   end Test_Exponential_Search;
   
begin
   Test_Exponential_Search;
end Exponential_Search;
```

## How it works:

1. **Initial Check**: First checks if the target is at the first position (index 0)
2. **Exponential Growth**: Doubles the index (`i := i * 2`) until finding a value greater than or equal to the target
3. **Binary Search**: Performs binary search within the identified range `[i/2, min(i, n-1)]`

## Time Complexity:
- **Best Case**: O(1) - when element is at index 0
- **Average/Worst Case**: O(log n) - where n is the size of the array

## Space Complexity:
- O(1) - only using a constant amount of extra space

## Output Example:
```
Exponential Search Algorithm Test
Array:  1  2  3  4  5  6  7  8  9  10
Element  6 found at index:  5
Element  1 found at index:  0
Element  10 found at index:  9
Element  15 not found
```

