# Jump Search Algorithm in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Jump_Search is
   
   -- Function to perform jump search
   function Jump_Search_Array(Arr : in array of Integer; 
                             Target : in Integer) return Integer is
      Length : constant Integer := Arr'Length;
      Step : constant Integer := Integer(Sqrt(Float(Length)));
      Prev : Integer := 0;
      I : Integer;
   begin
      -- Jump through the array
      while Arr(Math.Min(Step, Length) - 1) < Target loop
         Prev := Step;
         Step := Step + Integer(Sqrt(Float(Length)));
         if Prev >= Length then
            return -1;  -- Element not found
         end if;
      end loop;
      
      -- Linear search in the block
      I := Prev;
      while I < Math.Min(Step, Length) loop
         if Arr(I) = Target then
            return I;
         end if;
         I := I + 1;
      end loop;
      
      return -1;  -- Element not found
   end Jump_Search_Array;
   
   -- Helper function to find minimum of two integers
   function Min(A, B : Integer) return Integer is
   begin
      if A < B then
         return A;
      else
         return B;
      end if;
   end Min;
   
   -- Array to search in
   Array_To_Search : array(0..9) of Integer := (1, 3, 5, 7, 9, 11, 13, 15, 17, 19);
   
   -- Test values
   Test_Value : Integer;
   Result : Integer;
   
begin
   Put_Line("Jump Search Algorithm Example");
   Put_Line("============================");
   
   -- Display the array
   Put("Array: ");
   for I in Array_To_Search'Range loop
      Put(Array_To_Search(I), 3);
   end loop;
   New_Line;
   
   -- Test cases
   Test_Value := 7;
   Result := Jump_Search_Array(Array_To_Search, Test_Value);
   if Result >= 0 then
      Put_Line("Element " & Integer'Image(Test_Value) & " found at index " & Integer'Image(Result));
   else
      Put_Line("Element " & Integer'Image(Test_Value) & " not found");
   end if;
   
   Test_Value := 15;
   Result := Jump_Search_Array(Array_To_Search, Test_Value);
   if Result >= 0 then
      Put_Line("Element " & Integer'Image(Test_Value) & " found at index " & Integer'Image(Result));
   else
      Put_Line("Element " & Integer'Image(Test_Value) & " not found");
   end if;
   
   Test_Value := 4;
   Result := Jump_Search_Array(Array_To_Search, Test_Value);
   if Result >= 0 then
      Put_Line("Element " & Integer'Image(Test_Value) & " found at index " & Integer'Image(Result));
   else
      Put_Line("Element " & Integer'Image(Test_Value) & " not found");
   end if;
   
end Jump_Search;
```

## How it works:

1. **Calculate Jump Size**: The jump size is set to √n (where n is the array length)
2. **Jump Through Blocks**: Move through the array in jumps of the calculated size
3. **Find Block**: When we find a block where the target element could be, we perform linear search within that block
4. **Return Result**: Return the index if found, or -1 if not found

## Time Complexity:
- **Best Case**: O(1)
- **Average Case**: O(√n)
- **Worst Case**: O(√n)

## Space Complexity:
- O(1) - constant space

## Output Example:
```
Jump Search Algorithm Example
============================
Array:   1   3   5   7   9  11  13  15  17  19
Element   7 found at index   3
Element  15 found at index   7
Element   4 not found
```

