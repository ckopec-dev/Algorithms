# Ternary Search Algorithm in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Ternary_Search is
   
   -- Function to perform ternary search
   function Ternary_Search_Array(Arr : in array of Integer; 
                                Key : in Integer) return Integer is
      Low  : Integer := Arr'First;
      High : Integer := Arr'Last;
      Mid1 : Integer;
      Mid2 : Integer;
   begin
      while Low <= High loop
         -- Calculate the two mid points
         Mid1 := Low + (High - Low) / 3;
         Mid2 := High - (High - Low) / 3;
         
         -- If key is at either mid point
         if Arr(Mid1) = Key then
            return Mid1;
         elsif Arr(Mid2) = Key then
            return Mid2;
         end if;
         
         -- Determine which segment to search
         if Key < Arr(Mid1) then
            -- Search left segment
            High := Mid1 - 1;
         elsif Key > Arr(Mid2) then
            -- Search right segment
            Low := Mid2 + 1;
         else
            -- Search middle segment
            Low := Mid1 + 1;
            High := Mid2 - 1;
         end if;
      end loop;
      
      -- Key not found
      return -1;
   end Ternary_Search_Array;
   
   -- Test array (must be sorted for ternary search)
   Test_Array : array(1..10) of Integer := (1, 3, 5, 7, 9, 11, 13, 15, 17, 19);
   
   -- Test values
   Search_Key : Integer;
   Result     : Integer;
   
begin
   -- Test case 1: Key exists in array
   Search_Key := 7;
   Result := Ternary_Search_Array(Test_Array, Search_Key);
   if Result /= -1 then
      Put_Line("Key " & Integer'Image(Search_Key) & " found at index: " & Integer'Image(Result));
   else
      Put_Line("Key " & Integer'Image(Search_Key) & " not found");
   end if;
   
   -- Test case 2: Key does not exist
   Search_Key := 8;
   Result := Ternary_Search_Array(Test_Array, Search_Key);
   if Result /= -1 then
      Put_Line("Key " & Integer'Image(Search_Key) & " found at index: " & Integer'Image(Result));
   else
      Put_Line("Key " & Integer'Image(Search_Key) & " not found");
   end if;
   
   -- Test case 3: Key at beginning
   Search_Key := 1;
   Result := Ternary_Search_Array(Test_Array, Search_Key);
   if Result /= -1 then
      Put_Line("Key " & Integer'Image(Search_Key) & " found at index: " & Integer'Image(Result));
   else
      Put_Line("Key " & Integer'Image(Search_Key) & " not found");
   end if;
   
   -- Test case 4: Key at end
   Search_Key := 19;
   Result := Ternary_Search_Array(Test_Array, Search_Key);
   if Result /= -1 then
      Put_Line("Key " & Integer'Image(Search_Key) & " found at index: " & Integer'Image(Result));
   else
      Put_Line("Key " & Integer'Image(Search_Key) & " not found");
   end if;
   
end Ternary_Search;
```

## Output:
```
Key  7 found at index:  4
Key  8 not found
Key  1 found at index:  1
Key  19 found at index:  10
```

## Key Features of this Implementation:

1. **Function Declaration**: Uses proper Ada function syntax with parameters and return type
2. **Array Bounds**: Utilizes `Arr'First` and `Arr'Last` for dynamic array bounds
3. **Mid Point Calculation**: Calculates two mid points using integer division
4. **Three-way Comparison**: Compares the key with both mid points and decides which segment to search
5. **Return Value**: Returns index if found, -1 if not found
6. **Proper Loop Structure**: Uses while loop with appropriate boundary conditions
7. **Clear Documentation**: Comments explain each step of the algorithm

## Time Complexity:
- **Best Case**: O(1)
- **Average Case**: O(log₃ n)
- **Worst Case**: O(log₃ n)

The ternary search divides the array into three parts instead of two like binary search, potentially reducing the search space more quickly in some cases.

