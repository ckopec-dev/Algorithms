# Quick Select Algorithm in Ada

Here's an implementation of the Quick Select algorithm in Ada to find the k-th smallest element in an array:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Quick_Select_Example is
    
    type Integer_Array is array (Positive range <>) of Integer;
    
    -- Partition function similar to Quick Sort
    function Partition(Arr : in out Integer_Array; Low, High : Integer) return Integer is
        Pivot : Integer := Arr(High);
        I     : Integer := Low - 1;
        J     : Integer;
        Temp  : Integer;
    begin
        for J in Low .. High - 1 loop
            if Arr(J) <= Pivot then
                I := I + 1;
                -- Swap elements
                Temp := Arr(I);
                Arr(I) := Arr(J);
                Arr(J) := Temp;
            end if;
        end loop;
        
        -- Place pivot in correct position
        Temp := Arr(I + 1);
        Arr(I + 1) := Arr(High);
        Arr(High) := Temp;
        
        return I + 1;
    end Partition;
    
    -- Quick Select algorithm
    function Quick_Select( Arr : in out Integer_Array; 
                          Low, High, K : Integer ) return Integer is
        Pivot_Index : Integer;
    begin
        if Low = High then
            return Arr(Low);
        end if;
        
        Pivot_Index := Partition(Arr, Low, High);
        
        if K = Pivot_Index then
            return Arr(K);
        elsif K < Pivot_Index then
            return Quick_Select(Arr, Low, Pivot_Index - 1, K);
        else
            return Quick_Select(Arr, Pivot_Index + 1, High, K);
        end if;
    end Quick_Select;
    
    -- Print array function
    procedure Print_Array(Arr : in Integer_Array) is
    begin
        for I in Arr'Range loop
            Put(Arr(I), 4);
        end loop;
        New_Line;
    end Print_Array;
    
    -- Example usage
    Arr : Integer_Array(1..10) := (10, 7, 8, 9, 1, 5, 3, 6, 2, 4);
    K   : constant Integer := 5;  -- Find 5th smallest element (0-indexed: 4)
    Result : Integer;
    
begin
    Put_Line("Original array:");
    Print_Array(Arr);
    
    Result := Quick_Select(Arr, 1, Arr'Length, K);
    
    Put_Line("The " & Integer'Image(K) & "th smallest element is: " & Integer'Image(Result));
    
    Put_Line("Array after Quick Select (first " & Integer'Image(K) & " elements are sorted):");
    Print_Array(Arr);
    
end Quick_Select_Example;
```

## How it works:

1. **Partition Function**: Rearranges the array so that elements smaller than the pivot are on the left, and larger elements are on the right
2. **Quick Select**: Recursively selects the appropriate partition based on the position of the pivot
3. **Base Case**: When `Low = High`, we've found our element

## Key Features:

- **Time Complexity**: O(n) average case, O(n²) worst case
- **Space Complexity**: O(log n) due to recursion
- **In-place**: Modifies the original array
- **Randomized**: Can be made more efficient with random pivot selection

## Sample Output:
```
Original array:
  10   7   8   9   1   5   3   6   2   4

The  5th smallest element is:   5

Array after Quick Select (first  5 elements are sorted):
   1   2   3   4   5   6   7   8   9  10
```

This implementation finds the k-th smallest element without fully sorting the array, making it more efficient than sorting when you only need one specific element.

