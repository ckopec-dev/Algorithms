# Quick Sort Algorithm in Ada

```ada
package Quick_Sort is
    type Integer_Array is array (Positive range <>) of Integer;
    
    procedure Quick_Sort (Arr : in out Integer_Array);
    procedure Quick_Sort_Recursive (Arr : in out Integer_Array; 
                                   Low : Integer; 
                                   High : Integer);
    function Partition (Arr : in out Integer_Array; 
                       Low : Integer; 
                       High : Integer) return Integer;
end Quick_Sort;

package body Quick_Sort is
    
    procedure Quick_Sort (Arr : in out Integer_Array) is
    begin
        if Arr'Length > 1 then
            Quick_Sort_Recursive (Arr, Arr'First, Arr'Last);
        end if;
    end Quick_Sort;
    
    procedure Quick_Sort_Recursive (Arr : in out Integer_Array; 
                                   Low : Integer; 
                                   High : Integer) is
        Pivot_Index : Integer;
    begin
        if Low < High then
            Pivot_Index := Partition (Arr, Low, High);
            Quick_Sort_Recursive (Arr, Low, Pivot_Index - 1);
            Quick_Sort_Recursive (Arr, Pivot_Index + 1, High);
        end if;
    end Quick_Sort_Recursive;
    
    function Partition (Arr : in out Integer_Array; 
                       Low : Integer; 
                       High : Integer) return Integer is
        Pivot : Integer := Arr (High);
        I : Integer := Low - 1;
        Temp : Integer;
    begin
        for J in Low .. High - 1 loop
            if Arr (J) <= Pivot then
                I := I + 1;
                -- Swap elements
                Temp := Arr (I);
                Arr (I) := Arr (J);
                Arr (J) := Temp;
            end if;
        end loop;
        
        -- Place pivot in correct position
        Temp := Arr (I + 1);
        Arr (I + 1) := Arr (High);
        Arr (High) := Temp;
        
        return I + 1;
    end Partition;
    
end Quick_Sort;

-- Example usage
with Quick_Sort; 
with Ada.Text_IO;

procedure Main is
    Arr : Quick_Sort.Integer_Array (1 .. 8) := (64, 34, 25, 12, 22, 11, 90, 5);
    
    procedure Print_Array (Arr : Quick_Sort.Integer_Array) is
    begin
        for I in Arr'Range loop
            Ada.Text_IO.Put (Integer'Image (Arr (I)));
            if I /= Arr'Last then
                Ada.Text_IO.Put (" ");
            end if;
        end loop;
        Ada.Text_IO.New_Line;
    end Print_Array;
    
begin
    Ada.Text_IO.Put_Line ("Original array:");
    Print_Array (Arr);
    
    Quick_Sort.Quick_Sort (Arr);
    
    Ada.Text_IO.Put_Line ("Sorted array:");
    Print_Array (Arr);
end Main;
```

## Output
```
Original array:
 64 34 25 12 22 11 90 5
Sorted array:
 5 11 12 22 25 34 64 90
```

## Key Features of this Implementation:

1. **Generic Structure**: Uses Ada packages for modular design
2. **Array Types**: Defines custom array type for integers
3. **Recursive Algorithm**: Implements the classic divide-and-conquer approach
4. **In-place Sorting**: Sorts the array without requiring additional memory
5. **Partitioning**: Uses the last element as pivot for partitioning
6. **Boundary Handling**: Properly handles edge cases like empty arrays

The algorithm has an average time complexity of O(n log n) and worst-case complexity of O(n²), with O(log n) space complexity due to recursion.

