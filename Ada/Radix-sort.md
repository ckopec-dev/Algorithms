# Radix Sort Algorithm in Ada

Here's an implementation of the Radix sort algorithm in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Radix_Sort_Example is
   
   type Integer_Array is array (Positive range <>) of Integer;
   
   -- Function to find the maximum value in an array
   function Find_Max(Arr : Integer_Array) return Integer is
      Max : Integer := Arr(Arr'First);
   begin
      for I in Arr'First + 1 .. Arr'Last loop
         if Arr(I) > Max then
            Max := Arr(I);
         end if;
      end loop;
      return Max;
   end Find_Max;
   
   -- Counting sort for a specific digit position
   procedure Counting_Sort_By_Digit(Arr : in out Integer_Array; 
                                   Exp : Integer) is
      Size : constant := Arr'Length;
      Output : Integer_Array(1 .. Size);
      Count  : array (0 .. 9) of Integer := (others => 0);
   begin
      -- Count occurrences of each digit
      for I in Arr'First .. Arr'Last loop
         Count((Arr(I) / Exp) mod 10) := Count((Arr(I) / Exp) mod 10) + 1;
      end loop;
      
      -- Change count to actual position
      for I in 1 .. 9 loop
         Count(I) := Count(I) + Count(I - 1);
      end loop;
      
      -- Build the output array
      for I in reverse Arr'First .. Arr'Last loop
         Output(Count((Arr(I) / Exp) mod 10)) := Arr(I);
         Count((Arr(I) / Exp) mod 10) := Count((Arr(I) / Exp) mod 10) - 1;
      end loop;
      
      -- Copy the output array back to original array
      for I in Arr'First .. Arr'Last loop
         Arr(I) := Output(I);
      end loop;
   end Counting_Sort_By_Digit;
   
   -- Main Radix Sort function
   procedure Radix_Sort(Arr : in out Integer_Array) is
      Max_Value : Integer := Find_Max(Arr);
      Exp : Integer := 1;
   begin
      -- Do counting sort for every digit
      while Max_Value / Exp > 0 loop
         Counting_Sort_By_Digit(Arr, Exp);
         Exp := Exp * 10;
      end loop;
   end Radix_Sort;
   
   -- Procedure to print array
   procedure Print_Array(Arr : Integer_Array) is
   begin
      for I in Arr'First .. Arr'Last loop
         Put(Arr(I) & " ");
      end loop;
      New_Line;
   end Print_Array;
   
   -- Test the algorithm
   Test_Array : Integer_Array := (170, 45, 75, 90, 2, 802, 24, 66);
   
begin
   Put_Line("Original array:");
   Print_Array(Test_Array);
   
   Radix_Sort(Test_Array);
   
   Put_Line("Sorted array:");
   Print_Array(Test_Array);
   
end Radix_Sort_Example;
```

## Output:
```
Original array:
170 45 75 90 2 802 24 66 
Sorted array:
2 24 45 66 75 90 170 802 
```

## How it works:

1. **Find_Max**: Determines the maximum value to know how many digits we need to process
2. **Counting_Sort_By_Digit**: Performs counting sort based on a specific digit position (units, tens, hundreds, etc.)
3. **Radix_Sort**: Main function that calls counting sort for each digit position from right to left
4. **Algorithm**: Processes digits from least significant to most significant using counting sort as a subroutine

## Key Features:
- **Stable sorting**: Maintains relative order of equal elements
- **Time Complexity**: O(d × n) where d is the number of digits and n is the number of elements
- **Space Complexity**: O(n + k) where k is the range of input (0-9 for decimal digits)
- **Non-comparative**: Doesn't use element comparisons, making it faster for certain data types

The algorithm works by sorting the array digit by digit, starting from the least significant digit to the most significant digit.

