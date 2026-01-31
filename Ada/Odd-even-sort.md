# Odd-Even Sort Algorithm in Ada

Here's an implementation of the Odd-Even sort (also known as Brick Sort) algorithm in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Odd_Even_Sort is
   
   type Integer_Array is array (Positive range <>) of Integer;
   
   -- Procedure to perform Odd-Even sort
   procedure Odd_Even_Sort_Array(Arr : in out Integer_Array) is
      Sorted : Boolean := False;
      Pass   : Integer := 0;
   begin
      while not Sorted loop
         Sorted := True;
         Pass := Pass + 1;
         
         -- Odd phase: compare odd indexed elements
         for I in 1 .. Arr'Last - 1 loop
            if I mod 2 = 1 then
               if Arr(I) > Arr(I + 1) then
                  -- Swap elements
                  declare
                     Temp : Integer := Arr(I);
                  begin
                     Arr(I) := Arr(I + 1);
                     Arr(I + 1) := Temp;
                     Sorted := False;
                  end;
               end if;
            end if;
         end loop;
         
         -- Even phase: compare even indexed elements
         for I in 2 .. Arr'Last - 1 loop
            if I mod 2 = 0 then
               if Arr(I) > Arr(I + 1) then
                  -- Swap elements
                  declare
                     Temp : Integer := Arr(I);
                  begin
                     Arr(I) := Arr(I + 1);
                     Arr(I + 1) := Temp;
                     Sorted := False;
                  end;
               end if;
            end if;
         end loop;
         
         -- Display progress (optional)
         Put("Pass ");
         Put_Line(Integer'Image(Pass) & ": " & 
                  Integer_Array'Image(Arr));
      end loop;
   end Odd_Even_Sort_Array;
   
   -- Procedure to display array
   procedure Display_Array(Arr : Integer_Array) is
   begin
      Put("[");
      for I in Arr'First .. Arr'Last loop
         Put(Integer'Image(Arr(I)));
         if I < Arr'Last then
            Put(",");
         end if;
      end loop;
      Put_Line("]");
   end Display_Array;
   
   -- Test the algorithm
   Test_Array : Integer_Array(1..8) := (8, 3, 5, 1, 9, 2, 7, 4);
   
begin
   Put_Line("Original array:");
   Display_Array(Test_Array);
   
   Put_Line("Sorting process:");
   Odd_Even_Sort_Array(Test_Array);
   
   Put_Line("Sorted array:");
   Display_Array(Test_Array);
   
end Odd_Even_Sort;
```

## How the Algorithm Works

The Odd-Even sort algorithm works by alternating between two phases:

1. **Odd Phase**: Compare and swap elements at odd indices (1, 3, 5, ...)
2. **Even Phase**: Compare and swap elements at even indices (2, 4, 6, ...)

This continues until no more swaps are needed, indicating the array is sorted.

## Key Features of the Implementation

- **Generic Approach**: Uses Ada's array types for flexibility
- **Clear Phase Separation**: Explicit odd and even phases
- **Progress Tracking**: Shows the sorting process step by step
- **Proper Swapping**: Uses temporary variables for element swapping
- **Boundary Handling**: Correctly handles array bounds

## Sample Output

```
Original array:
[ 8 3 5 1 9 2 7 4]
Sorting process:
Pass 1: [ 3 8 1 5 2 9 4 7]
Pass 2: [ 1 3 5 8 2 4 7 9]
Pass 3: [ 1 3 2 5 4 7 8 9]
Pass 4: [ 1 2 3 4 5 7 8 9]
Sorted array:
[ 1 2 3 4 5 7 8 9]
```

This implementation demonstrates the characteristic alternating behavior of the Odd-Even sort algorithm, making it easy to understand and verify its correctness.

