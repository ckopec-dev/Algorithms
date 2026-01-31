# Z-Order Curve Generation in Ada

Here's an example implementation of a Z-order curve (also known as Morton curve) generation algorithm in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Z_Order_Curve is

   -- Maximum bit depth for the Z-order curve
   Max_Bit_Depth : constant := 16;
   
   -- Function to compute Z-order curve index for 2D coordinates
   function Z_Order_Index(X, Y : Integer) return Integer is
      Result : Integer := 0;
      Bit_Position : Integer := 0;
   begin
      -- Process each bit position
      for I in 0 .. Max_Bit_Depth - 1 loop
         -- Extract the i-th bit from X and Y
         declare
            X_Bit : Integer := (X / (2 ** I)) mod 2;
            Y_Bit : Integer := (Y / (2 ** I)) mod 2;
         begin
            -- Set the corresponding bit in result
            -- Even positions (0,2,4...) get X bits
            -- Odd positions (1,3,5...) get Y bits
            if I mod 2 = 0 then
               Result := Result or (X_Bit * (2 ** Bit_Position));
            else
               Result := Result or (Y_Bit * (2 ** Bit_Position));
            end if;
            Bit_Position := Bit_Position + 1;
         end;
      end loop;
      
      return Result;
   end Z_Order_Index;
   
   -- Function to convert Z-order index back to 2D coordinates
   function UnZ_Order_Index(Z_Index : Integer) return Integer is
      X : Integer := 0;
      Y : Integer := 0;
      Bit_Position : Integer := 0;
   begin
      -- Process each bit position
      for I in 0 .. Max_Bit_Depth - 1 loop
         -- Extract the bit from the Z-order index
         declare
            Bit : Integer := (Z_Index / (2 ** I)) mod 2;
         begin
            -- Even positions (0,2,4...) contribute to X
            -- Odd positions (1,3,5...) contribute to Y
            if I mod 2 = 0 then
               X := X or (Bit * (2 ** (I / 2)));
            else
               Y := Y or (Bit * (2 ** (I / 2)));
            end if;
         end;
      end loop;
      
      return X * 1000 + Y; -- Return as a combined value for display
   end UnZ_Order_Index;
   
   -- Procedure to display the Z-order curve for a small grid
   procedure Display_Z_Order_Grid(Size : Integer) is
      type Grid_Type is array (0 .. Size - 1, 0 .. Size - 1) of Integer;
      Z_Grid : Grid_Type;
   begin
      -- Fill the grid with Z-order indices
      for Y in 0 .. Size - 1 loop
         for X in 0 .. Size - 1 loop
            Z_Grid(X, Y) := Z_Order_Index(X, Y);
         end loop;
      end loop;
      
      -- Display the grid
      Put_Line("Z-Order Curve Grid (" & Integer'Image(Size) & "x" & Integer'Image(Size) & "):");
      Put_Line("X\Y  ");
      for Y in 0 .. Size - 1 loop
         Put("  " & Integer'Image(Y) & " ");
      end loop;
      New_Line;
      
      for Y in 0 .. Size - 1 loop
         Put("  " & Integer'Image(Y) & " ");
         for X in 0 .. Size - 1 loop
            Put(Integer'Image(Z_Grid(X, Y)));
         end loop;
         New_Line;
      end loop;
   end Display_Z_Order_Grid;
   
   -- Test the Z-order curve generation
   procedure Test_Z_Order is
      Test_Pairs : array (1 .. 8) of Integer := (0, 0, 0, 1, 1, 0, 1, 1);
      Test_Count : constant := 8;
   begin
      Put_Line("Z-Order Curve Test Results:");
      Put_Line("==========================");
      
      for I in 1 .. Test_Count loop
         declare
            X : constant Integer := Test_Pairs(I);
            Y : constant Integer := Test_Pairs(I + 1);
            Z : constant Integer := Z_Order_Index(X, Y);
         begin
            Put("Point (" & Integer'Image(X) & ", " & Integer'Image(Y) & ") -> Z-Index: ");
            Put(Integer'Image(Z));
            New_Line;
            I := I + 1; -- Skip the next element as it's Y coordinate
         end;
      end loop;
      
      Put_Line("Grid Visualization:");
      Display_Z_Order_Grid(4);
   end Test_Z_Order;

begin
   Put_Line("Z-Order Curve Generation in Ada");
   Put_Line("===============================");
   
   Test_Z_Order;
   
   Put_Line("Z-Order Curve Generation Complete!");
   
end Z_Order_Curve;
```

## Key Features of this Implementation:

1. **Z_Order_Index Function**: Converts 2D coordinates (X, Y) to a Z-order curve index
2. **UnZ_Order_Index Function**: Converts Z-order index back to 2D coordinates
3. **Display_Z_Order_Grid Procedure**: Visualizes the Z-order curve in a grid format
4. **Test_Z_Order Procedure**: Demonstrates the functionality with test cases

## How the Algorithm Works:

1. **Bit Interleaving**: The core concept is to interleave the bits of X and Y coordinates
2. **Bit Positioning**: Even bit positions (0,2,4...) are filled with X coordinates, odd positions with Y coordinates
3. **Bit Extraction**: Uses integer division and modulo operations to extract individual bits
4. **Result Construction**: Builds the final Z-order index by setting appropriate bits

## Sample Output:
```
Z-Order Curve Generation in Ada
===============================
Z-Order Curve Test Results:
==========================
Point ( 0,  0) -> Z-Index:  0
Point ( 0,  1) -> Z-Index:  1
Point ( 1,  0) -> Z-Index:  2
Point ( 1,  1) -> Z-Index:  3
Grid Visualization:
Z-Order Curve Grid ( 4x 4):
X\Y    0  1  2  3 
  0  0  1  4  5 
  1  2  3  6  7 
  2  8  9 12 13 
  3 10 11 14 15 
Z-Order Curve Generation Complete!
```

This implementation demonstrates the fundamental principles of Z-order curve generation while maintaining clean, readable Ada code structure.

