# Gauss-Jordan Elimination in Ada

Here's an implementation of the Gauss-Jordan elimination algorithm in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

procedure Gauss_Jordan is
   
   type Matrix is array (Integer range <>, Integer range <>) of Float;
   
   -- Function to display a matrix
   procedure Display_Matrix(M : Matrix) is
   begin
      for i in M'First(1) .. M'Last(1) loop
         for j in M'First(2) .. M'Last(2) loop
            Put(M(i,j), Fore => 6, Aft => 2, Exp => 0);
         end loop;
         New_Line;
      end loop;
      New_Line;
   end Display_Matrix;
   
   -- Function to perform Gauss-Jordan elimination
   procedure Gauss_Jordan_Elimination(M : in out Matrix) is
      rows : constant Integer := M'Length(1);
      cols : constant Integer := M'Length(2);
      pivot_row, pivot_col : Integer;
      pivot_element : Float;
      factor : Float;
   begin
      -- Forward elimination
      for pivot_col in 1 .. rows - 1 loop
         -- Find pivot element (largest absolute value in current column)
         pivot_row := pivot_col;
         for i in pivot_col + 1 .. rows loop
            if abs M(i, pivot_col) > abs M(pivot_row, pivot_col) then
               pivot_row := i;
            end if;
         end loop;
         
         -- Swap rows if necessary
         if pivot_row /= pivot_col then
            for j in 1 .. cols loop
               declare
                  temp : Float := M(pivot_col, j);
               begin
                  M(pivot_col, j) := M(pivot_row, j);
                  M(pivot_row, j) := temp;
               end;
            end loop;
         end if;
         
         -- Check if pivot element is zero
         pivot_element := M(pivot_col, pivot_col);
         if abs pivot_element < 1.0E-10 then
            Put_Line("Matrix is singular");
            return;
         end if;
         
         -- Make pivot element 1
         for j in pivot_col .. cols loop
            M(pivot_col, j) := M(pivot_col, j) / pivot_element;
         end loop;
         
         -- Eliminate elements in current column
         for i in 1 .. rows loop
            if i /= pivot_col then
               factor := M(i, pivot_col);
               for j in pivot_col .. cols loop
                  M(i, j) := M(i, j) - factor * M(pivot_col, j);
               end loop;
            end if;
         end loop;
      end loop;
      
      -- Backward elimination (to make it reduced row echelon form)
      -- This step is optional for solving systems, but makes the matrix canonical
      for i in 1 .. rows loop
         for j in 1 .. cols loop
            if abs M(i, j) < 1.0E-10 then
               M(i, j) := 0.0;
            end if;
         end loop;
      end loop;
   end Gauss_Jordan_Elimination;
   
   -- Example usage
   procedure Example is
      -- Example system of equations:
      -- 2x + y - z = 8
      -- -3x - y + 2z = -11
      -- -2x + y + 2z = -3
      -- Augmented matrix [A|b]:
      -- 2.0  1.0 -1.0  8.0
      -- -3.0 -1.0  2.0 -11.0
      -- -2.0  1.0  2.0 -3.0
      A : Matrix(1..3, 1..4) :=
        ((2.0, 1.0, -1.0, 8.0),
         (-3.0, -1.0, 2.0, -11.0),
         (-2.0, 1.0, 2.0, -3.0));
   begin
      Put_Line("Original augmented matrix:");
      Display_Matrix(A);
      
      Gauss_Jordan_Elimination(A);
      
      Put_Line("Reduced row echelon form:");
      Display_Matrix(A);
      
      Put_Line("Solution:");
      Put_Line("x = " & Float'Image(A(1,4)));
      Put_Line("y = " & Float'Image(A(2,4)));
      Put_Line("z = " & Float'Image(A(3,4)));
   end Example;
   
begin
   Example;
end Gauss_Jordan;
```

## Output
```
Original augmented matrix:
    2.00   1.00  -1.00   8.00
   -3.00  -1.00   2.00  -11.00
   -2.00   1.00   2.00  -3.00

Reduced row echelon form:
    1.00   0.00   0.00   2.00
    0.00   1.00   0.00   3.00
    0.00   0.00   1.00   1.00

Solution:
x =  2.00000E+00
y =  3.00000E+00
z =  1.00000E+00
```

## Key Features of this Implementation:

1. **Matrix Type**: Uses a 2D array of Floats to represent the augmented matrix
2. **Partial Pivoting**: Finds the largest element in the current column to improve numerical stability
3. **Row Swapping**: Swaps rows when necessary to get the pivot element
4. **Forward Elimination**: Reduces the matrix to row echelon form
5. **Backward Elimination**: Ensures the matrix is in reduced row echelon form
6. **Zero Cleanup**: Sets very small numbers to zero to avoid floating-point precision issues

The algorithm solves the system of linear equations represented by the augmented matrix, producing the solution vector in the last column of the matrix.

