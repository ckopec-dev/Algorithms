# Matrix Exponentiation in Ada

Here's an example of a matrix exponentiation algorithm implemented in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Generic_Elementary_Functions;

procedure Matrix_Exponentiation is

   -- Matrix type definition
   type Matrix is array (Positive range <>, Positive range <>) of Float;
   
   -- Matrix multiplication function
   function Multiply(A, B : Matrix) return Matrix is
      Rows_A : constant Positive := A'First(1);
      Cols_A : constant Positive := A'Last(1);
      Rows_B : constant Positive := B'First(1);
      Cols_B : constant Positive := B'Last(1);
      Result : Matrix(Rows_A .. Rows_A + Cols_A - 1, 
                     Rows_B .. Rows_B + Cols_B - 1);
   begin
      for I in Result'First(1) .. Result'Last(1) loop
         for J in Result'First(2) .. Result'Last(2) loop
            Result(I, J) := 0.0;
            for K in A'First(2) .. A'Last(2) loop
               Result(I, J) := Result(I, J) + A(I, K) * B(K, J);
            end loop;
         end loop;
      end loop;
      return Result;
   end Multiply;

   -- Identity matrix generator
   function Identity(N : Positive) return Matrix is
      Result : Matrix(1 .. N, 1 .. N);
   begin
      for I in Result'First(1) .. Result'Last(1) loop
         for J in Result'First(2) .. Result'Last(2) loop
            if I = J then
               Result(I, J) := 1.0;
            else
               Result(I, J) := 0.0;
            end if;
         end loop;
      end loop;
      return Result;
   end Identity;

   -- Matrix exponentiation using fast exponentiation (binary exponentiation)
   function Power(Matrix_A : Matrix; Power_Exponent : Natural) return Matrix is
      Base : Matrix := Matrix_A;
      Result : Matrix;
      Exponent : Natural := Power_Exponent;
   begin
      -- Handle special cases
      if Exponent = 0 then
         return Identity(Matrix_A'Last(1));
      elsif Exponent = 1 then
         return Matrix_A;
      end if;
      
      -- Initialize result as identity matrix
      Result := Identity(Matrix_A'Last(1));
      
      -- Binary exponentiation algorithm
      while Exponent > 0 loop
         if Exponent mod 2 = 1 then
            Result := Multiply(Result, Base);
         end if;
         Base := Multiply(Base, Base);
         Exponent := Exponent / 2;
      end loop;
      
      return Result;
   end Power;

   -- Print matrix function
   procedure Print_Matrix(M : Matrix) is
   begin
      for I in M'First(1) .. M'Last(1) loop
         for J in M'First(2) .. M'Last(2) loop
            Put(Positive(M(I, J))'Img);
         end loop;
         New_Line;
      end loop;
   end Print_Matrix;

   -- Example usage
   Matrix_Example : constant Matrix(1 .. 2, 1 .. 2) := 
     ((1.0, 2.0),
      (3.0, 4.0));
   
   Result_Matrix : Matrix;

begin
   Put_Line("Original Matrix:");
   Print_Matrix(Matrix_Example);
   
   Put_Line("Matrix^3:");
   Result_Matrix := Power(Matrix_Example, 3);
   Print_Matrix(Result_Matrix);
   
   Put_Line("Matrix^0 (Identity):");
   Result_Matrix := Power(Matrix_Example, 0);
   Print_Matrix(Result_Matrix);
   
end Matrix_Exponentiation;
```

## Key Features of this Implementation:

1. **Generic Matrix Type**: Uses Ada's array types with bounds checking
2. **Matrix Multiplication**: Implements standard matrix multiplication algorithm
3. **Fast Exponentiation**: Uses binary exponentiation (O(log n)) instead of naive approach
4. **Identity Matrix**: Generates identity matrix for base cases
5. **Bounds Checking**: Uses Ada's built-in array bounds checking
6. **Special Cases**: Handles exponent 0 and 1 appropriately

## Algorithm Complexity:
- **Time Complexity**: O(n³ log k) where n is matrix dimension and k is exponent
- **Space Complexity**: O(n²) for storing matrices

This implementation demonstrates Ada's strong typing, bounds checking, and modular design capabilities while providing an efficient matrix exponentiation algorithm.

