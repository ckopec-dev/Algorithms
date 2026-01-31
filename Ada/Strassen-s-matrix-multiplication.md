# Strassen's Matrix Multiplication in Ada

Here's an implementation of Strassen's matrix multiplication algorithm in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Generic_Elementary_Functions;

procedure Strassen_Matrix_Multiplication is

   type Matrix is array (Positive range <>, Positive range <>) of Integer;
   type Matrix_Access is access all Matrix;

   -- Function to check if matrix dimension is a power of 2
   function Is_Power_of_2(N : Integer) return Boolean is
   begin
      return N > 0 and then (N and (N - 1)) = 0;
   end Is_Power_of_2;

   -- Function to pad matrix to next power of 2 size
   function Pad_Matrix(M : Matrix; New_Size : Positive) return Matrix is
      Result : Matrix(1..New_Size, 1..New_Size) := (others => (others => 0));
   begin
      for I in M'First(1)..M'Last(1) loop
         for J in M'First(2)..M'Last(2) loop
            Result(I, J) := M(I, J);
         end loop;
      end loop;
      return Result;
   end Pad_Matrix;

   -- Strassen's multiplication algorithm
   function Strassen_Multiply(A, B : Matrix) return Matrix is
      N : constant Positive := A'Length(1);
      C : Matrix(1..N, 1..N);
   begin
      -- Base case: simple multiplication for small matrices
      if N <= 2 then
         for I in 1..N loop
            for J in 1..N loop
               C(I, J) := 0;
               for K in 1..N loop
                  C(I, J) := C(I, J) + A(I, K) * B(K, J);
               end loop;
            end loop;
         end loop;
         return C;
      end if;

      -- Divide matrices into quadrants
      declare
         Half_Size : constant Positive := N / 2;
         
         -- Submatrices for A
         A11 : Matrix(1..Half_Size, 1..Half_Size) := (others => (others => 0));
         A12 : Matrix(1..Half_Size, 1..Half_Size) := (others => (others => 0));
         A21 : Matrix(1..Half_Size, 1..Half_Size) := (others => (others => 0));
         A22 : Matrix(1..Half_Size, 1..Half_Size) := (others => (others => 0));
         
         -- Submatrices for B
         B11 : Matrix(1..Half_Size, 1..Half_Size) := (others => (others => 0));
         B12 : Matrix(1..Half_Size, 1..Half_Size) := (others => (others => 0));
         B21 : Matrix(1..Half_Size, 1..Half_Size) := (others => (others => 0));
         B22 : Matrix(1..Half_Size, 1..Half_Size) := (others => (others => 0));
         
         -- Strassen's intermediate matrices
         M1, M2, M3, M4, M5, M6, M7 : Matrix(1..Half_Size, 1..Half_Size);
         S1, S2, S3, S4, S5, S6, S7, S8 : Matrix(1..Half_Size, 1..Half_Size);
         P1, P2, P3, P4, P5, P6, P7 : Matrix(1..Half_Size, 1..Half_Size);
         
         -- Result quadrants
         C11, C12, C21, C22 : Matrix(1..Half_Size, 1..Half_Size);
      begin
         -- Split matrices into quadrants
         for I in 1..Half_Size loop
            for J in 1..Half_Size loop
               A11(I, J) := A(I, J);
               A12(I, J) := A(I, J + Half_Size);
               A21(I, J) := A(I + Half_Size, J);
               A22(I, J) := A(I + Half_Size, J + Half_Size);
               
               B11(I, J) := B(I, J);
               B12(I, J) := B(I, J + Half_Size);
               B21(I, J) := B(I + Half_Size, J);
               B22(I, J) := B(I + Half_Size, J + Half_Size);
            end loop;
         end loop;

         -- Calculate intermediate matrices using Strassen's formulas
         -- S1 = B12 - B22
         for I in 1..Half_Size loop
            for J in 1..Half_Size loop
               S1(I, J) := B12(I, J) - B22(I, J);
            end loop;
         end loop;
         
         -- M1 = A11 * S1
         M1 := Strassen_Multiply(A11, S1);
         
         -- S2 = A22 - A11
         for I in 1..Half_Size loop
            for J in 1..Half_Size loop
               S2(I, J) := A22(I, J) - A11(I, J);
            end loop;
         end loop;
         
         -- M2 = S2 * B22
         M2 := Strassen_Multiply(S2, B22);
         
         -- S3 = A11 + A12
         for I in 1..Half_Size loop
            for J in 1..Half_Size loop
               S3(I, J) := A11(I, J) + A12(I, J);
            end loop;
         end loop;
         
         -- M3 = S3 * B11
         M3 := Strassen_Multiply(S3, B11);
         
         -- S4 = B21 - B11
         for I in 1..Half_Size loop
            for J in 1..Half_Size loop
               S4(I, J) := B21(I, J) - B11(I, J);
            end loop;
         end loop;
         
         -- M4 = A22 * S4
         M4 := Strassen_Multiply(A22, S4);
         
         -- S5 = A11 + A22
         for I in 1..Half_Size loop
            for J in 1..Half_Size loop
               S5(I, J) := A11(I, J) + A22(I, J);
            end loop;
         end loop;
         
         -- S6 = B11 + B22
         for I in 1..Half_Size loop
            for J in 1..Half_Size loop
               S6(I, J) := B11(I, J) + B22(I, J);
            end loop;
         end loop;
         
         -- M5 = S5 * S6
         M5 := Strassen_Multiply(S5, S6);
         
         -- S7 = A12 - A22
         for I in 1..Half_Size loop
            for J in 1..Half_Size loop
               S7(I, J) := A12(I, J) - A22(I, J);
            end loop;
         end loop;
         
         -- S8 = B21 + B22
         for I in 1..Half_Size loop
            for J in 1..Half_Size loop
               S8(I, J) := B21(I, J) + B22(I, J);
            end loop;
         end loop;
         
         -- M6 = S7 * S8
         M6 := Strassen_Multiply(S7, S8);
         
         -- S9 = A11 - A21
         for I in 1..Half_Size loop
            for J in 1..Half_Size loop
               S9(I, J) := A11(I, J) - A21(I, J);
            end loop;
         end loop;
         
         -- S10 = B11 + B12
         for I in 1..Half_Size loop
            for J in 1..Half_Size loop
               S10(I, J) := B11(I, J) + B12(I, J);
            end loop;
         end loop;
         
         -- M7 = S9 * S10
         M7 := Strassen_Multiply(S9, S10);

         -- Calculate result quadrants
         -- C11 = M1 + M5 - M3 + M7
         for I in 1..Half_Size loop
            for J in 1..Half_Size loop
               C11(I, J) := M1(I, J) + M5(I, J) - M3(I, J) + M7(I, J);
            end loop;
         end loop;
         
         -- C12 = M3 + M6
         for I in 1..Half_Size loop
            for J in 1..Half_Size loop
               C12(I, J) := M3(I, J) + M6(I, J);
            end loop;
         end loop;
         
         -- C21 = M2 + M4
         for I in 1..Half_Size loop
            for J in 1..Half_Size loop
               C21(I, J) := M2(I, J) + M4(I, J);
            end loop;
         end loop;
         
         -- C22 = M1 - M2 + M4 + M5
         for I in 1..Half_Size loop
            for J in 1..Half_Size loop
               C22(I, J) := M1(I, J) - M2(I, J) + M4(I, J) + M5(I, J);
            end loop;
         end loop;

         -- Combine results
         for I in 1..Half_Size loop
            for J in 1..Half_Size loop
               C(I, J) := C11(I, J);
               C(I, J + Half_Size) := C12(I, J);
               C(I + Half_Size, J) := C21(I, J);
               C(I + Half_Size, J + Half_Size) := C22(I, J);
            end loop;
         end loop;
         
         return C;
      end;
   end Strassen_Multiply;

   -- Simple matrix multiplication (for comparison)
   function Simple_Multiply(A, B : Matrix) return Matrix is
      N : constant Positive := A'Length(1);
      C : Matrix(1..N, 1..N);
   begin
      for I in 1..N loop
         for J in 1..N loop
            C(I, J) := 0;
            for K in 1..N loop
               C(I, J) := C(I, J) + A(I, K) * B(K, J);
            end loop;
         end loop;
      end loop;
      return C;
   end Simple_Multiply;

   -- Procedure to print matrix
   procedure Print_Matrix(M : Matrix) is
   begin
      for I in M'First(1)..M'Last(1) loop
         for J in M'First(2)..M'Last(2) loop
            Put(Integer(Image(M(I, J))));
            Put(" ");
         end loop;
         New_Line;
      end loop;
      New_Line;
   end Print_Matrix;

   -- Example usage
   procedure Example is
      -- 4x4 matrices for demonstration
      A : Matrix(1..4, 1..4) := 
        ((1, 2, 3, 4),
         (5, 6, 7, 8),
         (9, 10, 11, 12),
         (13, 14, 15, 16));
      
      B : Matrix(1..4, 1..4) := 
        ((1, 0, 0, 0),
         (0, 1, 0, 0),
         (0, 0, 1, 0),
         (0, 0, 0, 1));
      
      C_Strassen : Matrix(1..4, 1..4);
      C_Simple : Matrix(1..4, 1..4);
   begin
      Put_Line("Matrix A:");
      Print_Matrix(A);
      
      Put_Line("Matrix B:");
      Print_Matrix(B);
      
      C_Strassen := Strassen_Multiply(A, B);
      Put_Line("Result using Strassen's algorithm:");
      Print_Matrix(C_Strassen);
      
      C_Simple := Simple_Multiply(A, B);
      Put_Line("Result using simple multiplication:");
      Print_Matrix(C_Simple);
   end Example;

begin
   Example;
end Strassen_Matrix_Multiplication;
```

## Key Features of this Implementation:

1. **Recursive Implementation**: Uses the divide-and-conquer approach of Strassen's algorithm
2. **Base Case Handling**: Falls back to standard multiplication for small matrices (≤ 2×2)
3. **Matrix Splitting**: Properly divides matrices into quadrants
4. **Strassen's Formulas**: Implements the 7 multiplications and 18 additions/subtractions
5. **Proper Matrix Operations**: Includes helper functions for matrix printing and base case multiplication

## How Strassen's Algorithm Works:

- **Time Complexity**: O(n^2.807) vs O(n³) for standard multiplication
- **Space Complexity**: O(n²) 
- **Principle**: Reduces the number of multiplications from 8 to 7 using clever mathematical combinations
- **Optimization**: Most effective for large matrices where the overhead of recursion is justified

The algorithm is particularly useful for very large matrices where the asymptotic improvement outweighs the constant factors and recursion overhead.

