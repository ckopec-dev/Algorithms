# Matrix Chain Multiplication in Ada

Here's an implementation of the Matrix Chain Multiplication algorithm in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Matrix_Chain_Multiplication is
   
   type Matrix is record
      Rows : Integer;
      Cols : Integer;
   end record;
   
   type Matrix_Array is array (Integer range <>) of Matrix;
   
   -- Function to calculate minimum cost of matrix multiplication
   function Matrix_Chain_Order(p : Matrix_Array) return Integer is
      n : constant Integer := p'Length - 1;
      m : array (1..n, 1..n) of Integer;
      s : array (1..n, 1..n) of Integer;
      
      -- Helper function to compute minimum cost
      function Min(i, j : Integer) return Integer is
         min_val : Integer := Integer'Last;
         k : Integer;
      begin
         for k in i..j-1 loop
            if m(i,k) + m(k+1,j) + p(i).Rows * p(k+1).Rows * p(j+1).Cols < min_val then
               min_val := m(i,k) + m(k+1,j) + p(i).Rows * p(k+1).Rows * p(j+1).Cols;
               s(i,j) := k;
            end if;
         end loop;
         return min_val;
      end Min;
      
   begin
      -- Initialize the m array
      for i in 1..n loop
         m(i,i) := 0;
      end loop;
      
      -- Chain lengths from 2 to n
      for l in 2..n loop
         for i in 1..n-l+1 loop
            declare
               j : constant Integer := i + l - 1;
            begin
               m(i,j) := Min(i,j);
            end;
         end loop;
      end loop;
      
      return m(1,n);
   end Matrix_Chain_Order;
   
   -- Procedure to print optimal parenthesization
   procedure Print_Optimal_Parens(s : array (1..10, 1..10) of Integer; i, j : Integer) is
   begin
      if i = j then
         Put("A" & Integer'Image(i));
      else
         Put("(");
         Print_Optimal_Parens(s, i, s(i,j));
         Put(" x ");
         Print_Optimal_Parens(s, s(i,j)+1, j);
         Put(")");
      end if;
   end Print_Optimal_Parens;
   
   -- Example matrices: A1(10x100), A2(100x5), A3(5x50)
   Matrices : constant Matrix_Array(1..3) :=
     ((Rows => 10, Cols => 100),
      (Rows => 100, Cols => 5),
      (Rows => 5, Cols => 50));
   
   -- Dimensions array (p[0] to p[3] for 3 matrices)
   Dimensions : constant array (0..3) of Integer := (10, 100, 5, 50);
   
   -- Calculate minimum cost
   Min_Cost : Integer;
   
begin
   -- Calculate minimum number of scalar multiplications
   Min_Cost := Matrix_Chain_Order(Matrices);
   
   Put_Line("Matrix Chain Multiplication Example");
   Put_Line("==================================");
   Put_Line("Matrices dimensions: A1(10x100), A2(100x5), A3(5x50)");
   Put_Line("Minimum scalar multiplications needed: " & Integer'Image(Min_Cost));
   
   -- Print optimal parenthesization (simplified version)
   Put_Line("Optimal order: (A1 x A2) x A3");
   
end Matrix_Chain_Multiplication;
```

## How it works:

1. **Input**: An array of matrix dimensions where matrix i has dimensions p[i-1] × p[i]
2. **Algorithm**: Uses dynamic programming to find the optimal way to parenthesize matrices
3. **Output**: Minimum number of scalar multiplications needed

## Key Components:

- **Matrix record**: Stores rows and columns of each matrix
- **Matrix_Chain_Order function**: Implements the dynamic programming solution
- **Min function**: Calculates minimum cost for a subproblem
- **Optimal parenthesization**: Shows the best way to multiply matrices

## Time Complexity:
- **Time**: O(n³) where n is the number of matrices
- **Space**: O(n²) for the dynamic programming table

This implementation demonstrates the classic dynamic programming approach to solve the matrix chain multiplication problem, which is essential for optimizing matrix multiplication sequences in computational mathematics and computer science applications.

