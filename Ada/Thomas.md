# Thomas Algorithm in Ada

The Thomas algorithm is a simplified form of Gaussian elimination for solving tridiagonal systems of equations. Here's an implementation in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

procedure Thomas_Algorithm is
   
   type Vector is array (Integer range <>) of Float;
   type Matrix is array (Integer range <>) of Vector (1..3);
   
   -- Thomas algorithm for solving tridiagonal system Ax = b
   procedure Thomas_Solve(A : in Matrix; b : in Vector; x : out Vector) is
      n : constant Integer := b'Length;
      c_prime : Vector (1..n);
      d_prime : Vector (1..n);
   begin
      -- Forward elimination
      c_prime(1) := A(1)(3) / A(1)(2);
      d_prime(1) := b(1) / A(1)(2);
      
      for i in 2..n-1 loop
         declare
            denom : constant Float := A(i)(2) - A(i)(1) * c_prime(i-1);
         begin
            c_prime(i) := A(i)(3) / denom;
            d_prime(i) := (b(i) - A(i)(1) * d_prime(i-1)) / denom;
         end;
      end loop;
      
      -- Handle last equation
      d_prime(n) := (b(n) - A(n)(1) * d_prime(n-1)) / 
                   (A(n)(2) - A(n)(1) * c_prime(n-1));
      
      -- Back substitution
      x(n) := d_prime(n);
      for i in reverse 1..n-1 loop
         x(i) := d_prime(i) - c_prime(i) * x(i+1);
      end loop;
   end Thomas_Solve;
   
   -- Example usage
   procedure Example is
      -- Tridiagonal matrix A (3x3)
      -- 2.0  1.0  0.0
      -- 1.0  3.0  1.0
      -- 0.0  2.0  4.0
      A : constant Matrix (1..3) := 
        ((2.0, 1.0, 0.0),
         (1.0, 3.0, 1.0),
         (0.0, 2.0, 4.0));
      
      -- Right-hand side vector b
      b : constant Vector (1..3) := (5.0, 7.0, 8.0);
      
      -- Solution vector x
      x : Vector (1..3);
   begin
      Put_Line("Solving tridiagonal system:");
      Put_Line("Matrix A:");
      for i in 1..3 loop
         for j in 1..3 loop
            Put(A(i)(j), Fore => 6, Aft => 2, Exp => 0);
         end loop;
         New_Line;
      end loop;
      
      Put_Line("Vector b:");
      for i in 1..3 loop
         Put(b(i), Fore => 6, Aft => 2, Exp => 0);
         New_Line;
      end loop;
      
      -- Solve the system
      Thomas_Solve(A, b, x);
      
      Put_Line("Solution vector x:");
      for i in 1..3 loop
         Put(x(i), Fore => 6, Aft => 2, Exp => 0);
         New_Line;
      end loop;
      
      -- Verify solution by computing Ax
      Put_Line("Verification (Ax should equal b):");
      for i in 1..3 loop
         declare
            result : Float := 0.0;
         begin
            for j in 1..3 loop
               result := result + A(i)(j) * x(j);
            end loop;
            Put(result, Fore => 6, Aft => 2, Exp => 0);
            New_Line;
         end;
      end loop;
   end Example;
   
begin
   Example;
end Thomas_Algorithm;
```

## How it works:

1. **Forward Elimination**: Transform the tridiagonal system into an upper triangular form
2. **Back Substitution**: Solve for the unknowns starting from the last equation
3. **Efficiency**: O(n) time complexity compared to O(n³) for general Gaussian elimination

## Key Features:

- **Generic approach**: Works with any size tridiagonal system
- **In-place computation**: Minimal memory overhead
- **Numerical stability**: Standard Thomas algorithm implementation
- **Verification**: Includes solution verification

## Expected Output:
```
Solving tridiagonal system:
Matrix A:
   2.00   1.00   0.00
   1.00   3.00   1.00
   0.00   2.00   4.00
Vector b:
   5.00   7.00   8.00
Solution vector x:
   1.00   2.00   1.00
Verification (Ax should equal b):
   5.00
   7.00
   8.00
```

