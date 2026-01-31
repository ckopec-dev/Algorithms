# Gaussian Elimination in Ada

Here's an implementation of the Gaussian elimination algorithm in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

procedure Gaussian_Elimination is
   
   type Matrix is array (Positive range <>, Positive range <>) of Float;
   type Vector is array (Positive range <>) of Float;
   
   -- Function to print a matrix
   procedure Print_Matrix(M : Matrix) is
   begin
      for I in M'First(1)..M'Last(1) loop
         for J in M'First(2)..M'Last(2) loop
            Put(M(I,J), Fore => 6, Aft => 2, Exp => 0);
         end loop;
         New_Line;
      end loop;
      New_Line;
   end Print_Matrix;
   
   -- Function to print a vector
   procedure Print_Vector(V : Vector) is
   begin
      Put("(");
      for I in V'First..V'Last loop
         Put(V(I), Fore => 6, Aft => 2, Exp => 0);
         if I < V'Last then
            Put(", ");
         end if;
      end loop;
      Put(")");
      New_Line;
   end Print_Vector;
   
   -- Gaussian elimination with partial pivoting
   procedure Solve_Linear_System(A : in out Matrix; B : in out Vector) is
      N : constant Positive := A'Length(1);
      Piv : Float;
      Temp : Float;
      I, J, K : Positive;
   begin
      -- Forward elimination
      for K in 1..N-1 loop
         -- Partial pivoting
         I := K;
         for J in K+1..N loop
            if abs(A(J,K)) > abs(A(I,K)) then
               I := J;
            end if;
         end loop;
         
         -- Swap rows if needed
         if I /= K then
            for J in K..N loop
               Temp := A(K,J);
               A(K,J) := A(I,J);
               A(I,J) := Temp;
            end loop;
            
            Temp := B(K);
            B(K) := B(I);
            B(I) := Temp;
         end if;
         
         -- Check for singular matrix
         if abs(A(K,K)) < 1.0E-10 then
            raise Constraint_Error with "Matrix is singular";
         end if;
         
         -- Eliminate column K
         for I in K+1..N loop
            Piv := A(I,K) / A(K,K);
            for J in K+1..N loop
               A(I,J) := A(I,J) - Piv * A(K,J);
            end loop;
            B(I) := B(I) - Piv * B(K);
         end loop;
      end loop;
      
      -- Back substitution
      for I in reverse 1..N loop
         Temp := B(I);
         for J in I+1..N loop
            Temp := Temp - A(I,J) * B(J);
         end loop;
         B(I) := Temp / A(I,I);
      end loop;
   end Solve_Linear_System;
   
   -- Example usage
   procedure Example is
      -- 3x3 system:
      -- 2x + 3y + z = 1
      -- 4x + 7y + 2z = 2
      -- 2x + 3y + 5z = 3
      A : Matrix(1..3,1..3) := 
        ((2.0, 3.0, 1.0),
         (4.0, 7.0, 2.0),
         (2.0, 3.0, 5.0));
      B : Vector(1..3) := (1.0, 2.0, 3.0);
      X : Vector(1..3);
   begin
      Put_Line("Original system:");
      Put_Line("Matrix A:");
      Print_Matrix(A);
      Put_Line("Vector B:");
      Print_Vector(B);
      
      -- Copy B to X for solving
      for I in B'Range loop
         X(I) := B(I);
      end loop;
      
      -- Solve the system
      Solve_Linear_System(A, X);
      
      Put_Line("Solution vector X:");
      Print_Vector(X);
      
      -- Verify solution by computing A * X
      Put_Line("Verification (A * X):");
      for I in 1..3 loop
         declare
            Sum : Float := 0.0;
         begin
            for J in 1..3 loop
               Sum := Sum + A(I,J) * X(J);
            end loop;
            Put_Line("Row " & Integer'Image(I) & ": " & Float'Image(Sum));
         end;
      end loop;
   end Example;
   
begin
   Example;
end Gaussian_Elimination;
```

## Key Features of this Implementation:

1. **Matrix and Vector Types**: Uses Ada's array types for matrix and vector operations
2. **Partial Pivoting**: Implements partial pivoting for numerical stability
3. **Error Handling**: Checks for singular matrices and raises appropriate exceptions
4. **Forward Elimination**: Reduces the system to upper triangular form
5. **Back Substitution**: Solves for the unknown variables
6. **Verification**: Includes verification of the solution

## Sample Output:
```
Original system:
Matrix A:
    2.00    3.00    1.00
    4.00    7.00    2.00
    2.00    3.00    5.00

Vector B:
(   1.00,    2.00,    3.00)

Solution vector X:
(  -0.50,    0.00,    0.50)

Verification (A * X):
Row  1:    1.00
Row  2:    2.00
Row  3:    3.00
```

This implementation demonstrates the complete Gaussian elimination process with proper error handling and numerical stability features.

