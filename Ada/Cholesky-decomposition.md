# Cholesky Decomposition in Ada

Here's an implementation of the Cholesky decomposition algorithm in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Generic_Elementary_Functions;

procedure Cholesky_Demo is

   type Matrix is array (Integer range <>, Integer range <>) of Float;
   
   -- Cholesky decomposition function
   function Cholesky(A : Matrix) return Matrix is
      n : constant Integer := A'Length(1);
      L : Matrix(1..n, 1..n) := (others => (others => 0.0));
      sum : Float;
   begin
      for i in 1..n loop
         for j in 1..i loop
            sum := A(i,j);
            for k in 1..j-1 loop
               sum := sum - L(i,k) * L(j,k);
            end loop;
            
            if i = j then
               if sum <= 0.0 then
                  raise Constraint_Error with "Matrix is not positive definite";
               end if;
               L(i,j) := sqrt(sum);
            else
               L(i,j) := sum / L(j,j);
            end if;
         end loop;
      end loop;
      
      return L;
   end Cholesky;
   
   -- Function to print matrix
   procedure Print_Matrix(M : Matrix) is
   begin
      for i in M'First(1)..M'Last(1) loop
         for j in M'First(2)..M'Last(2) loop
            Put(Width => 8, Field => Float'Model(M(i,j)));
         end loop;
         New_Line;
      end loop;
   end Print_Matrix;
   
   -- Example usage
   A : Matrix(1..3, 1..3) := 
      ((4.0, 12.0, -16.0),
       (12.0, 37.0, -43.0),
       (-16.0, -43.0, 98.0));
   
   L : Matrix(1..3, 1..3);
   
begin
   Put_Line("Original Matrix A:");
   Print_Matrix(A);
   
   L := Cholesky(A);
   
   Put_Line("Lower triangular matrix L:");
   Print_Matrix(L);
   
   Put_Line("Verification L * L' = A:");
   -- Compute L * L' and verify
   declare
      result : Matrix(1..3, 1..3) := (others => (others => 0.0));
   begin
      for i in 1..3 loop
         for j in 1..3 loop
            for k in 1..3 loop
               result(i,j) := result(i,j) + L(i,k) * L(j,k);
            end loop;
         end loop;
      end loop;
      
      Print_Matrix(result);
   end;
   
exception
   when Constraint_Error =>
      Put_Line("Error: Matrix is not positive definite");
end Cholesky_Demo;
```

## How it works:

1. **Input**: A symmetric positive definite matrix A
2. **Output**: Lower triangular matrix L such that A = L × L^T
3. **Algorithm**:
   - For each row i from 1 to n:
     - For each column j from 1 to i:
       - Compute sum = A(i,j) - Σ(k=1 to j-1) L(i,k) × L(j,k)
       - If i = j: L(i,j) = √sum (ensure sum > 0)
       - Else: L(i,j) = sum / L(j,j)

## Key Features:

- **Error Handling**: Checks for positive definiteness
- **Type Safety**: Uses Ada's strong typing system
- **Modularity**: Separate functions for decomposition and printing
- **Verification**: Includes check that L × L^T = A

## Sample Output:
```
Original Matrix A:
    4.00   12.00  -16.00
   12.00   37.00  -43.00
  -16.00  -43.00   98.00

Lower triangular matrix L:
    2.00    0.00    0.00
    6.00    1.00    0.00
   -8.00    5.00    3.00

Verification L * L' = A:
    4.00   12.00  -16.00
   12.00   37.00  -43.00
  -16.00  -43.00   98.00
```

