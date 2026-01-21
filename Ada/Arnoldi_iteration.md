# Arnoldi Iteration Algorithm in Ada

Here's an implementation of the Arnoldi iteration algorithm in Ada for computing eigenvalues of large sparse matrices:

```ada
with Ada.Numerics.Real_Arrays;
with Ada.Numerics.Generic_Elementary_Functions;
procedure Arnoldi_Iteration is

   -- Define real type alias
   type Real is new Float;
   type Vector is array (Positive range <>) of Real;
   type Matrix is array (Positive range <>, Positive range <>) of Real;
   
   -- Generic package for elementary functions
   package Math is new Ada.Numerics.Generic_Elementary_Functions (Real);
   use Math;
   
   -- Function to compute dot product of two vectors
   function Dot_Product (A, B : Vector) return Real is
   begin
      if A'Length /= B'Length then
         raise Constraint_Error with "Vector dimensions must match";
      end if;
      
      declare
         Result : Real := 0.0;
      begin
         for I in A'Range loop
            Result := Result + A(I) * B(I);
         end loop;
         return Result;
      end;
   end Dot_Product;
   
   -- Function to compute vector norm
   function Norm (V : Vector) return Real is
   begin
      return Sqrt (Dot_Product (V, V));
   end Norm;
   
   -- Function to normalize a vector
   procedure Normalize (V : in out Vector) is
      N : constant Real := Norm (V);
   begin
      if N /= 0.0 then
         for I in V'Range loop
            V(I) := V(I) / N;
         end loop;
      end if;
   end Normalize;
   
   -- Matrix-vector multiplication
   function Matrix_Vector_Multiply (A : Matrix; x : Vector) return Vector is
      Result : Vector (A'Row_First .. A'Row_Last);
   begin
      for I in A'Row_First .. A'Row_Last loop
         Result(I) := 0.0;
         for J in A'Col_First .. A'Col_Last loop
            Result(I) := Result(I) + A(I, J) * x(J);
         end loop;
      end loop;
      return Result;
   end Matrix_Vector_Multiply;
   
   -- Arnoldi iteration algorithm
   procedure Arnoldi_Iteration_Algorithm (
      A : Matrix;
      Initial_Vector : Vector;
      Max_Iterations : Positive;
      Eigenvalues : out Vector;
      Converged : out Boolean
   ) is
   
      -- Arnoldi matrix H (upper Hessenberg)
      H : Matrix (1 .. Max_Iterations, 1 .. Max_Iterations);
      
      -- Arnoldi basis vectors
      V : array (1 .. Max_Iterations + 1) of Vector (A'Col_First .. A'Col_Last);
      
      -- Temporary vectors
      w : Vector (A'Col_First .. A'Col_Last);
      
      -- Convergence tolerance
      Tolerance : constant Real := 1.0e-10;
      
      -- Iteration counter
      Iter : Positive := 1;
      
      -- Current vector norm
      beta : Real;
      
      -- Flag for convergence
      Converged_Flag : Boolean := False;
      
   begin
      -- Initialize with the first vector
      V(1) := Initial_Vector;
      Normalize (V(1));
      
      -- Arnoldi iteration loop
      while Iter <= Max_Iterations loop
         -- Compute w = A * V(Iter)
         w := Matrix_Vector_Multiply (A, V(Iter));
         
         -- Compute Hessenberg coefficients
         for J in 1 .. Iter loop
            H(J, Iter) := Dot_Product (w, V(J));
            w := w - H(J, Iter) * V(J);
         end loop;
         
         -- Compute beta = ||w||
         beta := Norm (w);
         H(Iter + 1, Iter) := beta;
         
         -- Check for convergence
         if beta < Tolerance then
            Converged_Flag := True;
            exit;
         end if;
         
         -- Normalize w to get next basis vector
         if beta /= 0.0 then
            V(Iter + 1) := w;
            Normalize (V(Iter + 1));
         else
            -- If beta is zero, we've found an exact eigenvector
            Converged_Flag := True;
            exit;
         end if;
         
         Iter := Iter + 1;
      end loop;
      
      -- Extract eigenvalues from the Hessenberg matrix
      -- (This is a simplified version - in practice, you'd use LAPACK or similar)
      if Iter >= 1 then
         -- For demonstration, we'll just return the diagonal elements
         -- as approximations to eigenvalues
         for I in 1 .. Iter loop
            if I <= Eigenvalues'Length then
               Eigenvalues(I) := H(I, I);
            end if;
         end loop;
      end if;
      
      Converged := Converged_Flag;
      
   end Arnoldi_Iteration_Algorithm;
   
   -- Example usage
   procedure Example_Usage is
      -- 4x4 test matrix
      Test_Matrix : Matrix (1 .. 4, 1 .. 4) :=
        ((4.0, 1.0, 0.0, 0.0),
         (1.0, 4.0, 1.0, 0.0),
         (0.0, 1.0, 4.0, 1.0),
         (0.0, 0.0, 1.0, 4.0));
      
      -- Initial vector
      Initial : Vector (1 .. 4) := (1.0, 1.0, 1.0, 1.0);
      
      -- Result storage
      Eigenvals : Vector (1 .. 4);
      Converged_Flag : Boolean;
      
   begin
      -- Run Arnoldi iteration
      Arnoldi_Iteration_Algorithm (
         A => Test_Matrix,
         Initial_Vector => Initial,
         Max_Iterations => 4,
         Eigenvalues => Eigenvals,
         Converged => Converged_Flag
      );
      
      -- Display results
      if Converged_Flag then
         Put_Line ("Arnoldi iteration converged:");
         for I in Eigenvals'Range loop
            Put_Line ("Eigenvalue " & I'Img & ": " & Eigenvals(I)'Img);
         end loop;
      else
         Put_Line ("Arnoldi iteration did not converge within maximum iterations");
      end if;
      
   end Example_Usage;
   
begin
   Example_Usage;
end Arnoldi_Iteration;
```

## Key Features of this Implementation:

1. **Modular Design**: Separate functions for vector operations, matrix-vector multiplication, and normalization
2. **Error Handling**: Includes basic constraint checking for vector dimensions
3. **Memory Management**: Uses Ada's built-in array handling with proper bounds checking
4. **Convergence Detection**: Implements basic convergence criteria based on vector norm
5. **Generic Structure**: Easy to adapt for different numeric types
6. **Clear Separation**: Clear distinction between Arnoldi iteration logic and eigenvalue extraction

## Usage Notes:

- This is a simplified implementation for demonstration purposes
- In practice, you'd want to use optimized BLAS/LAPACK libraries for better performance
- The eigenvalue extraction step is simplified - a full implementation would use QR decomposition
- The algorithm can be extended to handle complex numbers and more sophisticated convergence criteria

The algorithm computes an orthonormal basis for the Krylov subspace and reduces the original matrix to upper Hessenberg form, which is then used to approximate eigenvalues.

