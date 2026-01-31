# Conjugate Gradient Method in Ada

Here's an implementation of the conjugate gradient method algorithm in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Numerics.Real_Arrays;

procedure Conjugate_Gradient_Example is

   package Float_IO is new Ada.Text_IO.Float_IO (Float);
   package Real_Arrays is new Ada.Numerics.Real_Arrays (Float);
   use Real_Arrays;

   -- Matrix-vector multiplication A * x
   function Matrix_Vector_Multiply(A : Matrix; x : Vector) return Vector is
      result : Vector(1..A'Length(1));
   begin
      for i in result'Range loop
         result(i) := 0.0;
         for j in A(i)'Range loop
            result(i) := result(i) + A(i,j) * x(j);
         end loop;
      end loop;
      return result;
   end Matrix_Vector_Multiply;

   -- Vector dot product
   function Dot_Product(x, y : Vector) return Float is
   begin
      if x'Length /= y'Length then
         raise Constraint_Error;
      end if;
      
      declare
         sum : Float := 0.0;
      begin
         for i in x'Range loop
            sum := sum + x(i) * y(i);
         end loop;
         return sum;
      end;
   end Dot_Product;

   -- Vector addition: x + y
   function Vector_Add(x, y : Vector) return Vector is
      result : Vector(x'Range);
   begin
      if x'Length /= y'Length then
         raise Constraint_Error;
      end if;
      
      for i in x'Range loop
         result(i) := x(i) + y(i);
      end loop;
      return result;
   end Vector_Add;

   -- Vector subtraction: x - y
   function Vector_Subtract(x, y : Vector) return Vector is
      result : Vector(x'Range);
   begin
      if x'Length /= y'Length then
         raise Constraint_Error;
      end if;
      
      for i in x'Range loop
         result(i) := x(i) - y(i);
      end loop;
      return result;
   end Vector_Subtract;

   -- Scalar multiplication: alpha * x
   function Scalar_Multiply(alpha : Float; x : Vector) return Vector is
      result : Vector(x'Range);
   begin
      for i in x'Range loop
         result(i) := alpha * x(i);
      end loop;
      return result;
   end Scalar_Multiply;

   -- Conjugate Gradient Method
   function Conjugate_Gradient(A : Matrix; b : Vector; 
                              initial_x : Vector; 
                              tolerance : Float := 1.0e-6;
                              max_iterations : Integer := 1000) 
                              return Vector is
      x : Vector := initial_x;
      r : Vector := Vector_Subtract(b, Matrix_Vector_Multiply(A, x));
      p : Vector := r;
      rsold : Float := Dot_Product(r, r);
      rsnew : Float;
      alpha : Float;
      beta : Float;
      i : Integer := 0;
   begin
      loop
         i := i + 1;
         
         -- Check convergence
         if rsold < tolerance then
            Put_Line("Converged after " & Integer'Image(i) & " iterations");
            return x;
         end if;
         
         -- Check maximum iterations
         if i > max_iterations then
            Put_Line("Maximum iterations reached");
            return x;
         end if;
         
         -- Compute A * p
         declare
            Ap : Vector := Matrix_Vector_Multiply(A, p);
         begin
            -- Compute alpha
            alpha := rsold / Dot_Product(p, Ap);
            
            -- Update x
            x := Vector_Add(x, Scalar_Multiply(alpha, p));
            
            -- Update r
            r := Vector_Subtract(r, Scalar_Multiply(alpha, Ap));
            
            -- Compute new residual norm
            rsnew := Dot_Product(r, r);
            
            -- Compute beta
            beta := rsnew / rsold;
            
            -- Update p
            p := Vector_Add(r, Scalar_Multiply(beta, p));
            
            -- Update rsold for next iteration
            rsold := rsnew;
         end;
      end loop;
   end Conjugate_Gradient;

   -- Example system: Ax = b
   -- A = [4.0 1.0; 1.0 3.0]
   -- b = [1.0; 2.0]
   -- Solution should be [0.0909; 0.6364]
   
   A : Matrix(1..2, 1..2) := 
     ((4.0, 1.0),
      (1.0, 3.0));
   
   b : Vector(1..2) := (1.0, 2.0);
   
   initial_x : Vector(1..2) := (0.0, 0.0);
   
   solution : Vector(1..2);
   
begin
   Put_Line("Solving linear system Ax = b using Conjugate Gradient method");
   Put_Line("Matrix A:");
   Put_Line("  4.0  1.0");
   Put_Line("  1.0  3.0");
   Put_Line("Vector b: [1.0, 2.0]");
   
   solution := Conjugate_Gradient(A, b, initial_x);
   
   Put_Line("Solution x:");
   Float_IO.Put(solution(1), Fore => 1, Aft => 6, Exp => 0);
   Put_Line(" ");
   Float_IO.Put(solution(2), Fore => 1, Aft => 6, Exp => 0);
   Put_Line(" ");
   
   -- Verify solution
   declare
      verify : Vector(1..2) := Matrix_Vector_Multiply(A, solution);
   begin
      Put_Line("Verification (A * x):");
      Float_IO.Put(verify(1), Fore => 1, Aft => 6, Exp => 0);
      Put_Line(" ");
      Float_IO.Put(verify(2), Fore => 1, Aft => 6, Exp => 0);
      Put_Line(" ");
   end;
   
end Conjugate_Gradient_Example;
```

## Key Features of this Implementation:

1. **Matrix-Vector Operations**: Includes functions for matrix-vector multiplication, vector dot product, vector addition, subtraction, and scalar multiplication.

2. **Core Algorithm**: Implements the standard conjugate gradient method with:
   - Residual calculation
   - Search direction update
   - Step size computation (alpha)
   - Convergence checking
   - Maximum iteration limit

3. **Example Problem**: Solves a 2x2 linear system as a demonstration.

4. **Verification**: Includes verification that the computed solution satisfies the original equation.

## Algorithm Steps:

1. Initialize x₀, r₀ = b - Ax₀, p₀ = r₀
2. For each iteration:
   - Compute αₖ = (rₖᵀrₖ)/(pₖᵀApₖ)
   - Update xₖ₊₁ = xₖ + αₖpₖ
   - Update rₖ₊₁ = rₖ - αₖApₖ
   - Compute βₖ₊₁ = (rₖ₊₁ᵀrₖ₊₁)/(rₖᵀrₖ)
   - Update pₖ₊₁ = rₖ₊₁ + βₖ₊₁pₖ

The implementation is robust and handles convergence checking and maximum iteration limits appropriately.

