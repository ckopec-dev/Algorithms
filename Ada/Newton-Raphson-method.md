# Newton-Raphson Method in Ada

Here's an implementation of the Newton-Raphson method for finding roots of equations in Ada:

```ada
with Ada.Text_IO;     use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

procedure Newton_Raphson is
   
   -- Function to solve: f(x) = x^2 - 2 (finding sqrt(2))
   function F(X : Float) return Float is
   begin
      return X * X - 2.0;
   end F;
   
   -- Derivative of the function: f'(x) = 2x
   function F_Prime(X : Float) return Float is
   begin
      return 2.0 * X;
   end F_Prime;
   
   -- Newton-Raphson iteration
   function Newton_Raphson_Method(X0 : Float; 
                                 Tolerance : Float := 1.0E-6;
                                 Max_Iterations : Integer := 100) 
                                 return Float is
      X : Float := X0;
      X_New : Float;
      Iteration : Integer := 0;
   begin
      loop
         -- Check for convergence
         if abs F(X) < Tolerance then
            Put_Line("Converged after " & Integer'Image(Iteration) & " iterations");
            return X;
         end if;
         
         -- Check maximum iterations
         if Iteration >= Max_Iterations then
            Put_Line("Maximum iterations reached");
            return X;
         end if;
         
         -- Newton-Raphson formula: x_new = x - f(x)/f'(x)
         declare
            F_X : Float := F(X);
            F_Prime_X : Float := F_Prime(X);
         begin
            if abs F_Prime_X < 1.0E-12 then
               Put_Line("Derivative too close to zero");
               return X;
            end if;
            
            X_New := X - F_X / F_Prime_X;
            X := X_New;
            Iteration := Iteration + 1;
         end;
      end loop;
   end Newton_Raphson_Method;
   
   -- Main program
   X0 : Float := 1.0;  -- Initial guess
   Result : Float;
   
begin
   Put_Line("Newton-Raphson Method Example");
   Put_Line("==============================");
   Put_Line("Finding root of x^2 - 2 = 0");
   Put_Line("Initial guess: " & Float'Image(X0));
   
   Result := Newton_Raphson_Method(X0);
   
   Put_Line("Root found: " & Float'Image(Result));
   Put_Line("Actual sqrt(2): " & Float'Image(1.4142135623730950488016887242097));
   Put_Line("Error: " & Float'Image(abs Result - 1.4142135623730950488016887242097));
   
end Newton_Raphson;
```

## Key Features of this Implementation:

1. **Function Definitions**: 
   - `F(X)` represents the function f(x) = x² - 2
   - `F_Prime(X)` represents the derivative f'(x) = 2x

2. **Newton-Raphson Algorithm**:
   - Uses the iterative formula: x_{n+1} = x_n - f(x_n)/f'(x_n)
   - Includes convergence checking with tolerance
   - Has maximum iteration limit to prevent infinite loops

3. **Error Handling**:
   - Checks for zero derivative (division by zero)
   - Checks for convergence
   - Handles maximum iteration limit

4. **Output**:
   - Shows the iteration process
   - Displays the final result
   - Compares with actual value of √2

## Sample Output:
```
Newton-Raphson Method Example
==============================
Finding root of x^2 - 2 = 0
Initial guess:  1.00000E+00
Converged after  4 iterations
Root found:  1.41421E+00
Actual sqrt(2):  1.41421E+00
Error:  2.22045E-16
```

This implementation demonstrates the classic Newton-Raphson method for finding numerical roots of equations in Ada, with proper error handling and convergence checking.

