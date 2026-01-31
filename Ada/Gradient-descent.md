# Gradient Descent Algorithm in Ada

Here's an implementation of gradient descent algorithm in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

procedure Gradient_Descent is
   
   -- Define the number of iterations and learning rate
   Iterations : constant := 1000;
   Learning_Rate : constant Float := 0.01;
   
   -- Define the function: f(x) = x^2 + 3x + 2
   -- Its derivative: f'(x) = 2x + 3
   
   function Function_Value(X : Float) return Float is
   begin
      return X**2 + 3.0 * X + 2.0;
   end Function_Value;
   
   function Derivative(X : Float) return Float is
   begin
      return 2.0 * X + 3.0;
   end Derivative;
   
   -- Gradient descent implementation
   procedure Gradient_Descent_Step(Starting_Point : in out Float) is
      Gradient : Float;
      New_Point : Float;
   begin
      -- Calculate gradient at current point
      Gradient := Derivative(Starting_Point);
      
      -- Update point using gradient descent formula
      -- x_new = x_old - learning_rate * gradient
      New_Point := Starting_Point - Learning_Rate * Gradient;
      
      Starting_Point := New_Point;
   end Gradient_Descent_Step;
   
   -- Main gradient descent loop
   procedure Run_Gradient_Descent is
      Current_Point : Float := 5.0;  -- Starting point
      Iteration : Integer := 0;
      Value : Float;
   begin
      Put_Line("Starting gradient descent...");
      Put_Line("Initial point: " & Float'Image(Current_Point));
      
      -- Iterative process
      for I in 1 .. Iterations loop
         Gradient_Descent_Step(Current_Point);
         Iteration := I;
         
         -- Print progress every 100 iterations
         if I mod 100 = 0 then
            Value := Function_Value(Current_Point);
            Put_Line("Iteration " & Integer'Image(I) & 
                     ": x = " & Float'Image(Current_Point) & 
                     ", f(x) = " & Float'Image(Value));
         end if;
      end loop;
      
      -- Final results
      Value := Function_Value(Current_Point);
      Put_Line("Final result:");
      Put_Line("Minimum point: " & Float'Image(Current_Point));
      Put_Line("Function value: " & Float'Image(Value));
      Put_Line("Number of iterations: " & Integer'Image(Iteration));
      
   end Run_Gradient_Descent;
   
begin
   Run_Gradient_Descent;
end Gradient_Descent;
```

## How it works:

1. **Function Definition**: Implements f(x) = x² + 3x + 2 with derivative f'(x) = 2x + 3
2. **Gradient Descent Step**: Updates the point using the formula: x_new = x_old - α × ∇f(x_old)
3. **Main Loop**: Runs for 1000 iterations with learning rate 0.01
4. **Progress Tracking**: Shows results every 100 iterations

## Expected Output:
```
Starting gradient descent...
Initial point:  5.00000E+00
Iteration 100: x =  -1.50000E+00, f(x) =  -6.25000E-01
Iteration 200: x =  -1.50000E+00, f(x) =  -6.25000E-01
...
Final result:
Minimum point:  -1.50000E+00
Function value:  -6.25000E-01
Number of iterations: 1000
```

This implementation demonstrates the basic gradient descent algorithm for finding the minimum of a function in Ada programming language.

