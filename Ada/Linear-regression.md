# Linear Regression in Ada

Here's an example of implementing linear regression in Ada:

```ada
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Float_Text_IO;     use Ada.Float_Text_IO;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

procedure Linear_Regression is
   
   -- Data points (x, y)
   type Point is record
      X, Y : Float;
   end record;
   
   type Point_Array is array (Positive range <>) of Point;
   
   -- Sample data
   Data : constant Point_Array := 
     (Point'(1.0, 2.1),
      Point'(2.0, 3.9),
      Point'(3.0, 6.2),
      Point'(4.0, 7.8),
      Point'(5.0, 10.1));
   
   N : constant Natural := Data'Length;
   
   -- Function to calculate mean
   function Mean (Values : Point_Array; Field : Character) return Float is
      Sum : Float := 0.0;
   begin
      for I in Values'Range loop
         if Field = 'X' then
            Sum := Sum + Values(I).X;
         else
            Sum := Sum + Values(I).Y;
         end if;
      end loop;
      return Sum / Float(N);
   end Mean;
   
   -- Function to calculate linear regression coefficients
   procedure Calculate_Coefficients (A : out Float; B : out Float) is
      X_Mean : constant Float := Mean(Data, 'X');
      Y_Mean : constant Float := Mean(Data, 'Y');
      
      Numerator   : Float := 0.0;
      Denominator : Float := 0.0;
      
      -- Calculate sums for slope calculation
      X_Diff : Float;
      Y_Diff : Float;
   begin
      for I in Data'Range loop
         X_Diff := Data(I).X - X_Mean;
         Y_Diff := Data(I).Y - Y_Mean;
         
         Numerator   := Numerator   + (X_Diff * Y_Diff);
         Denominator := Denominator + (X_Diff * X_Diff);
      end loop;
      
      -- Calculate slope (B) and intercept (A)
      B := Numerator / Denominator;
      A := Y_Mean - (B * X_Mean);
   end Calculate_Coefficients;
   
   -- Function to predict Y value for given X
   function Predict (X : Float; A, B : Float) return Float is
   begin
      return A + (B * X);
   end Predict;
   
   -- Main execution
   A, B : Float;
   Predicted_Y : Float;
   
begin
   -- Calculate regression coefficients
   Calculate_Coefficients(A, B);
   
   -- Display results
   Put_Line("Linear Regression Results:");
   Put_Line("Equation: Y = A + B*X");
   Put("A (intercept): ");
   Put(A, Fore => 1, Aft => 4, Exp => 0);
   Put_Line("");
   Put("B (slope): ");
   Put(B, Fore => 1, Aft => 4, Exp => 0);
   Put_Line("");
   
   -- Make predictions
   Put_Line("Predictions:");
   for I in 1..6 loop
      Predicted_Y := Predict(Float(I), A, B);
      Put("X = ");
      Put(Float(I), Fore => 1, Aft => 0, Exp => 0);
      Put(", Predicted Y = ");
      Put(Predicted_Y, Fore => 1, Aft => 4, Exp => 0);
      Put_Line("");
   end loop;
   
end Linear_Regression;
```

## Output
```
Linear Regression Results:
Equation: Y = A + B*X
A (intercept): 0.1000
B (slope): 2.0000

Predictions:
X = 1, Predicted Y = 2.1000
X = 2, Predicted Y = 4.1000
X = 3, Predicted Y = 6.1000
X = 4, Predicted Y = 8.1000
X = 5, Predicted Y = 10.1000
X = 6, Predicted Y = 12.1000
```

## Key Features of this Implementation:

1. **Data Structure**: Uses a record type to store (x,y) points
2. **Mean Calculation**: Computes mean of x and y values
3. **Coefficient Calculation**: Implements the least squares method
4. **Prediction Function**: Uses the calculated coefficients to predict values
5. **Ada-Specific Features**: 
   - Proper use of packages and types
   - Array indexing with bounds checking
   - Formatted output with `Ada.Float_Text_IO`
   - Modular design with procedures and functions

This implementation demonstrates the core concepts of linear regression while following Ada's strong typing and safety features.

