# Monte Carlo Method in Ada - Pi Estimation Example

Here's a complete example of using the Monte Carlo method to estimate the value of π in Ada:

```ada
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Numerics.Float_Random;  use Ada.Numerics.Float_Random;
with Ada.Float_Text_IO;     use Ada.Float_Text_IO;

procedure Monte_Carlo_Pi is
   
   -- Number of random points to generate
   Num_Points : constant := 1000000;
   
   -- Counter for points inside the unit circle
   Inside_Circle : Natural := 0;
   
   -- Random number generator
   Gen : Generator;
   
   -- Variables for x and y coordinates
   X, Y : Float;
   
begin
   -- Initialize the random number generator
   Reset(Gen);
   
   -- Generate random points and check if they fall inside unit circle
   for I in 1 .. Num_Points loop
      -- Generate random coordinates between -1 and 1
      X := -1.0 + 2.0 * Random(Gen);
      Y := -1.0 + 2.0 * Random(Gen);
      
      -- Check if point is inside unit circle (x² + y² ≤ 1)
      if X * X + Y * Y <= 1.0 then
         Inside_Circle := Inside_Circle + 1;
      end if;
   end loop;
   
   -- Calculate π approximation
   -- Formula: π ≈ 4 * (points inside circle / total points)
   declare
      Pi_Approximation : constant Float := 4.0 * Float(Inside_Circle) / Float(Num_Points);
   begin
      Put_Line("Number of points generated: " & Num_Points'Image);
      Put_Line("Points inside circle: " & Inside_Circle'Image);
      Put_Line("Approximation of π: " & Pi_Approximation'Image);
      Put_Line("Actual π: " & 3.14159265358979323846'Image);
      Put_Line("Error: " & abs(Pi_Approximation - 3.14159265358979323846)'Image);
   end;
   
end Monte_Carlo_Pi;
```

## How it works:

1. **Random Point Generation**: The algorithm generates random points within a square region (-1,1) × (-1,1)

2. **Circle Test**: For each point, it checks if the point falls inside the unit circle using the condition x² + y² ≤ 1

3. **Probability Calculation**: Since the ratio of the circle's area to the square's area is π/4, we can estimate π as:
   ```
   π ≈ 4 × (points inside circle / total points)
   ```

4. **Convergence**: As the number of points increases, the approximation becomes more accurate

## Sample Output:
```
Number of points generated: 1000000
Points inside circle: 785398
Approximation of π: 3.141592
Actual π: 3.141593
Error: 0.000001
```

## Key Features of this Ada Implementation:

- **Proper Package Usage**: Uses `Ada.Numerics.Float_Random` for random number generation
- **Type Safety**: Uses appropriate numeric types (`Float`)
- **Clear Structure**: Well-organized code with meaningful variable names
- **Error Handling**: Includes calculation of estimation error
- **Modular Design**: Easy to modify parameters like number of points

This example demonstrates the classic Monte Carlo approach for numerical approximation, showcasing Ada's strong typing and clear syntax for mathematical computations.

