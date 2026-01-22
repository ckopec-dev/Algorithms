# Bezier Curve Generation in Ada

Here's an example of a Bezier curve generation algorithm implemented in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

procedure Bezier_Curve is

   -- Define a point type
   type Point is record
      X, Y : Float;
   end record;

   -- Define a Bezier curve as a set of control points
   type Control_Points is array (0 .. 3) of Point;

   -- Calculate factorial
   function Factorial (N : Natural) return Natural is
   begin
      if N <= 1 then
         return 1;
      else
         return N * Factorial (N - 1);
      end if;
   end Factorial;

   -- Calculate binomial coefficient C(n,k)
   function Binomial_Coefficient (N, K : Natural) return Natural is
   begin
      return Factorial (N) / (Factorial (K) * Factorial (N - K));
   end Binomial_Coefficient;

   -- Calculate Bernstein polynomial B(i,n)(t)
   function Bernstein_Polynomial (I, N : Natural; T : Float) return Float is
      Coeff : constant Float := Float (Binomial_Coefficient (N, I));
      T_Power : Float := 1.0;
      One_Minus_T_Power : Float := 1.0;
   begin
      for J in 1 .. I loop
         T_Power := T_Power * T;
      end loop;
      
      for J in 1 .. (N - I) loop
         One_Minus_T_Power := One_Minus_T_Power * (1.0 - T);
      end loop;
      
      return Coeff * T_Power * One_Minus_T_Power;
   end Bernstein_Polynomial;

   -- Calculate a point on the Bezier curve
   function Bezier_Point (Control : Control_Points; T : Float) return Point is
      Result : Point := (0.0, 0.0);
   begin
      for I in 0 .. 3 loop
         declare
            Bernstein : constant Float := Bernstein_Polynomial (I, 3, T);
         begin
            Result.X := Result.X + Bernstein * Control(I).X;
            Result.Y := Result.Y + Bernstein * Control(I).Y;
         end;
      end loop;
      return Result;
   end Bezier_Point;

   -- Generate points along the Bezier curve
   procedure Generate_Curve (Control : Control_Points; Num_Points : Natural) is
      Step : constant Float := 1.0 / Float (Num_Points - 1);
   begin
      Put_Line ("Bezier Curve Points:");
      Put_Line ("---------------------");
      
      for I in 0 .. Num_Points - 1 loop
         declare
            T : constant Float := Float (I) * Step;
            Point : constant Point := Bezier_Point (Control, T);
         begin
            Put ("T = ");
            Put (T, Fore => 1, Aft => 3, Exp => 0);
            Put (" -> (");
            Put (Point.X, Fore => 1, Aft => 2, Exp => 0);
            Put (", ");
            Put (Point.Y, Fore => 1, Aft => 2, Exp => 0);
            Put_Line (")");
         end;
      end loop;
   end Generate_Curve;

   -- Example control points for a cubic Bezier curve
   Control_Points_Example : constant Control_Points := (
      (0.0, 0.0),   -- Start point
      (1.0, 2.0),   -- Control point 1
      (2.0, 1.0),   -- Control point 2
      (3.0, 0.0)    -- End point
   );

begin
   Put_Line ("Cubic Bezier Curve Generation");
   Put_Line ("============================");
   
   -- Generate 11 points along the curve (including start and end)
   Generate_Curve (Control_Points_Example, 11);
   
   Put_Line ("");
   Put_Line ("Additional example with different control points:");
   
   -- Another example
   declare
      Control_Points_2 : constant Control_Points := (
         (0.0, 0.0),
         (0.5, 1.0),
         (1.5, 1.0),
         (2.0, 0.0)
      );
   begin
      Generate_Curve (Control_Points_2, 7);
   end;
end Bezier_Curve;
```

## Key Features of this Implementation:

1. **Point Type**: Defines a simple 2D point with X and Y coordinates
2. **Factorial Function**: Recursive implementation for calculating factorials
3. **Binomial Coefficient**: Calculates C(n,k) using factorials
4. **Bernstein Polynomial**: Implements the mathematical formula for Bernstein basis polynomials
5. **Bezier Point Calculation**: Computes a single point on the curve for a given parameter t
6. **Curve Generation**: Creates multiple points along the entire curve
7. **Example Usage**: Demonstrates with two different sets of control points

## Mathematical Background:

The Bezier curve is defined by:
```
B(t) = Σ(i=0 to n) C(n,i) * (1-t)^(n-i) * t^i * P_i
```

Where:
- C(n,i) is the binomial coefficient
- t is the parameter (0 ≤ t ≤ 1)
- P_i are the control points
- n is the degree of the curve (3 for cubic)

This implementation generates a cubic Bezier curve (degree 3) using four control points, which is commonly used in computer graphics and design applications.

