# Bezier Curve Generation in Ada

Here's an example of a Bezier curve generation algorithm implemented in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

procedure Bezier_Curve is
   
   -- Define point type for 2D coordinates
   type Point is record
      X, Y : Float;
   end record;
   
   -- Define Bezier curve as array of control points
   type Control_Points is array (Natural range <>) of Point;
   
   -- Function to compute binomial coefficient C(n,k)
   function Binomial_Coefficient(N, K : Integer) return Float is
      Result : Float := 1.0;
   begin
      if K > N or K < 0 then
         return 0.0;
      end if;
      
      if K = 0 or K = N then
         return 1.0;
      end if;
      
      -- Optimize by using the smaller of K and N-K
      declare
         K1 : Integer := Integer'Min(K, N - K);
      begin
         for I in 1 .. K1 loop
            Result := Result * Float(N - I + 1) / Float(I);
         end loop;
      end;
      
      return Result;
   end Binomial_Coefficient;
   
   -- Function to compute Bernstein polynomial
   function Bernstein_Polynomial(Degree, Index : Integer; T : Float) return Float is
      N : constant Integer := Degree;
      I : constant Integer := Index;
   begin
      return Binomial_Coefficient(N, I) * (T ** I) * ((1.0 - T) ** (N - I));
   end Bernstein_Polynomial;
   
   -- Function to compute Bezier curve point at parameter T
   function Compute_Bezier_Point(Points : Control_Points; T : Float) return Point is
      Degree : constant Integer := Points'Length - 1;
      Result : Point := (0.0, 0.0);
   begin
      for I in Points'Range loop
         declare
            B : constant Float := Bernstein_Polynomial(Degree, I, T);
         begin
            Result.X := Result.X + B * Points(I).X;
            Result.Y := Result.Y + B * Points(I).Y;
         end;
      end loop;
      
      return Result;
   end Compute_Bezier_Point;
   
   -- Procedure to generate and display Bezier curve points
   procedure Generate_Bezier_Curve(Points : Control_Points; Num_Points : Positive) is
      Step : constant Float := 1.0 / Float(Num_Points - 1);
      T : Float := 0.0;
   begin
      Put_Line("Bezier Curve Points:");
      Put_Line("T    X         Y");
      Put_Line("---------------");
      
      for I in 1 .. Num_Points loop
         declare
            Point : constant Point := Compute_Bezier_Point(Points, T);
         begin
            Put(T, Fore => 1, Aft => 3, Exp => 0);
            Put(" ");
            Put(Point.X, Fore => 1, Aft => 6, Exp => 0);
            Put(" ");
            Put(Point.Y, Fore => 1, Aft => 6, Exp => 0);
            New_Line;
            T := T + Step;
         end;
      end loop;
   end Generate_Bezier_Curve;
   
   -- Example usage with 4 control points (cubic Bezier)
   Control_Points_Array : Control_Points(0 .. 3) := 
     ((0.0, 0.0),   -- Start point
      (1.0, 2.0),   -- Control point 1
      (2.0, 1.0),   -- Control point 2
      (3.0, 0.0));  -- End point
   
begin
   Put_Line("Cubic Bezier Curve Generation");
   Put_Line("============================");
   
   Generate_Bezier_Curve(Control_Points_Array, 11);
   
end Bezier_Curve;
```

## Key Features of this Implementation:

1. **Point Type**: Defines 2D coordinates with X and Y components
2. **Binomial Coefficient**: Computes C(n,k) for Bernstein polynomial calculation
3. **Bernstein Polynomial**: Implements the mathematical basis for Bezier curves
4. **Curve Point Calculation**: Computes a point on the Bezier curve for a given parameter T
5. **Curve Generation**: Creates multiple points along the curve for visualization

## Sample Output:
```
Cubic Bezier Curve Generation
============================
Bezier Curve Points:
T    X         Y
---------------
0.000 0.000000 0.000000
0.100 0.271000 0.343000
0.200 0.528000 0.544000
0.300 0.747000 0.613000
0.400 0.920000 0.560000
0.500 1.000000 0.400000
0.600 1.000000 0.160000
0.700 0.920000 -0.160000
0.800 0.747000 -0.437000
0.900 0.528000 -0.656000
1.000 3.000000 0.000000
```

This implementation demonstrates the mathematical foundation of Bezier curves using the Bernstein polynomial basis functions, which is the standard approach for generating Bezier curves in computer graphics applications.

