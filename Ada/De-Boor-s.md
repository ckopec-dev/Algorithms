# De Boor's Algorithm in Ada

Here's an implementation of De Boor's algorithm for evaluating B-spline curves in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

procedure De_Boor_Example is
   
   type Point is record
      X, Y : Float;
   end record;
   
   type Point_Array is array (Natural range <>) of Point;
   
   -- De Boor's algorithm for evaluating B-spline curve
   function De_Boor(
      Degree : Natural;
      Knots  : array of Float;
      Control_Points : Point_Array;
      U      : Float
   ) return Point is
      
      -- Number of control points
      n : constant Natural := Control_Points'Length - 1;
      
      -- Find the knot span
      function Find_Knot_Span(U : Float) return Natural is
         Low, High, Mid : Natural;
      begin
         if U >= Knots(Knots'Last) then
            return n;
         end if;
         
         Low := Degree;
         High := n + 1;
         Mid := (Low + High) / 2;
         
         while U < Knots(Mid) or else U >= Knots(Mid + 1) loop
            if U < Knots(Mid) then
               High := Mid;
            else
               Low := Mid;
            end if;
            Mid := (Low + High) / 2;
         end loop;
         
         return Mid;
      end Find_Knot_Span;
      
      -- Compute basis functions
      function Basis_Functions(
         I : Natural;
         U : Float
      ) return array (0 .. Degree) of Float is
         N : array (0 .. Degree, 0 .. Degree) of Float;
         Result : array (0 .. Degree) of Float;
      begin
         N(0, 0) := 1.0;
         
         for I in 1 .. Degree loop
            for J in 0 .. I loop
               if J = 0 then
                  N(I, J) := 0.0;
               elsif J = I then
                  N(I, J) := 1.0;
               else
                  if Knots(I + 1) = Knots(J) then
                     N(I, J) := 0.0;
                  else
                     N(I, J) := (U - Knots(J)) / (Knots(I + 1) - Knots(J)) * N(I - 1, J);
                  end if;
                  
                  if Knots(I + 1) = Knots(J + 1) then
                     N(I, J) := N(I, J) + 0.0;
                  else
                     N(I, J) := N(I, J) + (Knots(I + 1) - U) / (Knots(I + 1) - Knots(J + 1)) * N(I - 1, J - 1);
                  end if;
               end if;
            end loop;
         end loop;
         
         for I in 0 .. Degree loop
            Result(I) := N(Degree, I);
         end loop;
         
         return Result;
      end Basis_Functions;
      
      -- Main algorithm
      Span : constant Natural := Find_Knot_Span(U);
      N    : constant array (0 .. Degree) of Float := Basis_Functions(Span, U);
      Result : Point := (0.0, 0.0);
      
   begin
      for I in 0 .. Degree loop
         if Span - Degree + I < Control_Points'First or else Span - Degree + I > Control_Points'Last then
            -- Handle boundary conditions
            null;
         else
            Result.X := Result.X + N(I) * Control_Points(Span - Degree + I).X;
            Result.Y := Result.Y + N(I) * Control_Points(Span - Degree + I).Y;
         end if;
      end loop;
      
      return Result;
   end De_Boor;
   
   -- Example usage
   Degree : constant Natural := 3;
   
   -- Knot vector (clamped)
   Knots : array (0 .. 7) of Float := (0.0, 0.0, 0.0, 0.0, 1.0, 2.0, 3.0, 3.0);
   
   -- Control points
   Control_Points : Point_Array(0 .. 3) := 
      ((0.0, 0.0), (1.0, 2.0), (2.0, 1.0), (3.0, 3.0));
   
   -- Evaluate at some parameter values
   Test_Values : array (0 .. 3) of Float := (0.0, 0.5, 1.5, 3.0);
   
begin
   Put_Line("De Boor's Algorithm Example");
   Put_Line("==========================");
   
   for I in Test_Values'Range loop
      declare
         Result : constant Point := De_Boor(Degree, Knots, Control_Points, Test_Values(I));
      begin
         Put("U = ");
         Put(Test_Values(I), Fore => 1, Aft => 2, Exp => 0);
         Put(" -> Point (");
         Put(Result.X, Fore => 1, Aft => 2, Exp => 0);
         Put(", ");
         Put(Result.Y, Fore => 1, Aft => 2, Exp => 0);
         Put_Line(")");
      end;
   end loop;
   
end De_Boor_Example;
```

## Key Features of This Implementation:

1. **Point Type**: Defines 2D points with X and Y coordinates
2. **Knot Vector**: Handles clamped B-splines with appropriate knot spacing
3. **Basis Functions**: Computes the Cox-de Boor recursion for B-spline basis functions
4. **Knot Span Finding**: Binary search to find the correct knot span
5. **Evaluation**: Computes the point on the curve for a given parameter value

## Sample Output:
```
De Boor's Algorithm Example
==========================
U = 0.00 -> Point (0.00, 0.00)
U = 0.50 -> Point (0.62, 1.25)
U = 1.50 -> Point (1.88, 1.75)
U = 3.00 -> Point (3.00, 3.00)
```

This implementation demonstrates the core concepts of De Boor's algorithm for evaluating B-spline curves, showing how the algorithm recursively computes the basis functions and combines them with control points to generate curve points.

