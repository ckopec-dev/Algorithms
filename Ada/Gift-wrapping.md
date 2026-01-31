# Gift Wrapping Algorithm in Ada

Here's an implementation of the Gift Wrapping (Jarvis March) algorithm for finding the convex hull of a set of points in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

procedure Gift_Wrapping is
   
   type Point is record
      X, Y : Float;
   end record;
   
   type Point_Array is array (Positive range <>) of Point;
   
   -- Function to calculate cross product of three points
   function Cross_Product(P1, P2, P3 : Point) return Float is
   begin
      return (P2.X - P1.X) * (P3.Y - P1.Y) - (P2.Y - P1.Y) * (P3.X - P1.X);
   end Cross_Product;
   
   -- Function to find the leftmost point
   function Find_Leftmost_Point(Points : Point_Array) return Positive is
      Min_X : Float := Points(1).X;
      Index : Positive := 1;
   begin
      for I in Points'First + 1 .. Points'Last loop
         if Points(I).X < Min_X then
            Min_X := Points(I).X;
            Index := I;
         end if;
      end loop;
      return Index;
   end Find_Leftmost_Point;
   
   -- Function to find the next point in the hull
   function Find_Next_Point(Points : Point_Array; Current_Point, Start_Point : Positive) 
      return Positive is
      Next_Point : Positive := Points'First;
      Max_Cross : Float := -1.0;
   begin
      for I in Points'First .. Points'Last loop
         if I /= Current_Point then
            declare
               Cross : Float := Cross_Product(Points(Current_Point), Points(I), Points(Next_Point));
            begin
               if Cross > Max_Cross or (Cross = Max_Cross and 
                  (Points(I).X - Points(Current_Point).X) * (Points(Next_Point).Y - Points(Current_Point).Y) < 
                  (Points(Next_Point).X - Points(Current_Point).X) * (Points(I).Y - Points(Current_Point).Y)) then
                  Max_Cross := Cross;
                  Next_Point := I;
               end if;
            end;
         end if;
      end loop;
      return Next_Point;
   end Find_Next_Point;
   
   -- Main Gift Wrapping algorithm
   procedure Convex_Hull(Points : Point_Array; Hull : out Point_Array) is
      Leftmost : Positive := Find_Leftmost_Point(Points);
      Current_Point : Positive := Leftmost;
      Start_Point : Positive := Leftmost;
      Index : Positive := 1;
   begin
      loop
         Hull(Index) := Points(Current_Point);
         Index := Index + 1;
         
         Current_Point := Find_Next_Point(Points, Current_Point, Start_Point);
         
         -- Stop when we return to the starting point
         exit when Current_Point = Start_Point;
      end loop;
   end Convex_Hull;
   
   -- Procedure to print points
   procedure Print_Points(Points : Point_Array) is
   begin
      for I in Points'First .. Points'Last loop
         Put("Point " & Integer'Image(I) & ": (");
         Put(Points(I).X, Fore => 1, Aft => 2, Exp => 0);
         Put(", ");
         Put(Points(I).Y, Fore => 1, Aft => 2, Exp => 0);
         Put_Line(")");
      end loop;
   end Print_Points;
   
   -- Sample points
   Points : Point_Array(1..8) := (
      (X => 0.0, Y => 3.0),
      (X => 1.0, Y => 1.0),
      (X => 2.0, Y => 2.0),
      (X => 4.0, Y => 4.0),
      (X => 0.0, Y => 0.0),
      (X => 1.0, Y => 2.0),
      (X => 3.0, Y => 1.0),
      (X => 3.0, Y => 3.0)
   );
   
   Hull : Point_Array(1..8);
   
begin
   Put_Line("Input Points:");
   Print_Points(Points);
   
   Convex_Hull(Points, Hull);
   
   Put_Line("Convex Hull Points:");
   for I in 1..8 loop
      if Hull(I).X = 0.0 and Hull(I).Y = 0.0 then
         exit;
      end if;
      Put("Point: (");
      Put(Hull(I).X, Fore => 1, Aft => 2, Exp => 0);
      Put(", ");
      Put(Hull(I).Y, Fore => 1, Aft => 2, Exp => 0);
      Put_Line(")");
   end loop;
   
end Gift_Wrapping;
```

## Algorithm Explanation

The Gift Wrapping algorithm (also known as Jarvis March) works by:

1. **Finding the starting point**: The leftmost point in the set
2. **Building the hull**: Starting from the leftmost point, it finds the next point by selecting the point that makes the smallest counterclockwise angle with the current edge
3. **Continuing**: It continues this process until it returns to the starting point

## Key Components

- **Cross_Product function**: Calculates the cross product to determine the orientation of three points
- **Find_Leftmost_Point**: Identifies the starting point of the hull
- **Find_Next_Point**: Determines the next point in the hull construction
- **Convex_Hull procedure**: Implements the main algorithm logic

## Time Complexity
- **Time**: O(nh) where n is the number of input points and h is the number of points on the hull
- **Space**: O(h) for storing the hull points

## Output Example
The algorithm will output the points that form the convex hull of the input set, effectively "wrapping" all points in the smallest convex polygon.

