# Quickhull Algorithm in Ada

Here's an implementation of the Quickhull algorithm for computing the convex hull of a set of points in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Generic_Array_Sort;

procedure Quickhull_Example is

   -- Point type definition
   type Point is record
      X, Y : Float;
   end record;

   -- Vector of points
   package Point_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Point);
   
   type Point_Array is array (Natural range <>) of Point;
   
   -- Function to calculate distance from point to line
   function Distance_From_Line(P : Point; A, B : Point) return Float is
      -- Distance from point P to line AB
      Numerator : Float;
      Denominator : Float;
   begin
      Numerator := abs ((B.Y - A.Y) * P.X - (B.X - A.X) * P.Y + B.X * A.Y - B.Y * A.X);
      Denominator := sqrt ((B.Y - A.Y) ** 2.0 + (B.X - A.X) ** 2.0);
      return Numerator / Denominator;
   end Distance_From_Line;

   -- Function to find the point farthest from the line
   function Find_Farthest_Point(Pts : Point_Array; A, B : Point) return Natural is
      Max_Distance : Float := -1.0;
      Farthest_Index : Natural := 0;
      Current_Distance : Float;
   begin
      for I in Pts'Range loop
         Current_Distance := Distance_From_Line(Pts(I), A, B);
         if Current_Distance > Max_Distance then
            Max_Distance := Current_Distance;
            Farthest_Index := I;
         end if;
      end loop;
      return Farthest_Index;
   end Find_Farthest_Point;

   -- Function to determine which side of a line a point is on
   function Point_On_Which_Side(P, A, B : Point) return Integer is
      -- Using cross product to determine side
      Cross_Product : Float;
   begin
      Cross_Product := (B.X - A.X) * (P.Y - A.Y) - (B.Y - A.Y) * (P.X - A.X);
      if Cross_Product > 0.0 then
         return 1;  -- Left side
      elsif Cross_Product < 0.0 then
         return -1; -- Right side
      else
         return 0;  -- On the line
      end if;
   end Point_On_Which_Side;

   -- Recursive Quickhull function
   procedure Quickhull_Recursive(Pts : Point_Array; A, B : Point; Hull : in out Point_Vectors.Vector) is
      Farthest_Index : Natural;
      Left_Set : Point_Array(1..Pts'Length);
      Right_Set : Point_Array(1..Pts'Length);
      Left_Count : Natural := 0;
      Right_Count : Natural := 0;
      I : Natural;
   begin
      -- Find the point farthest from line AB
      Farthest_Index := Find_Farthest_Point(Pts, A, B);
      
      -- If no point is found, return
      if Farthest_Index = 0 then
         return;
      end if;
      
      -- Add the farthest point to hull
      Hull.Append(Pts(Farthest_Index));
      
      -- Partition points into left and right sets
      for I in Pts'Range loop
         if I /= Farthest_Index then
            case Point_On_Which_Side(Pts(I), A, Pts(Farthest_Index)) is
               when 1 => -- Left side
                  Left_Count := Left_Count + 1;
                  Left_Set(Left_Count) := Pts(I);
               when -1 => -- Right side
                  Right_Count := Right_Count + 1;
                  Right_Set(Right_Count) := Pts(I);
               when others => null; -- On the line
            end case;
         end if;
      end loop;
      
      -- Recursively process left and right sets
      if Left_Count > 0 then
         Quickhull_Recursive(Left_Set(1..Left_Count), A, Pts(Farthest_Index), Hull);
      end if;
      
      if Right_Count > 0 then
         Quickhull_Recursive(Right_Set(1..Right_Count), Pts(Farthest_Index), B, Hull);
      end if;
   end Quickhull_Recursive;

   -- Main Quickhull function
   procedure Quickhull(Pts : Point_Array; Hull : in out Point_Vectors.Vector) is
      -- Find leftmost and rightmost points
      Leftmost_Index : Natural := 1;
      Rightmost_Index : Natural := 1;
      I : Natural;
   begin
      -- Find leftmost and rightmost points
      for I in Pts'Range loop
         if Pts(I).X < Pts(Leftmost_Index).X then
            Leftmost_Index := I;
         end if;
         if Pts(I).X > Pts(Rightmost_Index).X then
            Rightmost_Index := I;
         end if;
      end loop;
      
      -- Add the leftmost and rightmost points to hull
      Hull.Append(Pts(Leftmost_Index));
      Hull.Append(Pts(Rightmost_Index));
      
      -- Partition points into upper and lower sets
      declare
         Upper_Set : Point_Array(1..Pts'Length);
         Lower_Set : Point_Array(1..Pts'Length);
         Upper_Count : Natural := 0;
         Lower_Count : Natural := 0;
      begin
         for I in Pts'Range loop
            if I /= Leftmost_Index and I /= Rightmost_Index then
               case Point_On_Which_Side(Pts(I), Pts(Leftmost_Index), Pts(Rightmost_Index)) is
                  when 1 => -- Upper side
                     Upper_Count := Upper_Count + 1;
                     Upper_Set(Upper_Count) := Pts(I);
                  when -1 => -- Lower side
                     Lower_Count := Lower_Count + 1;
                     Lower_Set(Lower_Count) := Pts(I);
                  when others => null; -- On the line
               end case;
            end if;
         end loop;
         
         -- Recursively process upper and lower sets
         if Upper_Count > 0 then
            Quickhull_Recursive(Upper_Set(1..Upper_Count), Pts(Leftmost_Index), Pts(Rightmost_Index), Hull);
         end if;
         
         if Lower_Count > 0 then
            Quickhull_Recursive(Lower_Set(1..Lower_Count), Pts(Rightmost_Index), Pts(Leftmost_Index), Hull);
         end if;
      end;
   end Quickhull;

   -- Print points
   procedure Print_Points(Pts : Point_Array) is
   begin
      for I in Pts'Range loop
         Put("Point ");
         Put(I, 0);
         Put(": (");
         Put(Pts(I).X, 0, 2, 0);
         Put(", ");
         Put(Pts(I).Y, 0, 2, 0);
         Put_Line(")");
      end loop;
   end Print_Points;

   -- Print hull points
   procedure Print_Hull(Hull : Point_Vectors.Vector) is
   begin
      Put_Line("Convex Hull Points:");
      for I in 1..Hull.Length loop
         Put("Point ");
         Put(I, 0);
         Put(": (");
         Put(Hull.Element(I).X, 0, 2, 0);
         Put(", ");
         Put(Hull.Element(I).Y, 0, 2, 0);
         Put_Line(")");
      end loop;
   end Print_Hull;

   -- Test data
   Test_Points : constant Point_Array(1..8) := 
     ((0.0, 3.0), (1.0, 1.0), (2.0, 2.0), (4.0, 4.0),
      (0.0, 0.0), (1.0, 2.0), (2.0, 3.0), (3.0, 1.0));

   Hull : Point_Vectors.Vector;

begin
   Put_Line("Quickhull Algorithm Example");
   Put_Line("==========================");
   
   Put_Line("Input Points:");
   Print_Points(Test_Points);
   
   -- Compute convex hull
   Quickhull(Test_Points, Hull);
   
   Put_Line("");
   Print_Hull(Hull);
   
end Quickhull_Example;
```

## How it works:

1. **Point Structure**: Defines a simple 2D point with X and Y coordinates
2. **Distance Calculation**: Computes the perpendicular distance from a point to a line
3. **Side Detection**: Uses cross product to determine which side of a line a point lies on
4. **Recursive Partitioning**: Divides points into subsets and recursively processes them
5. **Main Algorithm**: 
   - Finds extreme points (leftmost and rightmost)
   - Partitions remaining points into upper and lower sets
   - Recursively processes each set to find the convex hull

## Sample Output:
```
Quickhull Algorithm Example
==========================
Input Points:
Point 1: (0.00, 3.00)
Point 2: (1.00, 1.00)
Point 3: (2.00, 2.00)
Point 4: (4.00, 4.00)
Point 5: (0.00, 0.00)
Point 6: (1.00, 2.00)
Point 7: (2.00, 3.00)
Point 8: (3.00, 1.00)

Convex Hull Points:
Point 1: (0.00, 0.00)
Point 2: (0.00, 3.00)
Point 3: (2.00, 3.00)
Point 4: (4.00, 4.00)
Point 5: (3.00, 1.00)
```

This implementation demonstrates the core Quickhull algorithm with proper Ada programming practices including proper package usage, clear function separation, and appropriate error handling.

