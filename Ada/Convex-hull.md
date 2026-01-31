# Convex Hull Algorithm in Ada

Here's an implementation of the Graham Scan algorithm for computing the convex hull of a set of points in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

procedure Convex_Hull is
   
   type Point is record
      X, Y : Float;
   end record;
   
   type Point_Array is array (Positive range <>) of Point;
   
   -- Function to calculate cross product of three points
   function Cross_Product(P1, P2, P3 : Point) return Float is
   begin
      return (P2.X - P1.X) * (P3.Y - P1.Y) - (P2.Y - P1.Y) * (P3.X - P1.X);
   end Cross_Product;
   
   -- Function to calculate squared distance between two points
   function Distance_Squared(P1, P2 : Point) return Float is
   begin
      return (P2.X - P1.X)**2 + (P2.Y - P1.Y)**2;
   end Distance_Squared;
   
   -- Function to find the point with minimum y-coordinate (and minimum x if tie)
   function Find_Bottom_Point(Points : Point_Array) return Positive is
      Min_Y : Float := Points(1).Y;
      Min_X : Float := Points(1).X;
      Index : Positive := 1;
   begin
      for I in Points'First + 1 .. Points'Last loop
         if Points(I).Y < Min_Y or (Points(I).Y = Min_Y and then Points(I).X < Min_X) then
            Min_Y := Points(I).Y;
            Min_X := Points(I).X;
            Index := I;
         end if;
      end loop;
      return Index;
   end Find_Bottom_Point;
   
   -- Function to sort points by polar angle
   procedure Sort_By_Angle(Points : in out Point_Array; Base_Point : Point) is
      procedure Swap(I, J : in Positive) is
         Temp : Point := Points(I);
      begin
         Points(I) := Points(J);
         Points(J) := Temp;
      end Swap;
      
      procedure Quick_Sort(Low, High : in Positive) is
         Pivot : Point := Points(High);
         I : Positive := Low - 1;
         J : Positive;
      begin
         for J in Low .. High - 1 loop
            -- Compare angles using cross product
            if Cross_Product(Base_Point, Points(J), Pivot) >= 0 then
               I := I + 1;
               Swap(I, J);
            end if;
         end loop;
         Swap(I + 1, High);
         if I + 1 > Low then
            Quick_Sort(Low, I);
         end if;
         if I + 2 < High then
            Quick_Sort(I + 2, High);
         end if;
      end Quick_Sort;
   begin
      Quick_Sort(Points'First, Points'Last);
   end Sort_By_Angle;
   
   -- Graham Scan algorithm
   function Graham_Scan(Points : Point_Array) return Point_Array is
      Hull : Point_Array(1 .. Points'Length);
      Hull_Size : Positive := 0;
      Base_Point_Index : Positive;
   begin
      if Points'Length < 3 then
         return Points;
      end if;
      
      -- Find the bottom-most point
      Base_Point_Index := Find_Bottom_Point(Points);
      
      -- Sort points by polar angle with base point
      Sort_By_Angle(Points, Points(Base_Point_Index));
      
      -- Initialize hull with first three points
      Hull(1) := Points(1);
      Hull(2) := Points(2);
      Hull(3) := Points(3);
      Hull_Size := 3;
      
      -- Process remaining points
      for I in Points'First + 3 .. Points'Last loop
         -- Remove points that make clockwise turn
         while Hull_Size >= 2 and then 
               Cross_Product(Hull(Hull_Size - 1), Hull(Hull_Size), Points(I)) <= 0 loop
            Hull_Size := Hull_Size - 1;
         end loop;
         
         Hull_Size := Hull_Size + 1;
         Hull(Hull_Size) := Points(I);
      end loop;
      
      return Hull(1 .. Hull_Size);
   end Graham_Scan;
   
   -- Test data
   Points : Point_Array(1 .. 8) := (
      (X => 0.0, Y => 3.0),
      (X => 1.0, Y => 1.0),
      (X => 2.0, Y => 2.0),
      (X => 4.0, Y => 4.0),
      (X => 0.0, Y => 0.0),
      (X => 1.0, Y => 2.0),
      (X => 3.0, Y => 1.0),
      (X => 3.0, Y => 3.0)
   );
   
   Hull : Point_Array;
   
begin
   Put_Line("Computing Convex Hull...");
   Put_Line("Input points:");
   for I in Points'First .. Points'Last loop
      Put("Point "); Put(I); Put(": (");
      Put(Points(I).X, Fore => 1, Aft => 1, Exp => 0);
      Put(", ");
      Put(Points(I).Y, Fore => 1, Aft => 1, Exp => 0);
      Put_Line(")");
   end loop;
   
   Hull := Graham_Scan(Points);
   
   Put_Line("Convex Hull points:");
   for I in Hull'First .. Hull'Last loop
      Put("Hull Point "); Put(I); Put(": (");
      Put(Hull(I).X, Fore => 1, Aft => 1, Exp => 0);
      Put(", ");
      Put(Hull(I).Y, Fore => 1, Aft => 1, Exp => 0);
      Put_Line(")");
   end loop;
   
end Convex_Hull;
```

## Key Features of this Implementation:

1. **Point Structure**: Defines a `Point` record with X and Y coordinates
2. **Cross Product**: Used to determine the orientation of three points
3. **Sorting**: Sorts points by polar angle relative to the bottom-most point
4. **Graham Scan Algorithm**: Implements the standard convex hull algorithm
5. **Edge Cases**: Handles cases with fewer than 3 points

## Algorithm Steps:

1. Find the bottom-most point (or leftmost if tie)
2. Sort all points by polar angle with the bottom-most point
3. Process each point in sorted order, maintaining a stack of hull points
4. Remove points that create clockwise turns (using cross product)
5. Return the final convex hull points

This implementation demonstrates Ada's strong typing, array handling, and procedural programming capabilities while solving a classic computational geometry problem.

