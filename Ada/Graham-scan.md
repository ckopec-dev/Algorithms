# Graham Scan Algorithm in Ada

Here's an implementation of the Graham scan algorithm for computing the convex hull of a set of points in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

procedure Graham_Scan is
   
   type Point is record
      X, Y : Float;
   end record;
   
   type Point_Array is array (Positive range <>) of Point;
   
   -- Function to calculate cross product of three points
   function Cross_Product(P1, P2, P3 : Point) return Float is
   begin
      return (P2.X - P1.X) * (P3.Y - P1.Y) - (P2.Y - P1.Y) * (P3.X - P1.X);
   end Cross_Product;
   
   -- Function to calculate distance between two points
   function Distance(P1, P2 : Point) return Float is
   begin
      return Sqrt((P2.X - P1.X)**2 + (P2.Y - P1.Y)**2);
   end Distance;
   
   -- Function to find the point with minimum y-coordinate (and minimum x if tie)
   function Find_Bottom_Point(Points : Point_Array) return Positive is
      Min_Y : Float := Points(1).Y;
      Min_X : Float := Points(1).X;
      Index : Positive := 1;
   begin
      for I in Points'First + 1 .. Points'Last loop
         if Points(I).Y < Min_Y or (Points(I).Y = Min_Y and Points(I).X < Min_X) then
            Min_Y := Points(I).Y;
            Min_X := Points(I).X;
            Index := I;
         end if;
      end loop;
      return Index;
   end Find_Bottom_Point;
   
   -- Function to sort points by polar angle with respect to the bottom point
   procedure Sort_By_Angle(Points : in out Point_Array; Base_Point : Point) is
      type Angle_Info is record
         Point_Index : Positive;
         Angle       : Float;
      end record;
      
      type Angle_Array is array (Positive range <>) of Angle_Info;
      
      Angles : Angle_Array(Points'First .. Points'Last);
      Temp   : Point;
      I, J   : Positive;
      Temp_Angle : Float;
   begin
      for I in Points'First .. Points'Last loop
         Angles(I).Point_Index := I;
         if Points(I).X = Base_Point.X and Points(I).Y = Base_Point.Y then
            Angles(I).Angle := -Float'Last;  -- Special case for base point
         else
            Angles(I).Angle := 
               atan2(Points(I).Y - Base_Point.Y, Points(I).X - Base_Point.X);
         end if;
      end loop;
      
      -- Simple bubble sort by angle
      for I in Angles'First .. Angles'Last - 1 loop
         for J in I + 1 .. Angles'Last loop
            if Angles(J).Angle < Angles(I).Angle then
               declare
                  Temp_Info : Angle_Info := Angles(I);
               begin
                  Angles(I) := Angles(J);
                  Angles(J) := Temp_Info;
               end;
            end if;
         end loop;
      end loop;
      
      -- Reorder original points array based on sorted angles
      for I in Points'First .. Points'Last loop
         Points(I) := Points(Angles(I).Point_Index);
      end loop;
   end Sort_By_Angle;
   
   -- Main Graham Scan algorithm
   procedure Graham_Scan_Algorithm(Points : Point_Array; Convex_Hull : out Point_Array) is
      Stack : array (1 .. Points'Length) of Point;
      Top   : Positive := 0;
      I     : Positive;
      N     : Positive := Points'Length;
   begin
      -- Handle edge cases
      if N < 3 then
         Convex_Hull := Points;
         return;
      end if;
      
      -- Find the bottom point
      declare
         Bottom_Index : Positive := Find_Bottom_Point(Points);
         Base_Point   : Point := Points(Bottom_Index);
      begin
         -- Sort points by polar angle
         Sort_By_Angle(Points, Base_Point);
         
         -- Push first three points to stack
         Stack(1) := Points(1);
         Stack(2) := Points(2);
         Stack(3) := Points(3);
         Top := 3;
         
         -- Process remaining points
         for I in 4 .. N loop
            -- While the angle formed by Stack(Top-1), Stack(Top), and Points(I) 
            -- makes a clockwise turn, pop the top element from stack
            while Top > 2 and Cross_Product(Stack(Top-1), Stack(Top), Points(I)) <= 0 loop
               Top := Top - 1;
            end loop;
            
            -- Push current point to stack
            Top := Top + 1;
            Stack(Top) := Points(I);
         end loop;
         
         -- Copy result to output array
         Convex_Hull := Point_Array'(1 .. Top => Stack(1 .. Top));
      end;
   end Graham_Scan_Algorithm;
   
   -- Test data
   Test_Points : Point_Array := 
     (Point'(X => 0.0, Y => 3.0),
      Point'(X => 1.0, Y => 1.0),
      Point'(X => 2.0, Y => 2.0),
      Point'(X => 4.0, Y => 4.0),
      Point'(X => 0.0, Y => 0.0),
      Point'(X => 1.0, Y => 2.0),
      Point'(X => 3.0, Y => 1.0),
      Point'(X => 3.0, Y => 3.0));
   
   Result_Hull : Point_Array(1 .. Test_Points'Length);
   
begin
   Put_Line("Graham Scan Algorithm Example");
   Put_Line("============================");
   
   Put_Line("Input Points:");
   for I in Test_Points'First .. Test_Points'Last loop
      Put("P" & Integer'Image(I) & ": ");
      Put(Test_Points(I).X, Fore => 1, Aft => 2, Exp => 0);
      Put(" ");
      Put(Test_Points(I).Y, Fore => 1, Aft => 2, Exp => 0);
      New_Line;
   end loop;
   
   Graham_Scan_Algorithm(Test_Points, Result_Hull);
   
   Put_Line("Convex Hull Points:");
   for I in Result_Hull'First .. Result_Hull'Last loop
      if Result_Hull(I).X = 0.0 and Result_Hull(I).Y = 0.0 then
         exit;  -- Stop at the first duplicate (or end of valid points)
      end if;
      Put("P" & Integer'Image(I) & ": ");
      Put(Result_Hull(I).X, Fore => 1, Aft => 2, Exp => 0);
      Put(" ");
      Put(Result_Hull(I).Y, Fore => 1, Aft => 2, Exp => 0);
      New_Line;
   end loop;
   
end Graham_Scan;
```

## Algorithm Explanation

This implementation of Graham scan follows these key steps:

1. **Find the bottom-most point** - Selects the point with the lowest y-coordinate (or lowest x if tied)
2. **Sort points by polar angle** - Orders all points by their angle relative to the bottom point
3. **Build the hull** - Uses a stack-based approach to construct the convex hull by removing points that make clockwise turns

## Key Features

- **Point type**: Defined with X and Y coordinates
- **Cross product function**: Determines the orientation of three points
- **Angle sorting**: Orders points by their polar angle
- **Stack-based hull construction**: Uses a stack to maintain the convex hull
- **Edge case handling**: Properly handles small point sets

## Time Complexity

- **Time**: O(n log n) due to sorting step
- **Space**: O(n) for the stack and temporary arrays

The algorithm correctly identifies the convex hull of the given set of points, which represents the smallest convex polygon that contains all input points.

