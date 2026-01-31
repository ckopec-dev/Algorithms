# Jarvis March Algorithm in Ada

Here's an implementation of the Jarvis March (Gift Wrapping) algorithm for computing the convex hull of a set of points in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

procedure Jarvis_March is
   
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
   
   -- Function to find the next point in convex hull
   function Next_Point(Points : Point_Array; Current_Point, Start_Point : Positive) 
      return Positive is
      Next_Index : Positive := Points'First;
      Min_Angle : Float := Float'Last;
   begin
      for I in Points'First .. Points'Last loop
         if I /= Current_Point then
            declare
               Angle : Float := Cross_Product(Points(Start_Point), Points(Current_Point), Points(I));
            begin
               if Angle < Min_Angle then
                  Min_Angle := Angle;
                  Next_Index := I;
               end if;
            end;
         end if;
      end loop;
      return Next_Index;
   end Next_Point;
   
   -- Main Jarvis March algorithm
   procedure Compute_Convex_Hull(Points : Point_Array; Hull : out Point_Array) is
      Leftmost_Index : Positive;
      Current_Point : Positive;
      Start_Point : Positive;
      Hull_Size : Positive := 1;
      Visited : array (Points'First .. Points'Last) of Boolean := (others => False);
   begin
      -- Find the leftmost point
      Leftmost_Index := Find_Leftmost_Point(Points);
      Current_Point := Leftmost_Index;
      Start_Point := Leftmost_Index;
      
      loop
         -- Add current point to hull
         Hull(Hull_Size) := Points(Current_Point);
         Hull_Size := Hull_Size + 1;
         Visited(Current_Point) := True;
         
         -- Find next point
         declare
            Next_Index : Positive := Next_Point(Points, Current_Point, Start_Point);
         begin
            -- If we've come back to start point, we're done
            if Next_Index = Start_Point then
               exit;
            end if;
            
            Current_Point := Next_Index;
         end;
      end loop;
      
      -- Adjust hull size
      Hull_Size := Hull_Size - 1;
   end Compute_Convex_Hull;
   
   -- Sample points
   Sample_Points : Point_Array(1..6) := 
     ((0.0, 3.0), (1.0, 1.0), (2.0, 2.0), (4.0, 4.0), 
      (0.0, 0.0), (1.0, 2.0));
   
   Convex_Hull : Point_Array(1..6);
   
begin
   Put_Line("Jarvis March Algorithm - Convex Hull Computation");
   Put_Line("================================================");
   
   Put_Line("Input Points:");
   for I in Sample_Points'First .. Sample_Points'Last loop
      Put("Point ");
      Put(I);
      Put(" : (");
      Put(Sample_Points(I).X, Fore => 1, Aft => 1, Exp => 0);
      Put(", ");
      Put(Sample_Points(I).Y, Fore => 1, Aft => 1, Exp => 0);
      Put_Line(")");
   end loop;
   
   -- Compute convex hull
   Compute_Convex_Hull(Sample_Points, Convex_Hull);
   
   Put_Line("Convex Hull Points:");
   for I in 1 .. Convex_Hull'Length loop
      if Convex_Hull(I).X /= 0.0 or Convex_Hull(I).Y /= 0.0 then
         Put("Hull Point ");
         Put(I);
         Put(" : (");
         Put(Convex_Hull(I).X, Fore => 1, Aft => 1, Exp => 0);
         Put(", ");
         Put(Convex_Hull(I).Y, Fore => 1, Aft => 1, Exp => 0);
         Put_Line(")");
      end if;
   end loop;
   
end Jarvis_March;
```

## Algorithm Explanation

The Jarvis March algorithm works as follows:

1. **Find the starting point**: Identify the leftmost point (or lowest point if there's a tie)
2. **Build the hull**: Starting from the leftmost point, find the next point by selecting the point that makes the smallest counterclockwise angle with the current edge
3. **Continue until closure**: Repeat until we return to the starting point

## Key Components

- **Cross Product Function**: Used to determine the orientation of three points
- **Point Structure**: Simple record to store 2D coordinates
- **Main Algorithm**: Implements the gift wrapping approach step by step

## Time Complexity
- **Time**: O(nh) where n is the number of input points and h is the number of points on the hull
- **Space**: O(h) for storing the hull points

This implementation demonstrates the classic Jarvis March algorithm for computing convex hulls in Ada programming language.

