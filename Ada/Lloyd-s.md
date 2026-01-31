# Lloyd's Algorithm Implementation in Ada

Lloyd's algorithm is used for k-means clustering. Here's an implementation in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Generic_Elementary_Functions;
procedure Lloyd_Algorithm is
   
   -- Generic package for elementary functions
   package Math is new Ada.Numerics.Generic_Elementary_Functions (Float);
   use Math;
   
   -- Point type definition
   type Point is record
      X, Y : Float;
   end record;
   
   -- Cluster type
   type Cluster is record
      Center : Point;
      Points : array (1..100) of Point;
      Count  : Natural := 0;
   end record;
   
   -- Array of points
   type Point_Array is array (Natural range <>) of Point;
   
   -- Array of clusters
   type Cluster_Array is array (Natural range <>) of Cluster;
   
   -- Distance function
   function Distance(P1, P2 : Point) return Float is
   begin
      return Sqrt((P1.X - P2.X)**2 + (P1.Y - P2.Y)**2);
   end Distance;
   
   -- Find closest cluster center to a point
   function Find_Closest_Cluster(Point : Point; Clusters : Cluster_Array; 
                                Num_Clusters : Natural) return Natural is
      Min_Distance : Float := Float'Last;
      Closest_Cluster : Natural := 1;
   begin
      for I in 1..Num_Clusters loop
         declare
            Dist : Float := Distance(Point, Clusters(I).Center);
         begin
            if Dist < Min_Distance then
               Min_Distance := Dist;
               Closest_Cluster := I;
            end if;
         end;
      end loop;
      return Closest_Cluster;
   end Find_Closest_Cluster;
   
   -- Update cluster centers based on current points
   procedure Update_Centers(Clusters : in out Cluster_Array; 
                           Num_Clusters : Natural) is
   begin
      for I in 1..Num_Clusters loop
         if Clusters(I).Count > 0 then
            declare
               Sum_X, Sum_Y : Float := 0.0;
            begin
               for J in 1..Clusters(I).Count loop
                  Sum_X := Sum_X + Clusters(I).Points(J).X;
                  Sum_Y := Sum_Y + Clusters(I).Points(Y).Y;
               end loop;
               Clusters(I).Center.X := Sum_X / Float(Clusters(I).Count);
               Clusters(I).Center.Y := Sum_Y / Float(Clusters(I).Count);
            end;
         end if;
      end loop;
   end Update_Centers;
   
   -- Initialize clusters with random centers
   procedure Initialize_Clusters(Clusters : in out Cluster_Array; 
                                 Num_Clusters : Natural;
                                 Points : Point_Array) is
   begin
      for I in 1..Num_Clusters loop
         Clusters(I).Center := Points(I);  -- Simple initialization
         Clusters(I).Count := 0;
      end loop;
   end Initialize_Clusters;
   
   -- Main Lloyd's algorithm
   procedure Lloyd_Clustering(Points : Point_Array; 
                              Num_Points : Natural;
                              Num_Clusters : Natural;
                              Max_Iterations : Natural := 100) is
      Clusters : Cluster_Array(1..Num_Clusters);
      Iteration : Natural := 0;
      Changed : Boolean := True;
   begin
      -- Initialize clusters
      Initialize_Clusters(Clusters, Num_Clusters, Points);
      
      while Iteration < Max_Iterations and Changed loop
         Changed := False;
         Iteration := Iteration + 1;
         
         -- Clear clusters
         for I in 1..Num_Clusters loop
            Clusters(I).Count := 0;
         end loop;
         
         -- Assign points to closest cluster
         for I in 1..Num_Points loop
            declare
               Closest : Natural := Find_Closest_Cluster(Points(I), Clusters, Num_Clusters);
            begin
               Clusters(Closest).Count := Clusters(Closest).Count + 1;
               Clusters(Closest).Points(Clusters(Closest).Count) := Points(I);
            end;
         end loop;
         
         -- Update cluster centers
         Update_Centers(Clusters, Num_Clusters);
         
         -- Optional: Check for convergence
         -- (This is a simplified version - full implementation would check
         --  if centers have moved significantly)
      end loop;
      
      -- Output results
      Put_Line("Lloyd's Algorithm completed after " & Iteration'Img & " iterations");
      Put_Line("Final cluster centers:");
      for I in 1..Num_Clusters loop
         Put_Line("Cluster " & I'Img & ": (" & Clusters(I).Center.X'Img & ", " & 
                  Clusters(I).Center.Y'Img & ")");
      end loop;
   end Lloyd_Clustering;
   
   -- Sample data points
   Sample_Points : Point_Array(1..8) := (
      (1.0, 2.0), (1.5, 1.8), (5.0, 8.0), (8.0, 8.0),
      (1.0, 0.6), (9.0, 11.0), (8.0, 2.0), (10.0, 2.0)
   );
   
begin
   Put_Line("Lloyd's Algorithm Example");
   Put_Line("========================");
   
   -- Run Lloyd's algorithm with 3 clusters
   Lloyd_Clustering(Sample_Points, 8, 3);
   
end Lloyd_Algorithm;
```

This implementation demonstrates:

1. **Point and Cluster Types**: Basic data structures for points and clusters
2. **Distance Calculation**: Euclidean distance between points
3. **Cluster Assignment**: Assigning points to the nearest cluster center
4. **Center Update**: Recalculating cluster centers based on assigned points
5. **Main Algorithm Loop**: Iteratively improving cluster assignments until convergence

The algorithm will output the final cluster centers after the specified number of iterations, showing how Lloyd's algorithm converges to optimal cluster positions.

