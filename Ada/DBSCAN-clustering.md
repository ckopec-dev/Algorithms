# DBSCAN Clustering in Ada

Here's an example implementation of the DBSCAN clustering algorithm in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Vectors;

procedure DBSCAN_Example is

   -- Point type definition
   type Point is record
      X, Y : Float;
   end record;

   -- Vector of points
   package Point_Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type => Natural, Element_Type => Point);
   use Point_Vectors;

   -- Cluster type
   type Cluster is record
      Points : Point_Vectors.Vector;
      ID     : Natural;
   end record;

   -- Vector of clusters
   package Cluster_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Cluster);
   use Cluster_Vectors;

   -- Distance function (Euclidean)
   function Distance(P1, P2 : Point) return Float is
   begin
      return Sqrt((P1.X - P2.X)**2.0 + (P1.Y - P2.Y)**2.0);
   end Distance;

   -- DBSCAN algorithm implementation
   procedure DBSCAN(Points : in out Point_Vectors.Vector;
                    Eps    : Float;
                    MinPts : Natural;
                    Clusters : in out Cluster_Vectors.Vector) is

      type Point_Status is (Undefined, Core, Border, Noise);
      type Point_Status_Array is array (Natural range <>) of Point_Status;

      Visited : array (Natural range 0 .. Points.Length - 1) of Boolean := (others => False);
      Status  : Point_Status_Array (0 .. Points.Length - 1);
      Cluster_ID : Natural := 0;

      -- Find neighbors within epsilon distance
      function Region_Query(P_Index : Natural) return Point_Vectors.Vector is
         Neighbors : Point_Vectors.Vector;
         P         : Point := Points.Element(P_Index);
      begin
         for I in 0 .. Points.Length - 1 loop
            if I /= P_Index and then Distance(P, Points.Element(I)) <= Eps then
               Append(Neighbors, Points.Element(I));
            end if;
         end loop;
         return Neighbors;
      end Region_Query;

      -- Expand cluster
      procedure Expand_Cluster(P_Index : Natural;
                              Neighbors : Point_Vectors.Vector) is
         New_Cluster : Cluster;
         Current_Neighbors : Point_Vectors.Vector;
      begin
         -- Add point to cluster
         Append(New_Cluster.Points, Points.Element(P_Index));
         Status(P_Index) := Core;

         -- Process neighbors
         for I in 0 .. Neighbors.Length - 1 loop
            declare
               N_Index : Natural := 0;
               Found   : Boolean := False;
            begin
               -- Find neighbor index
               for J in 0 .. Points.Length - 1 loop
                  if Points.Element(J) = Neighbors.Element(I) then
                     N_Index := J;
                     Found := True;
                     exit;
                  end if;
               end loop;

               if Found then
                  if not Visited(N_Index) then
                     Visited(N_Index) := True;
                     declare
                        N_Neighbors : Point_Vectors.Vector := Region_Query(N_Index);
                     begin
                        if N_Neighbors.Length >= MinPts then
                           for J in 0 .. N_Neighbors.Length - 1 loop
                              Append(Current_Neighbors, N_Neighbors.Element(J));
                           end loop;
                        end if;
                     end;
                  end if;

                  if Status(N_Index) = Undefined then
                     Status(N_Index) := Border;
                  end if;
               end if;
            end;
         end loop;

         -- Recursively expand cluster
         if Current_Neighbors.Length > 0 then
            for I in 0 .. Current_Neighbors.Length - 1 loop
               declare
                  N_Index : Natural := 0;
                  Found   : Boolean := False;
               begin
                  for J in 0 .. Points.Length - 1 loop
                     if Points.Element(J) = Current_Neighbors.Element(I) then
                        N_Index := J;
                        Found := True;
                        exit;
                     end if;
                  end loop;

                     if Found and then not Visited(N_Index) then
                        Visited(N_Index) := True;
                        declare
                           N_Neighbors : Point_Vectors.Vector := Region_Query(N_Index);
                        begin
                           if N_Neighbors.Length >= MinPts then
                              Expand_Cluster(N_Index, N_Neighbors);
                           end if;
                        end;
                     end if;
               end;
            end loop;
         end if;

         -- Add cluster to results
         New_Cluster.ID := Cluster_ID;
         Append(Clusters, New_Cluster);
         Cluster_ID := Cluster_ID + 1;
      end Expand_Cluster;

   begin
      -- Initialize status array
      for I in 0 .. Points.Length - 1 loop
         Status(I) := Undefined;
      end loop;

      -- Process each point
      for I in 0 .. Points.Length - 1 loop
         if not Visited(I) then
            Visited(I) := True;
            declare
               Neighbors : Point_Vectors.Vector := Region_Query(I);
            begin
               if Neighbors.Length >= MinPts then
                  -- Found core point
                  Expand_Cluster(I, Neighbors);
               else
                  -- Found noise point
                  Status(I) := Noise;
               end if;
            end;
         end if;
      end loop;
   end DBSCAN;

   -- Sample data points
   Points : Point_Vectors.Vector;
   Clusters : Cluster_Vectors.Vector;

   -- Test data
   Sample_Points : array (1..10) of Point := 
     ((1.0, 1.0), (1.5, 1.0), (2.0, 1.5), (2.5, 2.0), (3.0, 2.5),
      (7.0, 7.0), (7.5, 7.5), (8.0, 8.0), (8.5, 8.5), (9.0, 9.0));

begin
   -- Populate points
   for I in Sample_Points'Range loop
      Append(Points, Sample_Points(I));
   end loop;

   -- Run DBSCAN
   DBSCAN(Points, 1.5, 2, Clusters);

   -- Display results
   Put_Line("DBSCAN Clustering Results:");
   Put_Line("Epsilon: 1.5, MinPts: 2");
   Put_Line("------------------------");

   for I in 0 .. Clusters.Length - 1 loop
      Put("Cluster ");
      Put(Clusters.Element(I).ID, 0, 0, 0);
      Put(" contains ");
      Put(Clusters.Element(I).Points.Length, 0, 0, 0);
      Put_Line(" points:");
      
      for J in 0 .. Clusters.Element(I).Points.Length - 1 loop
         declare
            P : Point := Clusters.Element(I).Points.Element(J);
         begin
            Put("  (");
            Put(P.X, 0, 2, 0);
            Put(", ");
            Put(P.Y, 0, 2, 0);
            Put_Line(")");
         end;
      end loop;
      New_Line;
   end loop;

end DBSCAN_Example;
```

## Key Features of this Implementation:

1. **Point Representation**: Uses a simple 2D point structure with X and Y coordinates
2. **Distance Calculation**: Implements Euclidean distance function
3. **Core Algorithm**: 
   - Identifies core points (points with sufficient neighbors)
   - Expands clusters from core points
   - Handles border and noise points
4. **Data Structures**: 
   - Uses Ada containers for dynamic arrays
   - Properly manages memory and collections
5. **Clustering Output**: Displays clusters with their points

## How to Compile and Run:

```bash
gnatmake dbscan_example.adb
./dbscan_example
```

This example demonstrates the fundamental DBSCAN clustering algorithm in Ada, showing how to identify clusters, core points, border points, and noise points in a 2D dataset.

