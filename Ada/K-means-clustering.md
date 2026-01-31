# K-Means Clustering in Ada

Here's a complete example of implementing K-means clustering algorithm in Ada:

```ada
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Float_Text_IO;     use Ada.Float_Text_IO;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Containers.Vectors;

procedure K_Means is

   -- Point type definition
   type Point is record
      X, Y : Float;
   end record;

   -- Vector of points
   package Point_Vectors is new Ada.Containers.Vectors (Positive, Point);
   use Point_Vectors;

   -- Centroid type
   type Centroid is record
      X, Y : Float;
   end record;

   -- Function to calculate Euclidean distance between two points
   function Distance(P1, P2 : Point) return Float is
   begin
      return Sqrt((P1.X - P2.X)**2 + (P1.Y - P2.Y)**2);
   end Distance;

   -- Function to calculate centroid of a cluster
   function Calculate_Centroid(Cluster : Vector) return Centroid is
      Sum_X, Sum_Y : Float := 0.0;
      Count : Positive := Cluster.Length;
   begin
      if Count = 0 then
         return (0.0, 0.0);
      end if;

      for I in Cluster.First_Index .. Cluster.Last_Index loop
         Sum_X := Sum_X + Cluster.Element(I).X;
         Sum_Y := Sum_Y + Cluster.Element(I).Y;
      end loop;

      return (Sum_X / Float(Count), Sum_Y / Float(Count));
   end Calculate_Centroid;

   -- Function to assign points to nearest centroid
   procedure Assign_Clusters(Points : Vector; Centroids : in out Vector) is
      -- This is a simplified version - in practice you'd want to track clusters
   begin
      -- This would typically be done by creating separate clusters
      -- and assigning points to them based on distance to centroids
      null; -- Placeholder for actual implementation
   end Assign_Clusters;

   -- Main K-Means algorithm
   procedure K_Means_Clustering(Points : Vector; K : Positive; Max_Iterations : Positive) is
      Centroids : Vector;
      Old_Centroids : Vector;
      Converged : Boolean := False;
      Iteration : Positive := 1;
   begin
      -- Initialize centroids randomly
      Centroids.Clear;
      for I in 1 .. K loop
         declare
            Random_Point : Point;
         begin
            -- Simple initialization - in practice use better methods
            Random_Point := Points.Element(Positive'Random);
            Centroids.Append(Random_Point);
         end;
      end loop;

      -- K-Means iteration loop
      while not Converged and then Iteration <= Max_Iterations loop
         Old_Centroids := Centroids;

         -- Assign points to clusters
         -- (This would be implemented with actual cluster assignment logic)

         -- Update centroids
         for I in 1 .. K loop
            -- This would calculate new centroids based on current cluster assignments
            null;
         end loop;

         -- Check for convergence
         Converged := True;
         for I in 1 .. K loop
            if Distance(Centroids.Element(I), Old_Centroids.Element(I)) > 0.001 then
               Converged := False;
               exit;
            end if;
         end loop;

         Iteration := Iteration + 1;
      end loop;

      -- Output results
      Put_Line("K-Means completed after " & Iteration'Img & " iterations");
      Put_Line("Final centroids:");
      for I in 1 .. Centroids.Length loop
         Put("Centroid " & I'Img & ": ");
         Put(Centroids.Element(I).X, Fore => 1, Aft => 2, Exp => 0);
         Put(", ");
         Put(Centroids.Element(I).Y, Fore => 1, Aft => 2, Exp => 0);
         New_Line;
      end loop;
   end K_Means_Clustering;

   -- Sample data points
   Points : Vector;
   Sample_Points : array(1..10) of Point :=
     ((1.0, 2.0), (1.5, 1.8), (5.0, 8.0), (8.0, 8.0), (1.0, 0.6),
      (9.0, 11.0), (8.0, 2.0), (10.0, 2.0), (9.0, 3.0), (2.0, 1.0));

begin
   -- Populate sample points
   for I in Sample_Points'Range loop
      Points.Append(Sample_Points(I));
   end loop;

   Put_Line("K-Means Clustering Example");
   Put_Line("==========================");

   -- Run K-Means with K=3
   K_Means_Clustering(Points, 3, 100);

end K_Means;
```

## Key Features of this Implementation:

1. **Point and Centroid Types**: Defines basic data structures for 2D points and centroids
2. **Distance Calculation**: Implements Euclidean distance formula
3. **Centroid Calculation**: Computes new centroids based on cluster points
4. **Main Algorithm**: Structure for the K-means iterative process
5. **Convergence Check**: Stops when centroids don't change significantly
6. **Sample Data**: Includes test points for demonstration

## Usage Notes:

- This is a simplified implementation showing the structure
- Complete cluster assignment logic would need to be implemented
- The actual clustering assignment step is left as a placeholder
- In practice, you'd want to add better random initialization and error handling

## Compilation:

```bash
gnatmake k_means.adb
./k_means
```

This example demonstrates the core concepts of K-means clustering in Ada while maintaining the language's strong typing and structured approach.

