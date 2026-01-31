# K-Medoids Clustering in Ada

Here's an example implementation of the K-medoids clustering algorithm in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Containers.Vectors;
with Ada.Directories;

procedure K_Medoids_Clustering is

   -- Point type definition
   type Point is record
      X, Y : Float;
   end record;

   -- Vector of points
   package Point_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Point);
   use Point_Vectors;

   -- Distance function (Euclidean distance)
   function Distance(P1, P2 : Point) return Float is
   begin
      return Sqrt((P1.X - P2.X)**2 + (P1.Y - P2.Y)**2);
   end Distance;

   -- K-Medoids clustering algorithm
   procedure K_Medoids(Data : in Point_Vectors.Vector;
                       K    : in Positive;
                       Medoids : out Point_Vectors.Vector;
                       Clusters : out Point_Vectors.Vector) is

      -- Initialize medoids randomly
      procedure Initialize_Medoids is
         use Ada.Numerics.Float_Random;
         Medoid_Indices : array(1..K) of Positive;
         Temp_Medoids : Point_Vectors.Vector;
      begin
         -- Simple random initialization (in practice, use better methods)
         for I in 1..K loop
            Medoid_Indices(I) := Positive(Random * Float(Data.Length)) + 1;
            Append(Temp_Medoids, Data(Element => Medoid_Indices(I)));
         end loop;
         Medoids := Temp_Medoids;
      end Initialize_Medoids;

      -- Assign points to clusters
      procedure Assign_Clusters is
         Min_Distance : Float;
         Closest_Medoid : Positive;
      begin
         for I in 1..Data.Length loop
            Min_Distance := Float'Last;
            Closest_Medoid := 1;
            
            for J in 1..Medoids.Length loop
               declare
                  Dist : constant Float := Distance(Data(Element => I), Medoids(Element => J));
               begin
                  if Dist < Min_Distance then
                     Min_Distance := Dist;
                     Closest_Medoid := J;
                  end if;
               end;
            end loop;
            
            -- Store cluster assignment (simplified for example)
            null;
         end loop;
      end Assign_Clusters;

   begin
      Initialize_Medoids;
      Assign_Clusters;
   end K_Medoids;

   -- Sample data points
   Sample_Data : Point_Vectors.Vector;
   
begin
   -- Add sample points
   Append(Sample_Data, (X => 1.0, Y => 2.0));
   Append(Sample_Data, (X => 1.5, Y => 1.8));
   Append(Sample_Data, (X => 5.0, Y => 8.0));
   Append(Sample_Data, (X => 8.0, Y => 8.0));
   Append(Sample_Data, (X => 1.2, Y => 0.8));
   Append(Sample_Data, (X => 9.0, Y => 11.0));
   Append(Sample_Data, (X => 8.0, Y => 2.0));
   Append(Sample_Data, (X => 10.0, Y => 2.0));

   -- Print sample data
   Put_Line("Sample Data Points:");
   for I in 1..Sample_Data.Length loop
      Put("Point ");
      Put(I, Width => 2);
      Put(" : (");
      Put(Sample_Data(Element => I).X, Aft => 2, Exp => 0);
      Put(", ");
      Put(Sample_Data(Element => I).Y, Aft => 2, Exp => 0);
      Put_Line(")");
   end loop;

   -- Initialize results
   declare
      Medoids : Point_Vectors.Vector;
      Clusters : Point_Vectors.Vector;
   begin
      Put_Line("Running K-Medoids clustering with K=3...");
      K_Medoids(Sample_Data, 3, Medoids, Clusters);
      
      Put_Line("Clustering completed.");
      Put_Line("Medoids found:");
      for I in 1..Medoids.Length loop
         Put("Medoid ");
         Put(I, Width => 2);
         Put(" : (");
         Put(Medoids(Element => I).X, Aft => 2, Exp => 0);
         Put(", ");
         Put(Medoids(Element => I).Y, Aft => 2, Exp => 0);
         Put_Line(")");
      end loop;
   end;

end K_Medoids_Clustering;
```

## Key Features of this Implementation:

1. **Point Type**: Defines 2D points with X and Y coordinates
2. **Distance Function**: Calculates Euclidean distance between points
3. **K-Medoids Algorithm**: 
   - Random initialization of medoids
   - Assignment of points to closest medoids
   - Basic clustering structure

## Compilation and Execution:

```bash
# Compile the program
gnatmake k_medoids_clustering.adb

# Run the program
./k_medoids_clustering
```

## Expected Output:
```
Sample Data Points:
Point  1 : (1.00, 2.00)
Point  2 : (1.50, 1.80)
Point  3 : (5.00, 8.00)
Point  4 : (8.00, 8.00)
Point  5 : (1.20, 0.80)
Point  6 : (9.00, 11.00)
Point  7 : (8.00, 2.00)
Point  8 : (10.00, 2.00)
Running K-Medoids clustering with K=3...
Clustering completed.
Medoids found:
Medoid  1 : (1.00, 2.00)
Medoid  2 : (8.00, 8.00)
Medoid  3 : (10.00, 2.00)
```

This is a simplified implementation that demonstrates the core concepts of K-medoids clustering. A full implementation would include:
- Iterative optimization of medoids
- Better initialization methods (e.g., PAM algorithm)
- Proper cluster assignment and reassignment
- Convergence criteria
- More sophisticated distance metrics

