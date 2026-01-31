# Iterative Closest Point (ICP) Algorithm in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

procedure ICP_Algorithm is

   -- Point type definition
   type Point is record
      X, Y : Float;
   end record;

   -- Point array type
   type Point_Array is array (Positive range <>) of Point;

   -- Transformation matrix type
   type Transform_Matrix is array (1..3, 1..3) of Float;

   -- Function to calculate Euclidean distance between two points
   function Distance(P1, P2 : Point) return Float is
   begin
      return Sqrt((P1.X - P2.X)**2 + (P1.Y - P2.Y)**2);
   end Distance;

   -- Function to find closest point in target set for a given source point
   function Find_Closest_Point(Source_Point : Point;
                              Target_Set   : Point_Array) return Point is
      Min_Distance : Float := Float'Last;
      Closest_Point : Point;
   begin
      for I in Target_Set'Range loop
         declare
            Current_Distance : Float := Distance(Source_Point, Target_Set(I));
         begin
            if Current_Distance < Min_Distance then
               Min_Distance := Current_Distance;
               Closest_Point := Target_Set(I);
            end if;
         end;
      end loop;
      return Closest_Point;
   end Find_Closest_Point;

   -- Function to calculate centroid of a point set
   function Calculate_Centroid(Points : Point_Array) return Point is
      Sum_X : Float := 0.0;
      Sum_Y : Float := 0.0;
   begin
      for I in Points'Range loop
         Sum_X := Sum_X + Points(I).X;
         Sum_Y := Sum_Y + Points(I).Y;
      end loop;
      
      return (X => Sum_X / Float(Points'Length),
              Y => Sum_Y / Float(Points'Length));
   end Calculate_Centroid;

   -- Function to compute transformation matrix from two point sets
   function Compute_Transformation(Source_Set, Target_Set : Point_Array) 
                                  return Transform_Matrix is
      Source_Centroid : Point := Calculate_Centroid(Source_Set);
      Target_Centroid : Point := Calculate_Centroid(Target_Set);
      
      -- Calculate covariance matrix
      Cov_X_X : Float := 0.0;
      Cov_X_Y : Float := 0.0;
      Cov_Y_X : Float := 0.0;
      Cov_Y_Y : Float := 0.0;
      
      -- Transformation matrix to return
      Transform : Transform_Matrix := ((1.0, 0.0, 0.0),
                                       (0.0, 1.0, 0.0),
                                       (0.0, 0.0, 1.0));
   begin
      -- Calculate covariance matrix elements
      for I in Source_Set'Range loop
         declare
            Source_X : Float := Source_Set(I).X - Source_Centroid.X;
            Source_Y : Float := Source_Set(I).Y - Source_Centroid.Y;
            Target_X : Float := Target_Set(I).X - Target_Centroid.X;
            Target_Y : Float := Target_Set(I).Y - Target_Centroid.Y;
         begin
            Cov_X_X := Cov_X_X + Source_X * Target_X;
            Cov_X_Y := Cov_X_Y + Source_X * Target_Y;
            Cov_Y_X := Cov_Y_X + Source_Y * Target_X;
            Cov_Y_Y := Cov_Y_Y + Source_Y * Target_Y;
         end;
      end loop;
      
      -- Simple rigid transformation (rotation and translation only)
      -- This is a simplified version - full SVD would be more accurate
      declare
         Det : Float := Cov_X_X * Cov_Y_Y - Cov_X_Y * Cov_Y_X;
         Norm : Float := Sqrt(Cov_X_X**2 + Cov_X_Y**2 + Cov_Y_X**2 + Cov_Y_Y**2);
      begin
         if Norm > 1.0e-10 then
            -- Simple rotation matrix calculation
            -- Note: This is a simplified approach for demonstration
            Transform(1, 1) := Cov_X_X / Norm;
            Transform(1, 2) := -Cov_X_Y / Norm;
            Transform(2, 1) := Cov_Y_X / Norm;
            Transform(2, 2) := Cov_Y_Y / Norm;
         end if;
         
         -- Translation components
         Transform(1, 3) := Target_Centroid.X - Transform(1, 1) * Source_Centroid.X - Transform(1, 2) * Source_Centroid.Y;
         Transform(2, 3) := Target_Centroid.Y - Transform(2, 1) * Source_Centroid.X - Transform(2, 2) * Source_Centroid.Y;
      end;
      
      return Transform;
   end Compute_Transformation;

   -- Function to apply transformation to a point
   function Apply_Transform(Point_To_Transform : Point;
                           Transform_Matrix : Transform_Matrix) return Point is
   begin
      return (X => Transform_Matrix(1, 1) * Point_To_Transform.X + 
                    Transform_Matrix(1, 2) * Point_To_Transform.Y + 
                    Transform_Matrix(1, 3),
              Y => Transform_Matrix(2, 1) * Point_To_Transform.X + 
                    Transform_Matrix(2, 2) * Point_To_Transform.Y + 
                    Transform_Matrix(2, 3));
   end Apply_Transform;

   -- ICP main algorithm
   procedure ICP_Algorithm_Main(Source_Set, Target_Set : in out Point_Array;
                                Max_Iterations : in Positive;
                                Tolerance : in Float) is
      Current_Source_Set : Point_Array(1..Source_Set'Length) := Source_Set;
      Iteration_Count : Natural := 0;
      Error : Float := Float'Last;
   begin
      loop
         -- Find closest points
         declare
            Closest_Points : Point_Array(1..Current_Source_Set'Length);
         begin
            for I in Current_Source_Set'Range loop
               Closest_Points(I) := Find_Closest_Point(Current_Source_Set(I), Target_Set);
            end loop;
            
            -- Compute transformation
            declare
               Transform : Transform_Matrix := Compute_Transformation(Current_Source_Set, Closest_Points);
            begin
               -- Apply transformation to source points
               for I in Current_Source_Set'Range loop
                  Current_Source_Set(I) := Apply_Transform(Current_Source_Set(I), Transform);
               end loop;
            end;
         end;
         
         Iteration_Count := Iteration_Count + 1;
         
         -- Check convergence
         if Iteration_Count >= Max_Iterations then
            Put_Line("Maximum iterations reached.");
            exit;
         end if;
         
         -- Calculate error (sum of distances)
         declare
            Total_Error : Float := 0.0;
         begin
            for I in Current_Source_Set'Range loop
               Total_Error := Total_Error + Distance(Current_Source_Set(I), Target_Set(I));
            end loop;
            Error := Total_Error / Float(Current_Source_Set'Length);
            
            if Error < Tolerance then
               Put_Line("Convergence reached.");
               exit;
            end if;
         end;
         
         Put_Line("Iteration " & Iteration_Count'Img & ", Error: " & Error'Img);
      end loop;
      
      -- Output final result
      Put_Line("Final source set after ICP:");
      for I in Current_Source_Set'Range loop
         Put("Point " & I'Img & ": ");
         Put(Current_Source_Set(I).X, Fore => 1, Aft => 3, Exp => 0);
         Put(" ");
         Put(Current_Source_Set(I).Y, Fore => 1, Aft => 3, Exp => 0);
         New_Line;
      end loop;
   end ICP_Algorithm_Main;

   -- Example usage
   Source_Points : Point_Array(1..4) := 
     ((X => 1.0, Y => 1.0),
      (X => 2.0, Y => 2.0),
      (X => 3.0, Y => 3.0),
      (X => 4.0, Y => 4.0));

   Target_Points : Point_Array(1..4) := 
     ((X => 1.1, Y => 1.2),
      (X => 2.1, Y => 2.2),
      (X => 3.1, Y => 3.2),
      (X => 4.1, Y => 4.2));

begin
   Put_Line("Starting ICP Algorithm...");
   Put_Line("Initial source points:");
   for I in Source_Points'Range loop
      Put("Point " & I'Img & ": ");
      Put(Source_Points(I).X, Fore => 1, Aft => 3, Exp => 0);
      Put(" ");
      Put(Source_Points(I).Y, Fore => 1, Aft => 3, Exp => 0);
      New_Line;
   end loop;
   
   Put_Line("Initial target points:");
   for I in Target_Points'Range loop
      Put("Point " & I'Img & ": ");
      Put(Target_Points(I).X, Fore => 1, Aft => 3, Exp => 0);
      Put(" ");
      Put(Target_Points(I).Y, Fore => 1, Aft => 3, Exp => 0);
      New_Line;
   end loop;
   
   -- Run ICP algorithm
   ICP_Algorithm_Main(Source_Points, Target_Points, 100, 0.001);
   
   Put_Line("ICP Algorithm completed.");
end ICP_Algorithm;
```

This Ada implementation of the ICP algorithm includes:

1. **Point and Matrix Types**: Defines basic point structures and transformation matrices
2. **Distance Calculation**: Computes Euclidean distance between points
3. **Closest Point Finding**: Finds the closest point in the target set for each source point
4. **Centroid Calculation**: Computes the centroid of a set of points
5. **Transformation Matrix**: Computes a rigid transformation between point sets
6. **Point Transformation**: Applies transformation matrix to points
7. **Main ICP Loop**: Implements the iterative closest point algorithm with convergence checking

The algorithm iteratively:
- Finds closest points between source and target sets
- Computes transformation matrix
- Applies transformation to source points
- Checks for convergence based on error tolerance

Note: This is a simplified implementation for demonstration purposes. A production ICP implementation would include more sophisticated techniques like SVD for rotation matrix computation and better convergence criteria.

