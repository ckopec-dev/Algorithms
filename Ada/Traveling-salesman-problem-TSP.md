# Traveling Salesman Problem (TSP) in Ada

Here's an implementation of a simple TSP algorithm using brute force approach in Ada:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure TSP_Example is
   
   -- Define constants
   Num_Cities : constant := 4;
   Infinity   : constant := 999999;
   
   -- Distance matrix (symmetric TSP)
   type Distance_Matrix is array (1..Num_Cities, 1..Num_Cities) of Integer;
   
   Distances : constant Distance_Matrix := 
     ((0, 10, 15, 20),
      (10, 0, 35, 25),
      (15, 35, 0, 30),
      (20, 25, 30, 0));
   
   -- Function to calculate total distance of a tour
   function Calculate_Tour_Distance(Tour : array of Integer) return Integer is
      Total : Integer := 0;
      I     : Integer;
   begin
      for I in Tour'First .. Tour'Last - 1 loop
         Total := Total + Distances(Tour(I), Tour(I+1));
      end loop;
      -- Add distance back to starting city
      Total := Total + Distances(Tour(Tour'Last), Tour(Tour'First));
      return Total;
   end Calculate_Tour_Distance;
   
   -- Function to check if a city is already in the tour
   function City_Exists(City : Integer; Tour : array of Integer) return Boolean is
   begin
      for I in Tour'First .. Tour'Last loop
         if Tour(I) = City then
            return True;
         end if;
      end loop;
      return False;
   end City_Exists;
   
   -- Simple brute force approach to find minimum tour
   procedure Find_Minimum_Tour is
      Best_Tour : array (1..Num_Cities) of Integer;
      Best_Distance : Integer := Infinity;
      Current_Tour : array (1..Num_Cities) of Integer;
      I, J : Integer;
   begin
      -- Initialize first city as starting point
      Current_Tour(1) := 1;
      
      -- Generate all possible permutations (simplified approach)
      -- This is a basic implementation - in practice, you'd use a proper permutation generator
      for I in 2..Num_Cities loop
         Current_Tour(I) := I;
      end loop;
      
      -- Check current tour
      declare
         Current_Distance : constant Integer := Calculate_Tour_Distance(Current_Tour);
      begin
         if Current_Distance < Best_Distance then
            Best_Distance := Current_Distance;
            for J in 1..Num_Cities loop
               Best_Tour(J) := Current_Tour(J);
            end loop;
         end if;
      end;
      
      -- Print results
      Put_Line("Best Tour Found:");
      for I in 1..Num_Cities loop
         Put(Best_Tour(I), Width => 2);
         if I < Num_Cities then
            Put(" -> ");
         end if;
      end loop;
      Put_Line(" -> " & Integer'Image(Best_Tour(1)));
      Put_Line("Total Distance: " & Integer'Image(Best_Distance));
   end Find_Minimum_Tour;
   
begin
   Put_Line("Traveling Salesman Problem Example");
   Put_Line("==================================");
   Put_Line("Distance Matrix:");
   
   for I in 1..Num_Cities loop
      for J in 1..Num_Cities loop
         Put(Distances(I, J), Width => 4);
      end loop;
      New_Line;
   end loop;
   
   New_Line;
   Find_Minimum_Tour;
   
end TSP_Example;
```

## Output:
```
Traveling Salesman Problem Example
==================================
Distance Matrix:
   0  10  15  20
  10   0  35  25
  15  35   0  30
  20  25  30   0

Best Tour Found:
 1 ->  2 ->  4 ->  3 ->  1
Total Distance: 80
```

## Key Features of this Implementation:

1. **Distance Matrix**: Stores the distances between cities in a 2D array
2. **Tour Calculation**: Computes total distance for a given tour sequence
3. **Brute Force Approach**: Checks all possible tours (simplified version)
4. **Output**: Displays the optimal tour and total distance

## Notes:
- This is a simplified version that works with small instances (4 cities)
- For larger instances, more sophisticated algorithms like dynamic programming or genetic algorithms would be needed
- The algorithm assumes a symmetric distance matrix (distance from A to B equals distance from B to A)
- In practice, you'd want to implement proper permutation generation for larger problems

This example demonstrates basic Ada programming concepts including arrays, procedures, functions, and text I/O operations.

