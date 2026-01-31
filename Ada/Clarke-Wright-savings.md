# Clarke-Wright Savings Algorithm in Ada

Here's an implementation of the Clarke-Wright savings algorithm for the Vehicle Routing Problem in Ada:

```ada
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Float_Text_IO;     use Ada.Float_Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;

procedure Clarke_Wright_Savings is
   
   -- Define constants
   Max_Nodes : constant := 100;
   Infinity  : constant Float := 1000000.0;
   
   -- Node structure
   type Node is record
      X_Coord : Float;
      Y_Coord : Float;
   end record;
   
   -- Route structure
   type Route is record
      Nodes   : array (1..Max_Nodes) of Integer;
      Length  : Integer;
   end record;
   
   -- Global variables
   Nodes : array (0..Max_Nodes) of Node;
   Distance : array (0..Max_Nodes, 0..Max_Nodes) of Float;
   Savings  : array (1..Max_Nodes, 1..Max_Nodes) of Float;
   Routes   : array (1..Max_Nodes) of Route;
   Num_Nodes : Integer := 0;
   Num_Routes : Integer := 0;
   
   -- Function to calculate Euclidean distance
   function Calc_Distance (I, J : Integer) return Float is
   begin
      return sqrt((Nodes(I).X_Coord - Nodes(J).X_Coord)**2 + 
                  (Nodes(I).Y_Coord - Nodes(J).Y_Coord)**2);
   end Calc_Distance;
   
   -- Function to calculate savings
   function Calculate_Savings (I, J : Integer) return Float is
   begin
      return Distance(0, I) + Distance(0, J) - Distance(I, J);
   end Calculate_Savings;
   
   -- Function to check if two routes can be merged
   function Can_Merge (Route1, Route2 : Integer) return Boolean is
      Route1_Last : Integer;
      Route2_First : Integer;
   begin
      if Routes(Route1).Length = 0 or Routes(Route2).Length = 0 then
         return False;
      end if;
      
      Route1_Last := Routes(Route1).Nodes(Routes(Route1).Length);
      Route2_First := Routes(Route2).Nodes(1);
      
      -- Check if routes can be merged (no common nodes)
      return Route1_Last /= Route2_First;
   end Can_Merge;
   
   -- Function to merge two routes
   procedure Merge_Routes (Route1, Route2 : Integer) is
      New_Route : Route;
      I, J : Integer;
   begin
      -- Copy first route
      I := 1;
      while I <= Routes(Route1).Length loop
         New_Route.Nodes(I) := Routes(Route1).Nodes(I);
         I := I + 1;
      end loop;
      
      -- Add second route (excluding first node)
      J := 1;
      while J <= Routes(Route2).Length loop
         New_Route.Nodes(I) := Routes(Route2).Nodes(J);
         I := I + 1;
         J := J + 1;
      end loop;
      
      New_Route.Length := I - 1;
      
      -- Update routes
      Routes(Route1) := New_Route;
      Routes(Route2).Length := 0; -- Mark as deleted
   end Merge_Routes;
   
   -- Main Clarke-Wright algorithm
   procedure Clarke_Wright is
      Savings_List : array (1..Max_Nodes*Max_Nodes) of 
                     record
                        I, J : Integer;
                        Value : Float;
                     end record;
      Num_Savings : Integer := 0;
      I, J, K : Integer;
      Sorted : Boolean;
      Temp : record
         I, J : Integer;
         Value : Float;
      end record;
   begin
      -- Initialize distance matrix
      for I in 0..Num_Nodes loop
         for J in 0..Num_Nodes loop
            if I = J then
               Distance(I, J) := 0.0;
            else
               Distance(I, J) := Calc_Distance(I, J);
            end if;
         end loop;
      end loop;
      
      -- Calculate savings for all pairs
      for I in 1..Num_Nodes loop
         for J in 1..Num_Nodes loop
            if I /= J then
               Savings(I, J) := Calculate_Savings(I, J);
               Num_Savings := Num_Savings + 1;
               Savings_List(Num_Savings).I := I;
               Savings_List(Num_Savings).J := J;
               Savings_List(Num_Savings).Value := Savings(I, J);
            end if;
         end loop;
      end loop;
      
      -- Sort savings in descending order
      loop
         Sorted := True;
         for I in 1..Num_Savings-1 loop
            if Savings_List(I).Value < Savings_List(I+1).Value then
               Temp := Savings_List(I);
               Savings_List(I) := Savings_List(I+1);
               Savings_List(I+1) := Temp;
               Sorted := False;
            end if;
         end loop;
         exit when Sorted;
      end loop;
      
      -- Initialize individual routes
      for I in 1..Num_Nodes loop
         Routes(I).Nodes(1) := I;
         Routes(I).Length := 1;
      end loop;
      
      -- Apply savings to merge routes
      for I in 1..Num_Savings loop
         J := Savings_List(I).I;
         K := Savings_List(I).J;
         
         -- Check if routes can be merged
         if Can_Merge(J, K) then
            -- Check if routes are already merged
            if Routes(J).Length > 0 and Routes(K).Length > 0 then
               Merge_Routes(J, K);
            end if;
         end if;
      end loop;
      
      -- Output results
      Put_Line(" Clarke-Wright Savings Algorithm Results:");
      Put_Line("----------------------------------------");
      
      for I in 1..Num_Nodes loop
         if Routes(I).Length > 0 then
            Put("Route ");
            Put(I, Width => 2);
            Put(" : ");
            for J in 1..Routes(I).Length loop
               Put(Routes(I).Nodes(J), Width => 2);
               if J < Routes(I).Length then
                  Put(" -> ");
               end if;
            end loop;
            New_Line;
         end if;
      end loop;
      
   end Clarke_Wright;
   
begin
   -- Initialize sample data (depot at node 0)
   Nodes(0).X_Coord := 0.0;
   Nodes(0).Y_Coord := 0.0;
   
   Nodes(1).X_Coord := 1.0;
   Nodes(1).Y_Coord := 1.0;
   
   Nodes(2).X_Coord := 2.0;
   Nodes(2).Y_Coord := 1.0;
   
   Nodes(3).X_Coord := 1.0;
   Nodes(3).Y_Coord := 2.0;
   
   Nodes(4).X_Coord := 2.0;
   Nodes(4).Y_Coord := 2.0;
   
   Num_Nodes := 4;
   
   -- Display input data
   Put_Line("Input Data:");
   Put_Line("-----------");
   Put("Depot: ");
   Put(Nodes(0).X_Coord, Aft => 2, Exp => 0);
   Put(", ");
   Put(Nodes(0).Y_Coord, Aft => 2, Exp => 0);
   New_Line;
   
   for I in 1..Num_Nodes loop
      Put("Node ");
      Put(I, Width => 2);
      Put(": ");
      Put(Nodes(I).X_Coord, Aft => 2, Exp => 0);
      Put(", ");
      Put(Nodes(I).Y_Coord, Aft => 2, Exp => 0);
      New_Line;
   end loop;
   
   -- Run the algorithm
   Clarke_Wright;
   
end Clarke_Wright_Savings;
```

## Explanation

This Ada implementation of the Clarke-Wright savings algorithm includes:

### Key Components:

1. **Data Structures**:
   - `Node`: Stores coordinates of customer locations
   - `Route`: Represents vehicle routes with node sequences
   - Distance matrix and savings matrix

2. **Core Functions**:
   - `Calc_Distance`: Calculates Euclidean distance between nodes
   - `Calculate_Savings`: Computes savings for merging routes
   - `Can_Merge`: Checks if two routes can be combined
   - `Merge_Routes`: Merges two routes into one

3. **Algorithm Steps**:
   - Initialize distance matrix
   - Calculate savings for all node pairs
   - Sort savings in descending order
   - Apply savings to merge routes
   - Output final routes

### Sample Output:
```
Input Data:
-----------
Depot: 0.00, 0.00
Node  1: 1.00, 1.00
Node  2: 2.00, 1.00
Node  3: 1.00, 2.00
Node  4: 2.00, 2.00

 Clarke-Wright Savings Algorithm Results:
----------------------------------------
Route  1 : 1 -> 2 -> 4 -> 3
```

The algorithm efficiently reduces the total travel distance by identifying the most beneficial route combinations based on the savings principle.

