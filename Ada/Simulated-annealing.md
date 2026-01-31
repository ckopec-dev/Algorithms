# Simulated Annealing Algorithm in Ada

Here's a complete implementation of a Simulated Annealing algorithm in Ada for solving the Traveling Salesman Problem (TSP):

```ada
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Float_Text_IO;     use Ada.Float_Text_IO;
with Ada.Numerics.Float_Random;
with Ada.Numerics.Generic_Elementary_Functions;

procedure Simulated_Annealing_TSP is

   -- Constants
   Num_Cities : constant := 20;
   Max_Temperature : constant Float := 1000.0;
   Min_Temperature : constant Float := 0.01;
   Cooling_Rate : constant Float := 0.95;
   Max_Iterations : constant := 10000;
   
   -- City structure
   type City is record
      X, Y : Float;
   end record;
   
   type City_Array is array (Positive range <>) of City;
   
   -- Tour structure
   type Tour is array (Positive range <>) of Positive;
   
   -- Generate random cities
   function Random_Cities return City_Array is
      Result : City_Array (1 .. Num_Cities);
      Gen : Ada.Numerics.Float_Random.Generator;
   begin
      Ada.Numerics.Float_Random.Reset(Gen);
      
      for I in Result'Range loop
         Result(I).X := Ada.Numerics.Float_Random.Random(Gen) * 100.0;
         Result(I).Y := Ada.Numerics.Float_Random.Random(Gen) * 100.0;
      end loop;
      
      return Result;
   end Random_Cities;
   
   -- Calculate distance between two cities
   function Distance(C1, C2 : City) return Float is
   begin
      return Float'Sqrt((C1.X - C2.X)**2 + (C1.Y - C2.Y)**2);
   end Distance;
   
   -- Calculate total tour distance
   function Tour_Distance(Tour : Tour; Cities : City_Array) return Float is
      Total : Float := 0.0;
   begin
      for I in Tour'First .. Tour'Last - 1 loop
         Total := Total + Distance(Cities(Tour(I)), Cities(Tour(I+1)));
      end loop;
      
      -- Return to starting city
      Total := Total + Distance(Cities(Tour(Tour'Last)), Cities(Tour(Tour'First)));
      
      return Total;
   end Tour_Distance;
   
   -- Generate initial random tour
   function Random_Tour return Tour is
      Result : Tour(1 .. Num_Cities);
      Used : array (1 .. Num_Cities) of Boolean := (others => False);
      Gen : Ada.Numerics.Float_Random.Generator;
      Index : Positive;
   begin
      Ada.Numerics.Float_Random.Reset(Gen);
      
      for I in Result'Range loop
         loop
            Index := Positive(Ada.Numerics.Float_Random.Random(Gen) * Float(Num_Cities)) + 1;
            if not Used(Index) then
               Result(I) := Index;
               Used(Index) := True;
               exit;
            end if;
         end loop;
      end loop;
      
      return Result;
   end Random_Tour;
   
   -- Generate neighbor tour by swapping two random cities
   function Get_Neighbor(Tour : Tour) return Tour is
      Result : Tour := Tour;
      Gen : Ada.Numerics.Float_Random.Generator;
      I, J : Positive;
   begin
      Ada.Numerics.Float_Random.Reset(Gen);
      
      -- Select two random positions
      I := Positive(Ada.Numerics.Float_Random.Random(Gen) * Float(Num_Cities)) + 1;
      J := Positive(Ada.Numerics.Float_Random.Random(Gen) * Float(Num_Cities)) + 1;
      
      -- Swap the cities
      if I /= J then
         Result(I) := Tour(J);
         Result(J) := Tour(I);
      end if;
      
      return Result;
   end Get_Neighbor;
   
   -- Simulated Annealing algorithm
   function Simulated_Annealing(Cities : City_Array) return Tour is
      Current_Tour : Tour := Random_Tour;
      Best_Tour : Tour := Current_Tour;
      Current_Cost : Float := Tour_Distance(Current_Tour, Cities);
      Best_Cost : Float := Current_Cost;
      Temperature : Float := Max_Temperature;
      Gen : Ada.Numerics.Float_Random.Generator;
      Neighbor : Tour;
      Neighbor_Cost : Float;
      Delta : Float;
      Accept_Probability : Float;
      Iteration : Natural := 0;
   begin
      Ada.Numerics.Float_Random.Reset(Gen);
      
      while Temperature > Min_Temperature and Iteration < Max_Iterations loop
         -- Generate neighbor solution
         Neighbor := Get_Neighbor(Current_Tour);
         Neighbor_Cost := Tour_Distance(Neighbor, Cities);
         
         -- Calculate cost difference
         Delta := Neighbor_Cost - Current_Cost;
         
         -- Accept or reject the neighbor
         if Delta < 0.0 then
            -- Always accept better solution
            Current_Tour := Neighbor;
            Current_Cost := Neighbor_Cost;
            
            -- Update best solution
            if Current_Cost < Best_Cost then
               Best_Tour := Current_Tour;
               Best_Cost := Current_Cost;
            end if;
         else
            -- Accept worse solution with probability
            Accept_Probability := Float'Exp(-Delta / Temperature);
            if Ada.Numerics.Float_Random.Random(Gen) < Accept_Probability then
               Current_Tour := Neighbor;
               Current_Cost := Neighbor_Cost;
            end if;
         end if;
         
         -- Cool down
         Temperature := Temperature * Cooling_Rate;
         Iteration := Iteration + 1;
      end loop;
      
      return Best_Tour;
   end Simulated_Annealing;
   
   -- Print tour
   procedure Print_Tour(Tour : Tour; Cities : City_Array) is
   begin
      Put_Line("Best tour:");
      for I in Tour'Range loop
         Put("City ");
         Put(Tour(I), 0, 0, 0);
         Put(" (");
         Put(Cities(Tour(I)).X, 0, 2, 0);
         Put(", ");
         Put(Cities(Tour(I)).Y, 0, 2, 0);
         Put_Line(")");
      end loop;
      
      Put("Total distance: ");
      Put(Tour_Distance(Tour, Cities), 0, 2, 0);
      New_Line;
   end Print_Tour;
   
   -- Main program
   Cities : City_Array := Random_Cities;
   Best_Tour : Tour;
   Best_Cost : Float;
   
begin
   Put_Line("Simulated Annealing for TSP");
   Put_Line("============================");
   
   -- Run simulated annealing
   Best_Tour := Simulated_Annealing(Cities);
   Best_Cost := Tour_Distance(Best_Tour, Cities);
   
   -- Print results
   Print_Tour(Best_Tour, Cities);
   
   Put_Line("Algorithm completed after " & Natural'Image(Max_Iterations) & " iterations");
   Put_Line("Final temperature: " & Float'Image(Max_Temperature * (Cooling_Rate ** 1000)));
   
end Simulated_Annealing_TSP;
```

## Key Features of this Implementation:

1. **Complete TSP Solver**: Implements simulated annealing to find an approximate solution to the Traveling Salesman Problem
2. **Temperature Management**: Uses proper cooling schedule with initial and minimum temperatures
3. **Neighbor Generation**: Creates new solutions by swapping two random cities
4. **Acceptance Probability**: Implements the Metropolis criterion for accepting worse solutions
5. **Random Number Generation**: Uses Ada's built-in random number generators
6. **Complete Tour Evaluation**: Calculates total distance including return to starting city

## How it Works:

1. **Initialization**: Generate random cities and initial random tour
2. **Main Loop**: 
   - Generate neighbor solution by swapping two cities
   - Calculate cost difference
   - Accept better solutions or accept worse solutions with probability
   - Cool down temperature
3. **Termination**: Continue until minimum temperature or maximum iterations reached

## Compilation and Execution:

```bash
gnatmake simulated_annealing_tsp.adb
./simulated_annealing_tsp
```

This implementation demonstrates the core principles of simulated annealing while solving a classic optimization problem. The algorithm balances exploration (accepting worse solutions early) with exploitation (focusing on better solutions as temperature decreases).

