# Tabu Search Algorithm in Ada

Here's an implementation of a Tabu Search algorithm in Ada for solving a simple optimization problem:

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with Ada.Command_Line; use Ada.Command_Line;

procedure Tabu_Search is
   
   -- Problem parameters
   Dimension : constant := 10;
   Max_Iterations : constant := 1000;
   Tabu_List_Size : constant := 20;
   
   -- Solution type
   type Solution is array (1..Dimension) of Integer;
   
   -- Tabu list type
   type Tabu_List_Type is array (1..Tabu_List_Size) of Solution;
   
   -- Global variables
   Current_Solution : Solution;
   Best_Solution : Solution;
   Best_Value : Float := Float'Last;
   Tabu_List : Tabu_List_Type;
   Tabu_Indices : array (1..Tabu_List_Size) of Integer := (others => 0);
   Iteration_Count : Integer := 0;
   
   -- Objective function (minimize sum of squares)
   function Objective(Sol : Solution) return Float is
      Total : Float := 0.0;
   begin
      for I in Sol'Range loop
         Total := Total + Float(Sol(I))**2.0;
      end loop;
      return Total;
   end Objective;
   
   -- Generate a random solution
   procedure Random_Solution(Sol : out Solution) is
      R : Float;
   begin
      for I in Sol'Range loop
         R := Random;
         Sol(I) := Integer(R * 20.0) - 10;  -- Range: -10 to 10
      end loop;
   end Random_Solution;
   
   -- Check if a solution is in the tabu list
   function Is_Tabu(Sol : Solution) return Boolean is
   begin
      for I in Tabu_List'Range loop
         if Tabu_Indices(I) > 0 then
            declare
               Match : Boolean := True;
            begin
               for J in Sol'Range loop
                  if Sol(J) /= Tabu_List(I)(J) then
                     Match := False;
                     exit;
                  end if;
               end loop;
               if Match then
                  return True;
               end if;
            end;
         end if;
      end loop;
      return False;
   end Is_Tabu;
   
   -- Move to a neighbor solution
   procedure Move_To_Neighbor(Current : in out Solution; 
                             New_Sol : out Solution) is
      Index : Integer;
      Direction : Integer;
   begin
      New_Sol := Current;
      
      -- Choose a random index to modify
      Index := Integer(Random * Float(Dimension)) + 1;
      
      -- Choose a random direction (+1 or -1)
      Direction := Integer(Random * 2.0);
      if Direction = 0 then
         Direction := -1;
      end if;
      
      -- Modify the selected index
      New_Sol(Index) := New_Sol(Index) + Direction;
      
      -- Keep within bounds (-10 to 10)
      if New_Sol(Index) < -10 then
         New_Sol(Index) := -10;
      elsif New_Sol(Index) > 10 then
         New_Sol(Index) := 10;
      end if;
   end Move_To_Neighbor;
   
   -- Update tabu list
   procedure Update_Tabu_List(New_Sol : Solution) is
      Oldest_Index : Integer := 1;
      Min_Index : Integer := 1;
   begin
      -- Find oldest entry (smallest index)
      for I in Tabu_List'Range loop
         if Tabu_Indices(I) > 0 and Tabu_Indices(I) < Tabu_Indices(Min_Index) then
            Min_Index := I;
         end if;
      end loop;
      
      -- Replace oldest entry
      Tabu_List(Min_Index) := New_Sol;
      Tabu_Indices(Min_Index) := Iteration_Count + Tabu_List_Size;
   end Update_Tabu_List;
   
   -- Main Tabu Search algorithm
   procedure Run_Tabu_Search is
      Current_Value : Float;
      Best_Neighbor : Solution;
      Best_Neighbor_Value : Float := Float'Last;
      Neighbor : Solution;
      Neighbor_Value : Float;
      Found_Better : Boolean;
   begin
      -- Initialize
      Random_Solution(Current_Solution);
      Best_Solution := Current_Solution;
      Best_Value := Objective(Current_Solution);
      
      Iteration_Count := 0;
      
      -- Main loop
      while Iteration_Count < Max_Iterations loop
         Iteration_Count := Iteration_Count + 1;
         Current_Value := Objective(Current_Solution);
         
         -- Reset best neighbor
         Best_Neighbor_Value := Float'Last;
         Found_Better := False;
         
         -- Generate neighborhood solutions
         for I in 1..Dimension * 5 loop  -- Generate 5*dimension neighbors
            Move_To_Neighbor(Current_Solution, Neighbor);
            Neighbor_Value := Objective(Neighbor);
            
            -- Check if neighbor is better and not tabu
            if (Neighbor_Value < Best_Neighbor_Value) and 
               (not Is_Tabu(Neighbor) or Neighbor_Value < Best_Value) then
               Best_Neighbor_Value := Neighbor_Value;
               Best_Neighbor := Neighbor;
               Found_Better := True;
            end if;
         end loop;
         
         -- If no better neighbor found, continue with current solution
         if not Found_Better then
            null;  -- Continue with current solution
         else
            -- Move to best neighbor
            Current_Solution := Best_Neighbor;
            
            -- Update best solution if found
            if Best_Neighbor_Value < Best_Value then
               Best_Value := Best_Neighbor_Value;
               Best_Solution := Best_Neighbor;
            end if;
            
            -- Update tabu list
            Update_Tabu_List(Best_Neighbor);
         end if;
         
         -- Update tabu indices (remove expired entries)
         for I in Tabu_List'Range loop
            if Tabu_Indices(I) <= Iteration_Count then
               Tabu_Indices(I) := 0;
            end if;
         end loop;
         
         -- Display progress every 100 iterations
         if Iteration_Count mod 100 = 0 then
            Put("Iteration: ");
            Put(Iteration_Count, 0);
            Put(" Best Value: ");
            Put(Best_Value, 0, 2, 0);
            New_Line;
         end if;
      end loop;
      
      -- Output final results
      Put_Line("Tabu Search Complete!");
      Put_Line("Best Solution:");
      for I in Best_Solution'Range loop
         Put(Best_Solution(I), 4);
      end loop;
      New_Line;
      Put("Best Value: ");
      Put(Best_Value, 0, 2, 0);
      New_Line;
   end Run_Tabu_Search;
   
begin
   Put_Line("Tabu Search Algorithm in Ada");
   Put_Line("============================");
   
   Run_Tabu_Search;
   
end Tabu_Search;
```

## Key Features of this Implementation:

1. **Solution Representation**: Uses an array of integers for the solution
2. **Tabu List Management**: Maintains a list of recently visited solutions
3. **Neighborhood Generation**: Creates new solutions by modifying one element
4. **Tabu Tenure**: Uses iteration-based tenure for tabu entries
5. **Aspiration Criterion**: Allows tabu solutions if they're better than current best
6. **Objective Function**: Minimizes the sum of squares of solution components

## How to Compile and Run:

```bash
gnatmake tabu_search.adb
./tabu_search
```

This implementation demonstrates the core concepts of Tabu Search:
- Tabu list to prevent cycling
- Aspiration criteria to allow better solutions
- Neighborhood search mechanism
- Iterative improvement process

The algorithm will find a near-optimal solution to the minimization problem of sum of squares of variables.

