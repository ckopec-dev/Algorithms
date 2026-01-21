# Artificial Bee Colony Algorithm in Ada

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Random; use Ada.Random;

procedure ABC_Algorithm is
   
   -- Problem: Minimize f(x,y) = x^2 + y^2 (sphere function)
   -- Global minimum at (0,0) with value 0
   
   type Position is record
      X, Y : Float;
   end record;
   
   type Bee is record
      Position : Position;
      Fitness  : Float;
      Trial    : Integer;
   end record;
   
   -- Parameters
   Population_Size : constant Integer := 50;
   Max_Iterations  : constant Integer := 1000;
   Max_Trial       : constant Integer := 100;
   Search_Limit    : constant Float := 5.0;
   
   -- Global best solution
   Global_Best : Bee;
   
   -- Population array
   Population : array(1..Population_Size) of Bee;
   
   -- Initialize random generator
   Rand : Ada.Random.Generator;
   
   -- Objective function (sphere function)
   function Objective_Function(P : Position) return Float is
   begin
      return P.X**2 + P.Y**2;
   end Objective_Function;
   
   -- Initialize population
   procedure Initialize_Population is
      Bee_Count : Integer := 0;
   begin
      for I in 1..Population_Size loop
         declare
            New_Bee : Bee;
         begin
            -- Random initialization within search space
            New_Bee.Position.X := (Random(Rand) - 0.5) * 2.0 * Search_Limit;
            New_Bee.Position.Y := (Random(Rand) - 0.5) * 2.0 * Search_Limit;
            New_Bee.Fitness := Objective_Function(New_Bee.Position);
            New_Bee.Trial := 0;
            
            Population(I) := New_Bee;
            
            -- Update global best
            if I = 1 or else New_Bee.Fitness < Global_Best.Fitness then
               Global_Best := New_Bee;
            end if;
         end;
      end loop;
   end Initialize_Population;
   
   -- Employed Bee Phase
   procedure Employed_Bee_Phase is
   begin
      for I in 1..Population_Size loop
         declare
            New_Position : Position;
            New_Bee      : Bee;
            J            : Integer;
            K            : Integer;
         begin
            -- Choose a random neighbor
            J := Integer(Random(Rand) * Float(Population_Size)) + 1;
            K := Integer(Random(Rand) * 2.0) + 1; -- 1 or 2
            
            -- Generate new position using employed bee rule
            if K = 1 then
               New_Position.X := Population(I).Position.X + 
                                (Random(Rand) - 0.5) * 2.0 * 
                                (Population(I).Position.X - Population(J).Position.X);
               New_Position.Y := Population(I).Position.Y + 
                                (Random(Rand) - 0.5) * 2.0 * 
                                (Population(I).Position.Y - Population(J).Position.Y);
            else
               New_Position.X := Population(I).Position.X + 
                                (Random(Rand) - 0.5) * 2.0 * 
                                (Global_Best.Position.X - Population(I).Position.X);
               New_Position.Y := Population(I).Position.Y + 
                                (Random(Rand) - 0.5) * 2.0 * 
                                (Global_Best.Position.Y - Population(I).Position.Y);
            end if;
            
            -- Ensure bounds
            if New_Position.X > Search_Limit then
               New_Position.X := Search_Limit;
            elsif New_Position.X < -Search_Limit then
               New_Position.X := -Search_Limit;
            end if;
            
            if New_Position.Y > Search_Limit then
               New_Position.Y := Search_Limit;
            elsif New_Position.Y < -Search_Limit then
               New_Position.Y := -Search_Limit;
            end if;
            
            -- Evaluate new solution
            New_Bee.Position := New_Position;
            New_Bee.Fitness := Objective_Function(New_Position);
            New_Bee.Trial := Population(I).Trial;
            
            -- Greedy selection
            if New_Bee.Fitness < Population(I).Fitness then
               Population(I) := New_Bee;
               if New_Bee.Fitness < Global_Best.Fitness then
                  Global_Best := New_Bee;
               end if;
               Population(I).Trial := 0;
            else
               Population(I).Trial := Population(I).Trial + 1;
            end if;
         end;
      end loop;
   end Employed_Bee_Phase;
   
   -- Onlooker Bee Phase
   procedure Onlooker_Bee_Phase is
      Total_Fitness : Float := 0.0;
      Selection_Prob : array(1..Population_Size) of Float;
      Accumulated_Prob : Float := 0.0;
   begin
      -- Calculate total fitness
      for I in 1..Population_Size loop
         Total_Fitness := Total_Fitness + (1.0 / (Population(I).Fitness + 1.0E-10));
      end loop;
      
      -- Calculate selection probabilities
      for I in 1..Population_Size loop
         Selection_Prob(I) := (1.0 / (Population(I).Fitness + 1.0E-10)) / Total_Fitness;
         Accumulated_Prob := Accumulated_Prob + Selection_Prob(I);
         Selection_Prob(I) := Accumulated_Prob;
      end loop;
      
      -- For each onlooker bee
      for I in 1..Population_Size loop
         declare
            R : Float := Random(Rand);
            J : Integer := 1;
         begin
            -- Select a bee based on probability
            while J <= Population_Size and then R > Selection_Prob(J) loop
               J := J + 1;
            end loop;
            
            -- Generate new solution (similar to employed bee)
            declare
               New_Position : Position;
               New_Bee      : Bee;
               K            : Integer;
            begin
               K := Integer(Random(Rand) * 2.0) + 1;
               
               if K = 1 then
                  New_Position.X := Population(J).Position.X + 
                                   (Random(Rand) - 0.5) * 2.0 * 
                                   (Population(J).Position.X - Population(I).Position.X);
                  New_Position.Y := Population(J).Position.Y + 
                                   (Random(Rand) - 0.5) * 2.0 * 
                                   (Population(J).Position.Y - Population(I).Position.Y);
               else
                  New_Position.X := Population(J).Position.X + 
                                   (Random(Rand) - 0.5) * 2.0 * 
                                   (Global_Best.Position.X - Population(J).Position.X);
                  New_Position.Y := Population(J).Position.Y + 
                                   (Random(Rand) - 0.5) * 2.0 * 
                                   (Global_Best.Position.Y - Population(J).Position.Y);
               end if;
               
               -- Ensure bounds
               if New_Position.X > Search_Limit then
                  New_Position.X := Search_Limit;
               elsif New_Position.X < -Search_Limit then
                  New_Position.X := -Search_Limit;
               end if;
               
               if New_Position.Y > Search_Limit then
                  New_Position.Y := Search_Limit;
               elsif New_Position.Y < -Search_Limit then
                  New_Position.Y := -Search_Limit;
               end if;
               
               -- Evaluate new solution
               New_Bee.Position := New_Position;
               New_Bee.Fitness := Objective_Function(New_Position);
               New_Bee.Trial := Population(J).Trial;
               
               -- Greedy selection
               if New_Bee.Fitness < Population(J).Fitness then
                  Population(J) := New_Bee;
                  if New_Bee.Fitness < Global_Best.Fitness then
                     Global_Best := New_Bee;
                  end if;
                  Population(J).Trial := 0;
               else
                  Population(J).Trial := Population(J).Trial + 1;
               end if;
            end;
         end;
      end loop;
   end Onlooker_Bee_Phase;
   
   -- Scout Bee Phase
   procedure Scout_Bee_Phase is
   begin
      for I in 1..Population_Size loop
         if Population(I).Trial >= Max_Trial then
            -- Replace with new random solution
            declare
               New_Bee : Bee;
            begin
               New_Bee.Position.X := (Random(Rand) - 0.5) * 2.0 * Search_Limit;
               New_Bee.Position.Y := (Random(Rand) - 0.5) * 2.0 * Search_Limit;
               New_Bee.Fitness := Objective_Function(New_Bee.Position);
               New_Bee.Trial := 0;
               
               Population(I) := New_Bee;
               
               -- Update global best if needed
               if New_Bee.Fitness < Global_Best.Fitness then
                  Global_Best := New_Bee;
               end if;
            end;
         end if;
      end loop;
   end Scout_Bee_Phase;
   
   -- Main ABC algorithm
   procedure Run_ABC is
      Iteration : Integer := 0;
   begin
      -- Initialize population
      Initialize_Population;
      
      -- Main loop
      while Iteration < Max_Iterations loop
         Employed_Bee_Phase;
         Onlooker_Bee_Phase;
         Scout_Bee_Phase;
         
         Iteration := Iteration + 1;
         
         -- Print progress every 100 iterations
         if Iteration mod 100 = 0 then
            Put("Iteration: ");
            Put(Iteration, Fore => 1, Aft => 0, Exp => 0);
            Put(" Best Fitness: ");
            Put(Global_Best.Fitness, Fore => 1, Aft => 6, Exp => 0);
            Put_Line(" Best Position: (" & 
                     Float'Image(Global_Best.Position.X) & 
                     ", " & 
                     Float'Image(Global_Best.Position.Y) & ")");
         end if;
      end loop;
      
      -- Final result
      Put_Line("Algorithm completed!");
      Put_Line("Final Best Solution:");
      Put("Position: (");
      Put(Global_Best.Position.X, Fore => 1, Aft => 6, Exp => 0);
      Put(", ");
      Put(Global_Best.Position.Y, Fore => 1, Aft => 6, Exp => 0);
      Put_Line(")");
      Put("Fitness: ");
      Put(Global_Best.Fitness, Fore => 1, Aft => 6, Exp => 0);
      Put_Line("");
   end Run_ABC;
   
begin
   -- Initialize random generator
   Reset(Rand);
   
   -- Run the ABC algorithm
   Run_ABC;
   
end ABC_Algorithm;
```

## Key Features of this Implementation

This Ada implementation of the Artificial Bee Colony (ABC) algorithm includes:

1. **Complete ABC Phases**:
   - Employed Bee Phase
   - Onlooker Bee Phase  
   - Scout Bee Phase

2. **Problem Setup**:
   - Minimizes the sphere function f(x,y) = x² + y²
   - Global minimum at (0,0) with value 0

3. **Core Components**:
   - Bee data structure with position, fitness, and trial count
   - Population management
   - Fitness evaluation
   - Boundary constraints
   - Greedy selection mechanism

4. **Algorithm Features**:
   - Random initialization within search space
   - Probability-based selection for onlooker bees
   - Trial counter for scout bee detection
   - Progress tracking and reporting

5. **Ada-Specific Elements**:
   - Proper use of Ada packages and procedures
   - Type declarations with records
   - Modular structure with clear separation of concerns
   - Error handling through bounds checking
   - Text I/O operations for results display

The algorithm will converge toward the global minimum of the sphere function, demonstrating the effectiveness of the ABC optimization approach in Ada.

