# Genetic Algorithm in Ada

Here's a complete example of a genetic algorithm implemented in Ada for solving the classic "One Max" problem (finding a binary string of all 1s):

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Genetic_Algorithm is

   -- Constants
   Population_Size : constant := 100;
   Chromosome_Length : constant := 20;
   Mutation_Rate : constant := 0.01;
   Crossover_Rate : constant := 0.8;
   Generations : constant := 1000;
   
   -- Type definitions
   type Bit is (Zero, One);
   type Chromosome is array (1..Chromosome_Length) of Bit;
   type Population is array (1..Population_Size) of Chromosome;
   
   -- Random number generator
   Gen : Generator;
   
   -- Function to create a random chromosome
   function Random_Chromosome return Chromosome is
      Result : Chromosome;
   begin
      for I in Result'Range loop
         if Random(Gen) < 0.5 then
            Result(I) := Zero;
         else
            Result(I) := One;
         end if;
      end loop;
      return Result;
   end Random_Chromosome;
   
   -- Function to calculate fitness (number of 1s)
   function Fitness(Chrom : Chromosome) return Natural is
      Count : Natural := 0;
   begin
      for I in Chrom'Range loop
         if Chrom(I) = One then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Fitness;
   
   -- Function to print a chromosome
   procedure Print_Chromosome(Chrom : Chromosome) is
   begin
      for I in Chrom'Range loop
         if Chrom(I) = One then
            Put('1');
         else
            Put('0');
         end if;
      end loop;
      New_Line;
   end Print_Chromosome;
   
   -- Function to print population statistics
   procedure Print_Population(Pop : Population) is
      Total_Fitness : Natural := 0;
      Best_Fitness : Natural := 0;
      Worst_Fitness : Natural := Natural'Last;
   begin
      for I in Pop'Range loop
         declare
            F : constant Natural := Fitness(Pop(I));
         begin
            Total_Fitness := Total_Fitness + F;
            if F > Best_Fitness then
               Best_Fitness := F;
            end if;
            if F < Worst_Fitness then
               Worst_Fitness := F;
            end if;
         end;
      end loop;
      
      Put("Best: "); Put(Best_Fitness, 2);
      Put(" Worst: "); Put(Worst_Fitness, 2);
      Put(" Average: "); Put(Total_Fitness / Population_Size, 2);
      New_Line;
   end Print_Population;
   
   -- Selection using tournament selection
   function Tournament_Selection(Pop : Population) return Natural is
      Tournament_Size : constant := 3;
      Best_Index : Natural := 1;
      Best_Fitness : Natural := Fitness(Pop(1));
   begin
      for I in 2..Tournament_Size loop
         declare
            Index : constant Natural := Integer(Random(Gen) * Population_Size) + 1;
            F : constant Natural := Fitness(Pop(Index));
         begin
            if F > Best_Fitness then
               Best_Fitness := F;
               Best_Index := Index;
            end if;
         end;
      end loop;
      return Best_Index;
   end Tournament_Selection;
   
   -- Crossover operation
   procedure Crossover(Chrom1, Chrom2 : in out Chromosome) is
      Crossover_Point : constant Natural := 
         Integer(Random(Gen) * (Chromosome_Length - 1)) + 1;
   begin
      for I in Crossover_Point..Chromosome_Length loop
         declare
            Temp : constant Bit := Chrom1(I);
         begin
            Chrom1(I) := Chrom2(I);
            Chrom2(I) := Temp;
         end;
      end loop;
   end Crossover;
   
   -- Mutation operation
   procedure Mutate(Chrom : in out Chromosome) is
   begin
      for I in Chrom'Range loop
         if Random(Gen) < Mutation_Rate then
            if Chrom(I) = Zero then
               Chrom(I) := One;
            else
               Chrom(I) := Zero;
            end if;
         end if;
      end loop;
   end Mutate;
   
   -- Main genetic algorithm
   procedure Run_Genetic_Algorithm is
      Population : Population;
      New_Population : Population;
      Best_Chromosome : Chromosome;
      Best_Fitness : Natural := 0;
      Generation : Natural := 0;
   begin
      -- Initialize population
      for I in Population'Range loop
         Population(I) := Random_Chromosome;
      end loop;
      
      -- Main loop
      for G in 1..Generations loop
         Generation := G;
         
         -- Find best individual in current population
         for I in Population'Range loop
            declare
               F : constant Natural := Fitness(Population(I));
            begin
               if F > Best_Fitness then
                  Best_Fitness := F;
                  Best_Chromosome := Population(I);
               end if;
            end;
         end loop;
         
         -- Print progress every 100 generations
         if G mod 100 = 0 then
            Put("Generation "); Put(G, 4);
            Put(" Best Fitness: "); Put(Best_Fitness, 2);
            Put(" Best Chromosome: ");
            Print_Chromosome(Best_Chromosome);
            Print_Population(Population);
         end if;
         
         -- Create new population
         for I in New_Population'Range loop
            -- Selection
            declare
               Parent1_Index : constant Natural := Tournament_Selection(Population);
               Parent2_Index : constant Natural := Tournament_Selection(Population);
               Child1 : Chromosome := Population(Parent1_Index);
               Child2 : Chromosome := Population(Parent2_Index);
            begin
               -- Crossover
               if Random(Gen) < Crossover_Rate then
                  Crossover(Child1, Child2);
               end if;
               
               -- Mutation
               Mutate(Child1);
               Mutate(Child2);
               
               -- Add to new population
               New_Population(I) := Child1;
               I := I + 1;
               if I <= New_Population'Last then
                  New_Population(I) := Child2;
               end if;
            end;
         end loop;
         
         -- Replace old population with new one
         Population := New_Population;
      end loop;
      
      -- Print final results
      Put_Line("Final Results:");
      Put("Generation: "); Put(Generation, 4);
      Put(" Best Fitness: "); Put(Best_Fitness, 2);
      Put(" Best Chromosome: ");
      Print_Chromosome(Best_Chromosome);
      
   end Run_Genetic_Algorithm;

begin
   -- Initialize random number generator
   Reset(Gen);
   
   -- Run the genetic algorithm
   Run_Genetic_Algorithm;
   
end Genetic_Algorithm;
```

## Key Features of this Implementation:

1. **Data Types**: 
   - `Bit` type with `Zero` and `One` values
   - `Chromosome` as array of bits
   - `Population` as array of chromosomes

2. **Genetic Operators**:
   - **Selection**: Tournament selection
   - **Crossover**: Single-point crossover
   - **Mutation**: Bit-flip mutation

3. **Fitness Function**: Counts number of 1s in chromosome

4. **Evolution Process**:
   - Initialize random population
   - Evaluate fitness of each individual
   - Select parents using tournament selection
   - Apply crossover and mutation
   - Replace old population with new one
   - Repeat for specified number of generations

5. **Statistics**: Tracks and displays best, worst, and average fitness values

## Expected Output:
The algorithm will show progress every 100 generations, demonstrating how the population evolves toward finding a chromosome with all 1s (optimal solution). The final output will display the best solution found and its fitness value.

This implementation demonstrates the core concepts of genetic algorithms in Ada while maintaining clean, readable code structure.

