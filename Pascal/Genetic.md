# Genetic Algorithm in Pascal

Here's a complete example of a Genetic Algorithm implemented in Pascal that solves the classic "One Max" problem (finding a binary string of all 1s):

```pascal
program GeneticAlgorithm;

uses
  crt, math;

const
  POPULATION_SIZE = 100;
  CHROMOSOME_LENGTH = 20;
  MAX_GENERATIONS = 1000;
  MUTATION_RATE = 0.01;
  CROSSOVER_RATE = 0.8;

type
  TChromosome = array[1..CHROMOSOME_LENGTH] of integer;
  TPopulation = array[1..POPULATION_SIZE] of TChromosome;

var
  population: TPopulation;
  fitness: array[1..POPULATION_SIZE] of integer;
  generation: integer;
  bestFitness: integer;
  bestChromosome: TChromosome;

// Initialize random population
procedure InitializePopulation;
var
  i, j: integer;
begin
  randomize;
  for i := 1 to POPULATION_SIZE do
  begin
    for j := 1 to CHROMOSOME_LENGTH do
    begin
      population[i][j] := random(2); // 0 or 1
    end;
  end;
end;

// Calculate fitness (number of 1s)
function CalculateFitness(chrome: TChromosome): integer;
var
  i, fit: integer;
begin
  fit := 0;
  for i := 1 to CHROMOSOME_LENGTH do
  begin
    if chrome[i] = 1 then
      inc(fit);
  end;
  CalculateFitness := fit;
end;

// Evaluate all fitness values
procedure EvaluateFitness;
var
  i: integer;
begin
  bestFitness := 0;
  for i := 1 to POPULATION_SIZE do
  begin
    fitness[i] := CalculateFitness(population[i]);
    if fitness[i] > bestFitness then
    begin
      bestFitness := fitness[i];
      bestChromosome := population[i];
    end;
  end;
end;

// Tournament selection
function TournamentSelection: integer;
var
  i, selected, tournamentSize, competitor: integer;
  bestFit: integer;
begin
  tournamentSize := 3;
  bestFit := -1;
  selected := 0;
  
  for i := 1 to tournamentSize do
  begin
    competitor := random(POPULATION_SIZE) + 1;
    if (bestFit = -1) or (fitness[competitor] > bestFit) then
    begin
      bestFit := fitness[competitor];
      selected := competitor;
    end;
  end;
  
  TournamentSelection := selected;
end;

// Single point crossover
procedure Crossover(parent1, parent2: TChromosome; var child1, child2: TChromosome);
var
  crossoverPoint, i: integer;
begin
  crossoverPoint := random(CHROMOSOME_LENGTH) + 1;
  
  for i := 1 to CHROMOSOME_LENGTH do
  begin
    if i <= crossoverPoint then
    begin
      child1[i] := parent1[i];
      child2[i] := parent2[i];
    end
    else
    begin
      child1[i] := parent2[i];
      child2[i] := parent1[i];
    end;
  end;
end;

// Mutation
procedure Mutate(var chromosome: TChromosome);
var
  i: integer;
begin
  for i := 1 to CHROMOSOME_LENGTH do
  begin
    if random(1.0) < MUTATION_RATE then
    begin
      if chromosome[i] = 0 then
        chromosome[i] := 1
      else
        chromosome[i] := 0;
    end;
  end;
end;

// Create new generation
procedure CreateNewGeneration;
var
  newPopulation: TPopulation;
  parent1, parent2, child1, child2: TChromosome;
  i, selected1, selected2: integer;
begin
  for i := 1 to POPULATION_SIZE div 2 do
  begin
    // Selection
    selected1 := TournamentSelection;
    selected2 := TournamentSelection;
    
    // Crossover
    if random(1.0) < CROSSOVER_RATE then
    begin
      Crossover(population[selected1], population[selected2], child1, child2);
    end
    else
    begin
      child1 := population[selected1];
      child2 := population[selected2];
    end;
    
    // Mutation
    Mutate(child1);
    Mutate(child2);
    
    // Add to new population
    newPopulation[i] := child1;
    newPopulation[POPULATION_SIZE div 2 + i] := child2;
  end;
  
  // Copy new population to current population
  population := newPopulation;
end;

// Display best solution
procedure DisplayBest;
var
  i: integer;
begin
  writeln('Generation: ', generation:3, ' Best Fitness: ', bestFitness:3);
  write('Best Chromosome: ');
  for i := 1 to CHROMOSOME_LENGTH do
    write(bestChromosome[i]);
  writeln;
end;

// Main genetic algorithm
procedure GeneticAlgorithm;
begin
  writeln('Genetic Algorithm - One Max Problem');
  writeln('====================================');
  writeln;
  
  InitializePopulation;
  
  for generation := 1 to MAX_GENERATIONS do
  begin
    EvaluateFitness;
    
    if generation mod 50 = 0 then
      DisplayBest;
    
    if bestFitness = CHROMOSOME_LENGTH then
    begin
      writeln('Solution found in generation ', generation);
      DisplayBest;
      halt;
    end;
    
    CreateNewGeneration;
  end;
  
  writeln('Maximum generations reached');
  EvaluateFitness;
  DisplayBest;
end;

begin
  GeneticAlgorithm;
  writeln;
  writeln('Press any key to exit...');
  readln;
end.
```

## Key Components Explained:

### 1. **Data Structures**
- `TChromosome`: Binary string of 0s and 1s
- `TPopulation`: Array of chromosomes

### 2. **Main Functions**
- **InitializePopulation**: Creates random binary strings
- **CalculateFitness**: Counts number of 1s (target is maximum fitness)
- **TournamentSelection**: Selects parents using tournament selection
- **Crossover**: Single-point crossover operation
- **Mutate**: Random bit flipping with mutation rate
- **CreateNewGeneration**: Evolutionary process combining selection, crossover, and mutation

### 3. **Algorithm Parameters**
- Population size: 100 individuals
- Chromosome length: 20 bits
- Mutation rate: 1%
- Crossover rate: 80%
- Maximum generations: 1000

### 4. **Expected Output**
The algorithm will find a binary string of all 1s (fitness = 20) through evolutionary processes, typically within a few hundred generations.

This implementation demonstrates core genetic algorithm concepts including selection, crossover, mutation, and fitness evaluation in a clear Pascal structure.

