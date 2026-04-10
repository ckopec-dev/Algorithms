# Artificial Bee Colony Algorithm in Pascal

Here's a complete implementation of the Artificial Bee Colony (ABC) algorithm in Pascal for solving optimization problems:

```pascal
program ArtificialBeeColony;

uses
  SysUtils, Math;

const
  MAX_DIMENSION = 10;        // Maximum number of dimensions
  MAX_POPULATION = 50;       // Population size
  MAX_ITERATIONS = 1000;     // Maximum iterations
  MAX_TRIES = 100;           // Maximum number of trials
  SEARCH_RANGE = 5.0;        // Search range [-5, 5]

type
  TPosition = array[1..MAX_DIMENSION] of Double;
  TFoodSource = record
    position: TPosition;
    fitness: Double;
    trial: Integer;
  end;
  TABC = record
    population: array[1..MAX_POPULATION] of TFoodSource;
    bestSolution: TFoodSource;
    iteration: Integer;
    dimension: Integer;
  end;

var
  abc: TABC;
  randomSeed: Integer;

// Objective function to minimize (Sphere function)
function ObjectiveFunction(position: TPosition): Double;
var
  i: Integer;
  sum: Double;
begin
  sum := 0.0;
  for i := 1 to abc.dimension do
    sum := sum + Sqr(position[i]);
  Result := sum;
end;

// Initialize random number generator
procedure InitializeRandom;
begin
  randomize;
  randomSeed := Random(1000);
end;

// Generate random number between min and max
function RandomRange(min, max: Double): Double;
begin
  Result := min + Random * (max - min);
end;

// Initialize food sources
procedure InitializePopulation;
var
  i, j: Integer;
begin
  for i := 1 to MAX_POPULATION do
  begin
    // Initialize each dimension randomly
    for j := 1 to abc.dimension do
      abc.population[i].position[j] := RandomRange(-SEARCH_RANGE, SEARCH_RANGE);
    
    // Calculate fitness
    abc.population[i].fitness := ObjectiveFunction(abc.population[i].position);
    abc.population[i].trial := 0;
  end;
  
  // Initialize best solution
  abc.bestSolution := abc.population[1];
  for i := 2 to MAX_POPULATION do
  begin
    if abc.population[i].fitness < abc.bestSolution.fitness then
      abc.bestSolution := abc.population[i];
  end;
end;

// Employed bee phase
procedure EmployedBeePhase;
var
  i, j, k: Integer;
  newSolution: TPosition;
  trialLimit: Integer;
begin
  for i := 1 to MAX_POPULATION do
  begin
    // Choose a random dimension
    j := Random(abc.dimension) + 1;
    
    // Choose a random employed bee
    repeat
      k := Random(MAX_POPULATION) + 1;
    until k <> i;
    
    // Generate new solution using formula
    newSolution := abc.population[i].position;
    newSolution[j] := abc.population[i].position[j] + 
                     RandomRange(-1.0, 1.0) * 
                     (abc.population[i].position[j] - abc.population[k].position[j]);
    
    // Boundary check
    if newSolution[j] < -SEARCH_RANGE then
      newSolution[j] := -SEARCH_RANGE
    else if newSolution[j] > SEARCH_RANGE then
      newSolution[j] := SEARCH_RANGE;
    
    // Calculate new fitness
    if ObjectiveFunction(newSolution) < abc.population[i].fitness then
    begin
      abc.population[i].position := newSolution;
      abc.population[i].fitness := ObjectiveFunction(newSolution);
      abc.population[i].trial := 0;
      
      // Update global best if needed
      if abc.population[i].fitness < abc.bestSolution.fitness then
        abc.bestSolution := abc.population[i];
    end
    else
      abc.population[i].trial := abc.population[i].trial + 1;
  end;
end;

// Onlooker bee phase
procedure OnlookerBeePhase;
var
  i, j, k: Integer;
  newSolution: TPosition;
  totalFitness: Double;
  probability: Double;
  r: Double;
begin
  // Calculate total fitness
  totalFitness := 0.0;
  for i := 1 to MAX_POPULATION do
    totalFitness := totalFitness + (1.0 / (abc.population[i].fitness + 1.0));
  
  // Each onlooker bee selects a food source
  for i := 1 to MAX_POPULATION do
  begin
    r := RandomRange(0.0, 1.0);
    probability := 0.0;
    
    // Roulette wheel selection
    for j := 1 to MAX_POPULATION do
    begin
      probability := probability + (1.0 / (abc.population[j].fitness + 1.0)) / totalFitness;
      if r <= probability then
      begin
        // Choose a random dimension
        k := Random(abc.dimension) + 1;
        
        // Choose a random employed bee
        repeat
          k := Random(MAX_POPULATION) + 1;
        until k <> j;
        
        // Generate new solution
        newSolution := abc.population[j].position;
        newSolution[k] := abc.population[j].position[k] + 
                         RandomRange(-1.0, 1.0) * 
                         (abc.population[j].position[k] - abc.population[k].position[k]);
        
        // Boundary check
        if newSolution[k] < -SEARCH_RANGE then
          newSolution[k] := -SEARCH_RANGE
        else if newSolution[k] > SEARCH_RANGE then
          newSolution[k] := SEARCH_RANGE;
        
        // Calculate new fitness
        if ObjectiveFunction(newSolution) < abc.population[j].fitness then
        begin
          abc.population[j].position := newSolution;
          abc.population[j].fitness := ObjectiveFunction(newSolution);
          abc.population[j].trial := 0;
          
          // Update global best if needed
          if abc.population[j].fitness < abc.bestSolution.fitness then
            abc.bestSolution := abc.population[j];
        end
        else
          abc.population[j].trial := abc.population[j].trial + 1;
        
        break;
      end;
    end;
  end;
end;

// Scout bee phase
procedure ScoutBeePhase;
var
  i: Integer;
begin
  for i := 1 to MAX_POPULATION do
  begin
    if abc.population[i].trial >= MAX_TRIES then
    begin
      // Replace with new random solution
      for j := 1 to abc.dimension do
        abc.population[i].position[j] := RandomRange(-SEARCH_RANGE, SEARCH_RANGE);
      
      abc.population[i].fitness := ObjectiveFunction(abc.population[i].position);
      abc.population[i].trial := 0;
      
      // Update global best if needed
      if abc.population[i].fitness < abc.bestSolution.fitness then
        abc.bestSolution := abc.population[i];
    end;
  end;
end;

// Main ABC algorithm
procedure RunABC;
var
  i: Integer;
begin
  writeln('Running Artificial Bee Colony Algorithm');
  writeln('=====================================');
  
  // Initialize
  abc.iteration := 0;
  InitializeRandom;
  InitializePopulation;
  
  // Main loop
  while abc.iteration < MAX_ITERATIONS do
  begin
    EmployedBeePhase;
    OnlookerBeePhase;
    ScoutBeePhase;
    
    Inc(abc.iteration);
    
    // Print progress every 100 iterations
    if abc.iteration mod 100 = 0 then
    begin
      writeln('Iteration ', abc.iteration, ': Best fitness = ', abc.bestSolution.fitness:8:6);
    end;
  end;
  
  writeln('Algorithm completed!');
  writeln('Best solution found:');
  writeln('Fitness: ', abc.bestSolution.fitness:8:6);
  writeln('Position:');
  for i := 1 to abc.dimension do
    write('x[', i, '] = ', abc.bestSolution.position[i]:8:6, ' ');
  writeln;
end;

// Main program
begin
  writeln('Artificial Bee Colony Algorithm Demo');
  writeln('====================================');
  
  // Set dimension
  abc.dimension := 5;
  
  // Run the algorithm
  RunABC;
  
  writeln('Press Enter to exit...');
  ReadLn;
end.
```

## Key Features of this Implementation:

1. **Complete ABC Algorithm Structure**: Implements all three phases - employed bees, onlooker bees, and scout bees
2. **Sphere Function Optimization**: Uses the classic Sphere function as the objective function
3. **Boundary Handling**: Ensures solutions stay within the defined search range
4. **Fitness Calculation**: Uses the inverse of fitness for selection (minimization problem)
5. **Progress Tracking**: Shows iteration progress and best fitness values
6. **Random Number Generation**: Properly initialized random number generator

## How it Works:

1. **Initialization**: Creates random population of food sources (solutions)
2. **Employed Bee Phase**: Each employed bee searches for better solutions in its neighborhood
3. **Onlooker Bee Phase**: Onlooker bees select food sources based on fitness probability
4. **Scout Bee Phase**: Abandoned food sources are replaced with new random solutions
5. **Iteration**: Repeats until maximum iterations are reached

## Usage:

To compile and run this program:
```bash
fpc artificial_bee_colony.pas
./artificial_bee_colony
```

The algorithm will output the progress and final best solution found for the optimization problem.

