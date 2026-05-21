# Simulated Annealing Algorithm in Pascal

Here's a complete implementation of the Simulated Annealing algorithm in Pascal for solving the Traveling Salesman Problem (TSP):

```pascal
program SimulatedAnnealingTSP;

uses
  crt, sysutils, math;

const
  MAX_CITIES = 100;
  MAX_ITERATIONS = 10000;
  INITIAL_TEMPERATURE = 1000.0;
  COOLING_RATE = 0.995;
  MIN_TEMPERATURE = 0.01;

type
  TCity = record
    x, y: real;
  end;

  TCityArray = array[1..MAX_CITIES] of TCity;
  TRoute = array[1..MAX_CITIES] of integer;

var
  cities: TCityArray;
  route: TRoute;
  bestRoute: TRoute;
  numCities: integer;
  bestDistance: real;
  currentDistance: real;
  temperature: real;

// Calculate Euclidean distance between two cities
function Distance(city1, city2: integer): real;
begin
  Distance := sqrt(sqr(cities[city1].x - cities[city2].x) + 
                   sqr(cities[city2].y - cities[city1].y));
end;

// Calculate total distance of a route
function TotalDistance(route: TRoute): real;
var
  i: integer;
  total: real;
begin
  total := 0;
  for i := 1 to numCities - 1 do
    total := total + Distance(route[i], route[i + 1]);
  // Return to starting city
  total := total + Distance(route[numCities], route[1]);
  TotalDistance := total;
end;

// Generate a random route
procedure GenerateRandomRoute(var route: TRoute);
var
  i, j, temp, swapIndex: integer;
begin
  // Initialize route with sequential cities
  for i := 1 to numCities do
    route[i] := i;
  
  // Randomly shuffle the route
  for i := 1 to numCities do
  begin
    swapIndex := random(numCities) + 1;
    temp := route[i];
    route[i] := route[swapIndex];
    route[swapIndex] := temp;
  end;
end;

// Generate a neighbor route by swapping two cities
procedure GenerateNeighbor(var route: TRoute; var neighbor: TRoute);
var
  i, j, temp: integer;
begin
  // Copy current route
  for i := 1 to numCities do
    neighbor[i] := route[i];
  
  // Swap two random cities
  i := random(numCities) + 1;
  repeat
    j := random(numCities) + 1;
  until i <> j;
  
  temp := neighbor[i];
  neighbor[i] := neighbor[j];
  neighbor[j] := temp;
end;

// Simulated Annealing algorithm
procedure SimulatedAnnealing;
var
  i, j: integer;
  neighbor: TRoute;
  delta: real;
  acceptanceProbability: real;
  currentRoute: TRoute;
begin
  // Initialize
  temperature := INITIAL_TEMPERATURE;
  GenerateRandomRoute(currentRoute);
  currentDistance := TotalDistance(currentRoute);
  bestDistance := currentDistance;
  
  for i := 1 to numCities do
  begin
    bestRoute[i] := currentRoute[i];
  end;
  
  // Main annealing loop
  for i := 1 to MAX_ITERATIONS do
  begin
    // Generate neighbor solution
    GenerateNeighbor(currentRoute, neighbor);
    
    // Calculate change in distance
    delta := TotalDistance(neighbor) - currentDistance;
    
    // Accept or reject the neighbor
    if delta < 0 then
    begin
      // Always accept better solutions
      for j := 1 to numCities do
        currentRoute[j] := neighbor[j];
      currentDistance := TotalDistance(currentRoute);
      
      // Update best solution
      if currentDistance < bestDistance then
      begin
        bestDistance := currentDistance;
        for j := 1 to numCities do
          bestRoute[j] := currentRoute[j];
      end;
    end
    else
    begin
      // Accept worse solutions with probability
      acceptanceProbability := exp(-delta / temperature);
      if random < acceptanceProbability then
      begin
        for j := 1 to numCities do
          currentRoute[j] := neighbor[j];
        currentDistance := TotalDistance(currentRoute);
      end;
    end;
    
    // Cool down the temperature
    temperature := temperature * COOLING_RATE;
    
    // Stop if temperature is too low
    if temperature < MIN_TEMPERATURE then
      break;
  end;
end;

// Print the best route and distance
procedure PrintResult;
var
  i: integer;
begin
  writeln('Best route found:');
  for i := 1 to numCities do
    write(bestRoute[i], ' ');
  writeln;
  writeln('Total distance: ', bestDistance:0:2);
end;

// Initialize sample cities
procedure InitializeCities;
begin
  numCities := 10;
  
  // Sample cities (you can modify these coordinates)
  cities[1].x := 60;  cities[1].y := 200;
  cities[2].x := 180; cities[2].y := 200;
  cities[3].x := 80;  cities[3].y := 180;
  cities[4].x := 140; cities[4].y := 180;
  cities[5].x := 20;  cities[5].y := 160;
  cities[6].x := 100; cities[6].y := 160;
  cities[7].x := 200; cities[7].y := 160;
  cities[8].x := 140; cities[8].y := 140;
  cities[9].x := 40;  cities[9].y := 120;
  cities[10].x := 100; cities[10].y := 120;
end;

begin
  randomize;
  writeln('Simulated Annealing - Traveling Salesman Problem');
  writeln('================================================');
  
  InitializeCities;
  
  writeln('Number of cities: ', numCities);
  writeln('Cities coordinates:');
  for var i := 1 to numCities do
    writeln('City ', i, ': (', cities[i].x:0:0, ', ', cities[i].y:0:0, ')');
  writeln;
  
  SimulatedAnnealing;
  PrintResult;
  
  writeln;
  writeln('Press any key to exit...');
  readln;
end.
```

## Key Components of the Algorithm:

### 1. **Core Functions:**
- `Distance()`: Calculates Euclidean distance between two cities
- `TotalDistance()`: Computes total route distance
- `GenerateRandomRoute()`: Creates initial random solution
- `GenerateNeighbor()`: Creates neighboring solution by swapping cities

### 2. **Simulated Annealing Logic:**
- **Temperature Cooling**: Starts high and gradually decreases
- **Acceptance Probability**: Accepts worse solutions with probability based on temperature
- **Exploration vs Exploitation**: Balances between exploring new solutions and exploiting good ones

### 3. **Parameters:**
- `INITIAL_TEMPERATURE`: Starting temperature (higher = more exploration)
- `COOLING_RATE`: How quickly temperature decreases
- `MIN_TEMPERATURE`: Stop when temperature gets too low
- `MAX_ITERATIONS`: Maximum number of iterations

### 4. **Algorithm Steps:**
1. Generate initial random solution
2. For each iteration:
   - Generate neighbor solution
   - Calculate energy difference (distance change)
   - Accept neighbor if better or with probability based on temperature
   - Cool down temperature
3. Return best solution found

This implementation demonstrates how simulated annealing can escape local optima and find good solutions to the TSP by gradually reducing the probability of accepting worse solutions as the algorithm progresses.

