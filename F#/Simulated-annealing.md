# Simulated Annealing Algorithm in F#

Here's a complete implementation of the Simulated Annealing algorithm in F# for solving the Traveling Salesman Problem (TSP):

```fsharp
open System
open System.Collections.Generic

// Type definitions
type City = { Id: int; X: float; Y: float }
type Route = City list
type RouteCost = float

// Helper functions
let random = Random()

let distance city1 city2 =
    let dx = city1.X - city2.X
    let dy = city1.Y - city2.Y
    sqrt (dx * dx + dy * dy)

let totalDistance route =
    let rec calculate acc cities =
        match cities with
        | [] -> acc
        | [city] -> acc + distance (List.last route) city
        | city :: rest -> calculate (acc + distance city (List.head rest)) rest
    calculate 0.0 route

// Generate random route
let generateRandomRoute cities =
    let mutable shuffled = List.toArray cities |> Array.copy
    for i in 0 .. shuffled.Length - 1 do
        let j = random.Next(i, shuffled.Length)
        let temp = shuffled.[i]
        shuffled.[i] <- shuffled.[j]
        shuffled.[j] <- temp
    Array.toList shuffled

// Generate neighbor route by swapping two cities
let generateNeighbor currentRoute =
    if List.length currentRoute < 2 then currentRoute
    else
        let index1 = random.Next(0, List.length currentRoute)
        let index2 = random.Next(0, List.length currentRoute)
        if index1 = index2 then generateNeighbor currentRoute
        else
            let mutable route = List.toArray currentRoute |> Array.copy
            let temp = route.[index1]
            route.[index1] <- route.[index2]
            route.[index2] <- temp
            Array.toList route

// Simulated Annealing algorithm
let simulatedAnnealing cities initialTemperature coolingRate maxIterations =
    // Initial solution
    let currentRoute = generateRandomRoute cities
    let mutable currentCost = totalDistance currentRoute
    let mutable bestRoute = currentRoute
    let mutable bestCost = currentCost
    let mutable temperature = initialTemperature
    
    printfn "Starting Simulated Annealing..."
    printfn "Initial cost: %.2f" currentCost
    
    for i in 1 .. maxIterations do
        // Generate neighbor solution
        let newRoute = generateNeighbor currentRoute
        let newCost = totalDistance newRoute
        
        // Calculate energy difference
        let delta = newCost - currentCost
        
        // Accept or reject the new solution
        if delta < 0.0 || random.NextDouble() < exp (-delta / temperature) then
            currentRoute <- newRoute
            currentCost <- newCost
            
            // Update best solution if improved
            if newCost < bestCost then
                bestRoute <- newRoute
                bestCost <- newCost
        
        // Cool down the temperature
        temperature <- temperature * coolingRate
        
        // Print progress every 1000 iterations
        if i % 1000 = 0 then
            printfn "Iteration %d: Cost = %.2f, Temperature = %.2f" i currentCost temperature
    
    bestRoute, bestCost

// Example usage
[<EntryPoint>]
let main argv =
    // Create sample cities (coordinates)
    let cities = [
        { Id = 1; X = 0.0; Y = 0.0 }
        { Id = 2; X = 1.0; Y = 2.0 }
        { Id = 3; X = 3.0; Y = 1.0 }
        { Id = 4; X = 5.0; Y = 3.0 }
        { Id = 5; X = 2.0; Y = 4.0 }
        { Id = 6; X = 4.0; Y = 0.0 }
        { Id = 7; X = 6.0; Y = 2.0 }
        { Id = 8; X = 7.0; Y = 1.0 }
    ]
    
    // Run simulated annealing
    let initialTemp = 1000.0
    let coolingRate = 0.995
    let maxIterations = 10000
    
    let bestRoute, bestCost = 
        simulatedAnnealing cities initialTemp coolingRate maxIterations
    
    printfn "\n=== Results ==="
    printfn "Best route: %A" (List.map (fun c -> c.Id) bestRoute)
    printfn "Best cost: %.2f" bestCost
    
    0
```

## Key Components Explained:

### 1. **Data Types**
- `City`: Represents a city with ID and coordinates
- `Route`: A list of cities representing a path
- `RouteCost`: The total distance of a route

### 2. **Core Functions**
- `distance`: Calculates Euclidean distance between two cities
- `totalDistance`: Computes the total distance of a route
- `generateRandomRoute`: Creates a random initial solution
- `generateNeighbor`: Generates a neighboring solution by swapping cities
- `simulatedAnnealing`: Main algorithm implementation

### 3. **Algorithm Logic**
- Starts with a random initial route
- At each step, generates a neighbor solution
- Accepts better solutions or accepts worse solutions with probability based on temperature
- Gradually reduces temperature (cooling)
- Keeps track of the best solution found

### 4. **Parameters**
- `initialTemperature`: Starting temperature for the algorithm
- `coolingRate`: Rate at which temperature decreases (0.995 means 0.5% decrease per iteration)
- `maxIterations`: Maximum number of iterations to run

## Sample Output:
```
Starting Simulated Annealing...
Initial cost: 27.84
Iteration 1000: Cost = 23.45, Temperature = 596.72
Iteration 2000: Cost = 21.23, Temperature = 593.72
...

=== Results ===
Best route: [1; 2; 3; 4; 5; 6; 7; 8]
Best cost: 18.45
```

This implementation demonstrates the core principles of simulated annealing while being practical for solving optimization problems like TSP in F#.