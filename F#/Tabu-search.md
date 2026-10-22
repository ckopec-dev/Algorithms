```fsharp
// Tabu Search Algorithm Implementation in F#

open System
open System.Collections.Generic

// Problem: Find minimum value of a function using tabu search
// Example: Minimize f(x) = x^2 + 5*sin(x) over range [-10, 10]

type Solution = {
    X: float
    Value: float
}

type TabuSearchState = {
    CurrentSolution: Solution
    BestSolution: Solution
    TabuList: HashSet<string>
    Iteration: int
    MaxIterations: int
    TabuTenure: int
}

// Objective function to minimize
let objectiveFunction x = x ** 2.0 + 5.0 * sin(x)

// Generate neighboring solutions
let generateNeighbors currentSolution neighborhoodSize stepSize =
    [ for i in -neighborhoodSize .. neighborhoodSize do
        let neighborX = currentSolution.X + float i * stepSize
        if neighborX >= -10.0 && neighborX <= 10.0 then
            yield { X = neighborX; Value = objectiveFunction neighborX } ]

// Check if solution is in tabu list
let isInTabuList (tabuList: HashSet<string>) (solution: Solution) =
    tabuList.Contains(sprintf "%.2f" solution.X)

// Add solution to tabu list
let addToTabuList (tabuList: HashSet<string>) (solution: Solution) tenure =
    tabuList.Add(sprintf "%.2f" solution.X) |> ignore
    // Remove oldest entries if needed
    if tabuList.Count > tenure then
        let oldest = tabuList |> Seq.head
        tabuList.Remove(oldest) |> ignore

// Tabu Search Algorithm
let tabuSearch (initialX: float) maxIterations tabuTenure neighborhoodSize stepSize =
    let mutable state = {
        CurrentSolution = { X = initialX; Value = objectiveFunction initialX }
        BestSolution = { X = initialX; Value = objectiveFunction initialX }
        TabuList = HashSet<string>()
        Iteration = 0
        MaxIterations = maxIterations
        TabuTenure = tabuTenure
    }

    printfn "Starting Tabu Search..."
    printfn "Initial solution: x=%.2f, f(x)=%.4f" state.CurrentSolution.X state.CurrentSolution.Value

    while state.Iteration < state.MaxIterations do
        // Generate neighbors
        let neighbors = generateNeighbors state.CurrentSolution neighborhoodSize stepSize
        
        // Find best non-tabu neighbor
        let bestNeighbor = 
            neighbors
            |> Seq.filter (fun neighbor -> not (isInTabuList state.TabuList neighbor))
            |> Seq.minBy (fun neighbor -> neighbor.Value)
        
        // Update current solution
        state <- { 
            state with 
                CurrentSolution = bestNeighbor
                Iteration = state.Iteration + 1
        }
        
        // Update best solution if found better one
        if bestNeighbor.Value < state.BestSolution.Value then
            state <- { state with BestSolution = bestNeighbor }
        
        // Add current solution to tabu list
        addToTabuList state.TabuList state.CurrentSolution state.TabuTenure
        
        // Print progress
        if state.Iteration % 10 = 0 then
            printfn "Iteration %d: x=%.2f, f(x)=%.4f, Best=%.4f" 
                state.Iteration 
                state.CurrentSolution.X 
                state.CurrentSolution.Value 
                state.BestSolution.Value

    state

// Run the algorithm
let main() =
    printfn "=== Tabu Search Algorithm Demo ==="
    
    let initialX = 2.0
    let maxIterations = 100
    let tabuTenure = 5
    let neighborhoodSize = 10
    let stepSize = 0.5
    
    let result = 
        tabuSearch initialX maxIterations tabuTenure neighborhoodSize stepSize
    
    printfn "\n=== Results ==="
    printfn "Final solution: x=%.4f, f(x)=%.6f" result.BestSolution.X result.BestSolution.Value
    printfn "Iterations completed: %d" result.Iteration

// Execute the example
main()
```

This F# implementation demonstrates:

1. **Problem Setup**: Minimizing a mathematical function using tabu search
2. **Key Components**:
   - `Solution` type to represent candidate solutions
   - `TabuSearchState` to track algorithm progress
   - `objectiveFunction` for the problem to solve
   - `generateNeighbors` to create neighboring solutions
   - Tabu list management functions

3. **Tabu Search Logic**:
   - Generates neighbors around current solution
   - Avoids tabu solutions (prevents cycling)
   - Updates best solution found
   - Maintains tabu tenure for recently visited solutions

4. **Parameters**:
   - `maxIterations`: Maximum search iterations
   - `tabuTenure`: How long a solution stays in tabu list
   - `neighborhoodSize`: Size of neighborhood to explore
   - `stepSize`: Step size for generating neighbors

The algorithm efficiently explores the search space while avoiding previously visited solutions, making it suitable for complex optimization problems where local search might get stuck in local optima.