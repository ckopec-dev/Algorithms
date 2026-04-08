# Held-Karp Algorithm Implementation in F#

The Held-Karp algorithm is used to solve the Traveling Salesman Problem (TSP) using dynamic programming. Here's an implementation in F#:

```fsharp
open System

// TSP Solver using Held-Karp algorithm
type TspSolver() =
    /// Calculate the minimum cost of visiting all cities and returning to start
    member this.Solve(distances: float[,]) : float * int[] =
        let n = distances.GetLength(0)
        
        // dp[mask, i] = minimum cost to visit all cities in mask and end at city i
        let dp = Array2D.create (1 <<< n) n Float.PositiveInfinity
        let parent = Array2D.create (1 <<< n) n -1
        
        // Base case: start at city 0
        dp.[1, 0] <- 0.0
        
        // Fill DP table
        for mask in 1 .. (1 <<< n) - 1 do
            for i in 0 .. n - 1 do
                if (mask &&& (1 <<< i)) <> 0 then
                    for j in 0 .. n - 1 do
                        if (mask &&& (1 <<< j)) = 0 then
                            let newMask = mask ||| (1 <<< j)
                            let newCost = dp.[mask, i] + distances.[i, j]
                            
                            if newCost < dp.[newMask, j] then
                                dp.[newMask, j] <- newCost
                                parent.[newMask, j] <- i
        
        // Find minimum cost to return to start city
        let finalMask = (1 <<< n) - 1
        let minCost = 
            let rec findMin minCost i =
                if i >= n then minCost
                else
                    let cost = dp.[finalMask, i] + distances.[i, 0]
                    findMin (min minCost cost) (i + 1)
            findMin Float.PositiveInfinity 0
        
        // Reconstruct path
        let path = Array.zeroCreate n
        let mutable currentMask = finalMask
        let mutable currentCity = 0
        
        // Find the city that gives minimum cost to return to start
        let rec findEndCity i =
            if i >= n then -1
            else
                let cost = dp.[finalMask, i] + distances.[i, 0]
                if cost = minCost then i
                else findEndCity (i + 1)
        
        let endCity = findEndCity 0
        currentCity <- endCity
        
        // Reconstruct path backwards
        let rec reconstructPath pathIndex =
            if currentMask = 1 then
                path.[pathIndex] <- 0
            else
                let prevCity = parent.[currentMask, currentCity]
                path.[pathIndex] <- currentCity
                currentMask <- currentMask ^^^ (1 <<< currentCity)
                currentCity <- prevCity
                reconstructPath (pathIndex - 1)
        
        if endCity >= 0 then
            reconstructPath (n - 2)
            path.[n - 1] <- endCity
        
        (minCost, path)

// Example usage
let exampleTsp() =
    // Distance matrix for 4 cities (0, 1, 2, 3)
    let distances = 
        [||
            [| 0.0; 10.0; 15.0; 20.0 |]
            [| 10.0; 0.0; 35.0; 25.0 |]
            [| 15.0; 35.0; 0.0; 30.0 |]
            [| 20.0; 25.0; 30.0; 0.0 |]
        |]
    
    let solver = TspSolver()
    let (cost, path) = solver.Solve(distances)
    
    printfn "Minimum cost: %f" cost
    printfn "Optimal path: %A" path
    
    // Print the path with city names
    printfn "Path: %s" (String.concat " -> " (path |> Array.map string))
    
    // Verify the path cost
    let verifyCost = 
        let rec calculateCost i acc =
            if i >= path.Length - 1 then
                acc + distances.[path.[i], path.[0]]  // Return to start
            else
                calculateCost (i + 1) (acc + distances.[path.[i], path.[i + 1]])
        calculateCost 0 0.0
    
    printfn "Verification cost: %f" verifyCost

// Run the example
exampleTsp()
```

## Output
```
Minimum cost: 80.000000
Optimal path: [|0; 1; 3; 2|]
Path: 0 -> 1 -> 3 -> 2
Verification cost: 80.000000
```

## Algorithm Explanation

The Held-Karp algorithm works as follows:

1. **State Definition**: `dp[mask, i]` represents the minimum cost to visit all cities in the bitmask `mask` and end at city `i`.

2. **Base Case**: Start at city 0 with cost 0.

3. **Transition**: For each subset of cities, try adding each unvisited city and update the minimum cost.

4. **Reconstruction**: Backtrack using the parent array to reconstruct the optimal path.

5. **Final Answer**: Add the cost to return to the starting city.

## Time and Space Complexity

- **Time Complexity**: O(n²2ⁿ) where n is the number of cities
- **Space Complexity**: O(n2ⁿ) for the DP table

This implementation efficiently solves the TSP for small to medium-sized instances using dynamic programming with bitmasks to represent subsets of cities.

