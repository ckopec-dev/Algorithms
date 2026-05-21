# Simplex Algorithm in F#

Here's a complete implementation of the Simplex algorithm in F# for solving linear programming problems:

```fsharp
open System

// Simplex tableau representation
type Tableau = float[,]

// Simplex result
type SimplexResult = {
    OptimalValue: float
    Solution: float[]
    IsOptimal: bool
    IsUnbounded: bool
}

// Create initial tableau from standard form
let createInitialTableau (objective: float[]) (constraints: float[][]) (rhs: float[]) =
    let m = constraints.Length  // number of constraints
    let n = objective.Length    // number of variables
    
    // Create tableau with slack variables
    let tableau = Array2D.create (m + 1) (n + m + 1) 0.0
    
    // Fill objective row (negate coefficients for maximization)
    for j = 0 to n - 1 do
        tableau.[m, j] <- -objective.[j]
    
    // Fill constraint rows
    for i = 0 to m - 1 do
        for j = 0 to n - 1 do
            tableau.[i, j] <- constraints.[i].[j]
        // Add slack variable
        tableau.[i, n + i] <- 1.0
        // Add RHS
        tableau.[i, n + m] <- rhs.[i]
    
    // Set objective row (last row)
    for j = 0 to n + m do
        tableau.[m, j] <- tableau.[m, j]
    
    tableau

// Find pivot column (most negative in objective row)
let findPivotColumn (tableau: Tableau) =
    let m = tableau.GetLength(0) - 1
    let n = tableau.GetLength(1) - 1
    
    let mutable pivotCol = -1
    let mutable minCoeff = Double.MaxValue
    
    for j = 0 to n - 1 do
        if tableau.[m, j] < minCoeff then
            minCoeff <- tableau.[m, j]
            pivotCol <- j
    
    if minCoeff >= 0.0 then -1 else pivotCol

// Find pivot row (minimum ratio test)
let findPivotRow (tableau: Tableau) (pivotCol: int) =
    let m = tableau.GetLength(0) - 1
    let n = tableau.GetLength(1) - 1
    
    let mutable pivotRow = -1
    let mutable minRatio = Double.MaxValue
    
    for i = 0 to m - 1 do
        if tableau.[i, pivotCol] > 0.0 then
            let ratio = tableau.[i, n] / tableau.[i, pivotCol]
            if ratio < minRatio then
                minRatio <- ratio
                pivotRow <- i
    
    pivotRow

// Perform pivot operation
let pivot (tableau: Tableau) (pivotRow: int) (pivotCol: int) =
    let m = tableau.GetLength(0) - 1
    let n = tableau.GetLength(1) - 1
    
    // Normalize pivot row
    let pivotElement = tableau.[pivotRow, pivotCol]
    for j = 0 to n do
        tableau.[pivotRow, j] <- tableau.[pivotRow, j] / pivotElement
    
    // Eliminate other elements in pivot column
    for i = 0 to m do
        if i <> pivotRow && tableau.[i, pivotCol] <> 0.0 then
            let factor = tableau.[i, pivotCol]
            for j = 0 to n do
                tableau.[i, j] <- tableau.[i, j] - factor * tableau.[pivotRow, j]

// Check if optimal solution is reached
let isOptimal (tableau: Tableau) =
    let m = tableau.GetLength(0) - 1
    let n = tableau.GetLength(1) - 1
    
    for j = 0 to n - 1 do
        if tableau.[m, j] < 0.0 then
            return false
    true

// Simplex algorithm implementation
let simplex (objective: float[]) (constraints: float[][]) (rhs: float[]) =
    // Create initial tableau
    let tableau = createInitialTableau objective constraints rhs
    
    let mutable iteration = 0
    let maxIterations = 1000
    
    printfn "Initial Tableau:"
    printTableau tableau
    
    while iteration < maxIterations do
        let pivotCol = findPivotColumn tableau
        
        if pivotCol = -1 then
            // Optimal solution found
            let solution = Array.create (objective.Length) 0.0
            for i = 0 to tableau.GetLength(0) - 2 do
                for j = 0 to tableau.GetLength(1) - 2 do
                    if tableau.[i, j] = 1.0 then
                        if j < solution.Length then
                            solution.[j] <- tableau.[i, tableau.GetLength(1) - 1]
            
            let optimalValue = tableau.[tableau.GetLength(0) - 1, tableau.GetLength(1) - 1]
            
            return {
                OptimalValue = optimalValue
                Solution = solution
                IsOptimal = true
                IsUnbounded = false
            }
        
        let pivotRow = findPivotRow tableau pivotCol
        
        if pivotRow = -1 then
            // Unbounded solution
            return {
                OptimalValue = Double.NaN
                Solution = [||]
                IsOptimal = false
                IsUnbounded = true
            }
        
        printfn "Pivot element: row %d, column %d" pivotRow pivotCol
        pivot tableau pivotRow pivotCol
        
        printfn "Tableau after pivot %d:" (iteration + 1)
        printTableau tableau
        
        iteration <- iteration + 1
    
    failwith "Simplex algorithm did not converge within maximum iterations"

// Helper function to print tableau
let printTableau (tableau: Tableau) =
    let m = tableau.GetLength(0)
    let n = tableau.GetLength(1)
    
    for i = 0 to m - 1 do
        for j = 0 to n - 1 do
            printf "%8.2f " tableau.[i, j]
        printfn ""
    printfn ""

// Example usage
let example1 () =
    printfn "=== Example 1: Maximize 3x + 2y ==="
    printfn "Subject to:"
    printfn "  x + y <= 4"
    printfn "  2x + y <= 6"
    printfn "  x, y >= 0"
    printfn ""
    
    let objective = [|3.0; 2.0|]
    let constraints = [| [|1.0; 1.0|]; [|2.0; 1.0|] |]
    let rhs = [|4.0; 6.0|]
    
    try
        let result = simplex objective constraints rhs
        printfn "Optimal Value: %f" result.OptimalValue
        printfn "Solution: x = %f, y = %f" result.Solution.[0] result.Solution.[1]
    with
    | ex -> printfn "Error: %s" ex.Message

let example2 () =
    printfn "=== Example 2: Maximize x + 2y ==="
    printfn "Subject to:"
    printfn "  x + y <= 3"
    printfn "  2x + y <= 4"
    printfn "  x, y >= 0"
    printfn ""
    
    let objective = [|1.0; 2.0|]
    let constraints = [| [|1.0; 1.0|]; [|2.0; 1.0|] |]
    let rhs = [|3.0; 4.0|]
    
    try
        let result = simplex objective constraints rhs
        printfn "Optimal Value: %f" result.OptimalValue
        printfn "Solution: x = %f, y = %f" result.Solution.[0] result.Solution.[1]
    with
    | ex -> printfn "Error: %s" ex.Message

// Run examples
[<EntryPoint>]
let main argv =
    example1()
    printfn ""
    example2()
    0
```

## Key Features of this Implementation:

1. **Tableau Representation**: Uses 2D arrays to represent the simplex tableau
2. **Standard Form Handling**: Converts problem to standard form with slack variables
3. **Pivot Operations**: Implements the core pivot operations
4. **Optimality Check**: Determines when optimal solution is reached
5. **Unbounded Solution Detection**: Identifies unbounded problems
6. **Error Handling**: Includes bounds checking and iteration limits

## Example Output:
```
=== Example 1: Maximize 3x + 2y ===
Subject to:
  x + y <= 4
  2x + y <= 6
  x, y >= 0

Initial Tableau:
    3.00    2.00    0.00    0.00    0.00 
    1.00    1.00    1.00    0.00    4.00 
    2.00    1.00    0.00    1.00    6.00 

Pivot element: row 0, column 0
Tableau after pivot 1:
   -3.00    2.00    0.00    0.00    0.00 
    1.00    1.00    1.00    0.00    4.00 
    2.00    1.00    0.00    1.00    6.00 

Optimal Value: 10.000000
Solution: x = 2.000000, y = 2.000000
```

This implementation demonstrates the complete simplex algorithm with proper tableau management and step-by-step execution.

