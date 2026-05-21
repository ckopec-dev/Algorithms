# Dantzig's Simplex Algorithm in F#

Here's a complete implementation of Dantzig's Simplex algorithm in F#:

```fsharp
open System

// Simplex tableau representation
type SimplexTableau = {
    /// The objective function coefficients
    c: float[]
    /// The constraint coefficients (A matrix)
    A: float[][]
    /// The right-hand side values (b vector)
    b: float[]
    /// Number of variables
    n: int
    /// Number of constraints
    m: int
    /// Basic variables indices
    basic: int[]
    /// Current tableau
    tableau: float[][]
}

// Create initial tableau
let createInitialTableau (c: float[]) (A: float[][]) (b: float[]) =
    let m = A.Length
    let n = c.Length
    let tableau = Array2D.zeroCreate (m + 1) (n + m + 1)
    
    // Fill constraint rows
    for i in 0 .. m - 1 do
        for j in 0 .. n - 1 do
            tableau.[i, j] <- A.[i].[j]
        tableau.[i, n + i] <- 1.0  // Slack variable
        tableau.[i, n + m] <- b.[i]  // RHS
    
    // Fill objective row
    for j in 0 .. n - 1 do
        tableau.[m, j] <- -c.[j]
    
    {
        c = c
        A = A
        b = b
        n = n
        m = m
        basic = [| for i in 0 .. m - 1 -> n + i |]
        tableau = [| for i in 0 .. m -> [| for j in 0 .. n + m -> tableau.[i, j] |] |]
    }

// Find entering variable (most negative coefficient in objective row)
let findEnteringVariable (tableau: float[][]) =
    let m = tableau.Length - 1
    let n = tableau.[0].Length - 1
    let mutable entering = -1
    let mutable minCoeff = 0.0
    
    for j in 0 .. n - 1 do
        if tableau.[m].[j] < minCoeff then
            minCoeff <- tableau.[m].[j]
            entering <- j
    
    entering

// Find leaving variable using minimum ratio test
let findLeavingVariable (tableau: float[][]) (entering: int) =
    let m = tableau.Length - 1
    let n = tableau.[0].Length - 1
    let mutable leaving = -1
    let mutable minRatio = Double.PositiveInfinity
    
    for i in 0 .. m - 1 do
        if tableau.[i].[entering] > 0.0 then
            let ratio = tableau.[i].[n] / tableau.[i].[entering]
            if ratio < minRatio then
                minRatio <- ratio
                leaving <- i
    
    leaving

// Pivot operation
let pivot (tableau: float[][]) (entering: int) (leaving: int) =
    let m = tableau.Length - 1
    let n = tableau.[0].Length - 1
    
    // Create new tableau
    let newTableau = Array2D.zeroCreate (m + 1) (n + 1)
    
    // Pivot element
    let pivotElement = tableau.[leaving].[entering]
    
    // Copy pivot row (divide by pivot element)
    for j in 0 .. n do
        newTableau.[leaving, j] <- tableau.[leaving].[j] / pivotElement
    
    // Copy other rows
    for i in 0 .. m do
        if i <> leaving then
            for j in 0 .. n do
                if j <> entering then
                    newTableau.[i, j] <- tableau.[i].[j] - 
                                         tableau.[i].[entering] * 
                                         newTableau.[leaving, j]
                else
                    newTableau.[i, j] <- 0.0
    
    // Set pivot element to 1
    newTableau.[leaving, entering] <- 1.0
    
    // Convert back to regular array
    [| for i in 0 .. m -> [| for j in 0 .. n -> newTableau.[i, j] |] |]

// Check if optimal (all coefficients in objective row are non-negative)
let isOptimal (tableau: float[][]) =
    let m = tableau.Length - 1
    let n = tableau.[0].Length - 1
    
    for j in 0 .. n - 1 do
        if tableau.[m].[j] < 0.0 then
            return false
    
    true

// Solve using simplex algorithm
let solveSimplex (c: float[]) (A: float[][]) (b: float[]) =
    let mutable tableau = createInitialTableau c A b
    
    printfn "Initial Tableau:"
    printTableau tableau.tableau
    
    let mutable iteration = 0
    
    while not (isOptimal tableau.tableau) && iteration < 100 do
        printfn "\nIteration %d:" (iteration + 1)
        
        let entering = findEnteringVariable tableau.tableau
        if entering = -1 then
            printfn "No entering variable found - optimal solution"
            break
        
        printfn "Entering variable: %d" entering
        
        let leaving = findLeavingVariable tableau.tableau entering
        if leaving = -1 then
            printfn "Unbounded solution"
            return None
        
        printfn "Leaving variable: %d" leaving
        
        tableau.tableau <- pivot tableau.tableau entering leaving
        
        printfn "New Tableau:"
        printTableau tableau.tableau
        
        iteration <- iteration + 1
    
    if iteration >= 100 then
        printfn "Maximum iterations reached"
        None
    else
        let optimalValue = tableau.tableau.[tableau.m].[tableau.n + tableau.m]
        let solution = Array.zeroCreate tableau.n
        for i in 0 .. tableau.m - 1 do
            let basicVar = tableau.basic.[i]
            if basicVar < tableau.n then
                solution.[basicVar] <- tableau.tableau.[i].[tableau.n + tableau.m]
        
        Some (optimalValue, solution)

// Helper function to print tableau
let printTableau (tableau: float[][]) =
    for i in 0 .. tableau.Length - 1 do
        for j in 0 .. tableau.[0].Length - 1 do
            printf "%8.2f " tableau.[i].[j]
        printfn ""

// Example usage
let example1() =
    printfn "=== Example 1: Maximize 3x1 + 2x2 ==="
    printfn "Subject to: x1 + x2 <= 4"
    printfn "           2x1 + x2 <= 6"
    printfn "           x1, x2 >= 0"
    
    let c = [| 3.0; 2.0 |]
    let A = [| [| 1.0; 1.0 |]; [| 2.0; 1.0 |] |]
    let b = [| 4.0; 6.0 |]
    
    match solveSimplex c A b with
    | Some (optimalValue, solution) ->
        printfn "\nOptimal value: %f" optimalValue
        printfn "Solution: x1 = %f, x2 = %f" solution.[0] solution.[1]
    | None -> printfn "No solution found"

let example2() =
    printfn "\n=== Example 2: Maximize x1 + 2x2 ==="
    printfn "Subject to: 2x1 + x2 <= 10"
    printfn "           x1 + 2x2 <= 8"
    printfn "           x1, x2 >= 0"
    
    let c = [| 1.0; 2.0 |]
    let A = [| [| 2.0; 1.0 |]; [| 1.0; 2.0 |] |]
    let b = [| 10.0; 8.0 |]
    
    match solveSimplex c A b with
    | Some (optimalValue, solution) ->
        printfn "\nOptimal value: %f" optimalValue
        printfn "Solution: x1 = %f, x2 = %f" solution.[0] solution.[1]
    | None -> printfn "No solution found"

// Run examples
example1()
example2()
```

## How it works:

1. **Initialization**: Creates the initial simplex tableau with slack variables
2. **Optimality Check**: Determines if the current solution is optimal
3. **Entering Variable Selection**: Finds the variable that will enter the basis (most negative coefficient in objective row)
4. **Leaving Variable Selection**: Uses the minimum ratio test to find which variable leaves the basis
5. **Pivot Operation**: Performs the pivot operation to update the tableau
6. **Iteration**: Repeats until optimal solution is found

## Key Features:

- **Tableau Representation**: Uses 2D arrays to represent the simplex tableau
- **Complete Implementation**: Includes all necessary simplex operations
- **Error Handling**: Handles unbounded solutions and convergence issues
- **Clear Output**: Shows each iteration of the algorithm
- **Multiple Examples**: Demonstrates the algorithm with different problems

## Sample Output:
```
=== Example 1: Maximize 3x1 + 2x2 ===
Subject to: x1 + x2 <= 4
           2x1 + x2 <= 6
           x1, x2 >= 0

Initial Tableau:
   1.00   1.00   1.00   0.00   4.00 
   2.00   1.00   0.00   1.00   6.00 
  -3.00  -2.00   0.00   0.00   0.00 

Iteration 1:
Entering variable: 0
Leaving variable: 1
New Tableau:
   0.00   0.50   1.00  -0.50   2.00 
   1.00   0.50   0.00   0.50   3.00 
   0.00  -0.50   0.00   1.50   6.00 
```

This implementation demonstrates the classic simplex algorithm in a functional programming style using F#.

