```fsharp
// Vogel's Approximation Method Implementation in F#

open System

type TransportationProblem = {
    Supply: int[]
    Demand: int[]
    CostMatrix: int[,]
}

type Allocation = {
    Row: int
    Col: int
    Quantity: int
}

// Calculate row penalties (difference between two smallest costs in each row)
let calculateRowPenalties (costs: int[,]) rows cols =
    [| for i in 0 .. rows - 1 do
        let row = [| for j in 0 .. cols - 1 -> costs.[i, j] |]
        let sortedRow = Array.sort row
        if sortedRow.Length >= 2 then
            yield sortedRow.[1] - sortedRow.[0]
        else
            yield 0 |]

// Calculate column penalties (difference between two smallest costs in each column)
let calculateColumnPenalties (costs: int[,]) rows cols =
    [| for j in 0 .. cols - 1 do
        let col = [| for i in 0 .. rows - 1 -> costs.[i, j] |]
        let sortedCol = Array.sort col
        if sortedCol.Length >= 2 then
            yield sortedCol.[1] - sortedCol.[0]
        else
            yield 0 |]

// Find maximum penalty and corresponding cell
let findMaximumPenalty (rowPenalties: int[]) (colPenalties: int[]) rows cols =
    let maxRowPenalty = Array.max rowPenalties
    let maxColPenalty = Array.max colPenalties
    
    if maxRowPenalty >= maxColPenalty then
        // Find row with maximum penalty
        let row = Array.findIndex (fun x -> x = maxRowPenalty) rowPenalties
        let col = 
            let rowCosts = [| for j in 0 .. cols - 1 -> costs.[row, j] |]
            let sortedRow = Array.sort rowCosts
            if sortedRow.Length >= 2 then
                let minCost = sortedRow.[0]
                Array.findIndex (fun x -> x = minCost) rowCosts
            else
                0
        (row, col, maxRowPenalty)
    else
        // Find column with maximum penalty
        let col = Array.findIndex (fun x -> x = maxColPenalty) colPenalties
        let row = 
            let colCosts = [| for i in 0 .. rows - 1 -> costs.[i, col] |]
            let sortedCol = Array.sort colCosts
            if sortedCol.Length >= 2 then
                let minCost = sortedCol.[0]
                Array.findIndex (fun x -> x = minCost) colCosts
            else
                0
        (row, col, maxColPenalty)

// Vogel's Approximation Method implementation
let vogelsApproximationMethod (problem: TransportationProblem) =
    let rows = problem.Supply.Length
    let cols = problem.Demand.Length
    
    // Create copies of supply and demand arrays
    let mutable supply = problem.Supply |> Array.copy
    let mutable demand = problem.Demand |> Array.copy
    let mutable costs = problem.CostMatrix |> Array2D.copy
    
    let mutable allocations = []
    
    while (Array.sum supply > 0) && (Array.sum demand > 0) do
        // Calculate penalties for rows and columns
        let rowPenalties = calculateRowPenalties costs rows cols
        let colPenalties = calculateColumnPenalties costs rows cols
        
        // Find maximum penalty
        let (row, col, _) = findMaximumPenalty rowPenalties colPenalties rows cols
        
        // Allocate minimum of supply and demand
        let quantity = min supply.[row] demand.[col]
        
        if quantity > 0 then
            allocations <- { Row = row; Col = col; Quantity = quantity } :: allocations
            
            // Update supply and demand
            supply.[row] <- supply.[row] - quantity
            demand.[col] <- demand.[col] - quantity
            
            // Set allocated cells to infinity (or a very large number)
            for i in 0 .. rows - 1 do
                costs.[i, col] <- Int32.MaxValue
            for j in 0 .. cols - 1 do
                costs.[row, j] <- Int32.MaxValue
    
    allocations

// Calculate total cost of the solution
let calculateTotalCost (problem: TransportationProblem) (allocations: Allocation[]) =
    allocations 
    |> Array.sumBy (fun alloc -> problem.CostMatrix.[alloc.Row, alloc.Col] * alloc.Quantity)

// Example usage
let example = {
    Supply = [| 30; 20; 10 |]
    Demand = [| 15; 25; 10 |]
    CostMatrix = 
        [|| 2; 3; 1 |
          | 5; 4; 8 |
          | 7; 6; 9 |]
}

// Run Vogel's Approximation Method
let result = vogelsApproximationMethod example

// Print results
printfn "Vogel's Approximation Method Results:"
printfn "Allocations:"
result 
|> Array.iter (fun alloc -> 
    printfn "  Cell (%d,%d): %d units" alloc.Row alloc.Col alloc.Quantity)

let totalCost = calculateTotalCost example result
printfn "\nTotal Cost: %d" totalCost

// Alternative version with detailed step-by-step output
let vogelsStepByStep (problem: TransportationProblem) =
    let rows = problem.Supply.Length
    let cols = problem.Demand.Length
    
    let mutable supply = problem.Supply |> Array.copy
    let mutable demand = problem.Demand |> Array.copy
    let mutable costs = problem.CostMatrix |> Array2D.copy
    
    printfn "Initial Supply: %A" supply
    printfn "Initial Demand: %A" demand
    printfn "Initial Cost Matrix:"
    for i in 0 .. rows - 1 do
        printfn "%A" (costs.[i, 0 .. cols - 1])
    
    let mutable allocations = []
    let mutable iteration = 1
    
    while (Array.sum supply > 0) && (Array.sum demand > 0) do
        printfn "\n--- Iteration %d ---" iteration
        
        // Calculate penalties
        let rowPenalties = calculateRowPenalties costs rows cols
        let colPenalties = calculateColumnPenalties costs rows cols
        
        printfn "Row Penalties: %A" rowPenalties
        printfn "Column Penalties: %A" colPenalties
        
        // Find maximum penalty and allocate
        let (row, col, maxPenalty) = findMaximumPenalty rowPenalties colPenalties rows cols
        let quantity = min supply.[row] demand.[col]
        
        printfn "Selected cell (%d,%d) with penalty %d" row col maxPenalty
        
        if quantity > 0 then
            allocations <- { Row = row; Col = col; Quantity = quantity } :: allocations
            supply.[row] <- supply.[row] - quantity
            demand.[col] <- demand.[col] - quantity
            
            printfn "Allocated %d units to cell (%d,%d)" quantity row col
            printfn "Updated Supply: %A" supply
            printfn "Updated Demand: %A" demand
            
            // Update cost matrix (set allocated cells to infinity)
            for i in 0 .. rows - 1 do
                costs.[i, col] <- Int32.MaxValue
            for j in 0 .. cols - 1 do
                costs.[row, j] <- Int32.MaxValue
            
            printfn "Updated Cost Matrix:"
            for i in 0 .. rows - 1 do
                printfn "%A" (costs.[i, 0 .. cols - 1])
            
        iteration <- iteration + 1
    
    allocations

// Run step-by-step version
printfn "\n=== Step-by-Step Execution ==="
let stepByStepResult = vogelsStepByStep example
let totalCostStepByStep = calculateTotalCost example stepByStepResult
printfn "\nFinal Total Cost: %d" totalCostStepByStep
```

This F# implementation of Vogel's Approximation Method includes:

1. **Data Structures**: 
   - `TransportationProblem` record for the problem data
   - `Allocation` record to store allocation results

2. **Core Functions**:
   - `calculateRowPenalties`: Computes penalties for each row
   - `calculateColumnPenalties`: Computes penalties for each column
   - `findMaximumPenalty`: Identifies the cell with maximum penalty
   - `vogelsApproximationMethod`: Main algorithm implementation

3. **Key Features**:
   - Handles supply and demand constraints properly
   - Updates cost matrix after each allocation
   - Calculates total transportation cost
   - Includes step-by-step execution for educational purposes

4. **Example Usage**:
   - Demonstrates with a sample transportation problem
   - Shows both final results and detailed iteration process
   - Calculates the minimum total cost using Vogel's method

The algorithm follows the standard Vogel's Approximation Method approach: calculate penalties, select maximum penalty cell, allocate minimum of supply/demand, update constraints, and repeat until all allocations are made.