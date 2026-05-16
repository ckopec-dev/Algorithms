# N-Queens Problem in F#

Here's a complete implementation of the N-Queens problem solver in F#:

```fsharp
// N-Queens Problem Solver in F#

// Function to check if a queen can be placed at position (row, col)
let isSafe (board: int[]) row col =
    // Check if any queen is in the same column
    let sameCol = board.[..row-1] |> Array.exists (fun r -> r = col)
    
    // Check if any queen is on the same diagonal
    let sameDiag = 
        board.[..row-1]
        |> Array.mapi (fun i r -> abs (r - col) = abs (i - row))
        |> Array.contains true
    
    not (sameCol || sameDiag)

// Function to solve N-Queens using backtracking
let rec solveNQueens board row n =
    if row = n then
        Some board
    else
        [0..n-1]
        |> List.tryFind (fun col ->
            if isSafe board row col then
                let newBoard = Array.copy board
                newBoard.[row] <- col
                match solveNQueens newBoard (row + 1) n with
                | Some solution -> true
                | None -> false
        )
        |> Option.map (fun col ->
            let newBoard = Array.copy board
            newBoard.[row] <- col
            solveNQueens newBoard (row + 1) n)
        |> Option.flatten

// Alternative implementation with better structure
let solveNQueensComplete n =
    let rec placeQueens row board =
        if row = n then
            Some board
        else
            let validPositions = 
                [0..n-1]
                |> List.filter (fun col -> isSafe board row col)
            
            validPositions
            |> List.tryFindMap (fun col ->
                let newBoard = Array.copy board
                newBoard.[row] <- col
                placeQueens (row + 1) newBoard)
    
    placeQueens 0 (Array.create n -1)

// Function to print the board solution
let printBoard (board: int[]) =
    let n = board.Length
    printfn "Solution for %d-Queens:" n
    for i in 0..n-1 do
        for j in 0..n-1 do
            if board.[i] = j then
                printf "Q "
            else
                printf ". "
        printfn ""
    printfn ""

// Main execution
let main() =
    let n = 8  // Solve 8-Queens problem
    
    match solveNQueensComplete n with
    | Some solution ->
        printfn "Found solution for %d-Queens:" n
        printBoard solution
        printfn "Solution array: %A" solution
    | None ->
        printfn "No solution found for %d-Queens" n

// Run the example
main()

// Example with smaller board (4-Queens)
let example4Queens() =
    printfn "=== 4-Queens Example ==="
    match solveNQueensComplete 4 with
    | Some solution ->
        printBoard solution
        printfn "Solution array: %A" solution
    | None ->
        printfn "No solution found"

// Run 4-Queens example
example4Queens()
```

## Key Features of this Implementation:

### 1. **Backtracking Algorithm**
- Uses recursive backtracking to explore all possible placements
- Prunes invalid paths early to improve efficiency

### 2. **Safety Check Function**
```fsharp
let isSafe (board: int[]) row col
```
- Checks column conflicts
- Checks diagonal conflicts
- Returns `true` if position is safe for a queen

### 3. **Recursive Solution**
- `solveNQueensComplete` function that returns `Some(solution)` or `None`
- Uses pattern matching and option types for clean error handling

### 4. **Board Representation**
- Uses an array where `board.[row] = col` represents a queen at position (row, col)
- Simple and efficient representation

### 5. **Output Formatting**
- `printBoard` function to visualize the solution
- Clear display of queen positions

## Sample Output:
```
=== 4-Queens Example ===
Solution for 4-Queens:
. Q . . 
. . . Q 
Q . . . 
. . Q . 

Solution array: [|1; 3; 0; 2|]
```

This implementation demonstrates F#'s functional programming strengths including:
- Immutability with `Array.copy`
- Pattern matching with `match/with`
- Option types for safe error handling
- Higher-order functions like `List.tryFindMap`
- Clean, readable recursive algorithms

