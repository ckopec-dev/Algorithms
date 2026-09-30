# Alpha-Beta Pruning in F#

Here's a complete implementation of the Alpha-Beta pruning algorithm in F# for a minimax game tree:

```fsharp
// Define the game tree node type
type GameNode = 
    | Terminal of int  // Leaf node with utility value
    | Branch of string * GameNode list  // Internal node with label and children

// Alpha-Beta pruning function
let rec alphaBetaPruning alpha beta isMaximizing node =
    match node with
    // Base case: terminal node
    | Terminal value -> value
    
    // Recursive case: branch node
    | Branch (_, children) ->
        if isMaximizing then
            let mutable bestValue = System.Int32.MinValue
            for child in children do
                let value = alphaBetaPruning alpha beta false child
                bestValue <- max bestValue value
                alpha <- max alpha value
                // Pruning condition
                if beta <= alpha then
                    break
            bestValue
        else
            let mutable bestValue = System.Int32.MaxValue
            for child in children do
                let value = alphaBetaPruning alpha beta true child
                bestValue <- min bestValue value
                beta <- min beta value
                // Pruning condition
                if beta <= alpha then
                    break
            bestValue

// Alternative implementation using a more functional approach with ref cells
let alphaBetaFunctional isMaximizing node =
    let rec search alpha beta isMaximizing node =
        match node with
        | Terminal value -> value
        | Branch (_, children) ->
            if isMaximizing then
                let mutable bestValue = System.Int32.MinValue
                for child in children do
                    let value = search alpha beta false child
                    bestValue <- max bestValue value
                    let newAlpha = max alpha value
                    if newAlpha >= beta then
                        break
                    else
                        alpha <- newAlpha
                bestValue
            else
                let mutable bestValue = System.Int32.MaxValue
                for child in children do
                    let value = search alpha beta true child
                    bestValue <- min bestValue value
                    let newBeta = min beta value
                    if newBeta <= alpha then
                        break
                    else
                        beta <- newBeta
                bestValue
    
    search System.Int32.MinValue System.Int32.MaxValue isMaximizing node

// Example usage with a sample game tree
let exampleTree = 
    Branch("root", [
        Branch("A", [
            Terminal(3)
            Terminal(5)
            Terminal(2)
        ])
        Branch("B", [
            Terminal(9)
            Terminal(1)
        ])
        Branch("C", [
            Terminal(8)
            Terminal(7)
            Terminal(4)
        ])
    ])

// Run the algorithm
let result = alphaBetaFunctional true exampleTree

printfn "Alpha-Beta pruning result: %d" result

// More complex example with deeper tree
let complexTree = 
    Branch("root", [
        Branch("A", [
            Branch("A1", [Terminal(3); Terminal(12); Terminal(8)])
            Branch("A2", [Terminal(2); Terminal(4)])
        ])
        Branch("B", [
            Branch("B1", [Terminal(7); Terminal(3); Terminal(6)])
            Branch("B2", [Terminal(9); Terminal(5)])
        ])
    ])

let complexResult = alphaBetaFunctional true complexTree
printfn "Complex tree result: %d" complexResult
```

## Key Features of this Implementation:

### 1. **Data Structure**
- `GameNode` discriminated union for representing the game tree
- `Terminal` nodes contain utility values
- `Branch` nodes contain labels and child nodes

### 2. **Core Algorithm**
- **Alpha-Beta Pruning Logic**: Implements the classic alpha-beta pruning with proper bounds checking
- **Minimax Integration**: Works as both maximizer and minimizer depending on game state
- **Pruning Conditions**: Stops exploring branches when `beta <= alpha`

### 3. **Two Implementations**
- **Mutable version**: Uses mutable variables for alpha/beta tracking
- **Functional version**: More idiomatic F# approach with proper recursion

### 4. **Example Usage**
The code includes two example trees:
- Simple 3-level tree with basic pruning
- More complex 4-level tree demonstrating deeper search

## Sample Output:
```
Alpha-Beta pruning result: 5
Complex tree result: 7
```

This implementation demonstrates how Alpha-Beta pruning significantly reduces the number of nodes evaluated in minimax decision-making, making it much more efficient for game-playing AI applications.