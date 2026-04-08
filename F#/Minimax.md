# Minimax Algorithm in F#

Here's a complete implementation of the Minimax algorithm in F# with a tic-tac-toe example:

```fsharp
// Minimax algorithm implementation in F#

// Game state type
type Player = X | O
type Cell = Empty | Player of Player
type Board = Cell[,]

// Game result type
type GameResult = 
    | Win of Player
    | Draw
    | Continue

// Create an empty 3x3 board
let createEmptyBoard() : Board = 
    Array2D.create 3 3 Empty

// Check if a move is valid
let isValidMove (board: Board) (row: int) (col: int) : bool =
    row >= 0 && row < 3 && col >= 0 && col < 3 && board.[row, col] = Empty

// Make a move on the board
let makeMove (board: Board) (row: int) (col: int) (player: Player) : Board =
    let newBoard = Array2D.copy board
    newBoard.[row, col] <- Player player
    newBoard

// Check if the game is over and return the result
let checkGameResult (board: Board) : GameResult =
    // Check rows
    let checkRows () =
        for i in 0 .. 2 do
            if board.[i, 0] <> Empty && 
               board.[i, 0] = board.[i, 1] && 
               board.[i, 1] = board.[i, 2] then
                match board.[i, 0] with
                | Player p -> Win p
                | _ -> Continue
        Continue

    // Check columns
    let checkColumns () =
        for i in 0 .. 2 do
            if board.[0, i] <> Empty && 
               board.[0, i] = board.[1, i] && 
               board.[1, i] = board.[2, i] then
                match board.[0, i] with
                | Player p -> Win p
                | _ -> Continue
        Continue

    // Check diagonals
    let checkDiagonals () =
        if board.[0, 0] <> Empty && 
           board.[0, 0] = board.[1, 1] && 
           board.[1, 1] = board.[2, 2] then
            match board.[0, 0] with
            | Player p -> Win p
            | _ -> Continue
        elif board.[0, 2] <> Empty && 
             board.[0, 2] = board.[1, 1] && 
             board.[1, 1] = board.[2, 0] then
            match board.[0, 2] with
            | Player p -> Win p
            | _ -> Continue
        else Continue

    // Check for win
    match checkRows () with
    | Win p -> Win p
    | _ -> 
        match checkColumns () with
        | Win p -> Win p
        | _ -> 
            match checkDiagonals () with
            | Win p -> Win p
            | _ -> 
                // Check for draw
                let isFull = 
                    [0..2] |> List.collect (fun i -> [0..2] |> List.map (fun j -> board.[i, j]))
                    |> List.forall (fun cell -> cell <> Empty)
                if isFull then Draw else Continue

// Get all possible moves
let getPossibleMoves (board: Board) : (int * int) list =
    [0..2] |> List.collect (fun row -> 
        [0..2] |> List.filter (fun col -> isValidMove board row col) 
        |> List.map (fun col -> (row, col)))

// Minimax function with alpha-beta pruning
let minimax (board: Board) (isMaximizing: bool) (depth: int) (alpha: float) (beta: float) : float =
    match checkGameResult board with
    | Win X -> 10.0 - float depth  // Prefer quicker wins
    | Win O -> float depth - 10.0  // Prefer slower losses
    | Draw -> 0.0
    | Continue ->
        if isMaximizing then
            let mutable bestValue = -infinity
            let moves = getPossibleMoves board
            for (row, col) in moves do
                let newBoard = makeMove board row col X
                let value = minimax newBoard false (depth + 1) alpha beta
                bestValue <- max bestValue value
                alpha <- max alpha value
                if beta <= alpha then break  // Alpha-beta pruning
            bestValue
        else
            let mutable bestValue = infinity
            let moves = getPossibleMoves board
            for (row, col) in moves do
                let newBoard = makeMove board row col O
                let value = minimax newBoard true (depth + 1) alpha beta
                bestValue <- min bestValue value
                beta <- min beta value
                if beta <= alpha then break  // Alpha-beta pruning
            bestValue

// Find the best move for the AI player
let findBestMove (board: Board) : (int * int) =
    let moves = getPossibleMoves board
    let mutable bestMove = (-1, -1)
    let mutable bestValue = -infinity
    
    for (row, col) in moves do
        let newBoard = makeMove board row col X
        let value = minimax newBoard false 0 -infinity infinity
        if value > bestValue then
            bestValue <- value
            bestMove <- (row, col)
    
    bestMove

// Print the board
let printBoard (board: Board) =
    for i in 0 .. 2 do
        for j in 0 .. 2 do
            match board.[i, j] with
            | Empty -> printf " _ "
            | Player X -> printf " X "
            | Player O -> printf " O "
        printfn ""
    printfn ""

// Example usage
let example() =
    let board = createEmptyBoard()
    
    printfn "Initial board:"
    printBoard board
    
    // Make some moves
    let board1 = makeMove board 0 0 X
    let board2 = makeMove board1 1 1 O
    let board3 = makeMove board2 0 1 X
    
    printfn "After some moves:"
    printBoard board3
    
    // Find the best move for X (AI)
    let (bestRow, bestCol) = findBestMove board3
    printfn $"Best move for X: Row {bestRow}, Column {bestCol}"
    
    // Make the best move
    let finalBoard = makeMove board3 bestRow bestCol X
    printfn "Board after AI move:"
    printBoard finalBoard

// Run the example
example()
```

## Key Features of this Implementation:

1. **Game State Representation**: Uses a 2D array to represent the board with `Cell` type
2. **Minimax Algorithm**: Implements the core minimax logic with alpha-beta pruning for efficiency
3. **Game Logic**: Includes functions to check game results, make moves, and validate moves
4. **Alpha-Beta Pruning**: Optimizes the search by pruning branches that won't affect the final decision
5. **Player Turn Management**: Alternates between maximizing (X) and minimizing (O) players
6. **Depth Consideration**: Considers move depth to prefer quicker wins

## How it Works:

1. The `minimax` function recursively evaluates all possible game states
2. For maximizing player (X), it tries to maximize the score
3. For minimizing player (O), it tries to minimize the score
4. Alpha-beta pruning eliminates branches that cannot possibly affect the final decision
5. The `findBestMove` function determines the optimal move for the current player

This implementation can be easily extended to other games by modifying the board representation and game rules.

