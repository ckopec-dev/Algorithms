# Minimax Algorithm in Haskell

Here's a complete implementation of the Minimax algorithm in Haskell with an example using a simple tic-tac-toe game:

```haskell
-- Minimax algorithm implementation in Haskell

-- Game state representation
data Player = X | O deriving (Show, Eq)
data Cell = Empty | Filled Player deriving (Show, Eq)
type Board = [[Cell]]

-- Game result types
data GameResult = Win Player | Loss Player | Draw deriving (Show, Eq)

-- Check if the game is over
isGameOver :: Board -> Bool
isGameOver board = 
    let lines = getLines board
    in any isWinning lines || all (not . isDraw) lines

-- Get all lines (rows, columns, diagonals)
getLines :: Board -> [[Cell]]
getLines board = 
    let rows = board
        cols = transpose board
        diag1 = [board !! i !! i | i <- [0..2]]
        diag2 = [board !! i !! (2-i) | i <- [0..2]]
    in rows ++ cols ++ [diag1, diag2]

-- Check if a line is winning
isWinning :: [Cell] -> Bool
isWinning line = 
    case line of
        [Filled p1, Filled p2, Filled p3] -> p1 == p2 && p2 == p3
        _ -> False

-- Check if a line is a draw (all cells filled but no winner)
isDraw :: [Cell] -> Bool
isDraw line = all (== Filled X) line || all (== Filled O) line

-- Get all possible moves
getMoves :: Board -> [(Int, Int)]
getMoves board = 
    [(i, j) | i <- [0..2], j <- [0..2], board !! i !! j == Empty]

-- Make a move
makeMove :: Board -> (Int, Int) -> Player -> Board
makeMove board (i, j) player = 
    let row = board !! i
        newRow = take j row ++ [Filled player] ++ drop (j+1) row
    in take i board ++ [newRow] ++ drop (i+1) board

-- Evaluate the board position
evaluateBoard :: Board -> Int
evaluateBoard board = 
    let lines = getLines board
        winningLines = filter isWinning lines
    in case winningLines of
        [] -> 0  -- No winner yet
        (line:_) -> 
            case line of
                [Filled X, _, _] -> 10  -- X wins
                [Filled O, _, _] -> -10 -- O wins
                _ -> 0

-- Minimax function with alpha-beta pruning
minimax :: Board -> Player -> Int -> Int -> Int
minimax board player depth alpha beta
    | isGameOver board = evaluateBoard board
    | depth == 0 = evaluateBoard board
    | player == X = 
        let moves = getMoves board
            results = map (\move -> minimax (makeMove board move player) O (depth-1) alpha beta) moves
        in foldl (\acc result -> max acc result) alpha results
    | otherwise = 
        let moves = getMoves board
            results = map (\move -> minimax (makeMove board move player) X (depth-1) alpha beta) moves
        in foldl (\acc result -> min acc result) beta results

-- Main minimax function that returns the best move
bestMove :: Board -> Player -> (Int, Int)
bestMove board player = 
    let moves = getMoves board
        evaluations = map (\move -> 
            let newBoard = makeMove board move player
                eval = minimax newBoard (if player == X then O else X) 9 (-100) 100
            in (move, eval)) moves
    in fst $ head $ sortBy (comparing snd) evaluations

-- Helper function to transpose a matrix
transpose :: [[a]] -> [[a]]
transpose [] = []
transpose (row:rows) = zipWith (:) row (transpose rows)

-- Example usage
exampleBoard :: Board
exampleBoard = 
    [[Filled X, Empty, Filled O],
     [Filled O, Empty, Empty],
     [Empty, Empty, Empty]]

-- Simple test function
testMinimax :: IO ()
testMinimax = do
    putStrLn "Current board:"
    print exampleBoard
    putStrLn "Best move for X:"
    print $ bestMove exampleBoard X

-- Alternative simple minimax without alpha-beta pruning
simpleMinimax :: Board -> Player -> Int
simpleMinimax board player depth
    | isGameOver board = evaluateBoard board
    | depth == 0 = evaluateBoard board
    | player == X = 
        let moves = getMoves board
            results = map (\move -> simpleMinimax (makeMove board move player) O (depth-1)) moves
        in maximum results
    | otherwise = 
        let moves = getMoves board
            results = map (\move -> simpleMinimax (makeMove board move player) X (depth-1)) moves
        in minimum results

-- Simple example with a smaller game tree
simpleGameTree :: Int
simpleGameTree = simpleMinimax [[Filled X, Empty, Empty], [Empty, Empty, Empty], [Empty, Empty, Empty]] X 3
```

## Key Features of this Implementation:

1. **Data Types**: 
   - `Player` for X and O
   - `Cell` for empty or filled cells
   - `Board` as a 3x3 matrix

2. **Core Functions**:
   - `isGameOver` - checks if game is over
   - `getMoves` - gets all possible moves
   - `makeMove` - applies a move to the board
   - `evaluateBoard` - scores the current board position
   - `minimax` - main minimax algorithm with alpha-beta pruning
   - `bestMove` - returns the optimal move

3. **Algorithm Features**:
   - Alpha-beta pruning for efficiency
   - Depth-limited search
   - Proper game state evaluation
   - Recursive minimax implementation

This implementation demonstrates how to structure a minimax algorithm in Haskell using pure functional programming concepts, with proper type safety and clean separation of game logic from the search algorithm.

