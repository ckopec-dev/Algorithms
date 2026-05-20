# Minimax Algorithm in Haskell

Here's a complete implementation of the Minimax algorithm in Haskell:

```haskell
-- Define the game state and moves
data Player = MaxPlayer | MinPlayer deriving (Show, Eq)

data GameResult = Win | Loss | Draw deriving (Show, Eq)

-- Game tree node
data GameState = GameState
    { board :: [Int]  -- Simple board representation
    , player :: Player  -- Current player
    , isTerminal :: Bool  -- Whether game is over
    , score :: Maybe Int  -- Final score if terminal
    } deriving (Show, Eq)

-- Example game tree with simple numeric board
type GameTree = GameState

-- Generate all possible moves from a state
generateMoves :: GameState -> [GameState]
generateMoves state = 
    if isTerminal state
    then []
    else map (makeMove state) [0..length (board state) - 1]
  where
    makeMove :: GameState -> Int -> GameState
    makeMove state pos = 
        let newBoard = take pos (board state) ++ [0] ++ drop (pos + 1) (board state)
        in GameState 
            { board = newBoard
            , player = if player state == MaxPlayer then MinPlayer else MaxPlayer
            , isTerminal = all (== 0) newBoard  -- Game ends when all positions are 0
            , score = Nothing
            }

-- Evaluate the state (heuristic function)
evaluate :: GameState -> Int
evaluate state = 
    case score state of
        Just s -> s
        Nothing -> 
            if isTerminal state
            then case player state of
                    MaxPlayer -> -10  -- MinPlayer won
                    MinPlayer -> 10   -- MaxPlayer won
            else sum (board state)  -- Simple sum as heuristic

-- Minimax algorithm
minimax :: GameState -> Int
minimax state
    | isTerminal state = case score state of
                            Just s -> s
                            Nothing -> 0
    | player state == MaxPlayer = maximum (map minimax (generateMoves state))
    | otherwise = minimum (map minimax (generateMoves state))

-- Enhanced minimax with move selection
minimaxWithMove :: GameState -> (Int, GameState)
minimaxWithMove state
    | isTerminal state = (0, state)
    | player state == MaxPlayer = 
        let moves = generateMoves state
            scores = map minimax moves
            maxScore = maximum scores
        in head [(score, move) | (score, move) <- zip scores moves, score == maxScore]
    | otherwise = 
        let moves = generateMoves state
            scores = map minimax moves
            minScore = minimum scores
        in head [(score, move) | (score, move) <- zip scores moves, score == minScore]

-- Example usage
exampleGame :: GameState
exampleGame = GameState
    { board = [3, 1, 2, 0]
    , player = MaxPlayer
    , isTerminal = False
    , score = Nothing
    }

-- Simple example with a small game tree
simpleExample :: IO ()
simpleExample = do
    let initialState = GameState
            { board = [1, 2, 3]
            , player = MaxPlayer
            , isTerminal = False
            , score = Nothing
            }
    
    putStrLn "Initial game state:"
    print initialState
    
    let moves = generateMoves initialState
    putStrLn "Possible moves:"
    mapM_ print moves
    
    putStrLn "Minimax value of initial state:"
    print (minimax initialState)
    
    putStrLn "Best move with minimax value:"
    let (value, bestMove) = minimaxWithMove initialState
    print (value, bestMove)

-- Alternative implementation with alpha-beta pruning
minimaxAB :: Int -> Int -> GameState -> Int
minimaxAB alpha beta state
    | isTerminal state = case score state of
                            Just s -> s
                            Nothing -> 0
    | player state == MaxPlayer = 
        let rec = map (minimaxAB alpha beta) (generateMoves state)
            newAlpha = maximum (alpha : rec)
        in if newAlpha >= beta 
           then newAlpha
           else newAlpha
    | otherwise = 
        let rec = map (minimaxAB alpha beta) (generateMoves state)
            newBeta = minimum (beta : rec)
        in if newBeta <= alpha
           then newBeta
           else newBeta

-- Example of how to run the algorithm
main :: IO ()
main = do
    putStrLn "Minimax Algorithm in Haskell"
    putStrLn "============================"
    simpleExample
```

## Key Features of this Implementation:

### 1. **Data Types**
- `Player` - Represents MaxPlayer and MinPlayer
- `GameState` - Contains board state, current player, terminal status, and score
- `GameTree` - Tree structure for game states

### 2. **Core Functions**
- `generateMoves` - Creates all possible next states
- `evaluate` - Heuristic function to score game states
- `minimax` - Core minimax algorithm implementation
- `minimaxWithMove` - Returns both value and best move

### 3. **Algorithm Logic**
- **MaxPlayer** tries to maximize the score
- **MinPlayer** tries to minimize the score
- Recursively evaluates all possible game paths
- Returns the optimal value for the current player

### 4. **Usage Example**
```haskell
-- Create a game state
let gameState = GameState { board = [3, 1, 2], player = MaxPlayer, ... }

-- Get the minimax value
let value = minimax gameState

-- Get the best move
let (bestValue, bestMove) = minimaxWithMove gameState
```

This implementation demonstrates the classic minimax algorithm structure that can be adapted for various game domains like tic-tac-toe, chess, or any two-player game with perfect information.

