# Minimax Algorithm in Lua

Here's a complete implementation of the Minimax algorithm in Lua for a simple game (Tic-Tac-Toe):

```lua
-- Minimax algorithm implementation in Lua

-- Game board representation
local function create_board()
    return {
        {0, 0, 0},
        {0, 0, 0},
        {0, 0, 0}
    }
end

-- Check if the game is over
local function is_game_over(board)
    -- Check for win
    for i = 1, 3 do
        -- Rows
        if board[i][1] ~= 0 and board[i][1] == board[i][2] and board[i][2] == board[i][3] then
            return board[i][1]
        end
        -- Columns
        if board[1][i] ~= 0 and board[1][i] == board[2][i] and board[2][i] == board[3][i] then
            return board[1][i]
        end
    end
    
    -- Diagonals
    if board[1][1] ~= 0 and board[1][1] == board[2][2] and board[2][2] == board[3][3] then
        return board[1][1]
    end
    if board[1][3] ~= 0 and board[1][3] == board[2][2] and board[2][2] == board[3][1] then
        return board[1][3]
    end
    
    -- Check for tie (board full)
    local is_full = true
    for i = 1, 3 do
        for j = 1, 3 do
            if board[i][j] == 0 then
                is_full = false
                break
            end
        end
        if not is_full then break end
    end
    
    if is_full then return 0 end -- Tie
    
    return nil -- Game continues
end

-- Get all possible moves
local function get_possible_moves(board)
    local moves = {}
    for i = 1, 3 do
        for j = 1, 3 do
            if board[i][j] == 0 then
                table.insert(moves, {i, j})
            end
        end
    end
    return moves
end

-- Make a move
local function make_move(board, row, col, player)
    local new_board = {}
    for i = 1, 3 do
        new_board[i] = {}
        for j = 1, 3 do
            new_board[i][j] = board[i][j]
        end
    end
    new_board[row][col] = player
    return new_board
end

-- Minimax algorithm
local function minimax(board, is_maximizing, depth)
    local result = is_game_over(board)
    
    -- Terminal states
    if result == 1 then return 10 - depth end   -- AI wins
    if result == -1 then return depth - 10 end  -- Human wins
    if result == 0 then return 0 end            -- Tie
    
    if is_maximizing then
        local max_eval = -math.huge
        local possible_moves = get_possible_moves(board)
        
        for _, move in ipairs(possible_moves) do
            local new_board = make_move(board, move[1], move[2], 1)  -- AI is 1
            local eval = minimax(new_board, false, depth + 1)
            max_eval = math.max(max_eval, eval)
        end
        
        return max_eval
    else
        local min_eval = math.huge
        local possible_moves = get_possible_moves(board)
        
        for _, move in ipairs(possible_moves) do
            local new_board = make_move(board, move[1], move[2], -1)  -- Human is -1
            local eval = minimax(new_board, true, depth + 1)
            min_eval = math.min(min_eval, eval)
        end
        
        return min_eval
    end
end

-- Find best move for AI
local function get_best_move(board)
    local best_score = -math.huge
    local best_move = nil
    
    local possible_moves = get_possible_moves(board)
    
    for _, move in ipairs(possible_moves) do
        local new_board = make_move(board, move[1], move[2], 1)  -- AI makes move
        local score = minimax(new_board, false, 0)
        
        if score > best_score then
            best_score = score
            best_move = move
        end
    end
    
    return best_move
end

-- Print board
local function print_board(board)
    for i = 1, 3 do
        local row = ""
        for j = 1, 3 do
            if board[i][j] == 0 then
                row = row .. " . "
            elseif board[i][j] == 1 then
                row = row .. " X "
            else
                row = row .. " O "
            end
        end
        print(row)
    end
    print()
end

-- Example usage
print("Tic-Tac-Toe with Minimax AI")
print("===========================")

local game_board = create_board()

-- Make some moves to demonstrate
game_board[1][1] = -1  -- Human plays O
game_board[2][2] = 1   -- AI plays X

print("Current board:")
print_board(game_board)

print("AI is thinking...")
local best_move = get_best_move(game_board)
print("Best move for AI: Row " .. best_move[1] .. ", Col " .. best_move[2])

-- Make the AI move
game_board[best_move[1]][best_move[2]] = 1
print("After AI move:")
print_board(game_board)
```

## Key Components Explained

### 1. **Board Representation**
- 3x3 grid using nested tables
- 0 = empty, 1 = AI (X), -1 = Human (O)

### 2. **Game Logic**
- `is_game_over()`: Checks for wins, losses, or ties
- `get_possible_moves()`: Returns all empty positions
- `make_move()`: Creates a new board state

### 3. **Minimax Algorithm**
- **Maximizing player** (AI): Tries to maximize score
- **Minimizing player** (Human): Tries to minimize score
- **Depth**: Prevents infinite recursion and favors quicker wins
- **Heuristic**: Scores based on game outcome and depth

### 4. **Scoring System**
- Win: +10 - depth (prefers quicker wins)
- Loss: -10 + depth (prefers longer losses)
- Tie: 0

## How to Run

```lua
-- Save as minimax_tictactoe.lua and run with:
-- lua minimax_tictactoe.lua
```

This implementation demonstrates the core concepts of the Minimax algorithm in a practical game scenario, showing how the AI evaluates possible moves and chooses the optimal strategy.

