# Minimax Algorithm Implementation in Python

Here's a complete implementation of the Minimax algorithm for a simple game (Tic-Tac-Toe):

```python
import math

class TicTacToe:
    def __init__(self):
        self.board = [' ' for _ in range(9)]  # 3x3 board represented as list
        self.current_player = 'X'
    
    def print_board(self):
        for i in range(3):
            print(f" {self.board[i*3]} | {self.board[i*3+1]} | {self.board[i*3+2]} ")
            if i < 2:
                print("-----------")
    
    def available_moves(self):
        return [i for i, spot in enumerate(self.board) if spot == ' ']
    
    def make_move(self, position, player):
        self.board[position] = player
    
    def undo_move(self, position):
        self.board[position] = ' '
    
    def check_winner(self):
        # Check rows, columns, and diagonals
        winning_combinations = [
            [0, 1, 2], [3, 4, 5], [6, 7, 8],  # rows
            [0, 3, 6], [1, 4, 7], [2, 5, 8],  # columns
            [0, 4, 8], [2, 4, 6]              # diagonals
        ]
        
        for combo in winning_combinations:
            if (self.board[combo[0]] == self.board[combo[1]] == 
                self.board[combo[2]] != ' '):
                return self.board[combo[0]]
        
        return None
    
    def is_board_full(self):
        return ' ' not in self.board

def minimax(game, depth, is_maximizing):
    """
    Minimax algorithm implementation
    
    Args:
        game: TicTacToe game instance
        depth: Current depth in the game tree
        is_maximizing: True if current player is maximizing (AI), False if minimizing (human)
    
    Returns:
        Best score for the current position
    """
    
    # Check for terminal states
    winner = game.check_winner()
    if winner == 'X':  # AI wins
        return 10 - depth
    elif winner == 'O':  # Human wins
        return depth - 10
    elif game.is_board_full():  # Tie
        return 0
    
    if is_maximizing:
        # AI's turn (maximizing player)
        best_score = -math.inf
        for move in game.available_moves():
            game.make_move(move, 'X')
            score = minimax(game, depth + 1, False)
            game.undo_move(move)
            best_score = max(score, best_score)
        return best_score
    else:
        # Human's turn (minimizing player)
        best_score = math.inf
        for move in game.available_moves():
            game.make_move(move, 'O')
            score = minimax(game, depth + 1, True)
            game.undo_move(move)
            best_score = min(score, best_score)
        return best_score

def get_best_move(game):
    """
    Find the best move for the AI player
    """
    best_score = -math.inf
    best_move = None
    
    for move in game.available_moves():
        game.make_move(move, 'X')
        score = minimax(game, 0, False)
        game.undo_move(move)
        
        if score > best_score:
            best_score = score
            best_move = move
    
    return best_move

# Example usage
def play_game():
    game = TicTacToe()
    print("Welcome to Tic-Tac-Toe!")
    print("You are 'O', AI is 'X'")
    print("Positions are numbered 0-8:")
    print(" 0 | 1 | 2 ")
    print("-----------")
    print(" 3 | 4 | 5 ")
    print("-----------")
    print(" 6 | 7 | 8 ")
    print()
    
    while True:
        game.print_board()
        
        # Human move
        try:
            human_move = int(input("Enter your move (0-8): "))
            if human_move not in game.available_moves():
                print("Invalid move! Try again.")
                continue
        except ValueError:
            print("Please enter a number between 0 and 8.")
            continue
            
        game.make_move(human_move, 'O')
        
        # Check if human won
        if game.check_winner() == 'O':
            print("You win!")
            break
        
        if game.is_board_full():
            print("It's a tie!")
            break
        
        # AI move
        print("AI is thinking...")
        ai_move = get_best_move(game)
        game.make_move(ai_move, 'X')
        
        # Check if AI won
        if game.check_winner() == 'X':
            print("AI wins!")
            break
        
        if game.is_board_full():
            print("It's a tie!")
            break

# Run the example
if __name__ == "__main__":
    # Simple demonstration
    print("=== Minimax Algorithm Demo ===")
    
    # Create a sample game state
    game = TicTacToe()
    game.board = ['X', 'O', 'X', 'O', ' ', ' ', ' ', ' ', ' ']
    
    print("Current board state:")
    game.print_board()
    print()
    
    # Find best move for AI (X)
    print("AI's best move:")
    best_move = get_best_move(game)
    print(f"Best move position: {best_move}")
    
    # Show what happens with that move
    game.make_move(best_move, 'X')
    print("After AI move:")
    game.print_board()
```

## Key Components Explained:

### 1. **Game State Representation**
- Board stored as a list of 9 elements
- Players represented as 'X' (AI) and 'O' (human)

### 2. **Minimax Function Logic**
- **Terminal States**: Check for win/loss/tie conditions
- **Maximizing Player**: AI tries to maximize score (returns highest value)
- **Minimizing Player**: Human tries to minimize score (returns lowest value)
- **Scoring**: 
  - Win: Positive score (with depth adjustment)
  - Loss: Negative score (with depth adjustment)
  - Tie: Zero score

### 3. **Depth Consideration**
- Scores are adjusted based on depth to prefer quicker wins
- `10 - depth` for AI win, `depth - 10` for human win

### 4. **How It Works**
1. **Generate all possible moves**
2. **For each move, recursively evaluate the game tree**
3. **Backtrack to find the optimal move**
4. **Choose the move that leads to the best outcome**

This implementation demonstrates the core principles of the minimax algorithm: exploring all possible game states and choosing the move that maximizes the minimum gain (or minimizes the maximum loss) for the current player.

