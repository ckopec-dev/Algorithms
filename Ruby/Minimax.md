# Minimax Algorithm in Ruby

Here's a complete implementation of the Minimax algorithm in Ruby with a simple tic-tac-toe game example:

```ruby
class TicTacToe
  def initialize
    @board = Array.new(3) { Array.new(3, ' ') }
  end

  def display_board
    puts " #{@board[0][0]} | #{@board[0][1]} | #{@board[0][2]} "
    puts "-----------"
    puts " #{@board[1][0]} | #{@board[1][1]} | #{@board[1][2]} "
    puts "-----------"
    puts " #{@board[2][0]} | #{@board[2][1]} | #{@board[2][2]} "
  end

  def available_moves
    moves = []
    @board.each_with_index do |row, i|
      row.each_with_index do |cell, j|
        moves << [i, j] if cell == ' '
      end
    end
    moves
  end

  def make_move(row, col, player)
    @board[row][col] = player
  end

  def undo_move(row, col)
    @board[row][col] = ' '
  end

  def check_winner
    # Check rows
    @board.each do |row|
      return row[0] if row[0] == row[1] && row[1] == row[2] && row[0] != ' '
    end

    # Check columns
    (0..2).each do |col|
      if @board[0][col] == @board[1][col] && @board[1][col] == @board[2][col] && @board[0][col] != ' '
        return @board[0][col]
      end
    end

    # Check diagonals
    if @board[0][0] == @board[1][1] && @board[1][1] == @board[2][2] && @board[0][0] != ' '
      return @board[0][0]
    end
    if @board[0][2] == @board[1][1] && @board[1][1] == @board[2][0] && @board[0][2] != ' '
      return @board[0][2]
    end

    nil
  end

  def is_board_full?
    @board.each do |row|
      return false if row.include?(' ')
    end
    true
  end

  def game_over?
    check_winner || is_board_full?
  end

  def minimax(depth, is_maximizing)
    winner = check_winner
    
    # Terminal states
    if winner == 'X'
      return 10 - depth
    elsif winner == 'O'
      return depth - 10
    elsif is_board_full?
      return 0
    end

    if is_maximizing
      max_eval = -Float::INFINITY
      available_moves.each do |move|
        row, col = move
        make_move(row, col, 'X')
        eval = minimax(depth + 1, false)
        undo_move(row, col)
        max_eval = [max_eval, eval].max
      end
      max_eval
    else
      min_eval = Float::INFINITY
      available_moves.each do |move|
        row, col = move
        make_move(row, col, 'O')
        eval = minimax(depth + 1, true)
        undo_move(row, col)
        min_eval = [min_eval, eval].min
      end
      min_eval
    end
  end

  def best_move
    best_score = -Float::INFINITY
    best_move = nil
    
    available_moves.each do |move|
      row, col = move
      make_move(row, col, 'X')
      score = minimax(0, false)
      undo_move(row, col)
      
      if score > best_score
        best_score = score
        best_move = move
      end
    end
    
    best_move
  end
end

# Example usage
game = TicTacToe.new

puts "Welcome to Tic-Tac-Toe!"
puts "You are 'O', computer is 'X'"
puts

# Make some moves to demonstrate
game.make_move(0, 0, 'O')
game.make_move(1, 1, 'X')
game.make_move(0, 1, 'O')

puts "Current board:"
game.display_board
puts

puts "Computer is thinking..."
best_move = game.best_move
puts "Computer's best move: #{best_move}"

# Make the computer's move
game.make_move(best_move[0], best_move[1], 'X')
puts

puts "After computer's move:"
game.display_board
```

## Key Components Explained

### 1. **Game State Representation**
- 3x3 board using nested arrays
- Empty cells represented by spaces `' '`

### 2. **Minimax Algorithm Implementation**
- **Recursive function** that evaluates all possible moves
- **Depth parameter** to track game tree depth
- **Maximizing/Minimizing** player turns
- **Terminal conditions**:
  - Win for 'X' (computer) = +10 - depth
  - Win for 'O' (human) = -10 + depth  
  - Draw = 0

### 3. **Algorithm Logic**
- **Maximizing player** ('X') tries to maximize score
- **Minimizing player** ('O') tries to minimize score
- **Backtracking** through all possible game states
- **Returns optimal move** based on best evaluation

### 4. **Time Complexity**
- **O(b^d)** where b is branching factor (9 for tic-tac-toe) and d is depth
- **Space complexity**: O(d) for recursion stack

This implementation demonstrates the core principles of the minimax algorithm: evaluating all possible future game states and choosing the move that leads to the best outcome, assuming the opponent plays optimally.

