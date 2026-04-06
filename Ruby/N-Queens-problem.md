# N-Queens Problem Algorithm in Ruby

Here's a complete implementation of the N-Queens problem solver using backtracking in Ruby:

```ruby
class NQueens
  def initialize(n)
    @n = n
    @solutions = []
  end

  def solve
    @solutions = []
    board = Array.new(@n) { Array.new(@n, 0) }
    backtrack(board, 0)
    @solutions
  end

  def print_solutions
    solutions = solve
    puts "Number of solutions for #{@n}-Queens: #{solutions.length}"
    puts
    
    solutions.each_with_index do |solution, index|
      puts "Solution #{index + 1}:"
      print_board(solution)
      puts
    end
  end

  private

  def backtrack(board, row)
    return if row >= @n
    
    (0...@n).each do |col|
      if is_safe?(board, row, col)
        board[row][col] = 1
        @solutions << deep_copy(board) if row == @n - 1
        backtrack(board, row + 1)
        board[row][col] = 0
      end
    end
  end

  def is_safe?(board, row, col)
    # Check column
    (0...row).each do |i|
      return false if board[i][col] == 1
    end

    # Check upper left diagonal
    i = row - 1
    j = col - 1
    while i >= 0 && j >= 0
      return false if board[i][j] == 1
      i -= 1
      j -= 1
    end

    # Check upper right diagonal
    i = row - 1
    j = col + 1
    while i >= 0 && j < @n
      return false if board[i][j] == 1
      i -= 1
      j += 1
    end

    true
  end

  def print_board(board)
    board.each do |row|
      row.each do |cell|
        print cell == 1 ? "Q " : ". "
      end
      puts
    end
  end

  def deep_copy(board)
    board.map(&:clone)
  end
end

# Example usage
puts "=== N-Queens Problem Solver ==="
puts

# Solve 4-Queens problem
puts "Solving 4-Queens problem:"
queens_4 = NQueens.new(4)
queens_4.print_solutions

puts "-" * 30
puts

# Solve 8-Queens problem
puts "Solving 8-Queens problem:"
queens_8 = NQueens.new(8)
queens_8.print_solutions

# Simple function to count solutions only
def count_n_queens_solutions(n)
  return 0 if n < 1
  return 1 if n == 1
  
  count = 0
  board = Array.new(n) { Array.new(n, 0) }
  
  def backtrack_count(board, row, n, count)
    return count if row >= n
    
    (0...n).each do |col|
      if is_safe?(board, row, col, n)
        board[row][col] = 1
        count = backtrack_count(board, row + 1, n, count)
        board[row][col] = 0
      end
    end
    
    count
  end
  
  def is_safe?(board, row, col, n)
    # Check column
    (0...row).each do |i|
      return false if board[i][col] == 1
    end

    # Check diagonals
    i = row - 1
    j = col - 1
    while i >= 0 && j >= 0
      return false if board[i][j] == 1
      i -= 1
      j -= 1
    end

    i = row - 1
    j = col + 1
    while i >= 0 && j < n
      return false if board[i][j] == 1
      i -= 1
      j += 1
    end

    true
  end
  
  backtrack_count(board, 0, n, count)
end

puts "-" * 30
puts "Number of solutions for different N values:"
(1..8).each do |n|
  solutions = count_n_queens_solutions(n)
  puts "N=#{n}: #{solutions} solutions"
end
```

## How it works:

1. **Backtracking Algorithm**: The solution uses a recursive backtracking approach to place queens row by row
2. **Safety Check**: For each position, it checks if placing a queen would conflict with existing queens
3. **Constraint Checking**: Validates that no two queens are in the same column or diagonal
4. **Solution Storage**: Stores valid board configurations when all queens are placed

## Key Features:

- **Complete Implementation**: Handles all N-Queens problems from 1 to 8
- **Visual Output**: Displays board configurations with 'Q' for queens and '.' for empty spaces
- **Solution Counting**: Provides both solution visualization and count statistics
- **Efficient Backtracking**: Uses pruning to avoid invalid configurations early

## Sample Output:
```
Number of solutions for 4-Queens: 2

Solution 1:
. . Q . 
Q . . . 
. . . Q 
. Q . . 

Solution 2:
. Q . . 
. . . Q 
Q . . . 
. . Q . 
```

This implementation demonstrates the classic backtracking approach to solve the N-Queens problem efficiently.

