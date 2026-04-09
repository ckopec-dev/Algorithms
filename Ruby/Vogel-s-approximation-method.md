# Vogel's Approximation Method (VAM) in Ruby

Vogel's Approximation Method is used to find an initial basic feasible solution for transportation problems. Here's a complete implementation:

```ruby
class TransportationProblem
  def initialize(cost_matrix, supply, demand)
    @cost_matrix = cost_matrix
    @supply = supply.dup
    @demand = demand.dup
    @rows = supply.length
    @cols = demand.length
    @solution = Array.new(@rows) { Array.new(@cols, 0) }
  end

  def vogel_approximation_method
    puts "Initial Cost Matrix:"
    print_matrix(@cost_matrix)
    puts "\nSupply: #{@supply}"
    puts "Demand: #{@demand}"
    
    total_cost = 0
    iterations = 0
    
    while !is_complete?
      iterations += 1
      puts "\n--- Iteration #{iterations} ---"
      
      # Step 1: Calculate penalties for each row and column
      row_penalties = calculate_row_penalties
      col_penalties = calculate_column_penalties
      
      puts "Row penalties: #{row_penalties}"
      puts "Column penalties: #{col_penalties}"
      
      # Step 2: Find maximum penalty
      max_row_penalty = row_penalties.max
      max_col_penalty = col_penalties.max
      
      puts "Max row penalty: #{max_row_penalty}"
      puts "Max column penalty: #{max_col_penalty}"
      
      # Step 3: Select cell with maximum penalty
      row, col = select_cell_with_max_penalty(row_penalties, col_penalties)
      
      puts "Selected cell: [#{row}, #{col}] with cost #{@cost_matrix[row][col]}"
      
      # Step 4: Allocate maximum possible amount
      allocation = [(@supply[row]).to_f, (@demand[col]).to_f].min
      @solution[row][col] = allocation
      total_cost += allocation * @cost_matrix[row][col]
      
      puts "Allocated: #{allocation}"
      puts "Cost for this allocation: #{allocation * @cost_matrix[row][col]}"
      
      # Step 5: Update supply and demand
      @supply[row] -= allocation
      @demand[col] -= allocation
      
      puts "Updated supply: #{@supply}"
      puts "Updated demand: #{@demand}"
      
      # Step 6: Remove exhausted row or column
      if @supply[row] == 0
        puts "Row #{row} exhausted"
      end
      
      if @demand[col] == 0
        puts "Column #{col} exhausted"
      end
      
      print_solution
    end
    
    puts "\n--- Final Solution ---"
    print_solution
    puts "\nTotal Transportation Cost: #{total_cost}"
    
    return @solution, total_cost
  end

  private

  def calculate_row_penalties
    penalties = []
    @rows.times do |i|
      if @supply[i] > 0
        valid_costs = []
        @cols.times do |j|
          if @demand[j] > 0
            valid_costs << @cost_matrix[i][j]
          end
        end
        
        if valid_costs.length >= 2
          valid_costs.sort!
          penalties << valid_costs[1] - valid_costs[0]
        else
          penalties << 0
        end
      else
        penalties << 0
      end
    end
    penalties
  end

  def calculate_column_penalties
    penalties = []
    @cols.times do |j|
      if @demand[j] > 0
        valid_costs = []
        @rows.times do |i|
          if @supply[i] > 0
            valid_costs << @cost_matrix[i][j]
          end
        end
        
        if valid_costs.length >= 2
          valid_costs.sort!
          penalties << valid_costs[1] - valid_costs[0]
        else
          penalties << 0
        end
      else
        penalties << 0
      end
    end
    penalties
  end

  def select_cell_with_max_penalty(row_penalties, col_penalties)
    max_penalty = [row_penalties.max, col_penalties.max].max
    
    if max_penalty == row_penalties.max
      # Find row with maximum penalty
      row_index = row_penalties.index(max_penalty)
      col_index = find_best_column_for_row(row_index)
      [row_index, col_index]
    else
      # Find column with maximum penalty
      col_index = col_penalties.index(max_penalty)
      row_index = find_best_row_for_column(col_index)
      [row_index, col_index]
    end
  end

  def find_best_column_for_row(row_index)
    min_cost = Float::INFINITY
    best_col = -1
    
    @cols.times do |j|
      if @demand[j] > 0 && @cost_matrix[row_index][j] < min_cost
        min_cost = @cost_matrix[row_index][j]
        best_col = j
      end
    end
    
    best_col
  end

  def find_best_row_for_column(col_index)
    min_cost = Float::INFINITY
    best_row = -1
    
    @rows.times do |i|
      if @supply[i] > 0 && @cost_matrix[i][col_index] < min_cost
        min_cost = @cost_matrix[i][col_index]
        best_row = i
      end
    end
    
    best_row
  end

  def is_complete?
    @supply.sum == 0 && @demand.sum == 0
  end

  def print_matrix(matrix)
    matrix.each do |row|
      puts row.map { |cell| "%6d" % cell }.join(" ")
    end
  end

  def print_solution
    puts "\nCurrent Solution:"
    print_matrix(@solution)
  end
end

# Example usage
puts "=== Vogel's Approximation Method Example ==="
puts

# Example problem:
# Cost matrix (3 suppliers, 4 demand points)
cost_matrix = [
  [3, 1, 7, 4],
  [2, 6, 5, 9],
  [8, 3, 3, 2]
]

# Supply for each supplier
supply = [30, 40, 50]

# Demand for each demand point
demand = [25, 35, 10, 30]

# Create transportation problem
problem = TransportationProblem.new(cost_matrix, supply, demand)

# Solve using VAM
solution, total_cost = problem.vogel_approximation_method

puts "\n=== Summary ==="
puts "Final Allocation Matrix:"
solution.each { |row| puts row.join(" ") }
puts "Total Cost: #{total_cost}"
```

## How the Algorithm Works:

1. **Calculate penalties**: For each row and column, find the difference between the two smallest costs
2. **Select maximum penalty**: Choose the row or column with the highest penalty
3. **Allocate optimally**: In the selected row/column, allocate to the cell with minimum cost
4. **Update supply/demand**: Reduce supply/demand accordingly
5. **Repeat**: Continue until all supplies and demands are satisfied

## Key Features:

- Handles any size transportation problem
- Shows step-by-step process
- Calculates total transportation cost
- Provides clear output of each iteration
- Properly handles exhausted rows/columns

The algorithm typically produces a solution that is close to optimal, making it a valuable initial approach for transportation problems before applying more complex optimization methods.

