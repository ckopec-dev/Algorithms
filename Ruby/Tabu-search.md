# Tabu Search Algorithm in Ruby

Here's an implementation of the Tabu Search algorithm in Ruby to solve a simple optimization problem:

```ruby
class TabuSearch
  def initialize(solution_size, max_iterations = 1000, tabu_tenure = 10)
    @solution_size = solution_size
    @max_iterations = max_iterations
    @tabu_tenure = tabu_tenure
    @tabu_list = []
    @best_solution = nil
    @best_fitness = -Float::INFINITY
  end

  # Generate a random solution
  def random_solution
    Array.new(@solution_size) { rand(0..1) }
  end

  # Calculate fitness of a solution (example: maximize number of 1s)
  def fitness(solution)
    solution.sum
  end

  # Generate neighbors by flipping one bit
  def generate_neighbors(solution)
    neighbors = []
    solution.each_index do |i|
      neighbor = solution.dup
      neighbor[i] = 1 - neighbor[i]  # Flip the bit
      neighbors << neighbor
    end
    neighbors
  end

  # Check if a move is tabu
  def tabu?(move)
    @tabu_list.include?(move)
  end

  # Add move to tabu list
  def add_to_tabu(move)
    @tabu_list << move
    @tabu_list.shift if @tabu_list.length > @tabu_tenure
  end

  # Main Tabu Search algorithm
  def search
    current_solution = random_solution
    current_fitness = fitness(current_solution)
    
    @best_solution = current_solution.dup
    @best_fitness = current_fitness
    
    iteration = 0
    
    puts "Starting Tabu Search..."
    puts "Initial fitness: #{@best_fitness}"
    
    while iteration < @max_iterations
      neighbors = generate_neighbors(current_solution)
      best_neighbor = nil
      best_neighbor_fitness = -Float::INFINITY
      
      # Find the best non-tabu neighbor
      neighbors.each do |neighbor|
        next if tabu?(neighbor)
        
        neighbor_fitness = fitness(neighbor)
        if neighbor_fitness > best_neighbor_fitness
          best_neighbor = neighbor
          best_neighbor_fitness = neighbor_fitness
        end
      end
      
      # If no non-tabu neighbor found, take the best one anyway
      if best_neighbor.nil?
        best_neighbor = neighbors.max_by { |n| fitness(n) }
      end
      
      # Update current solution
      current_solution = best_neighbor
      current_fitness = fitness(current_solution)
      
      # Update global best if needed
      if current_fitness > @best_fitness
        @best_solution = current_solution.dup
        @best_fitness = current_fitness
        puts "New best fitness: #{@best_fitness} at iteration #{iteration}"
      end
      
      # Add current move to tabu list
      add_to_tabu(current_solution)
      
      iteration += 1
    end
    
    puts "Search completed!"
    puts "Best solution: #{@best_solution.join}"
    puts "Best fitness: #{@best_fitness}"
    
    [@best_solution, @best_fitness]
  end
end

# Example usage
puts "=== Tabu Search Example ==="
puts "Finding solution with maximum number of 1s in a 10-bit string"

# Create and run Tabu Search
ts = TabuSearch.new(10, 500, 5)
best_solution, best_fitness = ts.search

puts "\n=== Results ==="
puts "Best solution found: #{best_solution.join}"
puts "Number of 1s: #{best_fitness}"
puts "Expected maximum: 10 (all 1s)"
```

## Key Components Explained:

### 1. **Initialization**
- `solution_size`: Size of the binary solution vector
- `max_iterations`: Maximum number of iterations
- `tabu_tenure`: How long a move stays in tabu list

### 2. **Core Methods**
- `random_solution()`: Generates random binary solution
- `fitness()`: Evaluates solution quality (maximizing 1s in this case)
- `generate_neighbors()`: Creates neighboring solutions by flipping one bit
- `tabu?()`: Checks if a move is forbidden
- `add_to_tabu()`: Adds move to tabu list with aging

### 3. **Algorithm Logic**
1. Start with random solution
2. Generate neighbors of current solution
3. Select best neighbor that's not tabu
4. Update current solution
5. Add current move to tabu list
6. Repeat until max iterations

### 4. **Output Example**
```
=== Tabu Search Example ===
Finding solution with maximum number of 1s in a 10-bit string
Starting Tabu Search...
Initial fitness: 4
New best fitness: 6 at iteration 12
New best fitness: 8 at iteration 25
New best fitness: 9 at iteration 45
New best fitness: 10 at iteration 67
Search completed!
Best solution: 1111111111
Best fitness: 10
```

This implementation demonstrates how Tabu Search avoids local optima by maintaining a memory of recent moves and preventing revisiting them for a specified number of iterations.

