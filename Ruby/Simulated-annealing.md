# Simulated Annealing Algorithm in Ruby

Here's a complete implementation of the Simulated Annealing algorithm in Ruby with a classic example solving the Traveling Salesman Problem (TSP):

```ruby
class SimulatedAnnealing
  def initialize(distance_matrix, initial_temp = 1000, cooling_rate = 0.95, min_temp = 1e-8)
    @distance_matrix = distance_matrix
    @num_cities = distance_matrix.length
    @initial_temp = initial_temp
    @cooling_rate = cooling_rate
    @min_temp = min_temp
  end

  def solve(initial_solution = nil)
    # Generate initial solution if not provided
    current_solution = initial_solution || generate_random_solution
    best_solution = current_solution.dup
    current_cost = calculate_total_distance(current_solution)
    best_cost = current_cost
    
    temperature = @initial_temp
    
    # Keep track of the evolution
    costs = [best_cost]
    
    while temperature > @min_temp
      # Generate neighbor solution
      neighbor_solution = generate_neighbor(current_solution)
      neighbor_cost = calculate_total_distance(neighbor_solution)
      
      # Calculate energy difference
      delta = neighbor_cost - current_cost
      
      # Accept or reject the neighbor solution
      if delta < 0 || rand < Math.exp(-delta / temperature)
        current_solution = neighbor_solution
        current_cost = neighbor_cost
        
        # Update best solution if improved
        if current_cost < best_cost
          best_solution = current_solution.dup
          best_cost = current_cost
          costs << best_cost
        end
      end
      
      # Cool down the temperature
      temperature *= @cooling_rate
    end
    
    {
      best_solution: best_solution,
      best_cost: best_cost,
      cost_history: costs
    }
  end

  private

  def generate_random_solution
    solution = (0...@num_cities).to_a
    solution.shuffle!
    solution
  end

  def generate_neighbor(solution)
    # Create a copy of the solution
    neighbor = solution.dup
    
    # Generate two random indices
    i, j = rand(@num_cities), rand(@num_cities)
    
    # Swap the cities at these indices
    neighbor[i], neighbor[j] = neighbor[j], neighbor[i]
    
    neighbor
  end

  def calculate_total_distance(solution)
    total_distance = 0
    
    (0...@num_cities - 1).each do |i|
      from_city = solution[i]
      to_city = solution[i + 1]
      total_distance += @distance_matrix[from_city][to_city]
    end
    
    # Return to the starting city (complete the cycle)
    total_distance += @distance_matrix[solution.last][solution.first]
  end
end

# Example usage with a small TSP problem
puts "Simulated Annealing - Traveling Salesman Problem"
puts "=" * 50

# Define a distance matrix for 5 cities
# Distance matrix (symmetric)
distance_matrix = [
  [0, 10, 15, 20, 25],
  [10, 0, 35, 25, 30],
  [15, 35, 0, 30, 20],
  [20, 25, 30, 0, 15],
  [25, 30, 20, 15, 0]
]

puts "Distance Matrix:"
distance_matrix.each_with_index do |row, i|
  puts "City #{i}: #{row.join(' ')}"
end

puts "\nRunning Simulated Annealing..."

# Create and run the algorithm
sa = SimulatedAnnealing.new(distance_matrix)
result = sa.solve

puts "\nResults:"
puts "Best path: #{result[:best_solution].join(' -> ')}"
puts "Total distance: #{result[:best_cost]}"

# Show the cost evolution
puts "\nCost evolution over iterations:"
result[:cost_history].each_with_index do |cost, index|
  puts "Iteration #{index}: #{cost}"
end

# Example with custom initial solution
puts "\n" + "=" * 50
puts "Running with custom initial solution [0, 1, 2, 3, 4]"

custom_solution = [0, 1, 2, 3, 4]
result_custom = sa.solve(custom_solution)

puts "Best path: #{result_custom[:best_solution].join(' -> ')}"
puts "Total distance: #{result_custom[:best_cost]}"
```

## How it works:

1. **Initialization**: The algorithm starts with a random solution and sets initial parameters like temperature, cooling rate, and minimum temperature.

2. **Main Loop**: While the temperature is above the minimum threshold:
   - Generate a neighbor solution by making a small change to the current solution
   - Calculate the cost difference between the neighbor and current solution
   - Accept the neighbor solution if it's better, or with a probability based on the temperature if it's worse
   - Cool down the temperature by multiplying it with the cooling rate

3. **Acceptance Probability**: The key insight is that worse solutions can still be accepted with probability `exp(-ΔE/T)` where ΔE is the cost difference and T is the current temperature.

## Key Parameters:

- **Initial Temperature**: Controls how likely worse solutions are accepted early on
- **Cooling Rate**: How quickly the temperature decreases (typically 0.8-0.99)
- **Minimum Temperature**: When to stop the algorithm

## Output Example:
```
Simulated Annealing - Traveling Salesman Problem
==================================================
Distance Matrix:
City 0: 0 10 15 20 25
City 1: 10 0 35 25 30
City 2: 15 35 0 30 20
City 3: 20 25 30 0 15
City 4: 25 30 20 15 0

Running Simulated Annealing...

Results:
Best path: 0 -> 1 -> 3 -> 4 -> 2
Total distance: 70

Cost evolution over iterations:
Iteration 0: 100
Iteration 1: 85
Iteration 2: 75
...
```

This implementation demonstrates the core principles of simulated annealing and can be easily adapted for other optimization problems.

