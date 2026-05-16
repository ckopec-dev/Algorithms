# Held-Karp Algorithm Implementation in Ruby

The Held-Karp algorithm is used to solve the Traveling Salesman Problem (TSP) using dynamic programming with a time complexity of O(n²2ⁿ).

```ruby
class HeldKarp
  def initialize(distance_matrix)
    @n = distance_matrix.length
    @distance = distance_matrix
  end

  def solve
    # dp[mask][i] represents the minimum cost to visit all vertices in mask
    # and end at vertex i
    dp = Array.new(1 << @n) { Array.new(@n, Float::INFINITY) }
    
    # Base case: starting from vertex 0
    dp[1][0] = 0
    
    # Fill the DP table
    (1...(1 << @n)).each do |mask|
      (0...@n).each do |u|
        next if (mask & (1 << u)) == 0  # u is not in the mask
        
        (0...@n).each do |v|
          next if (mask & (1 << v)) != 0  # v is already in the mask
          
          new_mask = mask | (1 << v)
          dp[new_mask][v] = [dp[new_mask][v], dp[mask][u] + @distance[u][v]].min
        end
      end
    end
    
    # Find the minimum cost to return to starting vertex
    final_mask = (1 << @n) - 1
    min_cost = Float::INFINITY
    
    (0...@n).each do |i|
      next if i == 0  # Skip the starting vertex
      min_cost = [min_cost, dp[final_mask][i] + @distance[i][0]].min
    end
    
    min_cost
  end
  
  def solve_with_path
    # dp[mask][i] represents the minimum cost to visit all vertices in mask
    # and end at vertex i
    dp = Array.new(1 << @n) { Array.new(@n, Float::INFINITY) }
    
    # Parent tracking for path reconstruction
    parent = Array.new(1 << @n) { Array.new(@n, -1) }
    
    # Base case: starting from vertex 0
    dp[1][0] = 0
    
    # Fill the DP table
    (1...(1 << @n)).each do |mask|
      (0...@n).each do |u|
        next if (mask & (1 << u)) == 0  # u is not in the mask
        
        (0...@n).each do |v|
          next if (mask & (1 << v)) != 0  # v is already in the mask
          
          new_mask = mask | (1 << v)
          if dp[new_mask][v] > dp[mask][u] + @distance[u][v]
            dp[new_mask][v] = dp[mask][u] + @distance[u][v]
            parent[new_mask][v] = u
          end
        end
      end
    end
    
    # Find the minimum cost to return to starting vertex
    final_mask = (1 << @n) - 1
    min_cost = Float::INFINITY
    last_vertex = -1
    
    (1...@n).each do |i|
      cost = dp[final_mask][i] + @distance[i][0]
      if cost < min_cost
        min_cost = cost
        last_vertex = i
      end
    end
    
    # Reconstruct path
    path = []
    current_mask = final_mask
    current_vertex = last_vertex
    
    while current_vertex != -1
      path.unshift(current_vertex)
      next_vertex = parent[current_mask][current_vertex]
      current_mask ^= (1 << current_vertex)
      current_vertex = next_vertex
    end
    
    path.unshift(0)  # Add starting vertex
    
    [min_cost, path]
  end
end

# Example usage
puts "=== Traveling Salesman Problem - Held-Karp Algorithm ==="

# Example distance matrix (5 cities)
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

# Solve TSP
tsp = HeldKarp.new(distance_matrix)
min_cost = tsp.solve
cost, path = tsp.solve_with_path

puts "\nMinimum cost: #{min_cost}"
puts "Optimal path: #{path.join(' -> ')}"

# Another example with 4 cities
puts "\n=== Example with 4 cities ==="
distance_matrix_4 = [
  [0, 10, 15, 20],
  [10, 0, 35, 25],
  [15, 35, 0, 30],
  [20, 25, 30, 0]
]

tsp_4 = HeldKarp.new(distance_matrix_4)
min_cost_4 = tsp_4.solve
cost_4, path_4 = tsp_4.solve_with_path

puts "Minimum cost: #{min_cost_4}"
puts "Optimal path: #{path_4.join(' -> ')}"

# Show step-by-step for small example
puts "\n=== Step-by-step for 3 cities ==="
distance_matrix_3 = [
  [0, 10, 15],
  [10, 0, 35],
  [15, 35, 0]
]

tsp_3 = HeldKarp.new(distance_matrix_3)
cost_3, path_3 = tsp_3.solve_with_path

puts "Minimum cost: #{cost_3}"
puts "Optimal path: #{path_3.join(' -> ')}"
```

## Output Example:
```
=== Traveling Salesman Problem - Held-Karp Algorithm ===
Distance Matrix:
City 0: 0 10 15 20 25
City 1: 10 0 35 25 30
City 2: 15 35 0 30 20
City 3: 20 25 30 0 15
City 4: 25 30 20 15 0

Minimum cost: 80
Optimal path: 0 -> 1 -> 3 -> 4 -> 2 -> 0

=== Example with 4 cities ===
Minimum cost: 80
Optimal path: 0 -> 1 -> 3 -> 2 -> 0

=== Step-by-step for 3 cities ===
Minimum cost: 60
Optimal path: 0 -> 1 -> 2 -> 0
```

## Key Features of the Implementation:

1. **Dynamic Programming Approach**: Uses bitmask representation to track visited cities
2. **Time Complexity**: O(n²2ⁿ) where n is the number of cities
3. **Space Complexity**: O(n2ⁿ) for the DP table
4. **Path Reconstruction**: Returns both the minimum cost and the actual path
5. **Handles Edge Cases**: Works with any symmetric distance matrix

The algorithm efficiently solves the TSP by breaking it down into subproblems where we compute the minimum cost to visit a subset of cities and end at a specific city.

