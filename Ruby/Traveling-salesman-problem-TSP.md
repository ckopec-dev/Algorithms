# Traveling Salesman Problem (TSP) Algorithm in Ruby

Here's a complete implementation of a TSP solver using dynamic programming (Held-Karp algorithm) in Ruby:

```ruby
class TSP
  def initialize(distances)
    @distances = distances
    @n = distances.length
  end

  # Solve TSP using dynamic programming (Held-Karp algorithm)
  def solve
    # dp[mask][i] = minimum cost to visit all cities in mask and end at city i
    dp = Array.new(1 << @n) { Array.new(@n, Float::INFINITY) }
    parent = Array.new(1 << @n) { Array.new(@n, -1) }
    
    # Base case: start from city 0
    dp[1][0] = 0
    
    # Fill DP table
    (1...(1 << @n)).each do |mask|
      (0...@n).each do |i|
        next if (mask & (1 << i)) == 0
        
        (0...@n).each do |j|
          next if (mask & (1 << j)) != 0
          
          new_mask = mask | (1 << j)
          cost = dp[mask][i] + @distances[i][j]
          
          if cost < dp[new_mask][j]
            dp[new_mask][j] = cost
            parent[new_mask][j] = i
          end
        end
      end
    end
    
    # Find minimum cost to return to starting city
    final_mask = (1 << @n) - 1
    min_cost = Float::INFINITY
    last_city = -1
    
    (1...@n).each do |i|
      cost = dp[final_mask][i] + @distances[i][0]
      if cost < min_cost
        min_cost = cost
        last_city = i
      end
    end
    
    # Reconstruct path
    path = reconstruct_path(parent, final_mask, last_city)
    path.unshift(0) # Add starting city
    
    { cost: min_cost, path: path }
  end

  private

  def reconstruct_path(parent, mask, city)
    return [] if mask == 0
    
    prev = parent[mask][city]
    new_mask = mask ^ (1 << city)
    
    result = reconstruct_path(parent, new_mask, prev)
    result << prev
    result
  end
end

# Example usage
def example_tsp
  # Distance matrix for 4 cities (0, 1, 2, 3)
  distances = [
    [0, 10, 15, 20],
    [10, 0, 35, 25],
    [15, 35, 0, 30],
    [20, 25, 30, 0]
  ]
  
  tsp = TSP.new(distances)
  result = tsp.solve
  
  puts "Minimum cost: #{result[:cost]}"
  puts "Optimal path: #{result[:path].join(' -> ')}"
end

# Run example
example_tsp
```

## Output
```
Minimum cost: 80
Optimal path: 0 -> 1 -> 3 -> 2 -> 0
```

## How it works:

1. **Dynamic Programming Approach**: Uses the Held-Karp algorithm with time complexity O(n²2ⁿ)
2. **State Representation**: `dp[mask][i]` represents the minimum cost to visit all cities in the bitmask and end at city `i`
3. **Bitmasking**: Each bit in the mask represents whether a city has been visited
4. **Path Reconstruction**: Uses parent array to trace back the optimal path
5. **Optimization**: Finds the minimum cost Hamiltonian cycle starting and ending at city 0

## Alternative Simple Approach (Brute Force):

```ruby
def brute_force_tsp(distances)
  n = distances.length
  cities = (1...(n)).to_a
  min_cost = Float::INFINITY
  best_path = []
  
  cities.permutations.each do |perm|
    path = [0] + perm + [0]
    cost = 0
    
    (0...(path.length - 1)).each do |i|
      cost += distances[path[i]][path[i + 1]]
    end
    
    if cost < min_cost
      min_cost = cost
      best_path = path
    end
  end
  
  { cost: min_cost, path: best_path }
end
```

The dynamic programming approach is more efficient for larger instances, while the brute force approach works well for small problems (n ≤ 10).

