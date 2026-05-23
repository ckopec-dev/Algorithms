# Knapsack Problem Algorithm in Ruby

Here's an implementation of the classic 0/1 Knapsack problem using dynamic programming in Ruby:

```ruby
def knapsack_01(weights, values, capacity)
  n = weights.length
  
  # Create a 2D array for dynamic programming
  # dp[i][w] represents maximum value with first i items and weight limit w
  dp = Array.new(n + 1) { Array.new(capacity + 1, 0) }
  
  # Fill the dp table
  (1..n).each do |i|
    (0..capacity).each do |w|
      # Don't take the item
      dp[i][w] = dp[i-1][w]
      
      # Take the item if it fits
      if weights[i-1] <= w
        dp[i][w] = [dp[i][w], dp[i-1][w - weights[i-1]] + values[i-1]].max
      end
    end
  end
  
  # Return the maximum value
  dp[n][capacity]
end

def knapsack_with_items(weights, values, capacity)
  n = weights.length
  dp = Array.new(n + 1) { Array.new(capacity + 1, 0) }
  
  # Fill the dp table
  (1..n).each do |i|
    (0..capacity).each do |w|
      dp[i][w] = dp[i-1][w]
      
      if weights[i-1] <= w
        dp[i][w] = [dp[i][w], dp[i-1][w - weights[i-1]] + values[i-1]].max
      end
    end
  end
  
  # Backtrack to find which items were selected
  selected_items = []
  w = capacity
  (n).downto(1) do |i|
    # If value is different from above row, item was included
    if dp[i][w] != dp[i-1][w]
      selected_items << i - 1  # 0-indexed item number
      w -= weights[i-1]
    end
  end
  
  {
    max_value: dp[n][capacity],
    selected_items: selected_items.reverse
  }
end

# Example usage
weights = [2, 1, 3, 2]
values = [12, 10, 20, 15]
capacity = 5

puts "Weights: #{weights}"
puts "Values:  #{values}"
puts "Capacity: #{capacity}"
puts

# Simple knapsack solution
max_value = knapsack_01(weights, values, capacity)
puts "Maximum value: #{max_value}"

# Knapsack solution with item tracking
result = knapsack_with_items(weights, values, capacity)
puts "Maximum value: #{result[:max_value]}"
puts "Selected items (0-indexed): #{result[:selected_items]}"

# Show the breakdown
puts "\nItem breakdown:"
result[:selected_items].each do |item|
  puts "Item #{item}: weight = #{weights[item]}, value = #{values[item]}"
end
```

## Output:
```
Weights: [2, 1, 3, 2]
Values:  [12, 10, 20, 15]
Capacity: 5

Maximum value: 37
Maximum value: 37
Selected items (0-indexed): [0, 1, 3]

Item breakdown:
Item 0: weight = 2, value = 12
Item 1: weight = 1, value = 10
Item 3: weight = 2, value = 15
```

## Key Features:

1. **Dynamic Programming Approach**: Uses a 2D table to store intermediate results
2. **Time Complexity**: O(n × W) where n is number of items and W is capacity
3. **Space Complexity**: O(n × W)
4. **Item Tracking**: The second method also returns which specific items were selected
5. **0/1 Knapsack**: Each item can be taken at most once

The algorithm works by building up solutions for smaller subproblems and using them to solve larger problems, which is the essence of dynamic programming.

