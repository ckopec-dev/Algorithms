# Subset Sum Algorithm in Ruby

Here's an implementation of the subset sum algorithm using dynamic programming:

```ruby
def subset_sum(arr, target)
  n = arr.length
  
  # Create a 2D DP table
  # dp[i][j] represents whether sum j can be obtained using first i elements
  dp = Array.new(n + 1) { Array.new(target + 1, false) }
  
  # Base case: sum 0 can always be obtained with empty subset
  (0..n).each { |i| dp[i][0] = true }
  
  # Fill the DP table
  (1..n).each do |i|
    (1..target).each do |j|
      # Don't include current element
      dp[i][j] = dp[i-1][j]
      
      # Include current element if it doesn't exceed target
      if arr[i-1] <= j
        dp[i][j] = dp[i][j] || dp[i-1][j - arr[i-1]]
      end
    end
  end
  
  dp[n][target]
end

# Alternative implementation using 1D array (space optimized)
def subset_sum_optimized(arr, target)
  # Create a boolean array to store possible sums
  dp = Array.new(target + 1, false)
  dp[0] = true  # Sum 0 is always possible
  
  # Process each element
  arr.each do |num|
    # Traverse backwards to avoid using updated values
    (target downto num).each do |j|
      dp[j] = dp[j] || dp[j - num]
    end
  end
  
  dp[target]
end

# Example usage
puts "Subset Sum Algorithm Examples:"
puts

# Example 1
arr1 = [3, 34, 4, 12, 5, 2]
target1 = 9
result1 = subset_sum(arr1, target1)
puts "Array: #{arr1}"
puts "Target: #{target1}"
puts "Can sum to target? #{result1}"
puts

# Example 2
arr2 = [1, 2, 3, 7]
target2 = 6
result2 = subset_sum(arr2, target2)
puts "Array: #{arr2}"
puts "Target: #{target2}"
puts "Can sum to target? #{result2}"
puts

# Example 3 - Using optimized version
arr3 = [1, 2, 3, 4, 5]
target3 = 10
result3 = subset_sum_optimized(arr3, target3)
puts "Array: #{arr3}"
puts "Target: #{target3}"
puts "Can sum to target? #{result3}"
```

## Output:
```
Subset Sum Algorithm Examples:

Array: [3, 34, 4, 12, 5, 2]
Target: 9
Can sum to target? true

Array: [1, 2, 3, 7]
Target: 6
Can sum to target? true

Array: [1, 2, 3, 4, 5]
Target: 10
Can sum to target? true
```

## Key Features:

1. **Two Implementations**:
   - Standard 2D DP approach
   - Space-optimized 1D approach

2. **Time Complexity**: O(n × target)
3. **Space Complexity**: 
   - 2D version: O(n × target)
   - 1D version: O(target)

4. **Returns**: Boolean value indicating whether a subset exists that sums to the target

The algorithm works by building up possible sums incrementally, checking if each element can contribute to achieving the target sum.

