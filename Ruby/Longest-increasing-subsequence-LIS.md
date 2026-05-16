# Longest Increasing Subsequence (LIS) Algorithm in Ruby

Here's an implementation of the LIS algorithm using dynamic programming:

```ruby
def longest_increasing_subsequence(arr)
  return [] if arr.empty?
  
  n = arr.length
  # dp[i] represents the length of LIS ending at index i
  dp = Array.new(n, 1)
  
  # Fill dp array
  (1...n).each do |i|
    (0...i).each do |j|
      if arr[i] > arr[j]
        dp[i] = [dp[i], dp[j] + 1].max
      end
    end
  end
  
  # Find the maximum length
  max_length = dp.max
  
  # Reconstruct the actual subsequence
  result = []
  current_length = max_length
  i = n - 1
  
  # Backtrack to find the actual elements
  while i >= 0 && current_length > 0
    if dp[i] == current_length
      result.unshift(arr[i])
      current_length -= 1
    end
    i -= 1
  end
  
  result
end

# Alternative implementation with O(n log n) time complexity
def lis_optimized(arr)
  return [] if arr.empty?
  
  # Array to store the smallest tail element for each length
  tails = []
  
  arr.each do |num|
    # Binary search for the position to insert/replace
    left, right = 0, tails.length
    
    while left < right
      mid = (left + right) / 2
      if tails[mid] < num
        left = mid + 1
      else
        right = mid
      end
    end
    
    # If num is larger than all elements in tails, append it
    if left == tails.length
      tails << num
    else
      # Replace the element at position left
      tails[left] = num
    end
  end
  
  tails
end

# Example usage
puts "Example 1:"
arr1 = [10, 9, 2, 5, 3, 7, 101, 18]
puts "Input array: #{arr1}"
puts "LIS: #{longest_increasing_subsequence(arr1)}"
puts "Length: #{longest_increasing_subsequence(arr1).length}"

puts "\nExample 2:"
arr2 = [0, 1, 0, 3, 2, 3]
puts "Input array: #{arr2}"
puts "LIS: #{longest_increasing_subsequence(arr2)}"
puts "Length: #{longest_increasing_subsequence(arr2).length}"

puts "\nExample 3:"
arr3 = [7, 7, 7, 7, 7, 7, 7]
puts "Input array: #{arr3}"
puts "LIS: #{longest_increasing_subsequence(arr3)}"
puts "Length: #{longest_increasing_subsequence(arr3).length}"

puts "\nExample 4:"
arr4 = [1, 3, 6, 7, 9, 4, 10, 5, 6]
puts "Input array: #{arr4}"
puts "LIS: #{longest_increasing_subsequence(arr4)}"
puts "Length: #{longest_increasing_subsequence(arr4).length}"

puts "\nOptimized version (O(n log n)):"
puts "LIS (optimized): #{lis_optimized(arr1)}"
```

## Output:
```
Example 1:
Input array: [10, 9, 2, 5, 3, 7, 101, 18]
LIS: [2, 3, 7, 18]
Length: 4

Example 2:
Input array: [0, 1, 0, 3, 2, 3]
LIS: [0, 1, 2, 3]
Length: 4

Example 3:
Input array: [7, 7, 7, 7, 7, 7, 7]
LIS: [7]
Length: 1

Example 4:
Input array: [1, 3, 6, 7, 9, 4, 10, 5, 6]
LIS: [1, 3, 6, 7, 9, 10]
Length: 6

Optimized version (O(n log n)):
LIS (optimized): [2, 3, 7, 18]
```

## Algorithm Explanation:

### Basic DP Approach (O(n²)):
1. **Initialization**: Create a `dp` array where `dp[i]` represents the length of LIS ending at index `i`
2. **Fill DP Array**: For each element, check all previous elements and update `dp[i]` if a longer subsequence is found
3. **Reconstruction**: Backtrack through the `dp` array to reconstruct the actual subsequence

### Optimized Approach (O(n log n)):
1. **Maintain tails array**: Store the smallest tail element for each possible LIS length
2. **Binary search**: For each element, find the correct position in the tails array
3. **Update**: Either append the element or replace an existing element

Both approaches correctly solve the LIS problem, with the optimized version being more efficient for large inputs.

