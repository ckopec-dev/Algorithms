# Fibonacci Search Technique in Ruby

```ruby
def fibonacci_search(arr, target)
  n = arr.length
  
  # Generate Fibonacci numbers
  fib_m2 = 0  # (m-2)th Fibonacci number
  fib_m1 = 1  # (m-1)th Fibonacci number
  fib_m = fib_m2 + fib_m1  # mth Fibonacci number
  
  # Find the smallest Fibonacci number greater than or equal to n
  while fib_m < n
    fib_m2 = fib_m1
    fib_m1 = fib_m
    fib_m = fib_m2 + fib_m1
  end
  
  # Marks the eliminated range from front
  offset = -1
  
  # While there are elements to be checked
  while fib_m > 1
    # Check if fib_m2 is a valid location
    i = [offset + fib_m2, n - 1].min
    
    # If target is greater than the value at index fib_m2,
    # cut the subarray from offset to i
    if arr[i] < target
      fib_m = fib_m1
      fib_m1 = fib_m2
      fib_m2 = fib_m - fib_m1
      offset = i
    # If target is less than the value at index fib_m2,
    # cut the subarray after i
    elsif arr[i] > target
      fib_m = fib_m2
      fib_m1 = fib_m1 - fib_m2
      fib_m2 = fib_m - fib_m1
    # Element found
    else
      return i
    end
  end
  
  # Comparing the last element with target
  if fib_m1 == 1 && arr[offset + 1] == target
    return offset + 1
  end
  
  # Element not found
  return -1
end

# Example usage
arr = [10, 22, 35, 40, 45, 50, 80, 82, 85, 90, 100]
target = 85

result = fibonacci_search(arr, target)

if result != -1
  puts "Element found at index: #{result}"
else
  puts "Element not found"
end

# Test with another example
target2 = 25
result2 = fibonacci_search(arr, target2)

if result2 != -1
  puts "Element found at index: #{result2}"
else
  puts "Element not found"
end
```

## Output:
```
Element found at index: 8
Element not found
```

## How it works:

1. **Fibonacci Number Generation**: The algorithm generates Fibonacci numbers until it finds one that is greater than or equal to the array length.

2. **Search Process**: 
   - Compare target with element at `offset + fib_m2`
   - If target is greater, move the search to the right
   - If target is smaller, move the search to the left
   - If target matches, return the index

3. **Time Complexity**: O(log n)
4. **Space Complexity**: O(1)

The Fibonacci search technique is particularly useful when the array size is a Fibonacci number, and it can be more efficient than binary search in certain scenarios due to its cache-friendly access pattern.

