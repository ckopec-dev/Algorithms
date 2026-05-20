# Jump Search Algorithm in Ruby

Jump search is a searching algorithm that works on sorted arrays by jumping ahead by fixed steps and then performing a linear search in the range where the target element might be located.

## Implementation

```ruby
def jump_search(arr, target)
  n = arr.length
  
  # Handle edge cases
  return -1 if n == 0
  
  # Calculate optimal jump size
  jump_size = Math.sqrt(n).to_i
  
  # Find the block where target may be present
  prev = 0
  while arr[[jump_size, n - 1].min] < target
    prev = jump_size
    jump_size += Math.sqrt(n).to_i
    
    # If we've gone beyond the array
    return -1 if prev >= n
  end
  
  # Linear search in the identified block
  (prev...[jump_size, n].min).each do |i|
    return i if arr[i] == target
  end
  
  -1  # Element not found
end

# Example usage
def example
  # Sorted array
  arr = [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29]
  
  puts "Array: #{arr}"
  puts "Array length: #{arr.length}"
  
  # Test cases
  targets = [7, 15, 25, 2, 30]
  
  targets.each do |target|
    result = jump_search(arr, target)
    if result != -1
      puts "Element #{target} found at index: #{result}"
    else
      puts "Element #{target} not found in array"
    end
  end
end

# Run example
example
```

## Output
```
Array: [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29]
Array length: 15
Element 7 found at index: 3
Element 15 found at index: 7
Element 25 found at index: 12
Element 2 not found in array
Element 30 not found in array
```

## How it works:

1. **Calculate jump size**: Use square root of array length as jump size
2. **Jump through blocks**: Move through the array in jumps of the calculated size
3. **Find range**: When we find an element greater than target, we know the target is in the previous block
4. **Linear search**: Perform linear search within the identified block

## Time Complexity:
- **Best case**: O(1) - element found at first jump
- **Average case**: O(√n)
- **Worst case**: O(√n)

## Space Complexity: O(1)

## Advantages:
- More efficient than linear search for large sorted arrays
- Simpler than binary search implementation
- Works well when the array is large and random access is expensive

## Disadvantages:
- Requires sorted array
- Not as efficient as binary search in most cases
- May require multiple comparisons in the final linear search phase

