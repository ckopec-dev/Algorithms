# Exponential Search Algorithm in Ruby

Exponential search is a searching algorithm that works by first finding a range where the target element might exist, then performing a binary search within that range.

## Implementation

```ruby
def exponential_search(arr, target)
  # Handle edge case of empty array
  return -1 if arr.empty?
  
  # If target is first element
  return 0 if arr[0] == target
  
  # Find range where target might exist
  index = 1
  while index < arr.length && arr[index] < target
    index *= 2
  end
  
  # Perform binary search in the range [index/2, min(index, arr.length-1)]
  left = index / 2
  right = [index, arr.length - 1].min
  
  # Binary search within the identified range
  while left <= right
    mid = (left + right) / 2
    
    if arr[mid] == target
      return mid
    elsif arr[mid] < target
      left = mid + 1
    else
      right = mid - 1
    end
  end
  
  -1 # Target not found
end

# Example usage
def example
  # Sorted array
  arr = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]
  
  puts "Array: #{arr}"
  puts
  
  # Test cases
  test_cases = [1, 5, 10, 15, 20]
  
  test_cases.each do |target|
    result = exponential_search(arr, target)
    if result != -1
      puts "Target #{target} found at index: #{result}"
    else
      puts "Target #{target} not found in array"
    end
  end
end

# Run example
example
```

## Output
```
Array: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]

Target 1 found at index: 0
Target 5 found at index: 4
Target 10 found at index: 9
Target 15 found at index: 14
Target 20 not found in array
```

## How it works:

1. **First Step**: Check if the first element is the target
2. **Range Finding**: Keep doubling the index until we find an element greater than or equal to the target
3. **Binary Search**: Perform binary search in the range [index/2, min(index, array_length-1)]

## Time Complexity:
- **Best Case**: O(1) - when target is the first element
- **Average/Worst Case**: O(log n) - where n is the number of elements

## Space Complexity:
- O(1) - only using a constant amount of extra space

## When to use:
- When the array is sorted and has unknown size
- When the target element is likely to be near the beginning of the array
- When you want to avoid the overhead of binary search when the array size is unknown

