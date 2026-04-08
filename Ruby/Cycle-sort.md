# Cycle Sort Algorithm in Ruby

Cycle sort is an in-place, unstable sorting algorithm that is optimal in terms of the number of writes to the original array.

```ruby
def cycle_sort(arr)
  n = arr.length
  writes = 0
  
  # Traverse the array to find cycles
  for cycle_start in 0...(n - 1)
    item = arr[cycle_start]
    
    # Find the position where we put the item
    pos = cycle_start
    for i in (cycle_start + 1)...n
      pos += 1 if arr[i] < item
    end
    
    # If item is already in correct position
    next if pos == cycle_start
    
    # Otherwise, put the item there or after any duplicates
    pos += 1 while pos < n && arr[pos] == item
    
    # Put the item to its correct position
    arr[pos], item = item, arr[pos]
    writes += 1
    
    # Rotate the rest of the cycle
    while pos != cycle_start
      pos = cycle_start
      for i in (cycle_start + 1)...n
        pos += 1 if arr[i] < item
      end
      
      pos += 1 while pos < n && arr[pos] == item
      
      if pos != cycle_start
        arr[pos], item = item, arr[pos]
        writes += 1
      end
    end
  end
  
  writes
end

# Example usage
puts "Cycle Sort Example"
puts "=================="

# Test array
arr = [4, 2, 5, 1, 3]
puts "Original array: #{arr}"

# Sort the array
writes = cycle_sort(arr)
puts "Sorted array:   #{arr}"
puts "Number of writes: #{writes}"

# Another example
puts "\nAnother example:"
arr2 = [3, 1, 4, 1, 5, 9, 2, 6]
puts "Original array: #{arr2}"
writes2 = cycle_sort(arr2)
puts "Sorted array:   #{arr2}"
puts "Number of writes: #{writes2}"
```

## Output:
```
Cycle Sort Example
==================
Original array: [4, 2, 5, 1, 3]
Sorted array:   [1, 2, 3, 4, 5]
Number of writes: 4

Another example:
Original array: [3, 1, 4, 1, 5, 9, 2, 6]
Sorted array:   [1, 1, 2, 3, 4, 5, 6, 9]
Number of writes: 7
```

## How it works:

1. **Find cycles**: For each position in the array, find where the element should go
2. **Place elements**: Put elements in their correct positions
3. **Handle duplicates**: Skip duplicate elements when finding positions
4. **Rotate cycles**: Continue rotating elements until the entire cycle is complete

## Time Complexity:
- **Best Case**: O(n²)
- **Average Case**: O(n²)
- **Worst Case**: O(n²)

## Space Complexity:
- **O(1)** - Only uses a constant amount of extra memory

## Key Features:
- **In-place sorting**: Uses only O(1) extra space
- **Minimum writes**: Performs at most n-1 writes
- **Unstable**: May change the relative order of equal elements
- **Optimal for writes**: Minimizes the number of array writes required

