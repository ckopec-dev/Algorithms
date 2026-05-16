# Quick Select Algorithm in Ruby

Quick Select is an efficient algorithm to find the kth smallest element in an unsorted array. Here's an implementation:

```ruby
def quick_select(arr, k)
  # Make a copy to avoid modifying the original array
  array = arr.dup
  quick_select_helper(array, 0, array.length - 1, k - 1)
end

def quick_select_helper(array, left, right, k)
  return array[k] if left == right
  
  # Partition the array and get the pivot index
  pivot_index = partition(array, left, right)
  
  if k == pivot_index
    return array[k]
  elsif k < pivot_index
    # Search in the left subarray
    return quick_select_helper(array, left, pivot_index - 1, k)
  else
    # Search in the right subarray
    return quick_select_helper(array, pivot_index + 1, right, k)
  end
end

def partition(array, left, right)
  # Choose the rightmost element as pivot
  pivot = array[right]
  
  # Index of smaller element (indicates right position of pivot)
  i = left - 1
  
  (left...right).each do |j|
    # If current element is smaller than or equal to pivot
    if array[j] <= pivot
      i += 1
      array[i], array[j] = array[j], array[i]  # Swap elements
    end
  end
  
  # Place pivot in its correct position
  array[i + 1], array[right] = array[right], array[i + 1]
  i + 1
end

# Example usage
numbers = [3, 2, 1, 5, 4, 6, 8, 7]
puts "Original array: #{numbers}"

# Find the 3rd smallest element
k = 3
result = quick_select(numbers, k)
puts "The #{k}rd smallest element is: #{result}"

# Find the 5th smallest element
k = 5
result = quick_select(numbers, k)
puts "The #{k}th smallest element is: #{result}"

# Find the 1st smallest element (minimum)
k = 1
result = quick_select(numbers, k)
puts "The #{k}st smallest element is: #{result}"
```

## Output:
```
Original array: [3, 2, 1, 5, 4, 6, 8, 7]
The 3rd smallest element is: 3
The 5th smallest element is: 5
The 1st smallest element is: 1
```

## How it works:

1. **Partitioning**: The algorithm partitions the array around a pivot element
2. **Recursion**: Based on the pivot's position, it recursively searches in the appropriate subarray
3. **Efficiency**: Average time complexity is O(n), with worst case O(n²)

## Key Features:

- **In-place**: Modifies the array during partitioning
- **Average O(n)**: Much faster than sorting the entire array
- **Flexible**: Can find any kth smallest element (1-indexed)
- **No need to sort**: Only partially sorts the array

The algorithm is particularly useful when you need to find a specific element without sorting the entire dataset.

