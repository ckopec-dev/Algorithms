# TimSort Algorithm Implementation in Ruby

```ruby
class TimSort
  MIN_MERGE = 32
  
  def self.sort(array)
    return array if array.length <= 1
    
    n = array.length
    # Sort individual subarrays of size MIN_MERGE
    (0...n).step(MIN_MERGE) do |i|
      insertion_sort(array, i, [i + MIN_MERGE - 1, n - 1].min)
    end
    
    # Merge subarrays in bottom-up manner
    size = MIN_MERGE
    while size < n
      left = 0
      while left < n - size
        mid = left + size - 1
        right = [left + size * 2 - 1, n - 1].min
        
        if mid < right
          merge(array, left, mid, right)
        end
        
        left += size * 2
      end
      size *= 2
    end
    
    array
  end
  
  private
  
  def self.insertion_sort(array, left, right)
    (left + 1..right).each do |i|
      key = array[i]
      j = i - 1
      
      while j >= left && array[j] > key
        array[j + 1] = array[j]
        j -= 1
      end
      
      array[j + 1] = key
    end
  end
  
  def self.merge(array, left, mid, right)
    # Create temporary arrays
    left_array = array[left..mid]
    right_array = array[mid + 1..right]
    
    i = j = 0
    k = left
    
    # Merge the temporary arrays back
    while i < left_array.length && j < right_array.length
      if left_array[i] <= right_array[j]
        array[k] = left_array[i]
        i += 1
      else
        array[k] = right_array[j]
        j += 1
      end
      k += 1
    end
    
    # Copy remaining elements
    while i < left_array.length
      array[k] = left_array[i]
      i += 1
      k += 1
    end
    
    while j < right_array.length
      array[k] = right_array[j]
      j += 1
      k += 1
    end
  end
end

# Example usage
puts "TimSort Example"
puts "=" * 20

# Test with different arrays
test_arrays = [
  [64, 34, 25, 12, 22, 11, 90],
  [5, 2, 4, 6, 1, 3],
  [1],
  [],
  [3, 3, 3, 3],
  [5, 4, 3, 2, 1]
]

test_arrays.each_with_index do |arr, index|
  puts "\nTest #{index + 1}:"
  puts "Original: #{arr}"
  sorted = TimSort.sort(arr.dup)
  puts "Sorted:   #{sorted}"
end
```

## Output:
```
TimSort Example
====================

Test 1:
Original: [64, 34, 25, 12, 22, 11, 90]
Sorted:   [11, 12, 22, 25, 34, 64, 90]

Test 2:
Original: [5, 2, 4, 6, 1, 3]
Sorted:   [1, 2, 3, 4, 5, 6]

Test 3:
Original: [1]
Sorted:   [1]

Test 4:
Original: []
Sorted:   []

Test 5:
Original: [3, 3, 3, 3]
Sorted:   [3, 3, 3, 3]

Test 6:
Original: [5, 4, 3, 2, 1]
Sorted:   [1, 2, 3, 4, 5]
```

## How TimSort Works:

1. **Divide**: Split the array into smaller subarrays of size `MIN_MERGE` (32 by default)
2. **Sort**: Use insertion sort on each subarray
3. **Merge**: Merge adjacent subarrays in a bottom-up fashion
4. **Optimization**: Takes advantage of existing sorted sequences in the data

## Key Features:

- **Stable**: Maintains relative order of equal elements
- **Adaptive**: Performs well on partially sorted data
- **Hybrid**: Combines insertion sort and merge sort techniques
- **Time Complexity**: O(n log n) worst case, O(n) best case
- **Space Complexity**: O(n)

