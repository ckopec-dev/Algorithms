# Pigeonhole Sort Algorithm in Ruby

```ruby
def pigeonhole_sort(array)
  # Handle edge cases
  return array if array.length <= 1
  
  # Find the minimum and maximum values
  min = array.min
  max = array.max
  
  # Calculate the range
  range = max - min + 1
  
  # Create pigeonholes (array of buckets)
  pigeonholes = Array.new(range, 0)
  
  # Place each element in its corresponding pigeonhole
  array.each do |element|
    pigeonholes[element - min] += 1
  end
  
  # Reconstruct the sorted array from pigeonholes
  sorted_array = []
  pigeonholes.each_with_index do |count, index|
    count.times do
      sorted_array << (index + min)
    end
  end
  
  sorted_array
end

# Example usage
puts "Original array: [8, 3, 2, 7, 4, 6, 1]"
result = pigeonhole_sort([8, 3, 2, 7, 4, 6, 1])
puts "Sorted array:   #{result}"

# Another example
puts "\nOriginal array: [5, 2, 8, 1, 9, 3]"
result2 = pigeonhole_sort([5, 2, 8, 1, 9, 3])
puts "Sorted array:   #{result2}"

# Example with duplicates
puts "\nOriginal array: [3, 1, 4, 1, 5, 9, 2, 6, 5]"
result3 = pigeonhole_sort([3, 1, 4, 1, 5, 9, 2, 6, 5])
puts "Sorted array:   #{result3}"
```

## Output:
```
Original array: [8, 3, 2, 7, 4, 6, 1]
Sorted array:   [1, 2, 3, 4, 6, 7, 8]

Original array: [5, 2, 8, 1, 9, 3]
Sorted array:   [1, 2, 3, 5, 8, 9]

Original array: [3, 1, 4, 1, 5, 9, 2, 6, 5]
Sorted array:   [1, 1, 2, 3, 4, 5, 5, 6, 9]
```

## How it works:

1. **Find range**: Determine the minimum and maximum values in the array
2. **Create pigeonholes**: Initialize an array with size equal to the range
3. **Distribute elements**: Place each element in its corresponding pigeonhole using `element - min` as index
4. **Collect elements**: Go through pigeonholes and reconstruct the sorted array

## Time Complexity:
- **Best case**: O(n + k) where n is the number of elements and k is the range
- **Average case**: O(n + k)
- **Worst case**: O(n + k)

## Space Complexity: O(k)

Pigeonhole sort is most efficient when the range of possible values (k) is not significantly larger than the number of elements (n).

