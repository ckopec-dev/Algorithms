# Cocktail Shaker Sort in Ruby

```ruby
def cocktail_shaker_sort(arr)
  return arr if arr.length <= 1
  
  left = 0
  right = arr.length - 1
  swapped = true
  
  while swapped
    swapped = false
    
    # Forward pass (left to right)
    (left...right).each do |i|
      if arr[i] > arr[i + 1]
        arr[i], arr[i + 1] = arr[i + 1], arr[i]
        swapped = true
      end
    end
    
    # Reduce the right boundary since the largest element is now at the end
    right -= 1
    
    # Backward pass (right to left)
    (right).downto(left + 1) do |i|
      if arr[i] < arr[i - 1]
        arr[i], arr[i - 1] = arr[i - 1], arr[i]
        swapped = true
      end
    end
    
    # Increase the left boundary since the smallest element is now at the beginning
    left += 1
  end
  
  arr
end

# Example usage
puts "Original array: [64, 34, 25, 12, 22, 11, 90]"
sorted_array = cocktail_shaker_sort([64, 34, 25, 12, 22, 11, 90])
puts "Sorted array:   #{sorted_array}"

# Another example
puts "\nOriginal array: [5, 2, 8, 1, 9]"
sorted_array2 = cocktail_shaker_sort([5, 2, 8, 1, 9])
puts "Sorted array:   #{sorted_array2}"
```

## Output:
```
Original array: [64, 34, 25, 12, 22, 11, 90]
Sorted array:   [11, 12, 22, 25, 34, 64, 90]

Original array: [5, 2, 8, 1, 9]
Sorted array:   [1, 2, 5, 8, 9]
```

## How it works:

1. **Bidirectional scanning**: Unlike regular bubble sort, cocktail shaker sort alternates between forward and backward passes
2. **Forward pass**: Moves the largest unsorted element to its correct position at the end
3. **Backward pass**: Moves the smallest unsorted element to its correct position at the beginning
4. **Boundary adjustment**: After each pass, the sorted boundaries (left and right) are adjusted
5. **Optimization**: The algorithm stops when no swaps occur in a complete forward-backward pass

The time complexity is O(n²) in the worst case and O(n) in the best case, while the space complexity is O(1).

