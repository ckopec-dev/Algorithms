# Comb Sort Algorithm in Ruby

Comb sort is an improved version of bubble sort that uses a gap sequence to eliminate turtles (small values near the end of the list).

```ruby
def comb_sort(array)
  n = array.length
  gap = n
  shrink_factor = 1.3
  sorted = false
  
  while !sorted
    # Calculate new gap
    gap = (gap / shrink_factor).to_i
    
    # If gap becomes 1, we're doing a final bubble sort
    if gap <= 1
      gap = 1
      sorted = true
    end
    
    # Compare elements with current gap
    i = 0
    while i + gap < n
      if array[i] > array[i + gap]
        # Swap elements
        array[i], array[i + gap] = array[i + gap], array[i]
        sorted = false
      end
      i += 1
    end
  end
  
  array
end

# Example usage
puts "Original array: [64, 34, 25, 12, 22, 11, 90]"
sorted_array = comb_sort([64, 34, 25, 12, 22, 11, 90])
puts "Sorted array:   #{sorted_array}"

# Another example
puts "\nOriginal array: [5, 2, 8, 1, 9]"
sorted_array2 = comb_sort([5, 2, 8, 1, 9])
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
1. **Initialize**: Start with a gap equal to the array length
2. **Shrink gap**: Divide the gap by the shrink factor (1.3) in each iteration
3. **Compare**: Compare elements separated by the current gap
4. **Swap**: If elements are out of order, swap them
5. **Repeat**: Continue until gap becomes 1 and no more swaps are needed
6. **Finish**: When gap is 1 and no swaps occur, the array is sorted

## Time Complexity:
- **Best case**: O(n log n)
- **Average case**: O(n²/2^p) where p is the number of increments
- **Worst case**: O(n²)

## Space Complexity:
- O(1) - sorts in place

The comb sort algorithm is more efficient than bubble sort because it eliminates turtles more quickly by using larger gaps initially.

