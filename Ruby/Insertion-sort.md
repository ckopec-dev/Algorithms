# Insertion Sort in Ruby

Here's an implementation of the insertion sort algorithm in Ruby:

```ruby
def insertion_sort(array)
  # Iterate through the array starting from the second element
  (1...array.length).each do |i|
    key = array[i]  # Current element to be inserted
    j = i - 1       # Index of the last element in sorted portion
    
    # Move elements greater than key one position ahead
    while j >= 0 && array[j] > key
      array[j + 1] = array[j]
      j -= 1
    end
    
    # Insert the key at its correct position
    array[j + 1] = key
  end
  
  array
end

# Example usage
numbers = [64, 34, 25, 12, 22, 11, 90]
puts "Original array: #{numbers}"

sorted_numbers = insertion_sort(numbers)
puts "Sorted array:   #{sorted_numbers}"

# Another example with different data
words = ["banana", "apple", "cherry", "date"]
puts "Original array: #{words}"

sorted_words = insertion_sort(words)
puts "Sorted array:   #{sorted_words}"
```

## Output:
```
Original array: [64, 34, 25, 12, 22, 11, 90]
Sorted array:   [11, 12, 22, 25, 34, 64, 90]
Original array: ["banana", "apple", "cherry", "date"]
Sorted array:   ["apple", "banana", "cherry", "date"]
```

## How it works:
1. Start from the second element (index 1) since a single element is already "sorted"
2. Take the current element as the `key`
3. Compare the `key` with elements in the sorted portion (to its left)
4. Shift larger elements one position to the right
5. Insert the `key` at its correct position
6. Repeat until the entire array is sorted

## Time Complexity:
- **Best case**: O(n) - when array is already sorted
- **Average case**: O(n²)
- **Worst case**: O(n²) - when array is reverse sorted

## Space Complexity: O(1) - sorts in place

