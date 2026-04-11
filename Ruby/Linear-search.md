# Linear Search Algorithm in Ruby

## Implementation

```ruby
def linear_search(array, target)
  # Iterate through each element in the array
  array.each_with_index do |element, index|
    # If we find the target element, return its index
    return index if element == target
  end
  
  # If target is not found, return -1
  -1
end

# Alternative implementation using a traditional loop
def linear_search_v2(array, target)
  i = 0
  while i < array.length
    return i if array[i] == target
    i += 1
  end
  -1
end
```

## Example Usage

```ruby
# Test array
numbers = [10, 25, 3, 47, 15, 8, 92, 33]

# Search for existing element
result = linear_search(numbers, 47)
puts "Element 47 found at index: #{result}"  # Output: Element 47 found at index: 3

# Search for non-existing element
result = linear_search(numbers, 100)
puts "Element 100 found at index: #{result}"  # Output: Element 100 found at index: -1

# Search for first element
result = linear_search(numbers, 10)
puts "Element 10 found at index: #{result}"  # Output: Element 10 found at index: 0
```

## How it works

1. **Start from the beginning** of the array
2. **Compare each element** with the target value
3. **Return the index** if a match is found
4. **Continue until the end** of the array
5. **Return -1** if the target is not found

## Time Complexity
- **Best Case**: O(1) - Target is the first element
- **Average Case**: O(n/2) - Target is somewhere in the middle
- **Worst Case**: O(n) - Target is the last element or not found

## Space Complexity
- O(1) - Only uses a constant amount of extra space

## Output Example
```
Element 47 found at index: 3
Element 100 found at index: -1
Element 10 found at index: 0
```

