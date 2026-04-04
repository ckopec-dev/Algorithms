# Binary Search Algorithm in Ruby

```ruby
def binary_search(arr, target)
  left = 0
  right = arr.length - 1
  
  while left <= right
    mid = (left + right) / 2
    
    if arr[mid] == target
      return mid  # Return the index if target is found
    elsif arr[mid] < target
      left = mid + 1  # Search in the right half
    else
      right = mid - 1  # Search in the left half
    end
  end
  
  return -1  # Return -1 if target is not found
end

# Example usage
sorted_array = [1, 3, 5, 7, 9, 11, 13, 15, 17, 19]
target = 7

result = binary_search(sorted_array, target)

if result != -1
  puts "Element #{target} found at index #{result}"
else
  puts "Element #{target} not found in the array"
end

# Output: Element 7 found at index 3
```

## How it works:

1. **Initialize pointers**: Set `left` to 0 and `right` to the last index
2. **Loop while valid**: Continue while `left <= right`
3. **Calculate middle**: Find the middle index
4. **Compare and search**:
   - If middle element equals target: return the index
   - If middle element is less than target: search right half
   - If middle element is greater than target: search left half
5. **Return -1**: If loop ends without finding target

## Time Complexity: O(log n)
## Space Complexity: O(1)

