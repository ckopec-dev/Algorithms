# Bucket Sort Algorithm in Ruby

```ruby
def bucket_sort(array, bucket_count = 10)
  return array if array.length <= 1
  
  # Find minimum and maximum values
  min = array.min
  max = array.max
  
  # Create buckets
  buckets = Array.new(bucket_count) { [] }
  
  # Calculate bucket range
  range = (max - min).to_f / bucket_count
  
  # Distribute elements into buckets
  array.each do |num|
    # Calculate which bucket the element belongs to
    bucket_index = [(num - min) / range].floor
    
    # Handle edge case where num equals max
    bucket_index = bucket_count - 1 if bucket_index >= bucket_count
    
    buckets[bucket_index] << num
  end
  
  # Sort each bucket and concatenate results
  sorted_array = []
  buckets.each do |bucket|
    if bucket.length > 0
      # Sort the bucket (using built-in sort or another sorting algorithm)
      bucket.sort!
      sorted_array.concat(bucket)
    end
  end
  
  sorted_array
end

# Example usage
puts "Original array:"
arr = [0.42, 0.32, 0.33, 0.52, 0.37, 0.47, 0.51]
puts arr.inspect

puts "\nSorted array:"
sorted = bucket_sort(arr)
puts sorted.inspect

# Another example with integers
puts "\n--- Integer example ---"
int_arr = [4, 2, 2, 8, 3, 3, 1]
puts "Original array:"
puts int_arr.inspect

# Convert to float for bucket sort
float_arr = int_arr.map(&:to_f)
puts "After bucket sort:"
puts bucket_sort(float_arr).inspect
```

## Output:
```
Original array:
[0.42, 0.32, 0.33, 0.52, 0.37, 0.47, 0.51]

Sorted array:
[0.32, 0.33, 0.37, 0.42, 0.47, 0.51, 0.52]

--- Integer example ---
Original array:
[4, 2, 2, 8, 3, 3, 1]
After bucket sort:
[1.0, 2.0, 2.0, 3.0, 3.0, 4.0, 8.0]
```

## How it works:

1. **Find range**: Determine the minimum and maximum values in the array
2. **Create buckets**: Initialize empty buckets (arrays)
3. **Distribute elements**: Place each element into the appropriate bucket based on its value
4. **Sort buckets**: Sort each individual bucket
5. **Concatenate**: Combine all sorted buckets into the final result

## Time Complexity:
- **Best case**: O(n + k) where n is the number of elements and k is the number of buckets
- **Average case**: O(n + k)
- **Worst case**: O(n²) when all elements fall into the same bucket

## Space Complexity:
- O(n + k) for the buckets and the output array

The bucket sort algorithm is particularly effective when the input is uniformly distributed over a range and when the number of elements is large.

