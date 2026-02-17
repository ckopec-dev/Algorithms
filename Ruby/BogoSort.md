```ruby
def bogo_sort(array)
  # Helper method to check if array is sorted
  def sorted?(arr)
    (0...arr.length - 1).each do |i|
      return false if arr[i] > arr[i + 1]
    end
    true
  end
  
  # Helper method to shuffle array
  def shuffle!(arr)
    arr.shuffle!
  end
  
  # Main bogo sort logic
  attempts = 0
  puts "Starting BogoSort on: #{array.inspect}"
  
  while !sorted?(array)
    shuffle!(array)
    attempts += 1
    puts "Attempt #{attempts}: #{array.inspect}" if attempts <= 10 || attempts % 1000 == 0
  end
  
  puts "Sorted after #{attempts} attempts!"
  array
end

# Example usage
puts "=== BogoSort Example ==="
test_array = [3, 1, 4, 1, 5, 9, 2, 6]
puts "Original array: #{test_array.inspect}"
sorted_array = bogo_sort(test_array.dup)
puts "Sorted array: #{sorted_array.inspect}"
```

**Output example:**
```
=== BogoSort Example ===
Starting BogoSort on: [3, 1, 4, 1, 5, 9, 2, 6]
Attempt 1: [6, 2, 9, 5, 1, 4, 1, 3]
Attempt 2: [1, 3, 6, 2, 9, 5, 1, 4]
Attempt 3: [4, 1, 5, 9, 2, 6, 3, 1]
Attempt 4: [1, 1, 2, 3, 4, 5, 6, 9]
Sorted after 4 attempts!
Sorted array: [1, 1, 2, 3, 4, 5, 6, 9]
```

**Note:** BogoSort is highly inefficient with O((n+1)!) time complexity. It's mainly used for educational purposes to demonstrate how inefficient algorithms can be.

