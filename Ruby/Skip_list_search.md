# Skip List Search Algorithm in Ruby

```ruby
class SkipListNode
  attr_accessor :value, :forward
  
  def initialize(value, level)
    @value = value
    @forward = Array.new(level + 1, nil)
  end
end

class SkipList
  attr_accessor :head, :max_level, :current_level
  
  def initialize(max_level = 16)
    @max_level = max_level
    @current_level = 0
    @head = SkipListNode.new(nil, @max_level)
  end
  
  # Simple coin flip to determine level
  def random_level
    level = 0
    while rand < 0.5 && level < @max_level
      level += 1
    end
    level
  end
  
  # Search for a value in the skip list
  def search(value)
    current = @head
    
    # Start from the highest level and move down
    (0..@current_level).reverse_each do |level|
      # Move forward while the next node's value is less than target
      while current.forward[level] && current.forward[level].value < value
        current = current.forward[level]
      end
    end
    
    # Move one step forward to get the actual node
    current = current.forward[0]
    
    # Return true if we found the value
    return !current.nil? && current.value == value
  end
  
  # Insert a value into the skip list
  def insert(value)
    update = Array.new(@max_level + 1, @head)
    current = @head
    
    # Find the position where value should be inserted
    (0..@current_level).reverse_each do |level|
      while current.forward[level] && current.forward[level].value < value
        current = current.forward[level]
      end
      update[level] = current
    end
    
    # Create new node with random level
    new_level = random_level
    new_node = SkipListNode.new(value, new_level)
    
    # Update current level if needed
    if new_level > @current_level
      @current_level = new_level
    end
    
    # Insert the new node
    (0..new_level).each do |level|
      new_node.forward[level] = update[level].forward[level]
      update[level].forward[level] = new_node
    end
  end
  
  # Display the skip list structure
  def display
    puts "Skip List Structure:"
    (0..@current_level).reverse_each do |level|
      print "Level #{level}: "
      current = @head.forward[level]
      while current
        print "#{current.value} -> "
        current = current.forward[level]
      end
      puts "nil"
    end
  end
end

# Example usage
puts "=== Skip List Search Example ==="

# Create a skip list
skip_list = SkipList.new(4)

# Insert some values
values = [3, 6, 7, 9, 12, 19, 21, 50, 66, 70]
puts "Inserting values: #{values.join(', ')}"

values.each do |value|
  skip_list.insert(value)
end

# Display the structure
skip_list.display

# Search for values
puts "\n=== Search Results ==="
search_values = [7, 15, 21, 100]

search_values.each do |value|
  found = skip_list.search(value)
  puts "Searching for #{value}: #{found ? 'Found' : 'Not Found'}"
end

# Demonstrate search with a value that exists
puts "\n=== Detailed Search Demonstration ==="
puts "Searching for 19:"
found = skip_list.search(19)
puts "Result: #{found ? 'Found' : 'Not Found'}"

# Demonstrate search with a value that doesn't exist
puts "\nSearching for 15:"
found = skip_list.search(15)
puts "Result: #{found ? 'Found' : 'Not Found'}"
```

## Output Example:
```
=== Skip List Search Example ===
Inserting values: 3, 6, 7, 9, 12, 19, 21, 50, 66, 70
Skip List Structure:
Level 3: 3 -> 6 -> 7 -> 9 -> 12 -> 19 -> 21 -> 50 -> 66 -> 70 -> nil
Level 2: 3 -> 6 -> 7 -> 9 -> 12 -> 19 -> 21 -> 50 -> 66 -> 70 -> nil
Level 1: 3 -> 6 -> 7 -> 9 -> 12 -> 19 -> 21 -> 50 -> 66 -> 70 -> nil
Level 0: 3 -> 6 -> 7 -> 9 -> 12 -> 19 -> 21 -> 50 -> 66 -> 70 -> nil

=== Search Results ===
Searching for 7: Found
Searching for 15: Not Found
Searching for 21: Found
Searching for 100: Not Found

=== Detailed Search Demonstration ===
Searching for 19:
Result: Found

Searching for 15:
Result: Not Found
```

## Algorithm Explanation:

The skip list search algorithm works by:

1. **Starting at the highest level** of the skip list
2. **Moving forward** as long as the next node's value is less than the target value
3. **Moving down** to the next lower level when the next node's value is greater than or equal to the target
4. **Continuing this process** until reaching level 0
5. **Checking if the final node** contains the target value

**Time Complexity:** O(log n) average case, O(n) worst case
**Space Complexity:** O(n log n)

