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
  
  # Search algorithm
  def search(target)
    current = @head
    
    # Start from the highest level and move down
    (0..@current_level).reverse_each do |level|
      # Move forward while the next node's value is less than target
      while current.forward[level] && current.forward[level].value < target
        current = current.forward[level]
      end
    end
    
    # Move one step forward to get the actual node
    current = current.forward[0]
    
    # Return true if we found the target
    return !current.nil? && current.value == target
  end
  
  # Insert method for completeness
  def insert(value)
    update = Array.new(@max_level + 1, @head)
    current = @head
    
    # Find the position to insert
    (0..@current_level).reverse_each do |level|
      while current.forward[level] && current.forward[level].value < value
        current = current.forward[level]
      end
      update[level] = current
    end
    
    # Create new node
    new_level = random_level
    new_node = SkipListNode.new(value, new_level)
    
    # Update levels
    if new_level > @current_level
      @current_level = new_level
    end
    
    # Insert the node
    (0..new_level).each do |level|
      new_node.forward[level] = update[level].forward[level]
      update[level].forward[level] = new_node
    end
  end
  
  # Display the skip list
  def display
    puts "Skip List:"
    (0..@current_level).reverse_each do |level|
      current = @head.forward[level]
      print "Level #{level}: "
      while current
        print "#{current.value} "
        current = current.forward[level]
      end
      puts
    end
  end
end

# Example usage
skip_list = SkipList.new(4)

# Insert some values
values = [3, 6, 7, 9, 12, 19, 21, 50, 65, 80]
values.each { |value| skip_list.insert(value) }

# Display the skip list
skip_list.display

# Search for values
puts "\nSearch Results:"
puts "Searching for 19: #{skip_list.search(19)}"
puts "Searching for 25: #{skip_list.search(25)}"
puts "Searching for 80: #{skip_list.search(80)}"
puts "Searching for 1: #{skip_list.search(1)}"
```

## Algorithm Explanation

The `search` method in the skip list works as follows:

1. **Start at the highest level** - Begin from the topmost level of the skip list
2. **Move forward** - Traverse forward while the next node's value is less than the target
3. **Drop down** - When you can't move forward anymore, drop down to the next lower level
4. **Continue until bottom level** - Repeat until reaching level 0
5. **Check result** - Verify if the found node contains the target value

## Time Complexity
- **Average case**: O(log n)
- **Worst case**: O(n)
- **Space complexity**: O(n log n)

## Output Example
```
Skip List:
Level 3: 3 6 7 9 12 19 21 50 65 80 
Level 2: 3 6 7 9 12 19 21 50 65 80 
Level 1: 3 6 7 9 12 19 21 50 65 80 
Level 0: 3 6 7 9 12 19 21 50 65 80 

Search Results:
Searching for 19: true
Searching for 25: false
Searching for 80: true
Searching for 1: false
```

