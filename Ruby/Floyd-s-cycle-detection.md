# Floyd's Cycle Detection Algorithm in Ruby

Floyd's Cycle Detection Algorithm (also known as the "Tortoise and Hare" algorithm) is used to detect cycles in linked lists. Here's an implementation with an example:

```ruby
# Define a Node class for the linked list
class Node
  attr_accessor :value, :next
  
  def initialize(value)
    @value = value
    @next = nil
  end
end

# Floyd's Cycle Detection Algorithm
def has_cycle?(head)
  # Handle edge cases
  return false if head.nil? || head.next.nil?
  
  # Initialize two pointers
  slow = head
  fast = head
  
  # Move pointers through the list
  while fast && fast.next
    slow = slow.next          # Move one step
    fast = fast.next.next     # Move two steps
    
    # If they meet, there's a cycle
    return true if slow == fast
  end
  
  # If we reach here, no cycle exists
  false
end

# Helper method to create a cycle for testing
def create_cycle(head, cycle_start_index)
  # Find the last node
  current = head
  while current.next
    current = current.next
  end
  
  # Find the node at cycle_start_index
  cycle_start = head
  (0...cycle_start_index).each { |i| cycle_start = cycle_start.next }
  
  # Create cycle by connecting last node to cycle_start
  current.next = cycle_start
end

# Example usage
puts "=== Floyd's Cycle Detection Algorithm ==="

# Create a linked list: 1 -> 2 -> 3 -> 4 -> 5
head = Node.new(1)
head.next = Node.new(2)
head.next.next = Node.new(3)
head.next.next.next = Node.new(4)
head.next.next.next.next = Node.new(5)

puts "List without cycle:"
puts "1 -> 2 -> 3 -> 4 -> 5 -> nil"
puts "Has cycle: #{has_cycle?(head)}"  # Should return false

# Create a cycle: 1 -> 2 -> 3 -> 4 -> 5 -> 2 (cycle back to node 2)
create_cycle(head, 1)  # Create cycle starting at index 1 (node with value 2)

puts "\nList with cycle (1 -> 2 -> 3 -> 4 -> 5 -> 2):"
puts "Has cycle: #{has_cycle?(head)}"  # Should return true

# Another example with a simple cycle
puts "\n=== Another Example ==="
node_a = Node.new('A')
node_b = Node.new('B')
node_c = Node.new('C')
node_d = Node.new('D')

node_a.next = node_b
node_b.next = node_c
node_c.next = node_d
node_d.next = node_b  # Creates cycle: A -> B -> C -> D -> B

puts "Has cycle: #{has_cycle?(node_a)}"  # Should return true

# Example with no cycle
node_x = Node.new('X')
node_y = Node.new('Y')
node_z = Node.new('Z')

node_x.next = node_y
node_y.next = node_z

puts "Has cycle: #{has_cycle?(node_x)}"  # Should return false
```

## How it works:

1. **Two Pointers**: Uses two pointers moving at different speeds
2. **Slow Pointer**: Moves one step at a time
3. **Fast Pointer**: Moves two steps at a time
4. **Cycle Detection**: If there's a cycle, the fast pointer will eventually "lap" the slow pointer
5. **Time Complexity**: O(n)
6. **Space Complexity**: O(1)

## Output:
```
=== Floyd's Cycle Detection Algorithm ===
List without cycle:
1 -> 2 -> 3 -> 4 -> 5 -> nil
Has cycle: false

List with cycle (1 -> 2 -> 3 -> 4 -> 5 -> 2):
Has cycle: true

=== Another Example ===
Has cycle: true
Has cycle: false
```

This algorithm is particularly useful for detecting cycles in linked lists and is commonly used in various applications including detecting loops in data structures and solving problems in computer science competitions.

