# Fibonacci Heap Implementation in Ruby

```ruby
class FibonacciHeapNode
  attr_accessor :key, :degree, :parent, :child, :left, :right, :mark, :deleted
  
  def initialize(key)
    @key = key
    @degree = 0
    @parent = nil
    @child = nil
    @left = self
    @right = self
    @mark = false
    @deleted = false
  end
end

class FibonacciHeap
  attr_accessor :min, :total_nodes
  
  def initialize
    @min = nil
    @total_nodes = 0
  end
  
  # Insert a new node with given key
  def insert(key)
    node = FibonacciHeapNode.new(key)
    
    if @min.nil?
      @min = node
    else
      # Add node to root list
      concatenate_nodes(@min, node)
      @min = node if key < @min.key
    end
    
    @total_nodes += 1
    node
  end
  
  # Extract minimum node
  def extract_min
    z = @min
    
    if !z.nil?
      # Add all children to root list
      if !z.child.nil?
        child = z.child
        current = child
        
        begin
          current.parent = nil
          concatenate_nodes(@min, current)
          current = current.right
        end while current != child
      end
      
      # Remove z from root list
      if z == z.right
        @min = nil
      else
        @min = z.right
        remove_node(z)
        consolidate
      end
      
      @total_nodes -= 1
      z.key
    end
  end
  
  # Decrease key of a node
  def decrease_key(node, new_key)
    if new_key > node.key
      raise "New key is greater than current key"
    end
    
    node.key = new_key
    parent = node.parent
    
    if !parent.nil? && node.key < parent.key
      cut(node, parent)
      cascading_cut(parent)
    end
    
    @min = node if node.key < @min.key
  end
  
  # Delete a node
  def delete(node)
    decrease_key(node, Float::INFINITY)
    extract_min
  end
  
  # Print heap structure (for debugging)
  def print_heap
    return "Empty heap" if @min.nil?
    
    result = []
    current = @min
    begin
      result << current.key
      current = current.right
    end while current != @min
    
    result.join(" -> ")
  end
  
  private
  
  # Consolidate root list
  def consolidate
    # Create array to hold roots of different degrees
    degree_array = Array.new(@total_nodes + 1) { nil }
    
    # Get all root nodes
    roots = []
    current = @min
    begin
      roots << current
      current = current.right
    end while current != @min
    
    # Process each root
    roots.each do |w|
      x = w
      d = x.degree
      
      while !degree_array[d].nil?
        y = degree_array[d]
        
        # Make sure x has smaller key
        if x.key > y.key
          x, y = y, x
        end
        
        # Make y a child of x
        link(y, x)
        
        # Remove y from root list
        degree_array[d] = nil
        d += 1
      end
      
      degree_array[d] = x
    end
    
    # Find new minimum
    @min = nil
    (0...degree_array.length).each do |i|
      next if degree_array[i].nil?
      
      if @min.nil?
        @min = degree_array[i]
      elsif degree_array[i].key < @min.key
        @min = degree_array[i]
      end
    end
  end
  
  # Link two nodes
  def link(y, x)
    # Remove y from root list
    remove_node(y)
    
    # Make y a child of x
    y.parent = x
    
    if x.child.nil?
      x.child = y
      y.left = y
      y.right = y
    else
      concatenate_nodes(x.child, y)
    end
    
    x.degree += 1
    y.mark = false
  end
  
  # Cut node from parent
  def cut(node, parent)
    remove_node(node)
    parent.degree -= 1
    node.parent = nil
    node.mark = false
    concatenate_nodes(@min, node)
  end
  
  # Cascading cut
  def cascading_cut(node)
    parent = node.parent
    
    if !parent.nil?
      if !node.mark
        node.mark = true
      else
        cut(node, parent)
        cascading_cut(parent)
      end
    end
  end
  
  # Concatenate two nodes in circular doubly linked list
  def concatenate_nodes(node1, node2)
    node1_right = node1.right
    node2_left = node2.left
    
    node1.right = node2
    node2.left = node1
    node2.right = node1_right
    node1_right.left = node2
  end
  
  # Remove a node from circular doubly linked list
  def remove_node(node)
    node.left.right = node.right
    node.right.left = node.left
  end
end

# Example usage
puts "=== Fibonacci Heap Example ==="

# Create a new Fibonacci heap
heap = FibonacciHeap.new

# Insert some values
puts "Inserting values: 10, 20, 5, 15, 3"
heap.insert(10)
heap.insert(20)
heap.insert(5)
heap.insert(15)
heap.insert(3)

puts "Heap structure: #{heap.print_heap}"
puts "Minimum value: #{heap.extract_min}"

puts "After extracting minimum:"
puts "Heap structure: #{heap.print_heap}"
puts "Minimum value: #{heap.extract_min}"

# Decrease key example
puts "\nDecreasing key of node with value 20 to 1"
node = heap.insert(20)
heap.decrease_key(node, 1)
puts "New minimum: #{heap.extract_min}"

puts "\nFinal heap structure: #{heap.print_heap}"
```

## Key Features of this Implementation:

1. **Node Structure**: Each node contains key, degree, parent, child, and circular links
2. **Core Operations**:
   - `insert(key)`: Insert a new node
   - `extract_min()`: Remove and return minimum node
   - `decrease_key(node, new_key)`: Decrease a node's key
   - `delete(node)`: Delete a specific node

3. **Advanced Features**:
   - Consolidation with degree-based linking
   - Cascading cut for maintaining heap property
   - Lazy deletion and marking for efficiency

## Time Complexities:
- Insert: O(1) amortized
- Extract Min: O(log n) amortized
- Decrease Key: O(1) amortized
- Delete: O(log n) amortized

This implementation demonstrates the Fibonacci heap's efficient operations, particularly useful in algorithms like Dijkstra's shortest path and Prim's minimum spanning tree.

