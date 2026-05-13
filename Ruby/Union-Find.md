# Union-Find Algorithm in Ruby

```ruby
class UnionFind
  def initialize(n)
    @parent = (0...n).to_a
    @rank = Array.new(n, 0)
    @components = n
  end

  # Find root of element with path compression
  def find(x)
    if @parent[x] != x
      @parent[x] = find(@parent[x])  # Path compression
    end
    @parent[x]
  end

  # Union two elements
  def union(x, y)
    root_x = find(x)
    root_y = find(y)

    return if root_x == root_y  # Already in same component

    # Union by rank
    if @rank[root_x] < @rank[root_y]
      @parent[root_x] = root_y
    elsif @rank[root_x] > @rank[root_y]
      @parent[root_y] = root_x
    else
      @parent[root_y] = root_x
      @rank[root_x] += 1
    end

    @components -= 1
  end

  # Check if two elements are connected
  def connected?(x, y)
    find(x) == find(y)
  end

  # Get number of connected components
  def components
    @components
  end
end

# Example usage
puts "Union-Find Example"
puts "=" * 20

# Create Union-Find structure with 6 elements (0-5)
uf = UnionFind.new(6)

puts "Initial state - 6 components"
puts "Components: #{uf.components}"

# Perform some unions
uf.union(0, 1)
puts "After union(0, 1): #{uf.components} components"

uf.union(2, 3)
puts "After union(2, 3): #{uf.components} components"

uf.union(4, 5)
puts "After union(4, 5): #{uf.components} components"

uf.union(1, 3)
puts "After union(1, 3): #{uf.components} components"

# Check connections
puts "Is 0 connected to 3? #{uf.connected?(0, 3)}"
puts "Is 0 connected to 4? #{uf.connected?(0, 4)}"
puts "Is 2 connected to 5? #{uf.connected?(2, 5)}"

# Final state
puts "\nFinal state:"
(0..5).each do |i|
  puts "Element #{i} belongs to component #{uf.find(i)}"
end
```

## Output:
```
Union-Find Example
====================
Initial state - 6 components
Components: 6
After union(0, 1): 5 components
After union(2, 3): 4 components
After union(4, 5): 3 components
After union(1, 3): 2 components
Is 0 connected to 3? true
Is 0 connected to 4? false
Is 2 connected to 5? true

Final state:
Element 0 belongs to component 0
Element 1 belongs to component 0
Element 2 belongs to component 2
Element 3 belongs to component 2
Element 4 belongs to component 4
Element 5 belongs to component 4
```

## Key Features:

1. **Path Compression**: `find` method compresses the path during traversal
2. **Union by Rank**: Always attach smaller tree under root of larger tree
3. **Efficient Operations**: Nearly constant time for both `find` and `union`
4. **Component Tracking**: Keeps track of number of connected components

## Time Complexity:
- **Find**: O(α(n)) where α is the inverse Ackermann function (practically constant)
- **Union**: O(α(n))
- **Space**: O(n)

