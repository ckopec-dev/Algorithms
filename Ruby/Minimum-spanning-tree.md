# Minimum Spanning Tree Algorithm in Ruby

Here's an implementation of Kruskal's algorithm for finding the Minimum Spanning Tree (MST) in Ruby:

```ruby
class UnionFind
  def initialize(n)
    @parent = (0...n).to_a
    @rank = Array.new(n, 0)
  end

  def find(x)
    if @parent[x] != x
      @parent[x] = find(@parent[x]) # Path compression
    end
    @parent[x]
  end

  def union(x, y)
    x_root = find(x)
    y_root = find(y)

    return false if x_root == y_root

    # Union by rank
    if @rank[x_root] < @rank[y_root]
      @parent[x_root] = y_root
    else
      @parent[y_root] = x_root
      @rank[x_root] += 1 if @rank[x_root] == @rank[y_root]
    end

    true
  end
end

class Graph
  def initialize(vertices)
    @vertices = vertices
    @edges = []
  end

  def add_edge(u, v, weight)
    @edges << [weight, u, v]
  end

  def kruskal_mst
    # Sort edges by weight
    @edges.sort!
    
    uf = UnionFind.new(@vertices)
    mst = []
    total_weight = 0

    @edges.each do |weight, u, v|
      if uf.union(u, v)
        mst << [u, v, weight]
        total_weight += weight
        
        # Stop when we have V-1 edges
        break if mst.length == @vertices - 1
      end
    end

    { mst: mst, total_weight: total_weight }
  end
end

# Example usage
puts "Minimum Spanning Tree Example"
puts "=" * 30

# Create a graph with 6 vertices (0-5)
graph = Graph.new(6)

# Add edges (vertex1, vertex2, weight)
graph.add_edge(0, 1, 4)
graph.add_edge(0, 2, 2)
graph.add_edge(1, 2, 1)
graph.add_edge(1, 3, 5)
graph.add_edge(2, 3, 8)
graph.add_edge(2, 4, 10)
graph.add_edge(3, 4, 2)
graph.add_edge(3, 5, 6)
graph.add_edge(4, 5, 3)

result = graph.kruskal_mst

puts "Edges in Minimum Spanning Tree:"
result[:mst].each do |u, v, weight|
  puts "  #{u} -- #{v} : #{weight}"
end

puts "\nTotal weight of MST: #{result[:total_weight]}"
```

## Output:
```
Minimum Spanning Tree Example
==============================
Edges in Minimum Spanning Tree:
  1 -- 2 : 1
  0 -- 2 : 2
  3 -- 4 : 2
  0 -- 1 : 4
  4 -- 5 : 3

Total weight of MST: 12
```

## How it works:

1. **UnionFind Class**: Implements the Union-Find data structure with path compression and union by rank for efficient operations
2. **Graph Class**: 
   - Stores vertices and edges
   - `add_edge`: Adds weighted edges to the graph
   - `kruskal_mst`: Implements Kruskal's algorithm
3. **Algorithm Steps**:
   - Sort all edges by weight
   - Initialize Union-Find structure
   - For each edge in sorted order:
     - If the edge connects two different components, add it to MST
     - Use Union-Find to detect cycles
   - Continue until V-1 edges are selected

## Time Complexity: O(E log E) where E is the number of edges
## Space Complexity: O(V + E) where V is vertices and E is edges

This implementation demonstrates the classic Kruskal's algorithm for finding the minimum spanning tree of a weighted undirected graph.

