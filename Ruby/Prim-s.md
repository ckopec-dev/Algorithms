## Prim’s Minimum Spanning Tree (MST) in Ruby  

Below is a compact, easy‑to‑read implementation of Prim’s algorithm that works with an **undirected weighted graph** represented as an adjacency list (hash of hashes).  
The method `prim_mst(graph)` returns:

* `mst_edges` – an array of edges `[u, v, weight]` that belong to the MST  
* `total_weight` – the sum of the weights in the MST  

```ruby
# prim_mst.rb
def prim_mst(graph)
  # graph is a Hash: { node => { neighbor => weight, ... }, ... }
  # Assumes the graph is connected.
  start_node = graph.keys.first          # arbitrary start
  visited = Set.new([start_node])
  # Min‑heap of candidate edges: [weight, from, to]
  heap = []                              # we’ll use a simple array + sort for demo
  # Initialise heap with edges from the start node
  graph[start_node].each do |nbr, w|
    heap << [w, start_node, nbr]
  end
  heap.sort! { |a, b| a[0] <=> b[0] }    # smallest weight first

  mst_edges = []
  total_weight = 0

  until visited.size == graph.keys.size   # stop when all vertices are in the MST
    # pop the cheapest edge that leads to an unvisited vertex
    edge = heap.shift                     # remove first element
    until edge.nil? || !visited.include?(edge[2])
      # If the edge points to a visited vertex, discard it and look at next
      edge = heap.shift
    end
    break if edge.nil?                    # safety check (should not happen for connected graph)

    weight, u, v = edge
    visited << v
    mst_edges << [u, v, weight]
    total_weight += weight

    # Add all edges leaving the newly‑added vertex to the heap
    graph[v].each do |nbr, w|
      heap << [w, v, nbr] unless visited.include?(nbr)
    end
    heap.sort! { |a, b| a[0] <=> b[0] }   # keep heap ordered (O(E log V) overall)
  end

  [mst_edges, total_weight]
end
```

### How to use it

```ruby
# Example undirected weighted graph
graph = {
  A: { B: 2,  C: 3 },
  B: { A: 2,  C: 1,  D: 4 },
  C: { A: 3,  B: 1,  D: 5 },
  D: { B: 4,  C: 5 }
}

mst, weight = prim_mst(graph)

puts "Minimum Spanning Tree edges:"
mst.each { |u, v, w| puts "#{u} - #{v} : #{w}" }
puts "Total weight of MST: #{weight}"
```

**Output**

```
Minimum Spanning Tree edges:
A - B : 2
B - C : 1
B - D : 4
Total weight of MST: 7
```

### Explanation of the steps

1. **Initialization** – pick an arbitrary start vertex, mark it visited, and push all its incident edges onto a min‑heap (priority queue) keyed by weight.
2. **Iterative expansion** – repeatedly extract the cheapest edge that connects a visited vertex to an unvisited one, add that edge to the MST, mark the new vertex visited, and push its outgoing edges onto the heap.
3. **Termination** – when every vertex has been visited, the heap is empty (or we stop early) and we have built the MST.

The implementation uses a plain Ruby array as a heap and re‑sorts it after each insertion (`heap.sort!`). For production‑grade code you would replace this with a proper binary‑heap or a library such as `algorithms`/`heap` to achieve the optimal **O(E log V)** runtime. The core logic, however, remains identical to the classic Prim’s algorithm.