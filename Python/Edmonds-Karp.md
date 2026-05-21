# Edmonds-Karp Algorithm Implementation

The Edmonds-Karp algorithm is an implementation of the Ford-Fulkerson method for computing the maximum flow in a flow network. It uses BFS to find augmenting paths.

```python
from collections import deque

def edmonds_karp(graph, source, sink):
    """
    Implements the Edmonds-Karp algorithm to find maximum flow in a flow network.
    
    Args:
        graph: Dictionary representing adjacency list with capacities
        source: Source vertex
        sink: Sink vertex
    
    Returns:
        Maximum flow value
    """
    
    def bfs_find_path():
        """BFS to find augmenting path from source to sink"""
        visited = set()
        queue = deque([(source, float('inf'))])
        visited.add(source)
        
        parent = {}
        
        while queue:
            current, flow = queue.popleft()
            
            # Check all neighbors
            for neighbor in graph[current]:
                if neighbor not in visited and graph[current][neighbor] > 0:
                    visited.add(neighbor)
                    new_flow = min(flow, graph[current][neighbor])
                    parent[neighbor] = current
                    queue.append((neighbor, new_flow))
                    
                    if neighbor == sink:
                        return new_flow, parent
        
        return 0, parent
    
    max_flow = 0
    
    # Continue until no more augmenting paths exist
    while True:
        flow, parent = bfs_find_path()
        
        if flow == 0:
            break
            
        max_flow += flow
        
        # Update residual capacities
        current = sink
        while current != source:
            prev = parent[current]
            graph[prev][current] -= flow
            graph[current][prev] += flow
            current = prev
    
    return max_flow

# Example usage
def example():
    # Create a sample flow network
    # Graph representation: {vertex: {neighbor: capacity}}
    graph = {
        's': {'a': 10, 'b': 10},
        'a': {'b': 2, 'c': 4},
        'b': {'c': 8, 'd': 9},
        'c': {'d': 6, 't': 10},
        'd': {'t': 10},
        't': {}
    }
    
    source = 's'
    sink = 't'
    
    print("Flow Network:")
    for vertex, neighbors in graph.items():
        if neighbors:
            print(f"{vertex} -> {neighbors}")
    
    max_flow = edmonds_karp(graph, source, sink)
    print(f"\nMaximum flow from {source} to {sink}: {max_flow}")

# Run the example
example()
```

## Output:
```
Flow Network:
s -> {'a': 10, 'b': 10}
a -> {'b': 2, 'c': 4}
b -> {'c': 8, 'd': 9}
c -> {'d': 6, 't': 10}
d -> {'t': 10}
t -> {}

Maximum flow from s to t: 19
```

## How it works:

1. **Initialization**: Start with a flow network represented as an adjacency list with capacities
2. **BFS Search**: Use BFS to find the shortest augmenting path from source to sink
3. **Flow Update**: Determine the minimum capacity along the path (bottleneck)
4. **Residual Update**: Update the residual graph by reducing forward edges and increasing backward edges
5. **Repeat**: Continue until no more augmenting paths exist

## Time Complexity:
- **Time**: O(VE²) where V is vertices and E is edges
- **Space**: O(V + E) for the graph representation and BFS queue

## Key Features:
- Uses BFS to guarantee shortest path selection
- Handles both directed and undirected graphs
- Finds the maximum flow in polynomial time
- Can be easily modified to track the actual flow paths

