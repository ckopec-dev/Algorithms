```python
def bellman_ford(graph, start):
    """
    Bellman-Ford algorithm implementation
    
    Args:
        graph: Dictionary where keys are nodes and values are lists of (neighbor, weight) tuples
        start: Starting node
    
    Returns:
        Dictionary with shortest distances from start to all other nodes
    """
    
    # Initialize distances
    distances = {node: float('inf') for node in graph}
    distances[start] = 0
    
    # Relax edges repeatedly
    vertices = len(graph)
    for i in range(vertices - 1):
        for node in graph:
            if distances[node] != float('inf'):
                for neighbor, weight in graph[node]:
                    if distances[node] + weight < distances[neighbor]:
                        distances[neighbor] = distances[node] + weight
    
    # Check for negative weight cycles
    for node in graph:
        if distances[node] != float('inf'):
            for neighbor, weight in graph[node]:
                if distances[node] + weight < distances[neighbor]:
                    raise ValueError("Graph contains negative weight cycle")
    
    return distances

# Example usage
if __name__ == "__main__":
    # Create a sample graph
    # Graph representation: {node: [(neighbor, weight), ...]}
    graph = {
        'A': [('B', 4), ('C', 2)],
        'B': [('C', 1), ('D', 5)],
        'C': [('D', 8), ('E', 10)],
        'D': [('E', 2)],
        'E': []
    }
    
    print("Graph edges:")
    for node in graph:
        for neighbor, weight in graph[node]:
            print(f"  {node} -> {neighbor} (weight: {weight})")
    
    # Run Bellman-Ford algorithm
    start_node = 'A'
    try:
        result = bellman_ford(graph, start_node)
        print(f"\nShortest distances from {start_node}:")
        for node, distance in result.items():
            print(f"  {start_node} -> {node}: {distance}")
    except ValueError as e:
        print(f"Error: {e}")
```

This implementation demonstrates:

1. **Initialization**: Setting all distances to infinity except the start node
2. **Relaxation**: Repeating the edge relaxation process (V-1) times
3. **Negative cycle detection**: Checking for negative weight cycles after relaxation
4. **Output**: Returns shortest distances from start to all other nodes

The algorithm handles:
- Positive and negative edge weights
- Detection of negative weight cycles
- Graph representation using adjacency lists

Time complexity: O(VE) where V is vertices and E is edges
Space complexity: O(V) for storing distances

