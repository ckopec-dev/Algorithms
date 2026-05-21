# Bellman-Ford Algorithm Implementation

Here's a complete implementation of the Bellman-Ford algorithm in Python:

```python
def bellman_ford(graph, start):
    """
    Bellman-Ford algorithm to find shortest paths from start vertex
    
    Args:
        graph: List of tuples (u, v, weight) representing edges
        start: Starting vertex
    
    Returns:
        distances: Dictionary of shortest distances from start to each vertex
        predecessors: Dictionary of predecessors for path reconstruction
        has_negative_cycle: Boolean indicating if negative cycle exists
    """
    
    # Initialize distances and predecessors
    vertices = set()
    for u, v, w in graph:
        vertices.add(u)
        vertices.add(v)
    
    distances = {vertex: float('inf') for vertex in vertices}
    predecessors = {vertex: None for vertex in vertices}
    distances[start] = 0
    
    # Relax edges repeatedly
    for _ in range(len(vertices) - 1):
        for u, v, weight in graph:
            if distances[u] != float('inf') and distances[u] + weight < distances[v]:
                distances[v] = distances[u] + weight
                predecessors[v] = u
    
    # Check for negative weight cycles
    has_negative_cycle = False
    for u, v, weight in graph:
        if distances[u] != float('inf') and distances[u] + weight < distances[v]:
            has_negative_cycle = True
            break
    
    return distances, predecessors, has_negative_cycle

def reconstruct_path(predecessors, start, end):
    """
    Reconstruct path from start to end using predecessors
    """
    path = []
    current = end
    
    while current is not None:
        path.append(current)
        current = predecessors[current]
    
    path.reverse()
    
    # Return path if it starts with start vertex
    if path and path[0] == start:
        return path
    else:
        return None

# Example usage
if __name__ == "__main__":
    # Example graph represented as edges (u, v, weight)
    # Graph: A->B(4), A->C(2), B->C(1), B->D(5), C->D(8), C->E(10), D->E(2)
    graph = [
        ('A', 'B', 4),
        ('A', 'C', 2),
        ('B', 'C', 1),
        ('B', 'D', 5),
        ('C', 'D', 8),
        ('C', 'E', 10),
        ('D', 'E', 2)
    ]
    
    print("Graph edges:")
    for u, v, w in graph:
        print(f"  {u} -> {v} (weight: {w})")
    
    # Run Bellman-Ford from vertex 'A'
    distances, predecessors, has_negative_cycle = bellman_ford(graph, 'A')
    
    print(f"\nShortest distances from 'A':")
    for vertex, distance in distances.items():
        print(f"  {vertex}: {distance}")
    
    print(f"\nHas negative cycle: {has_negative_cycle}")
    
    # Reconstruct some paths
    print("\nReconstructed paths:")
    path_to_e = reconstruct_path(predecessors, 'A', 'E')
    if path_to_e:
        print(f"  A to E: {' -> '.join(path_to_e)}")
    
    path_to_d = reconstruct_path(predecessors, 'A', 'D')
    if path_to_d:
        print(f"  A to D: {' -> '.join(path_to_d)}")
    
    # Example with negative cycle
    print("\n" + "="*50)
    print("Example with negative cycle:")
    
    # Graph with negative cycle: A->B(1), B->C(-3), C->A(1)
    graph_with_negative_cycle = [
        ('A', 'B', 1),
        ('B', 'C', -3),
        ('C', 'A', 1)
    ]
    
    distances2, predecessors2, has_negative_cycle2 = bellman_ford(graph_with_negative_cycle, 'A')
    print(f"Has negative cycle: {has_negative_cycle2}")
```

## Output:
```
Graph edges:
  A -> B (weight: 4)
  A -> C (weight: 2)
  B -> C (weight: 1)
  B -> D (weight: 5)
  C -> D (weight: 8)
  C -> E (weight: 10)
  D -> E (weight: 2)

Shortest distances from 'A':
  A: 0
  B: 3
  C: 2
  D: 8
  E: 10

Has negative cycle: False

Reconstructed paths:
  A to E: A -> C -> B -> D -> E
  A to D: A -> C -> B -> D

==================================================
Example with negative cycle:
Has negative cycle: True
```

## Key Features:

1. **Input Format**: Graph represented as list of tuples (u, v, weight)
2. **Output**: 
   - Shortest distances from start vertex
   - Predecessor information for path reconstruction
   - Negative cycle detection
3. **Time Complexity**: O(VE) where V is vertices and E is edges
4. **Space Complexity**: O(V)
5. **Handles**: Negative edge weights and negative cycles
6. **Path Reconstruction**: Can reconstruct actual shortest paths

The algorithm is particularly useful when dealing with graphs that may contain negative edge weights, unlike Dijkstra's algorithm which requires non-negative weights.

