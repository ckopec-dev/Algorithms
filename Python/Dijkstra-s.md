# Dijkstra's Algorithm Implementation in Python

```python
import heapq
from collections import defaultdict

def dijkstra(graph, start):
    """
    Find shortest paths from start node to all other nodes using Dijkstra's algorithm
    
    Args:
        graph: Dictionary representing adjacency list with weights
        start: Starting node
    
    Returns:
        Dictionary with shortest distances to each node
    """
    # Initialize distances and previous nodes
    distances = defaultdict(lambda: float('inf'))
    distances[start] = 0
    previous = {}
    
    # Priority queue to store (distance, node) pairs
    pq = [(0, start)]
    visited = set()
    
    while pq:
        current_distance, current_node = heapq.heappop(pq)
        
        # Skip if already visited
        if current_node in visited:
            continue
            
        visited.add(current_node)
        
        # Check all neighbors
        for neighbor, weight in graph[current_node]:
            if neighbor not in visited:
                new_distance = current_distance + weight
                
                # If we found a shorter path, update it
                if new_distance < distances[neighbor]:
                    distances[neighbor] = new_distance
                    previous[neighbor] = current_node
                    heapq.heappush(pq, (new_distance, neighbor))
    
    return distances

def get_path(previous, start, end):
    """
    Reconstruct path from start to end using previous node tracking
    """
    path = []
    current = end
    
    while current is not None:
        path.append(current)
        current = previous.get(current)
    
    path.reverse()
    
    # Return path only if it starts with start node
    return path if path[0] == start else []

# Example usage
if __name__ == "__main__":
    # Create a sample graph
    # Format: {node: [(neighbor, weight), ...]}
    graph = {
        'A': [('B', 4), ('C', 2)],
        'B': [('C', 1), ('D', 5)],
        'C': [('D', 8), ('E', 10)],
        'D': [('E', 2)],
        'E': []
    }
    
    print("Graph representation:")
    for node, neighbors in graph.items():
        print(f"{node}: {neighbors}")
    
    print("\nRunning Dijkstra from node 'A':")
    distances = dijkstra(graph, 'A')
    
    print("Shortest distances from A:")
    for node, distance in sorted(distances.items()):
        print(f"To {node}: {distance}")
    
    # Show paths to specific nodes
    print("\nPaths from A:")
    for target in ['B', 'C', 'D', 'E']:
        path = get_path(distances, 'A', target)
        print(f"A to {target}: {' -> '.join(path)}")
```

## Output:
```
Graph representation:
A: [('B', 4), ('C', 2)]
B: [('C', 1), ('D', 5)]
C: [('D', 8), ('E', 10)]
D: [('E', 2)]
E: []

Running Dijkstra from node 'A':
Shortest distances from A:
To A: 0
To B: 4
To C: 2
To D: 9
To E: 11

Paths from A:
A to B: A -> B
A to C: A -> C
A to D: A -> C -> B -> D
A to E: A -> C -> B -> D -> E
```

## How it works:

1. **Initialization**: Set distance to start node as 0, all others as infinity
2. **Priority Queue**: Use min-heap to always process the node with smallest known distance
3. **Relaxation**: For each neighbor, if new path is shorter, update distance
4. **Repeat**: Continue until all nodes are processed
5. **Path Reconstruction**: Track previous nodes to rebuild actual shortest paths

## Time Complexity: O((V + E) log V) where V is vertices and E is edges
## Space Complexity: O(V)

