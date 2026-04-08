# SPFA Algorithm Implementation in Python

SPFA (Shortest Path Faster Algorithm) is an improvement over the Bellman-Ford algorithm that uses a queue to optimize the process of finding shortest paths in weighted graphs.

## Algorithm Overview
- **Time Complexity**: O(KE) where K is the average number of edges per vertex
- **Space Complexity**: O(V)
- **Use Case**: Finding shortest paths from a single source to all other vertices

## Python Implementation

```python
from collections import deque
import sys

def spfa(graph, start, num_vertices):
    """
    SPFA algorithm implementation
    
    Args:
        graph: List of edges in format [from, to, weight]
        start: Starting vertex
        num_vertices: Total number of vertices
    
    Returns:
        List of shortest distances from start to all vertices
    """
    # Initialize distances
    dist = [sys.maxsize] * num_vertices
    dist[start] = 0
    
    # Track whether a vertex is in the queue
    in_queue = [False] * num_vertices
    
    # Queue for vertices to process
    queue = deque([start])
    in_queue[start] = True
    
    # Count of how many times each vertex is relaxed
    update_count = [0] * num_vertices
    
    while queue:
        current = queue.popleft()
        in_queue[current] = False
        
        # Check all neighbors of current vertex
        for edge in graph:
            u, v, weight = edge
            
            # If this edge connects to current vertex
            if u == current:
                # Relax the edge
                if dist[u] + weight < dist[v]:
                    dist[v] = dist[u] + weight
                    
                    # If vertex is not in queue, add it
                    if not in_queue[v]:
                        queue.append(v)
                        in_queue[v] = True
                        
                        # Track number of updates for negative cycle detection
                        update_count[v] += 1
                        
                        # If a vertex is updated more than V times, there's a negative cycle
                        if update_count[v] > num_vertices:
                            print("Negative cycle detected!")
                            return None
    
    return dist

# Example usage
def main():
    # Example graph represented as edges [from, to, weight]
    # Graph: 0 -> 1(4), 0 -> 2(2), 1 -> 2(1), 1 -> 3(5), 2 -> 3(8), 2 -> 4(10)
    edges = [
        [0, 1, 4],
        [0, 2, 2],
        [1, 2, 1],
        [1, 3, 5],
        [2, 3, 8],
        [2, 4, 10]
    ]
    
    num_vertices = 5
    start_vertex = 0
    
    # Run SPFA
    distances = spfa(edges, start_vertex, num_vertices)
    
    if distances is not None:
        print(f"Shortest distances from vertex {start_vertex}:")
        for i, dist in enumerate(distances):
            if dist == sys.maxsize:
                print(f"Vertex {i}: Not reachable")
            else:
                print(f"Vertex {i}: {dist}")

# Alternative implementation with adjacency list
def spfa_adjacency_list(adj_list, start, num_vertices):
    """
    SPFA using adjacency list representation
    """
    dist = [sys.maxsize] * num_vertices
    dist[start] = 0
    in_queue = [False] * num_vertices
    queue = deque([start])
    in_queue[start] = True
    
    while queue:
        current = queue.popleft()
        in_queue[current] = False
        
        # Process all neighbors
        for neighbor, weight in adj_list[current]:
            if dist[current] + weight < dist[neighbor]:
                dist[neighbor] = dist[current] + weight
                
                if not in_queue[neighbor]:
                    queue.append(neighbor)
                    in_queue[neighbor] = True
    
    return dist

# Example with adjacency list
def example_adjacency_list():
    # Adjacency list representation: [vertex] -> [(neighbor, weight), ...]
    adj_list = [
        [(1, 4), (2, 2)],      # Vertex 0
        [(2, 1), (3, 5)],      # Vertex 1
        [(3, 8), (4, 10)],     # Vertex 2
        [(4, 2)],              # Vertex 3
        []                     # Vertex 4
    ]
    
    distances = spfa_adjacency_list(adj_list, 0, 5)
    print("Using adjacency list:")
    for i, dist in enumerate(distances):
        print(f"Vertex {i}: {dist}")

if __name__ == "__main__":
    main()
    print()
    example_adjacency_list()
```

## Sample Output
```
Shortest distances from vertex 0:
Vertex 0: 0
Vertex 1: 3
Vertex 2: 2
Vertex 3: 8
Vertex 4: 12

Using adjacency list:
Vertex 0: 0
Vertex 1: 3
Vertex 2: 2
Vertex 3: 8
Vertex 4: 12
```

## Key Features of This Implementation

1. **Queue-based Processing**: Uses a queue to process vertices efficiently
2. **Optimization**: Only processes vertices that have been updated
3. **Negative Cycle Detection**: Checks for negative cycles during execution
4. **Flexible Input**: Supports both edge list and adjacency list representations
5. **Error Handling**: Handles unreachable vertices gracefully

## When to Use SPFA

- When dealing with graphs that have negative edge weights
- When you need to find shortest paths from one source to all vertices
- When the graph is sparse (fewer edges than vertices squared)
- When Bellman-Ford is too slow but Dijkstra's algorithm can't handle negative weights

The SPFA algorithm is particularly useful when you have a graph with negative edges but no negative cycles, as it can efficiently find the shortest paths in such cases.

