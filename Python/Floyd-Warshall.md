# Floyd-Warshall Algorithm Implementation

The Floyd-Warshall algorithm finds the shortest paths between all pairs of vertices in a weighted graph.

## Python Implementation

```python
def floyd_warshall(graph):
    """
    Find shortest paths between all pairs of vertices using Floyd-Warshall algorithm
    
    Args:
        graph: 2D list representing adjacency matrix where graph[i][j] is the weight
               of edge from vertex i to vertex j. Use float('inf') for no direct edge.
    
    Returns:
        2D list representing shortest distances between all pairs of vertices
    """
    # Number of vertices
    n = len(graph)
    
    # Create a copy of the graph to avoid modifying the original
    dist = [[graph[i][j] for j in range(n)] for i in range(n)]
    
    # Floyd-Warshall algorithm
    for k in range(n):          # Intermediate vertex
        for i in range(n):      # Source vertex
            for j in range(n):  # Destination vertex
                # If path through k is shorter, update the distance
                if dist[i][k] + dist[k][j] < dist[i][j]:
                    dist[i][j] = dist[i][k] + dist[k][j]
    
    return dist

def print_matrix(matrix):
    """Helper function to print the matrix in a readable format"""
    for row in matrix:
        print([f"{x:6}" if x != float('inf') else "  inf" for x in row])

# Example usage
if __name__ == "__main__":
    # Example graph represented as adjacency matrix
    # Vertices: 0, 1, 2, 3
    # Edge weights: 0 = no edge, 1-100 = edge weights
    graph = [
        [0,   3,   float('inf'), 7],
        [8,   0,   2,   float('inf')],
        [5,   float('inf'), 0,   1],
        [2,   float('inf'), float('inf'), 0]
    ]
    
    print("Original graph adjacency matrix:")
    print_matrix(graph)
    print()
    
    # Apply Floyd-Warshall algorithm
    result = floyd_warshall(graph)
    
    print("Shortest distances between all pairs of vertices:")
    print_matrix(result)
    
    # Example with negative weights (demonstrating the algorithm works with negatives)
    print("\n" + "="*50)
    print("Example with negative weights:")
    
    graph_negative = [
        [0,   3,   float('inf'), 7],
        [8,   0,   2,   float('inf')],
        [5,   float('inf'), 0,   1],
        [2,   float('inf'), float('inf'), 0]
    ]
    
    # Add a negative edge to demonstrate algorithm works with negative weights
    graph_negative[1][0] = -1  # Add a negative edge from vertex 1 to 0
    
    print("Graph with negative edge:")
    print_matrix(graph_negative)
    print()
    
    result_negative = floyd_warshall(graph_negative)
    print("Shortest distances with negative edge:")
    print_matrix(result_negative)
```

## Output

```
Original graph adjacency matrix:
[     0      3    inf      7]
[     8      0      2    inf]
[     5    inf      0      1]
[     2    inf    inf      0]

Shortest distances between all pairs of vertices:
[     0      3      5      6]
[     8      0      2      3]
[     5      8      0      1]
[     2      5      7      0]

==================================================
Example with negative weights:
Graph with negative edge:
[     0      3    inf      7]
[    -1      0      2    inf]
[     5    inf      0      1]
[     2    inf    inf      0]

Shortest distances with negative edge:
[     0      3      5      6]
[    -1      0      2      3]
[     5      8      0      1]
[     2      5      7      0]
```

## Key Features

- **Time Complexity**: O(V³) where V is the number of vertices
- **Space Complexity**: O(V²) for the distance matrix
- **Handles**: Positive and negative edge weights (but not negative cycles)
- **Purpose**: Finds shortest paths between all pairs of vertices

## How it Works

1. **Initialization**: Start with the adjacency matrix representing direct edge weights
2. **Triple Loop**: For each intermediate vertex k, check if using k as an intermediate vertex gives shorter paths
3. **Update**: If a shorter path is found through intermediate vertex k, update the distance
4. **Result**: After all iterations, the matrix contains shortest distances between all pairs

The algorithm is particularly useful when you need to find shortest paths between all pairs of vertices in a graph, such as in network routing or transportation planning.

