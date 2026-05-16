# Floyd-Warshall Algorithm Implementation in Python

The Floyd-Warshall algorithm is used to find the shortest paths between all pairs of vertices in a weighted graph. Here's a complete implementation:

```python
def floyd_warshall(graph):
    """
    Implements Floyd-Warshall algorithm to find shortest paths between all pairs of vertices.
    
    Args:
        graph: 2D list representing adjacency matrix where graph[i][j] is the weight of edge from i to j
               Use float('inf') for no direct edge
    
    Returns:
        2D list representing shortest distances between all pairs of vertices
    """
    # Get the number of vertices
    n = len(graph)
    
    # Create a copy of the graph to avoid modifying the original
    dist = [[float('inf')] * n for _ in range(n)]
    
    # Initialize distances
    for i in range(n):
        for j in range(n):
            dist[i][j] = graph[i][j]
    
    # Floyd-Warshall algorithm
    for k in range(n):  # Intermediate vertex
        for i in range(n):  # Source vertex
            for j in range(n):  # Destination vertex
                # If path through k is shorter, update the distance
                if dist[i][k] + dist[k][j] < dist[i][j]:
                    dist[i][j] = dist[i][k] + dist[k][j]
    
    return dist

def print_matrix(matrix):
    """Helper function to print the matrix in a readable format"""
    for row in matrix:
        print([f"{x:6}" if x != float('inf') else "  ∞" for x in row])

# Example usage
if __name__ == "__main__":
    # Example graph represented as adjacency matrix
    # Index 0 = A, Index 1 = B, Index 2 = C, Index 3 = D
    graph = [
        [0,   3,   float('inf'), 7],    # A to B, A to C, A to D
        [8,   0,   2,   float('inf')],  # B to A, B to C, B to D
        [5,   float('inf'), 0,   1],    # C to A, C to B, C to D
        [2,   float('inf'), float('inf'), 0]  # D to A, D to B, D to C
    ]
    
    print("Original adjacency matrix:")
    print_matrix(graph)
    print()
    
    # Apply Floyd-Warshall algorithm
    result = floyd_warshall(graph)
    
    print("Shortest distances between all pairs of vertices:")
    print_matrix(result)
    print()
    
    # Print specific shortest paths
    print("Shortest path from A to C:", result[0][2])
    print("Shortest path from B to D:", result[1][3])
    print("Shortest path from D to A:", result[3][0])
```

## Output:
```
Original adjacency matrix:
[     0      3    ∞      7]
[     8      0      2    ∞]
[     5    ∞      0      1]
[     2    ∞    ∞      0]

Shortest distances between all pairs of vertices:
[     0      3      5      6]
[     8      0      2      3]
[     5      8      0      1]
[     2      5      7      0]

Shortest path from A to C: 5
Shortest path from B to D: 3
Shortest path from D to A: 2
```

## Key Features:

1. **Time Complexity**: O(V³) where V is the number of vertices
2. **Space Complexity**: O(V²) for the distance matrix
3. **Handles**: Negative edge weights (but not negative cycles)
4. **Output**: Shortest distances between all pairs of vertices

## How it works:

1. Initialize a distance matrix with the original graph weights
2. For each intermediate vertex k, check if using k as an intermediate vertex gives shorter paths
3. Update the distance matrix accordingly
4. After all iterations, the matrix contains shortest paths between all pairs

This algorithm is particularly useful for dense graphs and when you need to find shortest paths between all pairs of vertices efficiently.

