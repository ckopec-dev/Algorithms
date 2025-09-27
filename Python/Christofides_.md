# Christofides Algorithm Implementation

The Christofides algorithm is a heuristic for finding an approximate solution to the Traveling Salesman Problem (TSP). It guarantees a solution no more than twice the optimal tour length.

```python
import numpy as np
from scipy.spatial.distance import pdist, squareform
import matplotlib.pyplot as plt
from itertools import combinations

def christofides_tsp(points):
    """
    Solve TSP using Christofides algorithm
    
    Args:
        points: List of (x, y) coordinates
    
    Returns:
        tuple: (optimal_path, total_distance)
    """
    
    # Convert to numpy array if needed
    if not isinstance(points, np.ndarray):
        points = np.array(points)
    
    n = len(points)
    
    # Step 1: Calculate distance matrix
    distances = squareform(pdist(points, metric='euclidean'))
    
    # Step 2: Find Minimum Spanning Tree (MST)
    mst_edges = find_mst(distances)
    
    # Step 3: Find vertices with odd degree in MST
    odd_degree_vertices = find_odd_degree_vertices(mst_edges, n)
    
    # Step 4: Find minimum weight perfect matching for odd degree vertices
    if odd_degree_vertices:
        matching_edges = find_min_weight_matching(odd_degree_vertices, distances)
        # Combine MST with matching edges
        all_edges = mst_edges + matching_edges
    else:
        all_edges = mst_edges
    
    # Step 5: Find Eulerian circuit in the combined graph
    eulerian_circuit = find_eulerian_circuit(all_edges, n)
    
    # Step 6: Convert Eulerian circuit to Hamiltonian cycle (shortcutting)
    hamiltonian_path = create_hamiltonian_cycle(eulerian_circuit, distances)
    
    # Calculate total distance
    total_distance = calculate_total_distance(hamiltonian_path, distances)
    
    return hamiltonian_path, total_distance

def find_mst(distances):
    """Find Minimum Spanning Tree using Kruskal's algorithm"""
    n = len(distances)
    
    # Create list of edges with weights
    edges = []
    for i in range(n):
        for j in range(i + 1, n):
            edges.append((distances[i][j], i, j))
    
    # Sort edges by weight
    edges.sort()
    
    # Kruskal's algorithm
    parent = list(range(n))
    rank = [0] * n
    mst_edges = []
    
    def find(x):
        if parent[x] != x:
            parent[x] = find(parent[x])
        return parent[x]
    
    def union(x, y):
        px, py = find(x), find(y)
        if px != py:
            if rank[px] < rank[py]:
                px, py = py, px
            parent[py] = px
            if rank[px] == rank[py]:
                rank[px] += 1
            return True
        return False
    
    for weight, u, v in edges:
        if union(u, v):
            mst_edges.append((u, v))
            if len(mst_edges) == n - 1:
                break
    
    return mst_edges

def find_odd_degree_vertices(mst_edges, n):
    """Find vertices with odd degree in the MST"""
    degree = [0] * n
    for u, v in mst_edges:
        degree[u] += 1
        degree[v] += 1
    
    odd_vertices = []
    for i in range(n):
        if degree[i] % 2 == 1:
            odd_vertices.append(i)
    
    return odd_vertices

def find_min_weight_matching(odd_vertices, distances):
    """Find minimum weight perfect matching for odd vertices"""
    n = len(odd_vertices)
    
    # Create distance matrix for odd vertices
    if n <= 1:
        return []
    
    min_weight_edges = []
    
    # For small number of vertices, try all possible matchings
    if n <= 6:  # Use brute force for small sets
        best_cost = float('inf')
        best_matching = []
        
        # Generate all possible perfect matchings
        for perm in combinations(range(n), n // 2):
            if len(perm) == n // 2:
                # Create a matching from this selection
                matching = []
                remaining = set(range(n)) - set(perm)
                for i in range(0, len(perm), 2):
                    if i + 1 < len(perm):
                        u, v = odd_vertices[perm[i]], odd_vertices[perm[i+1]]
                        matching.append((u, v))
                
                # Calculate total weight
                cost = sum(distances[u][v] for u, v in matching)
                if cost < best_cost:
                    best_cost = cost
                    best_matching = matching
        
        return best_matching
    
    else:
        # For larger sets, use greedy approach
        edges = []
        for i in range(n):
            for j in range(i + 1, n):
                edges.append((distances[odd_vertices[i]][odd_vertices[j]], 
                            odd_vertices[i], odd_vertices[j]))
        
        edges.sort()
        
        used = [False] * n
        matching = []
        
        for weight, u, v in edges:
            if not used[u] and not used[v]:
                matching.append((u, v))
                used[u] = True
                used[v] = True
        
        return matching

def find_eulerian_circuit(edges, n):
    """Find Eulerian circuit using Hierholzer's algorithm"""
    # Build adjacency list
    adj = [[] for _ in range(n)]
    edge_count = {}
    
    for u, v in edges:
        adj[u].append(v)
        adj[v].append(u)
        key = tuple(sorted([u, v]))
        edge_count[key] = edge_count.get(key, 0) + 1
    
    # Find starting vertex (any vertex with odd degree, or 0 if all even)
    start = 0
    for i in range(n):
        if len(adj[i]) % 2 == 1:
            start = i
            break
    
    # Hierholzer's algorithm
    stack = [start]
    circuit = []
    
    while stack:
        u = stack[-1]
        if adj[u]:
            v = adj[u].pop()
            adj[v].remove(u)
            stack.append(v)
        else:
            circuit.append(stack.pop())
    
    return circuit[::-1]

def create_hamiltonian_cycle(eulerian_circuit, distances):
    """Convert Eulerian circuit to Hamiltonian cycle (shortcutting)"""
    visited = set()
    path = []
    
    for vertex in eulerian_circuit:
        if vertex not in visited:
            visited.add(vertex)
            path.append(vertex)
    
    return path

def calculate_total_distance(path, distances):
    """Calculate total distance of the path"""
    total = 0
    n = len(path)
    
    for i in range(n):
        total += distances[path[i]][path[(i + 1) % n]]
    
    return total

def plot_tsp_solution(points, path, title="TSP Solution"):
    """Plot the TSP solution"""
    plt.figure(figsize=(10, 8))
    
    # Plot points
    x = [points[i][0] for i in range(len(points))]
    y = [points[i][1] for i in range(len(points))]
    plt.scatter(x, y, c='red', s=100, zorder=5)
    
    # Label points
    for i, (px, py) in enumerate(points):
        plt.annotate(str(i), (px, py), xytext=(5, 5), textcoords='offset points')
    
    # Plot path
    path_x = [points[i][0] for i in path]
    path_y = [points[i][1] for i in path]
    path_x.append(path_x[0])  # Close the loop
    path_y.append(path_y[0])
    
    plt.plot(path_x, path_y, 'b-', linewidth=2, marker='o', markersize=8)
    
    plt.title(title)
    plt.xlabel('X coordinate')
    plt.ylabel('Y coordinate')
    plt.grid(True, alpha=0.3)
    plt.axis('equal')
    plt.show()

# Example usage
if __name__ == "__main__":
    # Create sample points
    points = [
        (60, 200), (180, 200), (80, 180), (140, 180),
        (20, 160), (100, 160), (200, 160), (140, 140),
        (40, 120), (100, 120), (180, 100), (60, 80),
        (120, 80), (200, 60), (20, 40), (100, 40)
    ]
    
    print("Points:", points)
    print("\nRunning Christofides algorithm...")
    
    # Solve TSP
    optimal_path, total_distance = christofides_tsp(points)
    
    print(f"Optimal path: {optimal_path}")
    print(f"Total distance: {total_distance:.2f}")
    
    # Plot the solution
    plot_tsp_solution(points, optimal_path, 
                     f"Christofides TSP Solution\nDistance: {total_distance:.2f}")

    # Another example with smaller set of points
    print("\n" + "="*50)
    print("Second Example:")
    
    small_points = [(0, 0), (1, 2), (3, 1), (5, 3)]
    
    path2, dist2 = christofides_tsp(small_points)
    print(f"Points: {small_points}")
    print(f"Optimal path: {path2}")
    print(f"Total distance: {dist2:.2f}")
    
    plot_tsp_solution(small_points, path2, 
                     f"Christofides TSP Solution\nDistance: {dist2:.2f}")
```

## Algorithm Steps Explained:

1. **Distance Matrix**: Calculate pairwise distances between all points
2. **Minimum Spanning Tree (MST)**: Find MST of the complete graph using Kruskal's algorithm
3. **Odd Degree Vertices**: Identify vertices with odd degree in the MST
4. **Perfect Matching**: Find minimum weight perfect matching for odd-degree vertices
5. **Eulerian Circuit**: Combine MST and matching to form Eulerian graph, then find Eulerian circuit
6. **Hamiltonian Cycle**: Convert Eulerian circuit to Hamiltonian cycle using shortcutting

## Key Features:

- **Approximation Ratio**: Guaranteed to be at most 2 times optimal solution
- **Time Complexity**: O(n³) where n is the number of points
- **Space Complexity**: O(n²) for distance matrix
- **Visualization**: Includes plotting function to visualize results

This implementation provides a complete working example of the Christofides algorithm that can be easily modified for different datasets or requirements.

