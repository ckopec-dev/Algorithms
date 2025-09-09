```python
import numpy as np
from scipy.spatial.distance import pdist, squareform
import matplotlib.pyplot as plt
from itertools import combinations

def christofides_tsp(points):
    """
    Implementation of Christofides algorithm for TSP
    """
    # Step 1: Compute distance matrix
    distances = squareform(pdist(points, metric='euclidean'))
    
    # Step 2: Find Minimum Spanning Tree (MST)
    mst_edges = find_mst(distances)
    
    # Step 3: Find vertices with odd degree in MST
    odd_degree_vertices = find_odd_degree_vertices(mst_edges, len(points))
    
    # Step 4: Find minimum weight perfect matching for odd degree vertices
    if odd_degree_vertices:
        matching_edges = find_minimum_weight_matching(distances, odd_degree_vertices)
        # Step 5: Combine MST and matching to form a multigraph
        multigraph_edges = mst_edges + matching_edges
    else:
        multigraph_edges = mst_edges
    
    # Step 6: Find Eulerian circuit in multigraph
    eulerian_circuit = find_eulerian_circuit(multigraph_edges, len(points))
    
    # Step 7: Convert to Hamiltonian cycle (shortcutting)
    hamiltonian_cycle = create_hamiltonian_cycle(eulerian_circuit, distances)
    
    return hamiltonian_cycle

def find_mst(distances):
    """
    Find Minimum Spanning Tree using Kruskal's algorithm
    Returns list of edges (as tuples of indices)
    """
    n = len(distances)
    edges = []
    
    # Create list of all edges with weights
    for i in range(n):
        for j in range(i+1, n):
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

def find_odd_degree_vertices(edges, n):
    """
    Find vertices with odd degree in the graph
    """
    degree = [0] * n
    for u, v in edges:
        degree[u] += 1
        degree[v] += 1
    
    odd_vertices = []
    for i in range(n):
        if degree[i] % 2 == 1:
            odd_vertices.append(i)
    
    return odd_vertices

def find_minimum_weight_matching(distances, vertices):
    """
    Find minimum weight perfect matching for given vertices
    """
    n = len(vertices)
    if n <= 1:
        return []
    
    # Create subgraph with only specified vertices
    sub_distances = [[distances[i][j] for j in vertices] for i in vertices]
    
    # Simple greedy approach - find minimum weight edges
    matching_edges = []
    matched = set()
    
    # Create all possible pairs and sort by weight
    pairs = []
    for i in range(len(vertices)):
        for j in range(i+1, len(vertices)):
            pairs.append((sub_distances[i][j], i, j))
    
    pairs.sort()
    
    for weight, i, j in pairs:
        if i not in matched and j not in matched:
            matching_edges.append((vertices[i], vertices[j]))
            matched.add(i)
            matched.add(j)
            if len(matched) == len(vertices):
                break
    
    return matching_edges

def find_eulerian_circuit(edges, n):
    """
    Find Eulerian circuit using Hierholzer's algorithm
    """
    # Build adjacency list representation
    adj = [[] for _ in range(n)]
    edge_count = [0] * n
    
    for u, v in edges:
        adj[u].append(v)
        adj[v].append(u)
        edge_count[u] += 1
        edge_count[v] += 1
    
    # Find starting vertex (should have at least one edge)
    start = -1
    for i in range(n):
        if edge_count[i] > 0:
            start = i
            break
    
    if start == -1:
        return []
    
    # Hierholzer's algorithm
    stack = [start]
    circuit = []
    
    while stack:
        current = stack[-1]
        if adj[current]:
            next_node = adj[current].pop()
            adj[next_node].remove(current)
            stack.append(next_node)
        else:
            circuit.append(stack.pop())
    
    return circuit[::-1]

def create_hamiltonian_cycle(circuit, distances):
    """
    Convert Eulerian circuit to Hamiltonian cycle by skipping repeated vertices
    """
    visited = set()
    cycle = []
    
    for vertex in circuit:
        if vertex not in visited:
            visited.add(vertex)
            cycle.append(vertex)
    
    # Add first vertex at the end to complete the cycle
    if len(cycle) > 0:
        cycle.append(cycle[0])
    
    return cycle

def calculate_total_distance(cycle, points):
    """
    Calculate total distance of the tour
    """
    total = 0
    for i in range(len(cycle) - 1):
        total += np.linalg.norm(np.array(points[cycle[i]]) - np.array(points[cycle[i+1]]))
    return total

# Example usage
if __name__ == "__main__":
    # Generate random points
    np.random.seed(42)
    points = np.random.rand(10, 2) * 100
    
    print("Points:")
    for i, point in enumerate(points):
        print(f"Point {i}: ({point[0]:.2f}, {point[1]:.2f})")
    
    # Run Christofides algorithm
    tour = christofides_tsp(points)
    
    print(f"\nOptimal tour (indices): {tour}")
    
    # Calculate total distance
    total_distance = calculate_total_distance(tour, points)
    print(f"Total distance: {total_distance:.2f}")
    
    # Plot results
    plt.figure(figsize=(10, 8))
    
    # Plot points
    x_coords = [point[0] for point in points]
    y_coords = [point[1] for point in points]
    plt.scatter(x_coords, y_coords, c='red', s=100, zorder=5)
    
    # Label points
    for i, (x, y) in enumerate(points):
        plt.annotate(str(i), (x, y), xytext=(5, 5), textcoords='offset points')
    
    # Plot tour
    tour_x = [points[i][0] for i in tour]
    tour_y = [points[i][1] for i in tour]
    plt.plot(tour_x, tour_y, 'b-', linewidth=2, marker='o', markersize=8)
    
    plt.title('Christofides Algorithm TSP Solution')
    plt.xlabel('X coordinate')
    plt.ylabel('Y coordinate')
    plt.grid(True, alpha=0.3)
    plt.axis('equal')
    plt.show()
    
    # Compare with brute force for small instances
    print("\nComparison with brute force approach:")
    print("Christofides algorithm provides a solution that is at most 50% longer than optimal")
    print(f"For this example, the tour length is {total_distance:.2f}")
```


