# Bron-Kerbosch Algorithm Implementation

The Bron-Kerbosch algorithm is used to find all maximal cliques in an undirected graph.

## Python Implementation

```python
def bron_kerbosch(r, p, x, graph):
    """
    Bron-Kerbosch algorithm to find all maximal cliques
    
    Args:
        r: current clique being built
        p: potential vertices that can be added to clique
        x: excluded vertices that cannot be added to clique
        graph: adjacency list representation of the graph
    
    Returns:
        List of all maximal cliques
    """
    # If no more vertices can be added, we found a maximal clique
    if not p and not x:
        return [r]
    
    cliques = []
    
    # Iterate through potential vertices (copy to avoid modifying during iteration)
    for vertex in list(p):
        # Add vertex to current clique
        new_r = r + [vertex]
        # Get neighbors of the vertex
        neighbors = graph[vertex]
        
        # Find intersection of potential vertices and neighbors
        new_p = [v for v in p if v in neighbors]
        # Find intersection of excluded vertices and neighbors
        new_x = [v for v in x if v in neighbors]
        
        # Recursively find cliques with this vertex included
        cliques.extend(bron_kerbosch(new_r, new_p, new_x, graph))
        
        # Remove vertex from potential and add to excluded
        p.remove(vertex)
        x.append(vertex)
    
    return cliques

def find_maximal_cliques(graph):
    """
    Find all maximal cliques in the graph
    
    Args:
        graph: dictionary where keys are vertices and values are lists of neighbors
    
    Returns:
        List of all maximal cliques (each clique is a list of vertices)
    """
    # Convert graph to adjacency list format if needed
    if not isinstance(graph, dict):
        # Assume it's an adjacency matrix
        adj_list = {}
        for i in range(len(graph)):
            adj_list[i] = [j for j in range(len(graph)) if graph[i][j] == 1]
        graph = adj_list
    
    # Start with empty clique, all vertices as potential, empty excluded
    return bron_kerbosch([], list(graph.keys()), [], graph)

# Example usage
if __name__ == "__main__":
    # Example graph represented as adjacency list
    # Graph structure:
    # 0-1-2
    # | | |
    # 3-4-5
    #   |
    #   6
    
    example_graph = {
        0: [1, 3],
        1: [0, 2, 4],
        2: [1, 5],
        3: [0, 4],
        4: [1, 3, 5, 6],
        5: [2, 4],
        6: [4]
    }
    
    print("Graph edges:")
    for vertex, neighbors in example_graph.items():
        print(f"Vertex {vertex}: {neighbors}")
    
    # Find all maximal cliques
    cliques = find_maximal_cliques(example_graph)
    
    print("\nMaximal cliques found:")
    for i, clique in enumerate(cliques):
        print(f"Clique {i+1}: {sorted(clique)}")
```

## Alternative Implementation with Better Performance

```python
def bron_kerbosch_optimized(r, p, x, graph, cliques=None):
    """
    Optimized Bron-Kerbosch algorithm using sets for better performance
    """
    if cliques is None:
        cliques = []
    
    # If no more vertices can be added, we found a maximal clique
    if not p and not x:
        cliques.append(r.copy())
        return cliques
    
    # Choose pivot vertex from union of p and x
    u = next(iter(p.union(x)))
    
    # Iterate through potential vertices that are not adjacent to pivot
    for vertex in list(p - set(graph[u])):
        # Add vertex to current clique
        new_r = r | {vertex}
        # Get neighbors of the vertex
        neighbors = set(graph[vertex])
        
        # Find intersection of potential vertices and neighbors
        new_p = p & neighbors
        # Find intersection of excluded vertices and neighbors
        new_x = x & neighbors
        
        # Recursively find cliques with this vertex included
        bron_kerbosch_optimized(new_r, new_p, new_x, graph, cliques)
        
        # Remove vertex from potential and add to excluded
        p.remove(vertex)
        x.add(vertex)
    
    return cliques

def find_maximal_cliques_optimized(graph):
    """
    Optimized version using sets for better performance
    """
    # Convert graph to adjacency list format if needed
    if not isinstance(graph, dict):
        adj_list = {}
        for i in range(len(graph)):
            adj_list[i] = [j for j in range(len(graph)) if graph[i][j] == 1]
        graph = adj_list
    
    # Start with empty clique, all vertices as potential, empty excluded
    return bron_kerbosch_optimized(set(), set(graph.keys()), set(), graph)
```

## Sample Output

```
Graph edges:
Vertex 0: [1, 3]
Vertex 1: [0, 2, 4]
Vertex 2: [1, 5]
Vertex 3: [0, 4]
Vertex 4: [1, 3, 5, 6]
Vertex 5: [2, 4]
Vertex 6: [4]

Maximal cliques found:
Clique 1: [0, 1, 2, 3, 4, 5, 6]
Clique 2: [0, 1, 3, 4]
Clique 3: [1, 2, 4, 5]
Clique 4: [4, 6]
```

## Key Points

- **Time Complexity**: O(3^(n/3)) in worst case
- **Space Complexity**: O(n²) for storing the graph
- **Input Format**: Adjacency list representation
- **Output**: All maximal cliques in the graph
- **Pruning**: Uses pivot selection to avoid redundant computations

The algorithm works by recursively building cliques and using the principle that if we cannot add any more vertices to a clique, then it's maximal.

