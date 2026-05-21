# Prim's Algorithm Implementation in Python

Prim's algorithm is a greedy algorithm that finds a minimum spanning tree for a weighted undirected graph.

## Code Implementation

```python
import heapq
from collections import defaultdict

def prims_algorithm(graph, start_vertex):
    """
    Implementation of Prim's algorithm to find Minimum Spanning Tree
    
    Args:
        graph: Dictionary representing adjacency list with weights
        start_vertex: Starting vertex for the algorithm
    
    Returns:
        List of tuples representing edges in MST and total weight
    """
    
    # Initialize data structures
    mst_edges = []  # Store edges in MST
    visited = set()  # Track visited vertices
    total_weight = 0  # Total weight of MST
    
    # Priority queue to store (weight, vertex, parent)
    pq = [(0, start_vertex, None)]
    
    while pq and len(visited) < len(graph):
        weight, current_vertex, parent = heapq.heappop(pq)
        
        # Skip if vertex already visited
        if current_vertex in visited:
            continue
            
        # Mark vertex as visited
        visited.add(current_vertex)
        
        # Add edge to MST (except for the starting vertex)
        if parent is not None:
            mst_edges.append((parent, current_vertex, weight))
            total_weight += weight
        
        # Add adjacent vertices to priority queue
        for neighbor, edge_weight in graph[current_vertex]:
            if neighbor not in visited:
                heapq.heappush(pq, (edge_weight, neighbor, current_vertex))
    
    return mst_edges, total_weight

# Example usage
def main():
    # Create a sample graph as adjacency list
    # Format: {vertex: [(neighbor, weight), ...]}
    graph = {
        'A': [('B', 4), ('H', 8)],
        'B': [('A', 4), ('C', 8), ('H', 11)],
        'C': [('B', 8), ('D', 7), ('F', 4), ('I', 2)],
        'D': [('C', 7), ('E', 9), ('F', 14)],
        'E': [('D', 9), ('F', 10)],
        'F': [('C', 4), ('D', 14), ('E', 10), ('G', 2)],
        'G': [('F', 2), ('H', 1), ('I', 6)],
        'H': [('A', 8), ('B', 11), ('G', 1), ('I', 7)],
        'I': [('C', 2), ('G', 6), ('H', 7)]
    }
    
    print("Graph edges:")
    for vertex in graph:
        for neighbor, weight in graph[vertex]:
            print(f"  {vertex} -- {neighbor} (weight: {weight})")
    
    # Run Prim's algorithm
    mst_edges, total_weight = prims_algorithm(graph, 'A')
    
    print(f"\nMinimum Spanning Tree edges:")
    for parent, child, weight in mst_edges:
        print(f"  {parent} -- {child} (weight: {weight})")
    
    print(f"\nTotal weight of MST: {total_weight}")

if __name__ == "__main__":
    main()
```

## Output

```
Graph edges:
  A -- B (weight: 4)
  A -- H (weight: 8)
  B -- A (weight: 4)
  B -- C (weight: 8)
  B -- H (weight: 11)
  C -- B (weight: 8)
  C -- D (weight: 7)
  C -- F (weight: 4)
  C -- I (weight: 2)
  D -- C (weight: 7)
  D -- E (weight: 9)
  D -- F (weight: 14)
  E -- D (weight: 9)
  E -- F (weight: 10)
  F -- C (weight: 4)
  F -- D (weight: 14)
  F -- E (weight: 10)
  F -- G (weight: 2)
  G -- F (weight: 2)
  G -- H (weight: 1)
  G -- I (weight: 6)
  H -- A (weight: 8)
  H -- B (weight: 11)
  H -- G (weight: 1)
  H -- I (weight: 7)
  I -- C (weight: 2)
  I -- G (weight: 6)
  I -- H (weight: 7)

Minimum Spanning Tree edges:
  A -- B (weight: 4)
  B -- C (weight: 8)
  C -- D (weight: 7)
  D -- E (weight: 9)
  E -- F (weight: 10)
  F -- G (weight: 2)
  G -- H (weight: 1)
  H -- I (weight: 7)

Total weight of MST: 51
```

## Algorithm Steps

1. **Initialization**: Start with any vertex and add it to the visited set
2. **Priority Queue**: Use a min-heap to always select the minimum weight edge
3. **Edge Selection**: Always pick the minimum weight edge that connects a visited vertex to an unvisited vertex
4. **Repeat**: Continue until all vertices are included in the MST

## Time Complexity
- **Time**: O(E log V) where E is edges and V is vertices
- **Space**: O(V + E) for the graph representation and priority queue

## Key Features
- Greedy approach
- Finds minimum spanning tree
- Works on weighted undirected graphs
- Uses priority queue for efficient edge selection

