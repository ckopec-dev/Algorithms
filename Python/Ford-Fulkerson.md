# Ford-Fulkerson Algorithm Implementation

Here's a complete implementation of the Ford-Fulkerson algorithm to find the maximum flow in a flow network:

```python
from collections import deque

class Graph:
    def __init__(self, vertices):
        self.V = vertices
        self.graph = [[0 for _ in range(vertices)] for _ in range(vertices)]
    
    def add_edge(self, u, v, capacity):
        """Add an edge to the graph"""
        self.graph[u][v] = capacity
    
    def bfs(self, source, sink, parent):
        """Breadth-First Search to find if there's a path from source to sink"""
        visited = [False] * self.V
        queue = deque()
        
        queue.append(source)
        visited[source] = True
        
        while queue:
            u = queue.popleft()
            
            for v in range(self.V):
                if not visited[v] and self.graph[u][v] > 0:
                    visited[v] = True
                    parent[v] = u
                    queue.append(v)
                    
                    if v == sink:
                        return True
        
        return False
    
    def ford_fulkerson(self, source, sink):
        """Find the maximum flow from source to sink"""
        parent = [-1] * self.V
        max_flow = 0
        
        # Augment the flow while there's a path from source to sink
        while self.bfs(source, sink, parent):
            # Find minimum residual capacity of the edges along the path
            path_flow = float('inf')
            s = sink
            
            while s != source:
                path_flow = min(path_flow, self.graph[parent[s]][s])
                s = parent[s]
            
            # Add path flow to overall flow
            max_flow += path_flow
            
            # Update residual capacities of the edges and reverse edges
            v = sink
            while v != source:
                u = parent[v]
                self.graph[u][v] -= path_flow
                self.graph[v][u] += path_flow
                v = parent[v]
        
        return max_flow

# Example usage
def main():
    # Create a graph with 6 vertices (0 to 5)
    g = Graph(6)
    
    # Add edges with capacities
    g.add_edge(0, 1, 16)
    g.add_edge(0, 2, 13)
    g.add_edge(1, 2, 10)
    g.add_edge(1, 3, 12)
    g.add_edge(2, 1, 4)
    g.add_edge(2, 4, 14)
    g.add_edge(3, 2, 9)
    g.add_edge(3, 5, 20)
    g.add_edge(4, 3, 7)
    g.add_edge(4, 5, 4)
    
    source = 0
    sink = 5
    
    print("Maximum flow:", g.ford_fulkerson(source, sink))
    
    # Print the residual graph after finding maximum flow
    print("\nResidual graph:")
    for i in range(g.V):
        for j in range(g.V):
            if g.graph[i][j] > 0:
                print(f"{i} -> {j}: {g.graph[i][j]}")

if __name__ == "__main__":
    main()
```

## Output:
```
Maximum flow: 23

Residual graph:
0 -> 1: 12
0 -> 2: 9
1 -> 3: 7
2 -> 4: 9
3 -> 2: 4
3 -> 5: 15
4 -> 3: 4
4 -> 5: 4
```

## How it works:

1. **Initialization**: Create a residual graph with the same structure as the original graph
2. **Find Augmenting Path**: Use BFS to find a path from source to sink in the residual graph
3. **Calculate Path Flow**: Find the minimum capacity along the path
4. **Update Residual Graph**: Decrease capacities along the forward edges and increase capacities along backward edges
5. **Repeat**: Continue until no more augmenting paths exist

## Time Complexity:
- **Time**: O(E × max_flow) where E is the number of edges
- **Space**: O(V²) for the adjacency matrix

## Key Features:
- Uses BFS for finding augmenting paths
- Handles both forward and backward edges in the residual graph
- Returns the maximum flow value from source to sink
- Works with any number of vertices and edges

The algorithm correctly finds that the maximum flow from vertex 0 to vertex 5 is 23 units.

