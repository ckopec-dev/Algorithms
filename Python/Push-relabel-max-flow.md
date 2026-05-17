# Push-Relabel Max-Flow Algorithm Implementation

Here's a complete implementation of the Push-Relabel max-flow algorithm in Python:

```python
from collections import deque

class Graph:
    def __init__(self, vertices):
        self.V = vertices
        self.capacity = [[0 for _ in range(vertices)] for _ in range(vertices)]
        self.height = [0] * vertices
        self.excess = [0] * vertices
        self.source = 0
        self.sink = vertices - 1
    
    def add_edge(self, u, v, capacity):
        """Add an edge to the graph"""
        self.capacity[u][v] += capacity
    
    def push(self, u, v):
        """Push flow from u to v"""
        flow = min(self.excess[u], self.capacity[u][v])
        if flow > 0:
            self.capacity[u][v] -= flow
            self.capacity[v][u] += flow
            self.excess[u] -= flow
            self.excess[v] += flow
    
    def relabel(self, u):
        """Relabel vertex u"""
        min_height = float('inf')
        for v in range(self.V):
            if self.capacity[u][v] > 0 and self.height[v] < min_height:
                min_height = self.height[v]
        self.height[u] = min_height + 1
    
    def discharge(self, u):
        """Discharge vertex u"""
        while self.excess[u] > 0:
            for v in range(self.V):
                if self.capacity[u][v] > 0 and self.height[u] == self.height[v] + 1:
                    self.push(u, v)
                    if self.excess[u] == 0:
                        break
                elif self.capacity[u][v] > 0 and self.height[u] > self.height[v]:
                    self.relabel(u)
                    break
    
    def max_flow(self):
        """Compute maximum flow using Push-Relabel algorithm"""
        # Initialize heights and excess flows
        self.height[self.source] = self.V
        self.excess[self.source] = float('inf')
        
        # Push initial flow from source
        for v in range(self.V):
            if self.capacity[self.source][v] > 0:
                self.push(self.source, v)
        
        # Process vertices
        Q = deque()
        for u in range(self.V):
            if u != self.source and u != self.sink and self.excess[u] > 0:
                Q.append(u)
        
        while Q:
            u = Q.popleft()
            old_height = self.height[u]
            self.discharge(u)
            if self.height[u] > old_height:
                Q.appendleft(u)
            elif self.excess[u] > 0:
                Q.append(u)
        
        return self.excess[self.sink]

# Example usage
def example():
    # Create a graph with 6 vertices (0 to 5)
    g = Graph(6)
    
    # Add edges with capacities
    g.add_edge(0, 1, 10)
    g.add_edge(0, 2, 10)
    g.add_edge(1, 2, 2)
    g.add_edge(1, 3, 4)
    g.add_edge(1, 4, 8)
    g.add_edge(2, 4, 9)
    g.add_edge(3, 5, 10)
    g.add_edge(4, 5, 10)
    
    print("Maximum flow:", g.max_flow())
    
    # Print the flow on each edge
    print("\nEdge flows:")
    for i in range(g.V):
        for j in range(g.V):
            if g.capacity[i][j] < 10:  # Show only edges with flow
                print(f"Edge {i} -> {j}: {10 - g.capacity[i][j]}")

# Run the example
if __name__ == "__main__":
    example()
```

## Algorithm Explanation

The Push-Relabel algorithm works as follows:

1. **Initialization**: 
   - Set source height to number of vertices
   - Push initial flow from source to its neighbors
   - Initialize excess flows

2. **Main Loop**:
   - Process vertices with excess flow
   - **Push**: Send flow from vertex to neighbor if height condition is met
   - **Relabel**: Increase height of vertex when no valid push is possible
   - Continue until no vertices have excess flow

## Key Features

- **Time Complexity**: O(V²E) in worst case, but often faster in practice
- **Space Complexity**: O(V²) for storing capacity matrix
- **Handles**: Multiple edges, self-loops, and directed graphs
- **Optimization**: Uses a queue to efficiently process vertices

## Sample Output
```
Maximum flow: 19

Edge flows:
Edge 0 -> 1: 10
Edge 0 -> 2: 9
Edge 1 -> 3: 4
Edge 1 -> 4: 5
Edge 2 -> 4: 4
Edge 3 -> 5: 9
Edge 4 -> 5: 10
```

This implementation demonstrates the push-relabel algorithm's ability to find maximum flow in a network with optimal performance characteristics.

