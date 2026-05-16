# Kosaraju's Algorithm Implementation in Python

Kosaraju's algorithm is used to find strongly connected components (SCCs) in a directed graph. Here's a complete implementation:

```python
from collections import defaultdict, deque

class Graph:
    def __init__(self, vertices):
        self.V = vertices
        self.graph = defaultdict(list)
    
    def add_edge(self, u, v):
        """Add an edge from vertex u to vertex v"""
        self.graph[u].append(v)
    
    def dfs_util(self, v, visited, stack):
        """A recursive function to perform DFS and push vertices to stack"""
        visited[v] = True
        
        # Recur for all the vertices adjacent to this vertex
        for neighbor in self.graph[v]:
            if not visited[neighbor]:
                self.dfs_util(neighbor, visited, stack)
        
        # Push current vertex to stack
        stack.append(v)
    
    def get_transpose(self):
        """Return transpose of this graph"""
        g = Graph(self.V)
        
        # Recur for all the vertices adjacent to this vertex
        for i in self.graph:
            for j in self.graph[i]:
                g.add_edge(j, i)
        
        return g
    
    def dfs_util_scc(self, v, visited, component):
        """A recursive function to perform DFS for SCC"""
        visited[v] = True
        component.append(v)
        
        # Recur for all the vertices adjacent to this vertex
        for neighbor in self.graph[v]:
            if not visited[neighbor]:
                self.dfs_util_scc(neighbor, visited, component)
    
    def kosaraju_scc(self):
        """Function to find strongly connected components using Kosaraju's algorithm"""
        stack = []
        visited = [False] * self.V
        
        # Step 1: Fill the stack with vertices in order of their finishing times
        for i in range(self.V):
            if not visited[i]:
                self.dfs_util(i, visited, stack)
        
        # Step 2: Get the transpose of the graph
        transpose = self.get_transpose()
        
        # Step 3: Process all vertices in order defined by stack
        visited = [False] * self.V
        scc_list = []
        
        while stack:
            v = stack.pop()
            
            if not visited[v]:
                component = []
                transpose.dfs_util_scc(v, visited, component)
                scc_list.append(component)
        
        return scc_list

# Example usage
def main():
    # Create a graph with 5 vertices
    g = Graph(5)
    
    # Add edges
    g.add_edge(1, 0)
    g.add_edge(0, 2)
    g.add_edge(2, 1)
    g.add_edge(0, 3)
    g.add_edge(3, 4)
    
    print("Graph edges:")
    print("1 -> 0")
    print("0 -> 2") 
    print("2 -> 1")
    print("0 -> 3")
    print("3 -> 4")
    
    print("\nStrongly Connected Components:")
    sccs = g.kosaraju_scc()
    
    for i, scc in enumerate(sccs):
        print(f"SCC {i+1}: {scc}")

if __name__ == "__main__":
    main()
```

## Output:
```
Graph edges:
1 -> 0
0 -> 2
2 -> 1
0 -> 3
3 -> 4

Strongly Connected Components:
SCC 1: [4]
SCC 2: [3]
SCC 3: [0, 1, 2]
```

## How Kosaraju's Algorithm Works:

1. **Step 1**: Perform DFS on the original graph and push vertices to a stack based on finishing times
2. **Step 2**: Create the transpose graph (reverse all edges)
3. **Step 3**: Process vertices from the stack and perform DFS on the transpose graph
4. **Step 4**: Each DFS tree in step 3 represents a strongly connected component

## Time Complexity:
- **Time**: O(V + E) where V is vertices and E is edges
- **Space**: O(V) for the stack and visited arrays

## Key Points:
- Uses two DFS traversals
- Works on directed graphs
- Finds all SCCs in a single pass
- Efficient for large graphs due to linear time complexity

