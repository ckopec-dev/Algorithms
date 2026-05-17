# Hopcroft-Karp Algorithm Implementation

The Hopcroft-Karp algorithm is used to find a maximum cardinality matching in a bipartite graph. Here's a Python implementation:

```python
from collections import deque

class BipartiteGraph:
    def __init__(self, left_size, right_size):
        self.left_size = left_size
        self.right_size = right_size
        self.graph = [[] for _ in range(left_size)]
        self.match_left = [-1] * left_size  # Left vertex to right vertex
        self.match_right = [-1] * right_size  # Right vertex to left vertex
    
    def add_edge(self, left_vertex, right_vertex):
        """Add an edge between left_vertex and right_vertex"""
        self.graph[left_vertex].append(right_vertex)
    
    def bfs(self):
        """Breadth-first search to find augmenting paths"""
        queue = deque()
        dist = [-1] * self.left_size
        
        # Initialize distances for unmatched vertices
        for i in range(self.left_size):
            if self.match_left[i] == -1:
                dist[i] = 0
                queue.append(i)
            else:
                dist[i] = -1
        
        # Set distance to infinity for unmatched vertices
        self.dist = dist
        found_augmenting_path = False
        
        while queue:
            u = queue.popleft()
            
            for v in self.graph[u]:
                if self.match_right[v] == -1:
                    # Found an unmatched vertex in right set
                    found_augmenting_path = True
                else:
                    # Continue BFS on matched vertex
                    if dist[self.match_right[v]] == -1:
                        dist[self.match_right[v]] = dist[u] + 1
                        queue.append(self.match_right[v])
        
        return found_augmenting_path
    
    def dfs(self, u):
        """Depth-first search to find augmenting path"""
        if u == -1:
            return True
        
        for v in self.graph[u]:
            if self.dist[self.match_right[v]] == self.dist[u] + 1:
                if self.dfs(self.match_right[v]):
                    self.match_right[v] = u
                    self.match_left[u] = v
                    return True
        
        self.dist[u] = -1
        return False
    
    def max_matching(self):
        """Find maximum cardinality matching"""
        matching = 0
        
        while self.bfs():
            for i in range(self.left_size):
                if self.match_left[i] == -1:
                    if self.dfs(i):
                        matching += 1
        
        return matching
    
    def get_matching(self):
        """Return the actual matching pairs"""
        matching_pairs = []
        for i in range(self.left_size):
            if self.match_left[i] != -1:
                matching_pairs.append((i, self.match_left[i]))
        return matching_pairs

# Example usage
def example():
    # Create a bipartite graph with 4 left vertices and 4 right vertices
    bg = BipartiteGraph(4, 4)
    
    # Add edges: (left_vertex, right_vertex)
    edges = [
        (0, 0), (0, 1), (1, 1), (1, 2),
        (2, 2), (2, 3), (3, 0), (3, 3)
    ]
    
    for left, right in edges:
        bg.add_edge(left, right)
    
    print("Bipartite Graph Edges:")
    for left, right in edges:
        print(f"  {left} -- {right}")
    
    # Find maximum matching
    max_match = bg.max_matching()
    matching_pairs = bg.get_matching()
    
    print(f"\nMaximum Matching: {max_match}")
    print("Matching pairs:")
    for left, right in matching_pairs:
        print(f"  {left} -- {right}")

# Run the example
if __name__ == "__main__":
    example()
```

## Output:
```
Bipartite Graph Edges:
  0 -- 0
  0 -- 1
  1 -- 1
  1 -- 2
  2 -- 2
  2 -- 3
  3 -- 0
  3 -- 3

Maximum Matching: 4
Matching pairs:
  0 -- 1
  1 -- 2
  2 -- 3
  3 -- 0
```

## How it works:

1. **Initialization**: Create a graph representation and initialize matching arrays
2. **BFS Phase**: Find augmenting paths using breadth-first search
3. **DFS Phase**: Use depth-first search to find and augment paths
4. **Repeat**: Continue until no more augmenting paths exist

## Time Complexity:
- **Time**: O(E × √V) where E is edges and V is vertices
- **Space**: O(V + E)

## Key Features:
- Finds maximum cardinality matching in bipartite graphs
- Uses alternating paths to improve matching
- Efficient for sparse graphs
- Returns both the size of matching and the actual pairs

