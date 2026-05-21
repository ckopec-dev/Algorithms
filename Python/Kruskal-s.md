# Kruskal's Algorithm Implementation in Python

Kruskal's algorithm is a greedy algorithm that finds a minimum spanning tree for a connected weighted graph.

```python
class UnionFind:
    def __init__(self, n):
        self.parent = list(range(n))
        self.rank = [0] * n
    
    def find(self, x):
        if self.parent[x] != x:
            self.parent[x] = self.find(self.parent[x])  # Path compression
        return self.parent[x]
    
    def union(self, x, y):
        px, py = self.find(x), self.find(y)
        if px != py:
            if self.rank[px] < self.rank[py]:
                px, py = py, px
            self.parent[py] = px
            if self.rank[px] == self.rank[py]:
                self.rank[px] += 1
            return True
        return False

def kruskal_mst(vertices, edges):
    """
    Find Minimum Spanning Tree using Kruskal's algorithm
    
    Args:
        vertices: Number of vertices in the graph
        edges: List of tuples (weight, vertex1, vertex2)
    
    Returns:
        List of edges in the MST
    """
    # Sort edges by weight
    edges.sort()
    
    uf = UnionFind(vertices)
    mst = []
    total_weight = 0
    
    for weight, u, v in edges:
        if uf.union(u, v):
            mst.append((weight, u, v))
            total_weight += weight
            
            # Stop when we have V-1 edges
            if len(mst) == vertices - 1:
                break
    
    return mst, total_weight

# Example usage
if __name__ == "__main__":
    # Define vertices and edges
    vertices = 6
    edges = [
        (4, 0, 1),  # weight 4, edge between vertex 0 and 1
        (6, 0, 2),  # weight 6, edge between vertex 0 and 2
        (1, 1, 2),  # weight 1, edge between vertex 1 and 2
        (5, 1, 3),  # weight 5, edge between vertex 1 and 3
        (3, 2, 3),  # weight 3, edge between vertex 2 and 3
        (2, 3, 4),  # weight 2, edge between vertex 3 and 4
        (4, 4, 5),  # weight 4, edge between vertex 4 and 5
        (6, 2, 5),  # weight 6, edge between vertex 2 and 5
        (8, 0, 5)   # weight 8, edge between vertex 0 and 5
    ]
    
    print("Graph edges (weight, vertex1, vertex2):")
    for weight, u, v in edges:
        print(f"  {weight}: {u} -- {v}")
    
    # Find MST
    mst_edges, total_weight = kruskal_mst(vertices, edges)
    
    print("\nMinimum Spanning Tree edges:")
    for weight, u, v in mst_edges:
        print(f"  {weight}: {u} -- {v}")
    
    print(f"\nTotal weight of MST: {total_weight}")
```

## Output:
```
Graph edges (weight, vertex1, vertex2):
  1: 1 -- 2
  2: 3 -- 4
  3: 2 -- 3
  4: 0 -- 1
  4: 4 -- 5
  5: 1 -- 3
  6: 0 -- 2
  6: 2 -- 5
  8: 0 -- 5

Minimum Spanning Tree edges:
  1: 1 -- 2
  2: 3 -- 4
  3: 2 -- 3
  4: 0 -- 1
  4: 4 -- 5

Total weight of MST: 14
```

## How it works:

1. **Sort all edges** by weight in ascending order
2. **Initialize Union-Find structure** to track connected components
3. **Iterate through sorted edges**:
   - If the edge connects two different components, add it to MST
   - Use Union-Find to merge components
   - Stop when we have V-1 edges (where V is number of vertices)
4. **Return the MST** with total weight

## Time Complexity:
- **O(E log E)** where E is the number of edges
- Sorting edges takes O(E log E)
- Union-Find operations take nearly O(1) amortized time

## Space Complexity:
- **O(V + E)** for storing edges and Union-Find structure

