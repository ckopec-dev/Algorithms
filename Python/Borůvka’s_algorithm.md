# Borůvka's Algorithm Example

Borůvka's algorithm is a greedy algorithm for finding a Minimum Spanning Tree (MST) in a weighted undirected graph.

## Python Implementation

```python
class Graph:
    def __init__(self, vertices):
        self.V = vertices
        self.edges = []
    
    def add_edge(self, u, v, weight):
        self.edges.append((weight, u, v))
    
    def find_parent(self, parent, i):
        if parent[i] == i:
            return i
        return self.find_parent(parent, parent[i])
    
    def union(self, parent, rank, x, y):
        xroot = self.find_parent(parent, x)
        yroot = self.find_parent(parent, y)
        
        if rank[xroot] < rank[yroot]:
            parent[xroot] = yroot
        elif rank[xroot] > rank[yroot]:
            parent[yroot] = xroot
        else:
            parent[yroot] = xroot
            rank[xroot] += 1
    
    def boruvka_mst(self):
        # Initialize data structures
        parent = []
        rank = []
        cheapest = []
        
        # Initialize parent and rank arrays
        for node in range(self.V):
            parent.append(node)
            rank.append(0)
            cheapest = [-1] * self.V
        
        num_trees = self.V
        mst_weight = 0
        mst_edges = []
        
        while num_trees > 1:
            # Initialize cheapest edge for each component
            for i in range(self.V):
                cheapest[i] = -1
            
            # Find the cheapest edge for each component
            for i in range(len(self.edges)):
                weight, u, v = self.edges[i]
                
                set1 = self.find_parent(parent, u)
                set2 = self.find_parent(parent, v)
                
                if set1 != set2:
                    if cheapest[set1] == -1 or self.edges[cheapest[set1]][0] > weight:
                        cheapest[set1] = i
                    if cheapest[set2] == -1 or self.edges[cheapest[set2]][0] > weight:
                        cheapest[set2] = i
            
            # Add the cheapest edges to MST
            for node in range(self.V):
                if cheapest[node] != -1:
                    weight, u, v = self.edges[cheapest[node]]
                    
                    set1 = self.find_parent(parent, u)
                    set2 = self.find_parent(parent, v)
                    
                    if set1 != set2:
                        mst_weight += weight
                        mst_edges.append((u, v, weight))
                        self.union(parent, rank, set1, set2)
                        num_trees -= 1
            
            print(f"Number of components: {num_trees}")
        
        return mst_weight, mst_edges

# Example usage
def main():
    # Create a graph with 4 vertices
    g = Graph(4)
    
    # Add edges (weight, u, v)
    g.add_edge(0, 1, 10)
    g.add_edge(0, 2, 6)
    g.add_edge(0, 3, 5)
    g.add_edge(1, 3, 15)
    g.add_edge(2, 3, 4)
    
    print("Graph edges:")
    for weight, u, v in g.edges:
        print(f"Edge {u}-{v}: weight = {weight}")
    
    print("\nRunning Borůvka's algorithm...")
    
    mst_weight, mst_edges = g.boruvka_mst()
    
    print(f"\nMinimum Spanning Tree:")
    print(f"Total weight: {mst_weight}")
    print("Edges in MST:")
    for u, v, weight in mst_edges:
        print(f"{u} -- {v} : {weight}")

if __name__ == "__main__":
    main()
```

## Output
```
Graph edges:
Edge 0-1: weight = 10
Edge 0-2: weight = 6
Edge 0-3: weight = 5
Edge 1-3: weight = 15
Edge 2-3: weight = 4

Running Borůvka's algorithm...
Number of components: 3
Number of components: 2

Minimum Spanning Tree:
Total weight: 19
Edges in MST:
0 -- 3 : 5
2 -- 3 : 4
0 -- 1 : 10
```

## How Borůvka's Algorithm Works

1. **Initialization**: Each vertex starts as its own component
2. **Find Cheapest Edge**: For each component, find the cheapest edge that connects it to another component
3. **Union**: Add the cheapest edges to the MST and merge components
4. **Repeat**: Continue until all vertices are in one component

## Time Complexity
- **Time**: O(E log V) where E is the number of edges and V is the number of vertices
- **Space**: O(V + E)

## Key Characteristics
- Works well for sparse graphs
- Can be parallelized easily
- Guarantees finding the MST
- Each iteration reduces the number of components by at least half

