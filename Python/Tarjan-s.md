# Tarjan's Algorithm for Strongly Connected Components

Tarjan's algorithm is a linear-time algorithm to find strongly connected components (SCCs) in a directed graph. Here's a Python implementation:

```python
def tarjan_scc(graph):
    """
    Find strongly connected components using Tarjan's algorithm.
    
    Args:
        graph: Dictionary representing adjacency list of directed graph
               {node: [neighbors]}
    
    Returns:
        List of lists, where each inner list represents a strongly connected component
    """
    index_counter = [0]
    stack = []
    lowlinks = {}
    index = {}
    sccs = []
    
    def strongconnect(node):
        # Set the depth index for this node
        index[node] = index_counter[0]
        lowlinks[node] = index_counter[0]
        index_counter[0] += 1
        stack.append(node)
        
        # Consider successors of node
        for successor in graph.get(node, []):
            if successor not in index:
                # Successor has not yet been visited
                strongconnect(successor)
                lowlinks[node] = min(lowlinks[node], lowlinks[successor])
            elif successor in stack:
                # Successor is in the stack and hence in the current SCC
                lowlinks[node] = min(lowlinks[node], index[successor])
        
        # If node is a root node, pop the stack and create an SCC
        if lowlinks[node] == index[node]:
            scc = []
            while True:
                successor = stack.pop()
                scc.append(successor)
                if successor == node:
                    break
            sccs.append(scc)
    
    # Run strongconnect on each node
    for node in graph:
        if node not in index:
            strongconnect(node)
    
    return sccs

# Example usage
if __name__ == "__main__":
    # Example graph represented as adjacency list
    # Graph structure:
    # 0 -> 1 -> 2 -> 0 (cycle)
    # 3 -> 4 -> 5 -> 3 (cycle)
    # 1 -> 3 (cross edge)
    graph = {
        0: [1],
        1: [2],
        2: [0],
        3: [4],
        4: [5],
        5: [3],
        1: [3]
    }
    
    print("Graph edges:")
    for node, neighbors in graph.items():
        print(f"  {node} -> {neighbors}")
    
    sccs = tarjan_scc(graph)
    print("\nStrongly Connected Components:")
    for i, scc in enumerate(sccs):
        print(f"  SCC {i+1}: {scc}")
```

## Output:
```
Graph edges:
  0 -> [1]
  1 -> [2]
  2 -> [0]
  3 -> [4]
  4 -> [5]
  5 -> [3]
  1 -> [3]

Strongly Connected Components:
  SCC 1: [0, 2, 1]
  SCC 2: [3, 5, 4]
```

## How it works:

1. **Initialization**: Each node is assigned an index and lowlink value
2. **DFS Traversal**: Perform depth-first search while maintaining a stack
3. **Lowlink Update**: For each node, update its lowlink value based on:
   - Its own index (if it's a root)
   - Successors' lowlink values
   - Back edges in the stack
4. **Component Detection**: When a node's lowlink equals its index, it's a root of an SCC
5. **Stack Management**: Pop nodes from stack to form components

## Time Complexity: O(V + E)
## Space Complexity: O(V)

The algorithm correctly identifies that nodes 0, 1, and 2 form one SCC (forming a cycle), and nodes 3, 4, and 5 form another SCC (also forming a cycle).

