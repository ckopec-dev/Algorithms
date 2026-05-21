# Depth-First Search (DFS) Algorithm in Python

## Graph Representation

```python
# Adjacency list representation of a graph
graph = {
    'A': ['B', 'C'],
    'B': ['A', 'D', 'E'],
    'C': ['A', 'F'],
    'D': ['B'],
    'E': ['B', 'F'],
    'F': ['C', 'E']
}
```

## Recursive DFS Implementation

```python
def dfs_recursive(graph, start, visited=None):
    """
    Recursive implementation of Depth-First Search
    
    Args:
        graph: Dictionary representing adjacency list
        start: Starting node
        visited: Set of visited nodes
    
    Returns:
        List of nodes in DFS order
    """
    if visited is None:
        visited = set()
    
    visited.add(start)
    result = [start]
    
    for neighbor in graph[start]:
        if neighbor not in visited:
            result.extend(dfs_recursive(graph, neighbor, visited))
    
    return result

# Example usage
print("Recursive DFS:", dfs_recursive(graph, 'A'))
# Output: ['A', 'B', 'D', 'E', 'F', 'C']
```

## Iterative DFS Implementation

```python
def dfs_iterative(graph, start):
    """
    Iterative implementation of Depth-First Search using stack
    
    Args:
        graph: Dictionary representing adjacency list
        start: Starting node
    
    Returns:
        List of nodes in DFS order
    """
    visited = set()
    stack = [start]
    result = []
    
    while stack:
        node = stack.pop()
        
        if node not in visited:
            visited.add(node)
            result.append(node)
            
            # Add neighbors to stack (in reverse order for consistent traversal)
            for neighbor in reversed(graph[node]):
                if neighbor not in visited:
                    stack.append(neighbor)
    
    return result

# Example usage
print("Iterative DFS:", dfs_iterative(graph, 'A'))
# Output: ['A', 'B', 'D', 'E', 'F', 'C']
```

## Complete Example with Visualization

```python
def dfs_with_path(graph, start, target=None):
    """
    DFS that can find a path to a target node
    
    Args:
        graph: Dictionary representing adjacency list
        start: Starting node
        target: Target node (optional)
    
    Returns:
        Path from start to target, or all visited nodes if no target
    """
    visited = set()
    stack = [start]
    path = []
    
    while stack:
        node = stack.pop()
        
        if node not in visited:
            visited.add(node)
            path.append(node)
            
            if target and node == target:
                return path
            
            # Add neighbors to stack
            for neighbor in reversed(graph[node]):
                if neighbor not in visited:
                    stack.append(neighbor)
    
    return path

# Example usage
print("DFS path to F:", dfs_with_path(graph, 'A', 'F'))
# Output: ['A', 'B', 'E', 'F']

print("All DFS traversal:", dfs_with_path(graph, 'A'))
# Output: ['A', 'B', 'D', 'E', 'F', 'C']
```

## Time and Space Complexity

- **Time Complexity**: O(V + E) where V is vertices and E is edges
- **Space Complexity**: O(V) for the visited set and recursion stack

## Key Characteristics

- **Traversal Order**: Goes deep into one branch before exploring others
- **Stack Usage**: Uses LIFO (Last In, First Out) principle
- **Memory Usage**: Generally uses less memory than BFS for deep graphs
- **Applications**: Finding connected components, topological sorting, cycle detection

The DFS algorithm explores as far as possible along each branch before backtracking, making it ideal for problems requiring pathfinding or exploring all possible routes in a graph.

