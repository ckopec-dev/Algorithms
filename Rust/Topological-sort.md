# Topological Sort in Rust

Here's an example implementation of topological sort using Kahn's algorithm in Rust:

```rust
use std::collections::{HashMap, VecDeque};

fn topological_sort(vertices: usize, edges: &[(usize, usize)]) -> Result<Vec<usize>, String> {
    // Build adjacency list and in-degree count
    let mut adj_list: HashMap<usize, Vec<usize>> = HashMap::new();
    let mut in_degree = vec![0; vertices];
    
    // Initialize adjacency list and in-degrees
    for &(from, to) in edges {
        if from >= vertices || to >= vertices {
            return Err("Vertex index out of bounds".to_string());
        }
        adj_list.entry(from).or_insert_with(Vec::new).push(to);
        in_degree[to] += 1;
    }
    
    // Find all vertices with in-degree 0
    let mut queue = VecDeque::new();
    for i in 0..vertices {
        if in_degree[i] == 0 {
            queue.push_back(i);
        }
    }
    
    let mut result = Vec::new();
    
    // Process vertices in topological order
    while let Some(vertex) = queue.pop_front() {
        result.push(vertex);
        
        // Remove current vertex and update in-degrees
        if let Some(neighbors) = adj_list.get(&vertex) {
            for &neighbor in neighbors {
                in_degree[neighbor] -= 1;
                if in_degree[neighbor] == 0 {
                    queue.push_back(neighbor);
                }
            }
        }
    }
    
    // Check if all vertices were processed (no cycles)
    if result.len() == vertices {
        Ok(result)
    } else {
        Err("Cycle detected in the graph".to_string())
    }
}

fn main() {
    // Example: Task scheduling with dependencies
    // Tasks: 0, 1, 2, 3, 4
    // Dependencies: 0->1, 0->2, 1->3, 2->3, 3->4
    let vertices = 5;
    let edges = vec![(0, 1), (0, 2), (1, 3), (2, 3), (3, 4)];
    
    match topological_sort(vertices, &edges) {
        Ok(order) => {
            println!("Topological order: {:?}", order);
            // Output: Topological order: [0, 1, 2, 3, 4]
        }
        Err(e) => println!("Error: {}", e),
    }
    
    // Example with cycle detection
    let edges_with_cycle = vec![(0, 1), (1, 2), (2, 0)];
    match topological_sort(3, &edges_with_cycle) {
        Ok(order) => println!("Topological order: {:?}", order),
        Err(e) => println!("Error: {}", e),
        // Output: Error: Cycle detected in the graph
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_simple_topological_sort() {
        let edges = vec![(0, 1), (0, 2), (1, 3), (2, 3), (3, 4)];
        let result = topological_sort(5, &edges).unwrap();
        assert_eq!(result, vec![0, 1, 2, 3, 4]);
    }
    
    #[test]
    fn test_cycle_detection() {
        let edges = vec![(0, 1), (1, 2), (2, 0)];
        let result = topological_sort(3, &edges);
        assert!(result.is_err());
    }
    
    #[test]
    fn test_empty_graph() {
        let edges: Vec<(usize, usize)> = vec![];
        let result = topological_sort(3, &edges).unwrap();
        assert_eq!(result, vec![0, 1, 2]);
    }
}
```

## How it works:

1. **Build Graph Representation**: Create an adjacency list and in-degree count for each vertex
2. **Find Starting Points**: Identify all vertices with in-degree 0 (no dependencies)
3. **Process Vertices**: Use a queue to process vertices in topological order
4. **Update Dependencies**: Remove processed vertices and update in-degrees of neighbors
5. **Cycle Detection**: If not all vertices are processed, a cycle exists

## Key Features:

- **Cycle Detection**: Returns an error when a cycle is detected
- **Error Handling**: Proper error handling for invalid inputs
- **Generic**: Works with any number of vertices
- **Efficient**: O(V + E) time complexity
- **Tested**: Includes unit tests for various scenarios

## Time Complexity:
- **Time**: O(V + E) where V is vertices and E is edges
- **Space**: O(V + E) for the adjacency list and in-degree array

This implementation is commonly used for task scheduling, dependency resolution, and detecting circular dependencies in systems.

