# SPFA Algorithm Implementation in Rust

Here's a complete implementation of the Shortest Path Faster Algorithm (SPFA) in Rust:

```rust
use std::collections::VecDeque;

#[derive(Debug, Clone)]
struct Edge {
    to: usize,
    weight: i32,
}

fn spfa(graph: &[Vec<Edge>], start: usize, n: usize) -> Vec<Option<i32>> {
    let mut dist = vec![None; n];
    let mut in_queue = vec![false; n];
    let mut queue = VecDeque::new();
    
    // Initialize distances
    dist[start] = Some(0);
    queue.push_back(start);
    in_queue[start] = true;
    
    while let Some(u) = queue.pop_front() {
        in_queue[u] = false;
        
        // Relax all edges from current node
        for edge in &graph[u] {
            let v = edge.to;
            let weight = edge.weight;
            
            // Check if we can improve the distance to v
            if let Some(current_dist) = dist[u] {
                let new_dist = current_dist + weight;
                
                match dist[v] {
                    None => {
                        // First time reaching v
                        dist[v] = Some(new_dist);
                        if !in_queue[v] {
                            queue.push_back(v);
                            in_queue[v] = true;
                        }
                    }
                    Some(existing_dist) => {
                        if new_dist < existing_dist {
                            // Found a shorter path
                            dist[v] = Some(new_dist);
                            if !in_queue[v] {
                                queue.push_back(v);
                                in_queue[v] = true;
                            }
                        }
                    }
                }
            }
        }
    }
    
    dist
}

fn main() {
    // Example graph representation
    // Graph with 5 nodes (0, 1, 2, 3, 4)
    let graph = vec![
        vec![Edge { to: 1, weight: 4 }, Edge { to: 2, weight: 2 }],           // Node 0
        vec![Edge { to: 2, weight: 1 }, Edge { to: 3, weight: 5 }],           // Node 1
        vec![Edge { to: 3, weight: 8 }, Edge { to: 4, weight: 10 }],          // Node 2
        vec![Edge { to: 4, weight: 2 }],                                       // Node 3
        vec![],                                                                 // Node 4
    ];
    
    let start_node = 0;
    let num_nodes = 5;
    
    let distances = spfa(&graph, start_node, num_nodes);
    
    println!("Shortest distances from node {}:", start_node);
    for (i, dist) in distances.iter().enumerate() {
        match dist {
            Some(d) => println!("To node {}: {}", i, d),
            None => println!("To node {}: unreachable", i),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_spfa_basic() {
        let graph = vec![
            vec![Edge { to: 1, weight: 4 }, Edge { to: 2, weight: 2 }],
            vec![Edge { to: 2, weight: 1 }, Edge { to: 3, weight: 5 }],
            vec![Edge { to: 3, weight: 8 }, Edge { to: 4, weight: 10 }],
            vec![Edge { to: 4, weight: 2 }],
            vec![],
        ];
        
        let result = spfa(&graph, 0, 5);
        assert_eq!(result[0], Some(0));
        assert_eq!(result[1], Some(4));
        assert_eq!(result[2], Some(2));
        assert_eq!(result[3], Some(9));
        assert_eq!(result[4], Some(11));
    }
    
    #[test]
    fn test_spfa_single_node() {
        let graph = vec![vec![]];
        let result = spfa(&graph, 0, 1);
        assert_eq!(result[0], Some(0));
    }
}
```

## Key Features of this Implementation:

1. **Graph Representation**: Uses an adjacency list with `Vec<Vec<Edge>>` where each edge contains destination node and weight
2. **Queue Management**: Uses `VecDeque` for efficient queue operations
3. **Distance Tracking**: Maintains distances in a `Vec<Option<i32>>` to handle unreachable nodes
4. **Cycle Detection**: Uses `in_queue` boolean array to prevent duplicate entries in queue
5. **Edge Relaxation**: Implements the core SPFA relaxation logic

## Time and Space Complexity:

- **Time Complexity**: O(kE) where k is a small constant, worst case O(VE)
- **Space Complexity**: O(V + E) for the graph representation and auxiliary arrays

## Usage:

The algorithm finds shortest paths from a given source node to all other nodes in a weighted directed graph, handling negative edge weights (unlike Dijkstra's algorithm).

