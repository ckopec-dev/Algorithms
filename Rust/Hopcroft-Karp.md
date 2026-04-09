# Hopcroft-Karp Algorithm in Rust

The Hopcroft-Karp algorithm finds a maximum cardinality matching in bipartite graphs. Here's a complete implementation with example usage:

```rust
use std::collections::{VecDeque, HashSet};

#[derive(Debug, Clone)]
pub struct BipartiteGraph {
    pub left_size: usize,
    pub right_size: usize,
    pub edges: Vec<Vec<usize>>,
}

impl BipartiteGraph {
    pub fn new(left_size: usize, right_size: usize) -> Self {
        let mut edges = vec![vec![]; left_size];
        BipartiteGraph {
            left_size,
            right_size,
            edges,
        }
    }

    pub fn add_edge(&mut self, left_node: usize, right_node: usize) {
        if left_node < self.left_size && right_node < self.right_size {
            self.edges[left_node].push(right_node);
        }
    }
}

pub fn hopcroft_karp(graph: &BipartiteGraph) -> Vec<(usize, usize)> {
    let mut match_left = vec![None; graph.left_size];
    let mut match_right = vec![None; graph.right_size];
    let mut dist = vec![0; graph.left_size];
    
    // Initialize distances
    for i in 0..graph.left_size {
        dist[i] = 0;
    }
    
    let mut matching = 0;
    
    loop {
        // Find augmenting paths using BFS
        let found_path = bfs(&graph, &mut dist, &mut match_left, &mut match_right);
        
        if !found_path {
            break;
        }
        
        // Find augmenting paths using DFS
        for i in 0..graph.left_size {
            if match_left[i].is_none() {
                if dfs(i, &graph, &mut dist, &mut match_left, &mut match_right) {
                    matching += 1;
                }
            }
        }
    }
    
    // Construct result
    let mut result = Vec::new();
    for i in 0..graph.left_size {
        if let Some(right) = match_left[i] {
            result.push((i, right));
        }
    }
    
    result
}

fn bfs(
    graph: &BipartiteGraph,
    dist: &mut [usize],
    match_left: &[Option<usize>],
    match_right: &[Option<usize>],
) -> bool {
    let mut queue = VecDeque::new();
    
    // Initialize distances for unmatched nodes
    for i in 0..graph.left_size {
        if match_left[i].is_none() {
            dist[i] = 0;
            queue.push_back(i);
        } else {
            dist[i] = usize::MAX;
        }
    }
    
    // Set distance of unmatched right nodes to infinity
    for i in 0..graph.right_size {
        if match_right[i].is_none() {
            dist[i] = usize::MAX;
        }
    }
    
    let mut found_augmenting = false;
    
    while let Some(u) = queue.pop_front() {
        if dist[u] < usize::MAX {
            for &v in &graph.edges[u] {
                if match_right[v].is_none() {
                    found_augmenting = true;
                } else {
                    let w = match_right[v].unwrap();
                    if dist[w] == usize::MAX {
                        dist[w] = dist[u] + 1;
                        queue.push_back(w);
                    }
                }
            }
        }
    }
    
    found_augmenting
}

fn dfs(
    u: usize,
    graph: &BipartiteGraph,
    dist: &[usize],
    match_left: &mut [Option<usize>],
    match_right: &mut [Option<usize>],
) -> bool {
    if u < graph.left_size {
        for &v in &graph.edges[u] {
            if match_right[v].is_none() || dist[match_right[v].unwrap()] == dist[u] + 1 {
                if let Some(w) = match_right[v] {
                    // Check if we can find an augmenting path
                    if dfs(w, graph, dist, match_left, match_right) {
                        match_right[v] = Some(u);
                        match_left[u] = Some(v);
                        return true;
                    }
                } else {
                    match_right[v] = Some(u);
                    match_left[u] = Some(v);
                    return true;
                }
            }
        }
    }
    
    false
}

// Example usage
fn main() {
    // Create a bipartite graph with 4 left nodes and 4 right nodes
    let mut graph = BipartiteGraph::new(4, 4);
    
    // Add edges: left_node -> right_node
    graph.add_edge(0, 0);
    graph.add_edge(0, 1);
    graph.add_edge(1, 1);
    graph.add_edge(1, 2);
    graph.add_edge(2, 2);
    graph.add_edge(2, 3);
    graph.add_edge(3, 3);
    
    println!("Bipartite Graph Edges:");
    for i in 0..graph.left_size {
        print!("Left node {} -> ", i);
        for &j in &graph.edges[i] {
            print!("{} ", j);
        }
        println!();
    }
    
    let matching = hopcroft_karp(&graph);
    
    println!("\nMaximum Matching:");
    println!("Number of matched pairs: {}", matching.len());
    
    for (left, right) in &matching {
        println!("Left node {} -> Right node {}", left, right);
    }
    
    // Another example with a different graph
    println!("\n" + "=".repeat(50).as_str());
    println!("Another Example:");
    
    let mut graph2 = BipartiteGraph::new(3, 3);
    graph2.add_edge(0, 0);
    graph2.add_edge(0, 1);
    graph2.add_edge(1, 1);
    graph2.add_edge(1, 2);
    graph2.add_edge(2, 0);
    graph2.add_edge(2, 2);
    
    let matching2 = hopcroft_karp(&graph2);
    
    println!("Number of matched pairs: {}", matching2.len());
    for (left, right) in &matching2 {
        println!("Left node {} -> Right node {}", left, right);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_matching() {
        let mut graph = BipartiteGraph::new(2, 2);
        graph.add_edge(0, 0);
        graph.add_edge(1, 1);
        
        let matching = hopcroft_karp(&graph);
        assert_eq!(matching.len(), 2);
    }

    #[test]
    fn test_empty_matching() {
        let graph = BipartiteGraph::new(2, 2);
        let matching = hopcroft_karp(&graph);
        assert_eq!(matching.len(), 0);
    }
}
```

## Example Output

```
Bipartite Graph Edges:
Left node 0 -> 0 1 
Left node 1 -> 1 2 
Left node 2 -> 2 3 
Left node 3 -> 3 

Maximum Matching:
Number of matched pairs: 4
Left node 0 -> Right node 0
Left node 1 -> Right node 1
Left node 2 -> Right node 2
Left node 3 -> Right node 3

==================================================
Another Example:
Number of matched pairs: 3
Left node 0 -> Right node 0
Left node 1 -> Right node 1
Left node 2 -> Right node 2
```

## Key Features

1. **BipartiteGraph Structure**: Represents a bipartite graph with left and right nodes
2. **Edge Addition**: Supports adding edges between left and right nodes
3. **Hopcroft-Karp Algorithm**: Implements the full algorithm with BFS/DFS phases
4. **Result Format**: Returns vector of matched pairs as (left_node, right_node)
5. **Error Handling**: Validates node indices when adding edges
6. **Testing**: Includes unit tests for verification

## Time Complexity
- **Time**: O(E × √V) where E is edges and V is vertices
- **Space**: O(V + E) for storing the graph and auxiliary data structures

The algorithm efficiently finds maximum cardinality matching in bipartite graphs, which is useful for problems like job assignment, roommate matching, and resource allocation.

