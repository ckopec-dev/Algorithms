# Prim's Algorithm in Rust

Here's an implementation of Prim's algorithm to find the Minimum Spanning Tree (MST) in Rust:

```rust
use std::collections::BinaryHeap;
use std::cmp::Reverse;

#[derive(Debug, Clone)]
struct Edge {
    from: usize,
    to: usize,
    weight: i32,
}

impl Edge {
    fn new(from: usize, to: usize, weight: i32) -> Self {
        Edge { from, to, weight }
    }
}

fn prims_mst(graph: &[Vec<(usize, i32)>], start_vertex: usize) -> Vec<Edge> {
    let n = graph.len();
    let mut visited = vec![false; n];
    let mut mst_edges = Vec::new();
    let mut edge_heap = BinaryHeap::new();
    
    // Start with the given vertex
    visited[start_vertex] = true;
    
    // Add all edges from the starting vertex to the heap
    for &(to, weight) in &graph[start_vertex] {
        edge_heap.push(Reverse((weight, start_vertex, to)));
    }
    
    while !edge_heap.is_empty() {
        let Reverse((weight, from, to)) = edge_heap.pop().unwrap();
        
        // Skip if the destination vertex is already visited
        if visited[to] {
            continue;
        }
        
        // Add the edge to MST
        visited[to] = true;
        mst_edges.push(Edge::new(from, to, weight));
        
        // Add all edges from the newly added vertex to the heap
        for &(next_to, next_weight) in &graph[to] {
            if !visited[next_to] {
                edge_heap.push(Reverse((next_weight, to, next_to)));
            }
        }
    }
    
    mst_edges
}

fn main() {
    // Example graph represented as adjacency list
    // Vertex 0: connected to 1 (weight 4), 2 (weight 2)
    // Vertex 1: connected to 0 (weight 4), 2 (weight 1), 3 (weight 5)
    // Vertex 2: connected to 0 (weight 2), 1 (weight 1), 3 (weight 8)
    // Vertex 3: connected to 1 (weight 5), 2 (weight 8)
    let graph = vec![
        vec![(1, 4), (2, 2)],     // Vertex 0
        vec![(0, 4), (2, 1), (3, 5)], // Vertex 1
        vec![(0, 2), (1, 1), (3, 8)], // Vertex 2
        vec![(1, 5), (2, 8)],     // Vertex 3
    ];
    
    let mst = prims_mst(&graph, 0);
    
    println!("Minimum Spanning Tree edges:");
    let mut total_weight = 0;
    for edge in &mst {
        println!("{} -- {} (weight: {})", edge.from, edge.to, edge.weight);
        total_weight += edge.weight;
    }
    println!("Total weight: {}", total_weight);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_prims_mst() {
        let graph = vec![
            vec![(1, 4), (2, 2)],
            vec![(0, 4), (2, 1), (3, 5)],
            vec![(0, 2), (1, 1), (3, 8)],
            vec![(1, 5), (2, 8)],
        ];
        
        let mst = prims_mst(&graph, 0);
        assert_eq!(mst.len(), 3); // MST should have n-1 edges
        
        // Check that all vertices are included
        let mut vertices = std::collections::HashSet::new();
        for edge in &mst {
            vertices.insert(edge.from);
            vertices.insert(edge.to);
        }
        assert_eq!(vertices.len(), 4);
    }
}
```

## How it works:

1. **Initialization**: Start with any vertex and mark it as visited
2. **Edge Selection**: Use a priority queue (BinaryHeap) to always select the minimum weight edge
3. **Expansion**: Add the selected edge to the MST and mark the new vertex as visited
4. **Continue**: Add all edges from the newly added vertex to the priority queue
5. **Repeat**: Continue until all vertices are included in the MST

## Key Features:

- **Time Complexity**: O(E log V) where E is edges and V is vertices
- **Space Complexity**: O(V + E)
- **Uses BinaryHeap** for efficient edge selection
- **Handles disconnected graphs** by only processing connected components
- **Includes tests** for verification

## Sample Output:
```
Minimum Spanning Tree edges:
0 -- 2 (weight: 2)
2 -- 1 (weight: 1)
1 -- 3 (weight: 5)
Total weight: 8
```

