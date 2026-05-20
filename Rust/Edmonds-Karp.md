# Edmonds-Karp Algorithm in Rust

The Edmonds-Karp algorithm is an implementation of the Ford-Fulkerson method for computing the maximum flow in a flow network. It uses BFS to find augmenting paths.

```rust
use std::collections::VecDeque;

#[derive(Debug, Clone)]
struct Edge {
    to: usize,
    capacity: i32,
    reverse: usize, // Index of the reverse edge
}

#[derive(Debug)]
struct FlowNetwork {
    vertices: usize,
    edges: Vec<Vec<Edge>>,
}

impl FlowNetwork {
    fn new(vertices: usize) -> Self {
        FlowNetwork {
            vertices,
            edges: vec![Vec::new(); vertices],
        }
    }

    fn add_edge(&mut self, from: usize, to: usize, capacity: i32) {
        let from_edge_index = self.edges[from].len();
        let to_edge_index = self.edges[to].len();
        
        self.edges[from].push(Edge {
            to,
            capacity,
            reverse: to_edge_index,
        });
        
        self.edges[to].push(Edge {
            to: from,
            capacity: 0, // Reverse edge has 0 capacity initially
            reverse: from_edge_index,
        });
    }

    fn bfs(&self, source: usize, sink: usize, parent: &mut [Option<usize>], parent_edge: &mut [usize]) -> bool {
        let mut visited = vec![false; self.vertices];
        let mut queue = VecDeque::new();
        
        visited[source] = true;
        queue.push_back(source);
        
        while let Some(current) = queue.pop_front() {
            for (edge_index, edge) in self.edges[current].iter().enumerate() {
                if !visited[edge.to] && edge.capacity > 0 {
                    visited[edge.to] = true;
                    parent[edge.to] = Some(current);
                    parent_edge[edge.to] = edge_index;
                    
                    if edge.to == sink {
                        return true;
                    }
                    
                    queue.push_back(edge.to);
                }
            }
        }
        
        false
    }

    fn max_flow(&mut self, source: usize, sink: usize) -> i32 {
        let mut max_flow = 0;
        let mut parent = vec![None; self.vertices];
        let mut parent_edge = vec![0; self.vertices];
        
        loop {
            // Reset parent arrays for BFS
            parent.fill(None);
            parent_edge.fill(0);
            
            if !self.bfs(source, sink, &mut parent, &mut parent_edge) {
                break;
            }
            
            // Find minimum capacity along the path
            let mut path_flow = i32::MAX;
            let mut current = Some(sink);
            
            while let Some(node) = current {
                if let Some(prev) = parent[node] {
                    let edge_index = parent_edge[node];
                    let capacity = self.edges[prev][edge_index].capacity;
                    path_flow = path_flow.min(capacity);
                    current = Some(prev);
                } else {
                    break;
                }
            }
            
            // Update residual capacities
            let mut current = Some(sink);
            while let Some(node) = current {
                if let Some(prev) = parent[node] {
                    let edge_index = parent_edge[node];
                    
                    // Update forward edge
                    self.edges[prev][edge_index].capacity -= path_flow;
                    
                    // Update reverse edge
                    let reverse_edge_index = self.edges[prev][edge_index].reverse;
                    self.edges[node][reverse_edge_index].capacity += path_flow;
                    
                    current = Some(prev);
                } else {
                    break;
                }
            }
            
            max_flow += path_flow;
        }
        
        max_flow
    }
}

fn main() {
    // Create a flow network with 6 vertices (0 to 5)
    let mut network = FlowNetwork::new(6);
    
    // Add edges with capacities
    // Source: 0, Sink: 5
    network.add_edge(0, 1, 10);
    network.add_edge(0, 2, 10);
    network.add_edge(1, 2, 2);
    network.add_edge(1, 3, 4);
    network.add_edge(1, 4, 8);
    network.add_edge(2, 4, 9);
    network.add_edge(3, 5, 10);
    network.add_edge(4, 5, 10);
    
    println!("Flow network created:");
    println!("Source: 0, Sink: 5");
    println!("Maximum flow: {}", network.max_flow(0, 5));
    
    // Test with a simpler example
    println!("\n--- Simple Example ---");
    let mut simple_network = FlowNetwork::new(4);
    simple_network.add_edge(0, 1, 10);
    simple_network.add_edge(0, 2, 10);
    simple_network.add_edge(1, 3, 10);
    simple_network.add_edge(2, 3, 10);
    
    println!("Simple network max flow: {}", simple_network.max_flow(0, 3));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_flow() {
        let mut network = FlowNetwork::new(4);
        network.add_edge(0, 1, 10);
        network.add_edge(0, 2, 10);
        network.add_edge(1, 3, 10);
        network.add_edge(2, 3, 10);
        
        assert_eq!(network.max_flow(0, 3), 20);
    }

    #[test]
    fn test_no_flow() {
        let mut network = FlowNetwork::new(3);
        network.add_edge(0, 1, 5);
        network.add_edge(1, 2, 3);
        // No path from 0 to 2, so max flow should be 0
        assert_eq!(network.max_flow(0, 2), 3); // Actually, there is a path
    }
}
```

## How it works:

1. **Graph Representation**: Uses adjacency lists with edge objects that store:
   - Destination vertex (`to`)
   - Capacity of the edge
   - Index of the reverse edge for residual graph

2. **BFS Search**: Finds augmenting paths from source to sink using breadth-first search

3. **Path Augmentation**: For each path found, determines the minimum capacity and updates:
   - Forward edges (decrease capacity)
   - Reverse edges (increase capacity)

4. **Maximum Flow**: Continues until no more augmenting paths exist

## Time Complexity:
- **Time**: O(VE²) where V is vertices and E is edges
- **Space**: O(V + E)

## Key Features:
- Handles multiple paths between source and sink
- Updates residual graph after each augmentation
- Uses BFS to ensure shortest augmenting paths (this is what makes it Edmonds-Karp vs regular Ford-Fulkerson)
- Works with any flow network with integer capacities

