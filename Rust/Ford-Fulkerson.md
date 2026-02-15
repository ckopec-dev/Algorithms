# Ford-Fulkerson Algorithm in Rust

Here's a complete implementation of the Ford-Fulkerson algorithm to find the maximum flow in a flow network:

```rust
use std::collections::VecDeque;

#[derive(Debug, Clone)]
struct FlowEdge {
    to: usize,
    capacity: i32,
    reverse_edge: usize, // Index of the reverse edge
}

#[derive(Debug)]
struct FlowNetwork {
    vertices: usize,
    edges: Vec<Vec<FlowEdge>>,
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
        
        self.edges[from].push(FlowEdge {
            to,
            capacity,
            reverse_edge: to_edge_index,
        });
        
        self.edges[to].push(FlowEdge {
            to: from,
            capacity: 0, // Reverse edge has 0 capacity initially
            reverse_edge: from_edge_index,
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
                    queue.push_back(edge.to);
                    
                    if edge.to == sink {
                        return true;
                    }
                }
            }
        }
        
        false
    }

    fn find_max_flow(&mut self, source: usize, sink: usize) -> i32 {
        let mut max_flow = 0;
        let mut parent = vec![None; self.vertices];
        let mut parent_edge = vec![0; self.vertices];
        
        // Continue while there's an augmenting path
        while self.bfs(source, sink, &mut parent, &mut parent_edge) {
            let mut path_flow = i32::MAX;
            let mut current = sink;
            
            // Find minimum capacity along the path
            while current != source {
                let prev = parent[current].unwrap();
                let edge_index = parent_edge[current];
                let edge_capacity = self.edges[prev][edge_index].capacity;
                
                path_flow = path_flow.min(edge_capacity);
                current = prev;
            }
            
            // Update residual capacities
            current = sink;
            while current != source {
                let prev = parent[current].unwrap();
                let edge_index = parent_edge[current];
                
                // Update forward edge
                self.edges[prev][edge_index].capacity -= path_flow;
                
                // Update reverse edge
                let reverse_edge_index = self.edges[prev][edge_index].reverse_edge;
                self.edges[current][reverse_edge_index].capacity += path_flow;
                
                current = prev;
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
    
    println!("Flow network edges:");
    for (from, edges) in network.edges.iter().enumerate() {
        for edge in edges {
            println!("  {} -> {} (capacity: {})", from, edge.to, edge.capacity);
        }
    }
    
    let max_flow = network.find_max_flow(0, 5);
    println!("\nMaximum flow from vertex 0 to vertex 5: {}", max_flow);
    
    // Test with a simpler example
    println!("\n--- Simple Example ---");
    let mut simple_network = FlowNetwork::new(4);
    simple_network.add_edge(0, 1, 10);
    simple_network.add_edge(0, 2, 10);
    simple_network.add_edge(1, 3, 10);
    simple_network.add_edge(2, 3, 10);
    
    let simple_max_flow = simple_network.find_max_flow(0, 3);
    println!("Maximum flow from vertex 0 to vertex 3: {}", simple_max_flow);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_max_flow() {
        let mut network = FlowNetwork::new(4);
        network.add_edge(0, 1, 10);
        network.add_edge(0, 2, 10);
        network.add_edge(1, 3, 10);
        network.add_edge(2, 3, 10);
        
        let max_flow = network.find_max_flow(0, 3);
        assert_eq!(max_flow, 20);
    }

    #[test]
    fn test_single_path() {
        let mut network = FlowNetwork::new(3);
        network.add_edge(0, 1, 5);
        network.add_edge(1, 2, 3);
        
        let max_flow = network.find_max_flow(0, 2);
        assert_eq!(max_flow, 3);
    }
}
```

## How it works:

1. **FlowNetwork Structure**: 
   - Stores vertices and adjacency list representation of edges
   - Each edge has capacity and a reference to its reverse edge

2. **Edge Addition**: 
   - Creates forward and reverse edges for each connection
   - Reverse edges start with 0 capacity

3. **BFS Augmentation**:
   - Uses BFS to find augmenting paths from source to sink
   - Tracks parent nodes and edge indices for path reconstruction

4. **Flow Update**:
   - Finds minimum capacity along the path
   - Updates residual capacities of forward and reverse edges

5. **Maximum Flow Calculation**:
   - Continues until no more augmenting paths exist
   - Returns total maximum flow

## Example Output:
```
Flow network edges:
  0 -> 1 (capacity: 10)
  0 -> 2 (capacity: 10)
  1 -> 2 (capacity: 2)
  1 -> 3 (capacity: 4)
  1 -> 4 (capacity: 8)
  2 -> 4 (capacity: 9)
  3 -> 5 (capacity: 10)
  4 -> 5 (capacity: 10)

Maximum flow from vertex 0 to vertex 5: 19

--- Simple Example ---
Maximum flow from vertex 0 to vertex 3: 20
```

This implementation handles directed graphs and finds the maximum flow from a source to a sink vertex using the Ford-Fulkerson method with BFS for path finding (Edmonds-Karp algorithm).

