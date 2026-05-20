# Dijkstra's Algorithm in Rust

Here's a complete implementation of Dijkstra's algorithm in Rust:

```rust
use std::collections::BinaryHeap;
use std::cmp::Reverse;

#[derive(Debug, Clone)]
struct Edge {
    to: usize,
    weight: i32,
}

#[derive(Debug)]
struct Graph {
    vertices: usize,
    adj_list: Vec<Vec<Edge>>,
}

impl Graph {
    fn new(vertices: usize) -> Self {
        Graph {
            vertices,
            adj_list: vec![Vec::new(); vertices],
        }
    }

    fn add_edge(&mut self, from: usize, to: usize, weight: i32) {
        self.adj_list[from].push(Edge { to, weight });
    }

    fn dijkstra(&self, start: usize) -> Vec<Option<i32>> {
        let mut distances = vec![None; self.vertices];
        let mut visited = vec![false; self.vertices];
        let mut heap = BinaryHeap::new();

        // Initialize start distance
        distances[start] = Some(0);
        heap.push(Reverse((0, start)));

        while let Some(Reverse((current_dist, current_vertex))) = heap.pop() {
            if visited[current_vertex] {
                continue;
            }
            visited[current_vertex] = true;

            // Skip if we've already found a better path
            if let Some(dist) = distances[current_vertex] {
                if dist > current_dist {
                    continue;
                }
            }

            // Explore neighbors
            for edge in &self.adj_list[current_vertex] {
                if !visited[edge.to] {
                    let new_dist = current_dist + edge.weight;
                    
                    match distances[edge.to] {
                        None => {
                            distances[edge.to] = Some(new_dist);
                            heap.push(Reverse((new_dist, edge.to)));
                        }
                        Some(existing_dist) => {
                            if new_dist < existing_dist {
                                distances[edge.to] = Some(new_dist);
                                heap.push(Reverse((new_dist, edge.to)));
                            }
                        }
                    }
                }
            }
        }

        distances
    }
}

fn main() {
    // Create a graph with 6 vertices
    let mut graph = Graph::new(6);
    
    // Add edges (from, to, weight)
    graph.add_edge(0, 1, 4);
    graph.add_edge(0, 2, 2);
    graph.add_edge(1, 2, 1);
    graph.add_edge(1, 3, 5);
    graph.add_edge(2, 3, 8);
    graph.add_edge(2, 4, 10);
    graph.add_edge(3, 4, 2);
    graph.add_edge(3, 5, 6);
    graph.add_edge(4, 5, 3);

    println!("Graph edges:");
    for i in 0..graph.vertices {
        println!("Vertex {}: {:?}", i, graph.adj_list[i]);
    }

    let start_vertex = 0;
    let distances = graph.dijkstra(start_vertex);

    println!("\nShortest distances from vertex {}:", start_vertex);
    for (i, distance) in distances.iter().enumerate() {
        match distance {
            Some(dist) => println!("To vertex {}: {}", i, dist),
            None => println!("To vertex {}: unreachable", i),
        }
    }
}
```

## Output:
```
Graph edges:
Vertex 0: [Edge { to: 1, weight: 4 }, Edge { to: 2, weight: 2 }]
Vertex 1: [Edge { to: 2, weight: 1 }, Edge { to: 3, weight: 5 }]
Vertex 2: [Edge { to: 3, weight: 8 }, Edge { to: 4, weight: 10 }]
Vertex 3: [Edge { to: 4, weight: 2 }, Edge { to: 5, weight: 6 }]
Vertex 4: [Edge { to: 5, weight: 3 }]
Vertex 5: []

Shortest distances from vertex 0:
To vertex 0: 0
To vertex 1: 4
To vertex 2: 2
To vertex 3: 9
To vertex 4: 11
To vertex 5: 14
```

## Key Features:

1. **Graph Representation**: Uses an adjacency list with weighted edges
2. **Priority Queue**: Uses `BinaryHeap` with `Reverse` wrapper for min-heap behavior
3. **Distance Tracking**: Maintains distances to all vertices from the start vertex
4. **Visited Set**: Prevents reprocessing vertices
5. **Path Optimization**: Updates distances only when finding shorter paths

## Time Complexity:
- **Time**: O((V + E) log V) where V is vertices and E is edges
- **Space**: O(V + E) for the graph representation and auxiliary data structures

This implementation efficiently finds the shortest path from a single source to all other vertices in a weighted directed graph.

