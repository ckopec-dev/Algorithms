# Bellman-Ford Algorithm in Rust

Here's a complete implementation of the Bellman-Ford algorithm in Rust to find shortest paths from a source vertex to all other vertices in a weighted graph:

```rust
use std::collections::HashMap;
use std::i32;

#[derive(Debug, Clone)]
struct Edge {
    source: i32,
    destination: i32,
    weight: i32,
}

fn bellman_ford(
    edges: &Vec<Edge>,
    num_vertices: i32,
    source: i32,
) -> Result<Vec<i32>, String> {
    // Initialize distances from source to all vertices as infinity
    let mut distances = vec![i32::MAX; num_vertices as usize];
    distances[source as usize] = 0;

    // Relax edges repeatedly
    for _ in 0..(num_vertices - 1) {
        let mut updated = false;
        for edge in edges {
            let u = edge.source as usize;
            let v = edge.destination as usize;
            let weight = edge.weight;

            // If we can find a shorter path to v through u
            if distances[u] != i32::MAX && distances[u] + weight < distances[v] {
                distances[v] = distances[u] + weight;
                updated = true;
            }
        }
        
        // If no updates were made, we can stop early
        if !updated {
            break;
        }
    }

    // Check for negative weight cycles
    for edge in edges {
        let u = edge.source as usize;
        let v = edge.destination as usize;
        let weight = edge.weight;

        if distances[u] != i32::MAX && distances[u] + weight < distances[v] {
            return Err("Graph contains negative weight cycle".to_string());
        }
    }

    Ok(distances)
}

fn main() {
    // Create a sample graph with 5 vertices (0, 1, 2, 3, 4)
    let edges = vec![
        Edge { source: 0, destination: 1, weight: 4 },
        Edge { source: 0, destination: 2, weight: 2 },
        Edge { source: 1, destination: 2, weight: 1 },
        Edge { source: 1, destination: 3, weight: 5 },
        Edge { source: 2, destination: 3, weight: 8 },
        Edge { source: 2, destination: 4, weight: 10 },
        Edge { source: 3, destination: 4, weight: 2 },
    ];

    let num_vertices = 5;
    let source = 0;

    match bellman_ford(&edges, num_vertices, source) {
        Ok(distances) => {
            println!("Shortest distances from vertex {}:", source);
            for (i, &distance) in distances.iter().enumerate() {
                if distance == i32::MAX {
                    println!("Vertex {}: Infinity", i);
                } else {
                    println!("Vertex {}: {}", i, distance);
                }
            }
        }
        Err(error) => {
            println!("Error: {}", error);
        }
    }

    // Test with negative weight cycle
    let edges_with_negative_cycle = vec![
        Edge { source: 0, destination: 1, weight: 1 },
        Edge { source: 1, destination: 2, weight: 1 },
        Edge { source: 2, destination: 0, weight: -3 }, // Creates negative cycle
    ];

    println!("\nTesting with negative weight cycle:");
    match bellman_ford(&edges_with_negative_cycle, 3, 0) {
        Ok(_) => println!("No error - this shouldn't happen"),
        Err(error) => println!("Error caught: {}", error),
    }
}
```

## Output

```
Shortest distances from vertex 0:
Vertex 0: 0
Vertex 1: 4
Vertex 2: 2
Vertex 3: 9
Vertex 4: 11

Testing with negative weight cycle:
Error caught: Graph contains negative weight cycle
```

## Key Features of this Implementation

1. **Edge Structure**: Uses a custom `Edge` struct to represent weighted connections
2. **Error Handling**: Returns `Result<Vec<i32>, String>` to handle negative cycles
3. **Early Termination**: Stops early if no updates occur in an iteration
4. **Negative Cycle Detection**: Checks for negative weight cycles after relaxation
5. **Comprehensive Testing**: Includes both normal case and negative cycle detection

## Time and Space Complexity

- **Time Complexity**: O(VE) where V is vertices and E is edges
- **Space Complexity**: O(V) for storing distances

## Algorithm Steps

1. Initialize all distances to infinity except source (set to 0)
2. Relax all edges V-1 times
3. Check for negative weight cycles
4. Return the shortest distances

The Bellman-Ford algorithm is particularly useful when dealing with graphs that may contain negative edge weights, unlike Dijkstra's algorithm which requires non-negative weights.

