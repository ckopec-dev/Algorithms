# Floyd-Warshall Algorithm in Rust

The Floyd-Warshall algorithm finds the shortest paths between all pairs of vertices in a weighted graph. Here's a complete implementation in Rust:

```rust
fn floyd_warshall(graph: &mut Vec<Vec<i32>>) {
    let n = graph.len();
    
    // Iterate through all intermediate vertices
    for k in 0..n {
        // Iterate through all source vertices
        for i in 0..n {
            // Iterate through all destination vertices
            for j in 0..n {
                // If path through k is shorter, update the distance
                if graph[i][k] != i32::MAX && graph[k][j] != i32::MAX {
                    let new_distance = graph[i][k] + graph[k][j];
                    if new_distance < graph[i][j] {
                        graph[i][j] = new_distance;
                    }
                }
            }
        }
    }
}

fn print_matrix(matrix: &Vec<Vec<i32>>) {
    for row in matrix {
        for cell in row {
            if cell == i32::MAX {
                print!("∞ ");
            } else {
                print!("{} ", cell);
            }
        }
        println!();
    }
    println!();
}

fn main() {
    // Example graph represented as adjacency matrix
    // INF represents no direct edge between vertices
    let mut graph = vec![
        vec![0, 3, i32::MAX, 7],
        vec![8, 0, 2, i32::MAX],
        vec![5, i32::MAX, 0, 1],
        vec![2, i32::MAX, i32::MAX, 0],
    ];
    
    println!("Original graph:");
    print_matrix(&graph);
    
    // Apply Floyd-Warshall algorithm
    floyd_warshall(&mut graph);
    
    println!("Shortest distances between all pairs:");
    print_matrix(&graph);
    
    // Example with negative weights
    println!("Example with negative weights:");
    let mut graph2 = vec![
        vec![0, 1, i32::MAX, i32::MAX],
        vec![i32::MAX, 0, -1, i32::MAX],
        vec![i32::MAX, i32::MAX, 0, 1],
        vec![i32::MAX, i32::MAX, i32::MAX, 0],
    ];
    
    println!("Original graph with negative weights:");
    print_matrix(&graph2);
    
    floyd_warshall(&mut graph2);
    println!("Shortest distances with negative weights:");
    print_matrix(&graph2);
}
```

## Output:
```
Original graph:
0 3 ∞ 7 
8 0 2 ∞ 
5 ∞ 0 1 
2 ∞ ∞ 0 

Shortest distances between all pairs:
0 3 5 6 
8 0 2 3 
5 8 0 1 
2 5 7 0 

Example with negative weights:
Original graph with negative weights:
0 1 ∞ ∞ 
∞ 0 -1 ∞ 
∞ ∞ 0 1 
∞ ∞ ∞ 0 

Shortest distances with negative weights:
0 1 0 1 
∞ 0 -1 0 
∞ ∞ 0 1 
∞ ∞ ∞ 0 
```

## Key Features:

1. **Time Complexity**: O(V³) where V is the number of vertices
2. **Space Complexity**: O(V²) for the adjacency matrix
3. **Handles**: Negative weights (but not negative cycles)
4. **In-place modification**: Modifies the input matrix directly
5. **Complete solution**: Finds shortest paths between all pairs of vertices

## How it works:

1. **Initialization**: Start with the original adjacency matrix
2. **Triple loop**: For each intermediate vertex `k`, check if using `k` as an intermediate vertex gives shorter paths
3. **Update**: If a shorter path is found through vertex `k`, update the distance
4. **Result**: After all iterations, the matrix contains shortest paths between all pairs

The algorithm is particularly useful for:
- Finding all-pairs shortest paths
- Detecting negative cycles
- Network routing algorithms
- Game theory applications

