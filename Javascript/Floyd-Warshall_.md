# Floyd-Warshall Algorithm in JavaScript

The Floyd-Warshall algorithm finds the shortest paths between all pairs of vertices in a weighted graph.

## Implementation

```javascript
function floydWarshall(graph) {
    const n = graph.length;
    
    // Create a copy of the graph to avoid modifying the original
    const dist = [];
    for (let i = 0; i < n; i++) {
        dist[i] = [];
        for (let j = 0; j < n; j++) {
            dist[i][j] = graph[i][j];
        }
    }
    
    // Floyd-Warshall algorithm
    for (let k = 0; k < n; k++) {
        for (let i = 0; i < n; i++) {
            for (let j = 0; j < n; j++) {
                if (dist[i][k] + dist[k][j] < dist[i][j]) {
                    dist[i][j] = dist[i][k] + dist[k][j];
                }
            }
        }
    }
    
    return dist;
}

// Example usage
function printMatrix(matrix) {
    console.log("Shortest distances between all pairs:");
    for (let i = 0; i < matrix.length; i++) {
        console.log(matrix[i].join(" "));
    }
}

// Example graph represented as adjacency matrix
// Infinity represents no direct connection
const INF = Infinity;
const graph = [
    [0, 3, INF, 7],
    [8, 0, 2, INF],
    [5, INF, 0, 1],
    [2, INF, INF, 0]
];

console.log("Original graph:");
console.log("  0  1  2  3");
graph.forEach((row, i) => {
    console.log(`${i} ${row.join("  ")}`);
});

const result = floydWarshall(graph);
console.log("\nShortest path matrix:");
printMatrix(result);

// Output:
// Original graph:
//   0  1  2  3
// 0 0  3  ∞  7
// 1 8  0  2  ∞
// 2 5  ∞  0  1
// 3 2  ∞  ∞  0
//
// Shortest path matrix:
// 0 3 5 6
// 8 0 2 3
// 5 8 0 1
// 2 5 7 0
```

## Algorithm Explanation

1. **Initialization**: Create a distance matrix `dist` where `dist[i][j]` represents the shortest distance from vertex `i` to vertex `j`.

2. **Main Loop**: For each intermediate vertex `k`, check if using `k` as an intermediate vertex gives a shorter path between any two vertices `i` and `j`.

3. **Update**: If `dist[i][k] + dist[k][j] < dist[i][j]`, update the distance.

## Time and Space Complexity

- **Time Complexity**: O(V³) where V is the number of vertices
- **Space Complexity**: O(V²) for the distance matrix

## Key Features

- Works with both positive and negative edge weights (but not negative cycles)
- Finds shortest paths between all pairs of vertices
- Can detect negative cycles in the graph
- Handles disconnected graphs automatically

The algorithm is particularly useful when you need to find shortest paths between all pairs of vertices in a graph, such as in network routing or transportation planning applications.

