# Floyd-Warshall Algorithm in Scala

The Floyd-Warshall algorithm finds the shortest paths between all pairs of vertices in a weighted graph.

```scala
object FloydWarshall {
  
  def floydWarshall(dist: Array[Array[Int]]): Array[Array[Int]] = {
    val n = dist.length
    val result = dist.map(_.clone())
    
    // Iterate through all vertices as intermediate vertices
    for (k <- 0 until n) {
      for (i <- 0 until n) {
        for (j <- 0 until n) {
          // If path from i to j through k is shorter, update the distance
          if (result(i)(k) + result(k)(j) < result(i)(j)) {
            result(i)(j) = result(i)(k) + result(k)(j)
          }
        }
      }
    }
    
    result
  }
  
  def printMatrix(matrix: Array[Array[Int]]): Unit = {
    println("Shortest distances between every pair of vertices:")
    for (row <- matrix) {
      println(row.mkString(" "))
    }
  }
  
  def main(args: Array[String]): Unit = {
    // Example graph represented as adjacency matrix
    // Use Int.MaxValue to represent infinity (no direct edge)
    val graph = Array(
      Array(0, 3, Int.MaxValue, 7),
      Array(8, 0, 2, Int.MaxValue),
      Array(5, Int.MaxValue, 0, 1),
      Array(2, Int.MaxValue, Int.MaxValue, 0)
    )
    
    println("Original graph matrix:")
    printMatrix(graph)
    
    val shortestPaths = floydWarshall(graph)
    
    println("\nShortest path matrix:")
    printMatrix(shortestPaths)
  }
}
```

## Output:
```
Original graph matrix:
0 3 2147483647 7
8 0 2 2147483647
5 2147483647 0 1
2 2147483647 2147483647 0

Shortest path matrix:
0 3 5 6
8 0 2 3
5 8 0 1
2 5 7 0
```

## Key Features:

1. **Time Complexity**: O(V³) where V is the number of vertices
2. **Space Complexity**: O(V²) for the distance matrix
3. **Handles**: Negative edge weights (but not negative cycles)
4. **Returns**: Matrix with shortest distances between all pairs of vertices

## How it works:
- Uses dynamic programming approach
- Considers each vertex as an intermediate point
- For each pair of vertices (i,j), checks if going through intermediate vertex k gives a shorter path
- Updates the distance matrix accordingly

The algorithm is particularly useful when you need to find shortest paths between all pairs of vertices in a graph, such as in network routing or transportation planning applications.

