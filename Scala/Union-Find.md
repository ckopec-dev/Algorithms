# Union-Find Algorithm in Scala

Here's an implementation of the Union-Find (Disjoint Set Union) algorithm in Scala with path compression and union by rank optimizations:

```scala
class UnionFind(n: Int) {
  // Parent array where parent(i) = i if i is root
  private val parent = Array.tabulate(n)(i => i)
  // Rank array to keep track of tree depth for union by rank
  private val rank = Array.fill(n)(0)
  
  /**
   * Find root of element with path compression
   * @param x element to find root of
   * @return root of the set containing x
   */
  def find(x: Int): Int = {
    if (parent(x) != x) {
      parent(x) = find(parent(x)) // Path compression
    }
    parent(x)
  }
  
  /**
   * Union two elements by rank
   * @param x first element
   * @param y second element
   * @return true if elements were in different sets and union was performed
   */
  def union(x: Int, y: Int): Boolean = {
    val rootX = find(x)
    val rootY = find(y)
    
    if (rootX == rootY) {
      false // Already in same set
    } else {
      // Union by rank
      if (rank(rootX) < rank(rootY)) {
        parent(rootX) = rootY
      } else if (rank(rootX) > rank(rootY)) {
        parent(rootY) = rootX
      } else {
        parent(rootY) = rootX
        rank(rootX) += 1
      }
      true
    }
  }
  
  /**
   * Check if two elements are in the same set
   * @param x first element
   * @param y second element
   * @return true if elements are connected
   */
  def connected(x: Int, y: Int): Boolean = find(x) == find(y)
  
  /**
   * Get the number of elements
   */
  def size: Int = parent.length
}

// Example usage
object UnionFindExample extends App {
  // Create Union-Find structure with 6 elements (0-5)
  val uf = new UnionFind(6)
  
  println("Initial state:")
  println(s"Elements: 0, 1, 2, 3, 4, 5")
  
  // Perform some unions
  println("\nPerforming unions:")
  println("union(0, 1)")
  uf.union(0, 1)
  
  println("union(2, 3)")
  uf.union(2, 3)
  
  println("union(4, 5)")
  uf.union(4, 5)
  
  println("union(1, 3)")
  uf.union(1, 3)
  
  // Check connections
  println("\nChecking connections:")
  println(s"0 and 2 connected? ${uf.connected(0, 2)}") // Should be true
  println(s"0 and 4 connected? ${uf.connected(0, 4)}") // Should be false
  println(s"1 and 3 connected? ${uf.connected(1, 3)}") // Should be true
  println(s"4 and 5 connected? ${uf.connected(4, 5)}") // Should be true
  
  // Show final structure
  println("\nFinal structure:")
  for (i <- 0 until uf.size) {
    println(s"Element $i belongs to root: ${uf.find(i)}")
  }
}
```

## Output:
```
Initial state:
Elements: 0, 1, 2, 3, 4, 5

Performing unions:
union(0, 1)
union(2, 3)
union(4, 5)
union(1, 3)

Checking connections:
0 and 2 connected? true
0 and 4 connected? false
1 and 3 connected? true
4 and 5 connected? true

Final structure:
Element 0 belongs to root: 0
Element 1 belongs to root: 0
Element 2 belongs to root: 0
Element 3 belongs to root: 0
Element 4 belongs to root: 4
Element 5 belongs to root: 4
```

## Key Features:

1. **Path Compression**: `find` method compresses the path during traversal
2. **Union by Rank**: Always attach smaller tree under root of larger tree
3. **Efficient Operations**: Nearly constant time for both `find` and `union`
4. **Memory Efficient**: Uses simple arrays for storage

## Time Complexity:
- **Find**: O(α(n)) where α is the inverse Ackermann function (practically constant)
- **Union**: O(α(n))
- **Space**: O(n)

This implementation is commonly used in algorithms like Kruskal's Minimum Spanning Tree and connectivity problems.

