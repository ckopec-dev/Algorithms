# Z-Order Curve Generation Algorithm in Scala

```scala
object ZOrderCurve {
  
  /**
   * Generate Z-order curve for a given 2D space
   * @param width width of the space
   * @param height height of the space
   * @param maxDepth maximum depth of recursion
   * @return List of (x, y) coordinates in Z-order
   */
  def generateZOrderCurve(width: Int, height: Int, maxDepth: Int): List[(Int, Int)] = {
    val points = scala.collection.mutable.ListBuffer[(Int, Int)]()
    
    def zOrderHelper(x: Int, y: Int, depth: Int): Unit = {
      if (depth == maxDepth) {
        points += ((x, y))
        return
      }
      
      val half = 1 << (maxDepth - depth - 1)
      
      // Process quadrants in Z-order: 00, 01, 10, 11
      zOrderHelper(x, y, depth + 1)           // Top-left (00)
      zOrderHelper(x + half, y, depth + 1)    // Top-right (01)
      zOrderHelper(x, y + half, depth + 1)    // Bottom-left (10)
      zOrderHelper(x + half, y + half, depth + 1) // Bottom-right (11)
    }
    
    zOrderHelper(0, 0, 0)
    points.toList
  }
  
  /**
   * Alternative implementation using bit manipulation
   * @param x x-coordinate
   * @param y y-coordinate
   * @param bits number of bits for each coordinate
   * @return Z-order value
   */
  def zOrderValue(x: Int, y: Int, bits: Int): Long = {
    var result: Long = 0
    var xCopy = x
    var yCopy = y
    var bit = 0
    
    while (bit < bits) {
      result |= ((yCopy & 1) << (2 * bit + 1)) | ((xCopy & 1) << (2 * bit))
      xCopy >>= 1
      yCopy >>= 1
      bit += 1
    }
    
    result
  }
  
  /**
   * Generate Z-order curve by sorting points based on their Z-order values
   * @param width width of the space
   * @param height height of the space
   * @param bits number of bits for coordinates
   * @return List of (x, y) coordinates in Z-order
   */
  def generateZOrderBySorting(width: Int, height: Int, bits: Int): List[(Int, Int)] = {
    val points = for {
      x <- 0 until width
      y <- 0 until height
    } yield (x, y)
    
    points.sortBy { case (x, y) => zOrderValue(x, y, bits) }
  }
  
  /**
   * Print Z-order curve in a grid format
   * @param points list of points in Z-order
   * @param width width of the grid
   * @param height height of the grid
   */
  def printZOrderGrid(points: List[(Int, Int)], width: Int, height: Int): Unit = {
    val grid = Array.ofDim[Int](height, width)
    
    // Fill the grid with point indices
    points.zipWithIndex.foreach { case ((x, y), index) =>
      if (x < width && y < height) {
        grid(y)(x) = index + 1
      }
    }
    
    // Print the grid
    println("Z-Order Grid:")
    for (y <- 0 until height) {
      for (x <- 0 until width) {
        printf("%3d ", grid(y)(x))
      }
      println()
    }
  }
  
  def main(args: Array[String]): Unit = {
    println("Z-Order Curve Generation Example")
    println("=" * 40)
    
    // Example 1: Generate Z-order curve for 4x4 grid with depth 2
    val width = 4
    val height = 4
    val maxDepth = 2
    
    println(s"Generating Z-order curve for ${width}x${height} grid with depth $maxDepth")
    val curve1 = generateZOrderCurve(width, height, maxDepth)
    println(s"Generated points: $curve1")
    
    println("\n" + "=" * 40)
    
    // Example 2: Generate Z-order curve by sorting approach
    val bits = 2
    println(s"Generating Z-order curve using sorting approach with $bits bits")
    val curve2 = generateZOrderBySorting(width, height, bits)
    println(s"Generated points: $curve2")
    
    println("\n" + "=" * 40)
    
    // Example 3: Print grid visualization
    printZOrderGrid(curve2, width, height)
    
    println("\n" + "=" * 40)
    
    // Example 4: Z-order values for some points
    println("Z-order values for points:")
    val testPoints = List((0, 0), (1, 0), (0, 1), (1, 1), (2, 2), (3, 3))
    testPoints.foreach { case (x, y) =>
      val zValue = zOrderValue(x, y, 2)
      println(s"Point ($x, $y) -> Z-order value: $zValue")
    }
  }
}
```

## Key Features of this Implementation:

1. **Two Algorithms**:
   - Recursive subdivision approach (Z-order curve generation)
   - Sorting-based approach using bit manipulation

2. **Bit Manipulation**:
   - Uses bit interleaving to calculate Z-order values
   - Efficient calculation of spatial locality

3. **Visualization**:
   - Grid printing to show spatial ordering
   - Point indexing to demonstrate Z-order traversal

4. **Scalable Design**:
   - Configurable grid size and depth
   - Support for different bit depths

## Sample Output:
```
Z-Order Curve Generation Example
========================================
Generating Z-order curve for 4x4 grid with depth 2
Generated points: List((0,0), (1,0), (0,1), (1,1), (2,0), (3,0), (2,1), (3,1), (0,2), (1,2), (0,3), (1,3), (2,2), (3,2), (2,3), (3,3))

========================================
Generating Z-order curve using sorting approach with 2 bits
Generated points: List((0,0), (1,0), (0,1), (1,1), (2,0), (3,0), (2,1), (3,1), (0,2), (1,2), (0,3), (1,3), (2,2), (3,2), (2,3), (3,3))
```

This implementation demonstrates both the recursive subdivision method and the bit manipulation approach for generating Z-order curves, which are commonly used in spatial indexing and database systems.

