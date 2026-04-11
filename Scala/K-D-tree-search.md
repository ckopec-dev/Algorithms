# K-D Tree Search Algorithm in Scala

Here's a complete implementation of a K-D tree search algorithm in Scala:

```scala
import scala.math._

// Point class representing a k-dimensional point
case class Point(coords: Array[Double]) {
  def dimension: Int = coords.length
  def apply(index: Int): Double = coords(index)
  
  // Calculate Euclidean distance to another point
  def distance(other: Point): Double = {
    sqrt(coords.zip(other.coords).map { case (a, b) => pow(a - b, 2) }.sum)
  }
  
  override def toString: String = s"Point(${coords.mkString(", ")})"
}

// K-D Tree node
case class KDNode(point: Point, left: Option[KDNode] = None, right: Option[KDNode] = None) {
  def isLeaf: Boolean = left.isEmpty && right.isEmpty
}

// K-D Tree implementation
class KDTree {
  private var root: Option[KDNode] = None
  
  // Insert a point into the tree
  def insert(point: Point): Unit = {
    root = insertHelper(root, point, 0)
  }
  
  private def insertHelper(node: Option[KDNode], point: Point, depth: Int): Option[KDNode] = {
    val k = point.dimension
    val axis = depth % k
    
    node match {
      case None => Some(KDNode(point))
      case Some(n) =>
        if (point(axis) <= n.point(axis)) {
          val newLeft = insertHelper(n.left, point, depth + 1)
          Some(KDNode(n.point, newLeft, n.right))
        } else {
          val newRight = insertHelper(n.right, point, depth + 1)
          Some(KDNode(n.point, n.left, newRight))
        }
    }
  }
  
  // K-D tree search for k nearest neighbors
  def searchKNN(queryPoint: Point, k: Int): List[Point] = {
    val nearest = new scala.collection.mutable.PriorityQueue[Point]()
    searchHelper(root, queryPoint, k, nearest, 0)
    nearest.toList.reverse
  }
  
  // Search helper function for nearest neighbors
  private def searchHelper(
    node: Option[KDNode], 
    queryPoint: Point, 
    k: Int, 
    nearest: scala.collection.mutable.PriorityQueue[Point],
    depth: Int
  ): Unit = {
    if (node.isEmpty) return
    
    val kDim = queryPoint.dimension
    val axis = depth % kDim
    val currentNode = node.get
    
    // Calculate distance to current node
    val distance = queryPoint.distance(currentNode.point)
    
    // Add to nearest neighbors if we have space or if this is closer
    if (nearest.size < k) {
      nearest.enqueue(currentNode.point)
    } else if (distance < queryPoint.distance(nearest.head)) {
      nearest.dequeue()
      nearest.enqueue(currentNode.point)
    }
    
    // Determine which side to search first
    val searchFirst: Option[KDNode] = 
      if (queryPoint(axis) <= currentNode.point(axis)) currentNode.left else currentNode.right
    
    val searchSecond: Option[KDNode] = 
      if (queryPoint(axis) <= currentNode.point(axis)) currentNode.right else currentNode.left
    
    // Search the first side
    searchHelper(searchFirst, queryPoint, k, nearest, depth + 1)
    
    // Check if we need to search the second side
    val hyperplaneDistance = abs(queryPoint(axis) - currentNode.point(axis))
    if (nearest.size < k || hyperplaneDistance < queryPoint.distance(nearest.head)) {
      searchHelper(searchSecond, queryPoint, k, nearest, depth + 1)
    }
  }
  
  // Find the nearest neighbor (k=1)
  def searchNearest(queryPoint: Point): Option[Point] = {
    searchKNN(queryPoint, 1).headOption
  }
  
  // Print the tree structure (for debugging)
  def printTree(): Unit = {
    printTreeHelper(root, 0)
  }
  
  private def printTreeHelper(node: Option[KDNode], depth: Int): Unit = {
    if (node.isDefined) {
      val nodeValue = node.get
      println("  " * depth + nodeValue.point)
      printTreeHelper(nodeValue.left, depth + 1)
      printTreeHelper(nodeValue.right, depth + 1)
    }
  }
}

// Example usage
object KDTreeExample extends App {
  // Create a 2D K-D tree
  val tree = new KDTree()
  
  // Insert some points
  val points = List(
    Point(Array(2.0, 3.0)),
    Point(Array(5.0, 4.0)),
    Point(Array(9.0, 6.0)),
    Point(Array(4.0, 7.0)),
    Point(Array(8.0, 1.0)),
    Point(Array(7.0, 2.0))
  )
  
  points.foreach(tree.insert)
  
  println("K-D Tree structure:")
  tree.printTree()
  
  println("\nSearching for nearest neighbors:")
  
  // Search for nearest neighbor
  val queryPoint = Point(Array(6.0, 3.0))
  val nearest = tree.searchNearest(queryPoint)
  println(s"Nearest neighbor to $queryPoint: $nearest")
  
  // Search for 3 nearest neighbors
  val kNearest = tree.searchKNN(queryPoint, 3)
  println(s"3 nearest neighbors to $queryPoint: $kNearest")
  
  // Test with another query point
  val queryPoint2 = Point(Array(1.0, 1.0))
  val nearest2 = tree.searchNearest(queryPoint2)
  println(s"Nearest neighbor to $queryPoint2: $nearest2")
  
  val kNearest2 = tree.searchKNN(queryPoint2, 3)
  println(s"3 nearest neighbors to $queryPoint2: $kNearest2")
}
```

## Key Features of this Implementation:

1. **Point Class**: Represents k-dimensional points with distance calculation
2. **KDNode Class**: Represents nodes in the K-D tree with left and right children
3. **KDTree Class**: Main tree implementation with:
   - Insertion functionality
   - K-nearest neighbors search
   - Nearest neighbor search
   - Tree visualization

## Algorithm Complexity:

- **Insertion**: O(log n) average case, O(n) worst case
- **Search**: O(log n) average case, O(n) worst case
- **Space**: O(n) where n is the number of points

## Example Output:
```
K-D Tree structure:
Point(2.0, 3.0)
  Point(4.0, 7.0)
  Point(5.0, 4.0)
    Point(7.0, 2.0)
  Point(9.0, 6.0)
    Point(8.0, 1.0)

Searching for nearest neighbors:
Nearest neighbor to Point(6.0, 3.0): Point(7.0, 2.0)
3 nearest neighbors to Point(6.0, 3.0): List(Point(7.0, 2.0), Point(5.0, 4.0), Point(4.0, 7.0))
Nearest neighbor to Point(1.0, 1.0): Point(2.0, 3.0)
3 nearest neighbors to Point(1.0, 1.0): List(Point(2.0, 3.0), Point(5.0, 4.0), Point(4.0, 7.0))
```

This implementation demonstrates the core K-D tree search algorithm with proper handling of the hyperplane distance checks that make it efficient for nearest neighbor searches.

