# Gift Wrapping Algorithm (Jarvis March) in Scala

The gift wrapping algorithm is a simple algorithm for computing the convex hull of a set of points in the plane.

```scala
import scala.math._

// Point class to represent 2D points
case class Point(x: Double, y: Double) {
  override def toString: String = s"($x, $y)"
}

object GiftWrapping {
  
  // Calculate cross product of three points
  // Returns positive value if counter-clockwise turn, negative for clockwise, 0 for collinear
  def crossProduct(p1: Point, p2: Point, p3: Point): Double = {
    (p2.x - p1.x) * (p3.y - p2.y) - (p2.y - p1.y) * (p3.x - p2.x)
  }
  
  // Find the point with minimum y-coordinate (and minimum x if tie)
  def findBottomMostPoint(points: List[Point]): Point = {
    points.minBy { p =>
      (p.y, p.x)
    }
  }
  
  // Check if point p3 is to the left of the line formed by p1 and p2
  def isLeftTurn(p1: Point, p2: Point, p3: Point): Boolean = {
    crossProduct(p1, p2, p3) > 0
  }
  
  // Main gift wrapping algorithm
  def giftWrap(points: List[Point]): List[Point] = {
    if (points.length < 3) return points
    
    // Find the bottom-most point
    val bottomPoint = findBottomMostPoint(points)
    
    var hull = List[Point]()
    var current = bottomPoint
    var nextPoint = Point(0, 0)
    
    do {
      hull = hull :+ current
      
      // Find the next point that makes the largest left turn
      val candidates = points.filterNot(_ == current)
      
      if (candidates.nonEmpty) {
        nextPoint = candidates.minBy { candidate =>
          val cross = crossProduct(current, nextPoint, candidate)
          // If cross product is 0, we want the point that is farthest
          // If cross product > 0, we want the one with maximum cross product
          // But we want to minimize the angle, so we sort by cross product
          val angle = if (cross == 0) {
            // For collinear points, we want the one that is farthest
            -distance(current, candidate)
          } else {
            cross
          }
          angle
        }
      }
      
      current = nextPoint
    } while (current != bottomPoint && hull.length < points.length)
    
    hull
  }
  
  // Alternative cleaner implementation
  def giftWrapClean(points: List[Point]): List[Point] = {
    if (points.length < 3) return points
    
    // Find the bottom-most point
    val bottomPoint = findBottomMostPoint(points)
    
    var current = bottomPoint
    var hull = List[Point]()
    
    do {
      hull = hull :+ current
      val candidates = points.filterNot(_ == current)
      
      if (candidates.nonEmpty) {
        // Find the point that makes the largest left turn
        val next = candidates.foldLeft(candidates.head) { (best, candidate) =>
          if (isLeftTurn(current, best, candidate)) candidate else best
        }
        current = next
      }
    } while (current != bottomPoint && hull.length < points.length)
    
    hull
  }
  
  // Helper function to calculate distance between two points
  def distance(p1: Point, p2: Point): Double = {
    sqrt((p1.x - p2.x) * (p1.x - p2.x) + (p1.y - p2.y) * (p1.y - p2.y))
  }
  
  // Example usage
  def main(args: Array[String]): Unit = {
    // Sample points
    val points = List(
      Point(0, 3),
      Point(1, 1),
      Point(2, 2),
      Point(4, 4),
      Point(0, 0),
      Point(1, 2),
      Point(3, 1),
      Point(3, 3)
    )
    
    println("Input points:")
    points.foreach(println)
    
    val hull = giftWrapClean(points)
    
    println("\nConvex Hull points:")
    hull.foreach(println)
    
    println(s"\nNumber of points in hull: ${hull.length}")
  }
}

// Run the example
GiftWrapping.main(Array())
```

## Algorithm Explanation

The gift wrapping algorithm works as follows:

1. **Find the starting point**: Identify the bottom-most point (or leftmost if tied)
2. **Build the hull**: Starting from the bottom-most point, find the next point that makes the largest left turn
3. **Continue until return**: Keep adding points until we return to the starting point
4. **Handle collinearity**: Points that are collinear are handled by checking the cross product

## Time Complexity
- **Time**: O(nh) where n is the number of input points and h is the number of points on the hull
- **Space**: O(h) for storing the hull points

## Key Features

- **Simple implementation**: Easy to understand and implement
- **Works with any set of points**: Handles degenerate cases like collinear points
- **Deterministic**: Always produces the same result for the same input
- **Visualizable**: The algorithm mimics wrapping a gift with a ribbon

This implementation demonstrates the core concept of the gift wrapping algorithm while handling edge cases appropriately.

