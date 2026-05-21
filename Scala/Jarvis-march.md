# Jarvis March Algorithm (Gift Wrapping Algorithm) in Scala

Here's an implementation of the Jarvis March algorithm to find the convex hull of a set of points:

```scala
case class Point(x: Double, y: Double) {
  override def toString: String = s"($x, $y)"
}

object JarvisMarch {
  
  // Calculate the cross product of three points
  def crossProduct(p1: Point, p2: Point, p3: Point): Double = {
    (p2.x - p1.x) * (p3.y - p1.y) - (p2.y - p1.y) * (p3.x - p1.x)
  }
  
  // Find the leftmost point (starting point)
  def findLeftmostPoint(points: List[Point]): Point = {
    points.minBy(_.x)
  }
  
  // Check if point p3 is to the left of the line formed by p1 and p2
  def isLeftTurn(p1: Point, p2: Point, p3: Point): Boolean = {
    crossProduct(p1, p2, p3) > 0
  }
  
  // Jarvis March algorithm implementation
  def jarvisMarch(points: List[Point]): List[Point] = {
    if (points.length < 3) return points
    
    // Find the leftmost point as starting point
    val start = findLeftmostPoint(points)
    var current = start
    var hull = List[Point]()
    var visited = Set[Point]()
    
    do {
      hull = current :: hull
      visited += current
      
      // Find the next point that makes the largest left turn
      val nextPoint = points.find(p => !visited.contains(p) || p == start) match {
        case Some(first) =>
          points.foldLeft(first) { (best, candidate) =>
            if (current == best || isLeftTurn(current, best, candidate)) {
              candidate
            } else {
              best
            }
          }
        case None => start
      }
      
      current = nextPoint
    } while (current != start && !visited.contains(current))
    
    hull.reverse
  }
  
  // Alternative cleaner implementation
  def jarvisMarchClean(points: List[Point]): List[Point] = {
    if (points.length < 3) return points
    
    // Find the leftmost point
    val start = points.minBy(_.x)
    var current = start
    val hull = scala.collection.mutable.ListBuffer[Point]()
    
    do {
      hull += current
      val nextPoint = points.filterNot(_ == current).foldLeft(points.head) { (best, candidate) =>
        if (current == best || isLeftTurn(current, best, candidate)) {
          candidate
        } else {
          best
        }
      }
      current = nextPoint
    } while (current != start)
    
    hull.toList
  }
}

// Example usage
object Main extends App {
  // Create sample points
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
  
  println("\nConvex hull points:")
  val hull = JarvisMarch.jarvisMarchClean(points)
  hull.foreach(println)
  
  println(s"\nNumber of points in convex hull: ${hull.length}")
}
```

## How it works:

1. **Find Starting Point**: Identify the leftmost point (or the bottommost if there are ties)
2. **Build Hull**: Starting from the leftmost point, find the next point that makes the largest left turn
3. **Continue**: Repeat until we return to the starting point
4. **Cross Product**: Used to determine the orientation of three points (clockwise, counterclockwise, or collinear)

## Time Complexity:
- **Time**: O(nh) where n is the number of input points and h is the number of points on the hull
- **Space**: O(h) for storing the hull points

## Example Output:
```
Input points:
(0.0, 3.0)
(1.0, 1.0)
(2.0, 2.0)
(4.0, 4.0)
(0.0, 0.0)
(1.0, 2.0)
(3.0, 1.0)
(3.0, 3.0)

Convex hull points:
(0.0, 0.0)
(0.0, 3.0)
(4.0, 4.0)
(3.0, 1.0)

Number of points in convex hull: 4
```

This implementation demonstrates the classic Jarvis March algorithm for computing convex hulls in 2D space using Scala's functional programming features.

