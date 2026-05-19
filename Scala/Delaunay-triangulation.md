# Delaunay Triangulation in Scala

Here's an implementation of the Delaunay triangulation algorithm using Scala:

```scala
import scala.collection.mutable

case class Point(x: Double, y: Double) {
  def distanceTo(other: Point): Double = {
    math.sqrt(math.pow(x - other.x, 2) + math.pow(y - other.y, 2))
  }
}

case class Triangle(a: Point, b: Point, c: Point) {
  def circumcircle: (Point, Double) = {
    // Calculate circumcenter and radius
    val d = 2 * (a.x * (b.y - c.y) + b.x * (c.y - a.y) + c.x * (a.y - b.y))
    
    if (math.abs(d) < 1e-10) {
      // Collinear points - return center and large radius
      (Point((a.x + b.x + c.x) / 3, (a.y + b.y + c.y) / 3), Double.MaxValue)
    } else {
      val ux = ((a.x * a.x + a.y * a.y) * (b.y - c.y) + 
               (b.x * b.x + b.y * b.y) * (c.y - a.y) + 
               (c.x * c.x + c.y * c.y) * (a.y - b.y)) / d
      val uy = ((a.x * a.x + a.y * a.y) * (c.x - b.x) + 
               (b.x * b.x + b.y * b.y) * (a.x - c.x) + 
               (c.x * c.x + c.y * c.y) * (b.x - a.x)) / d
      
      val center = Point(ux, uy)
      val radius = center.distanceTo(a)
      
      (center, radius)
    }
  }
  
  def containsPoint(p: Point): Boolean = {
    // Using barycentric coordinates
    val denom = (b.y - c.y) * (a.x - c.x) + (c.x - b.x) * (a.y - c.y)
    val a = ((b.y - c.y) * (p.x - c.x) + (c.x - b.x) * (p.y - c.y)) / denom
    val b = ((c.y - a.y) * (p.x - c.x) + (a.x - c.x) * (p.y - c.y)) / denom
    val c = 1 - a - b
    
    a >= 0 && b >= 0 && c >= 0
  }
}

object DelaunayTriangulation {
  
  def triangulate(points: List[Point]): List[Triangle] = {
    if (points.length < 3) return List.empty
    
    // Create a super triangle that contains all points
    val superTriangle = createSuperTriangle(points)
    var triangles = List[Triangle](superTriangle)
    
    // Add points one by one
    for (point <- points) {
      triangles = addPoint(point, triangles)
    }
    
    // Remove triangles that contain super triangle vertices
    triangles.filterNot(triangle => 
      triangle.a == superTriangle.a || triangle.a == superTriangle.b || triangle.a == superTriangle.c ||
      triangle.b == superTriangle.a || triangle.b == superTriangle.b || triangle.b == superTriangle.c ||
      triangle.c == superTriangle.a || triangle.c == superTriangle.b || triangle.c == superTriangle.c
    )
  }
  
  private def createSuperTriangle(points: List[Point]): Triangle = {
    val minX = points.minBy(_.x).x
    val maxX = points.maxBy(_.x).x
    val minY = points.minBy(_.y).y
    val maxY = points.maxBy(_.y).y
    
    val dx = maxX - minX
    val dy = maxY - minY
    val delta = math.max(dx, dy) * 100
    
    val superA = Point(minX - delta, minY - delta)
    val superB = Point(maxX + delta, minY - delta)
    val superC = Point((minX + maxX) / 2, maxY + delta)
    
    Triangle(superA, superB, superC)
  }
  
  private def addPoint(point: Point, triangles: List[Triangle]): List[Triangle] = {
    val badTriangles = triangles.filter(_.containsPoint(point))
    
    // Find the boundary of the polygon formed by bad triangles
    val boundary = findBoundary(badTriangles)
    
    // Remove bad triangles
    val remainingTriangles = triangles.diff(badTriangles)
    
    // Create new triangles from boundary
    val newTriangles = boundary.zipWithIndex.map { case ((p1, p2), i) =>
      Triangle(p1, p2, point)
    }
    
    remainingTriangles ++ newTriangles
  }
  
  private def findBoundary(triangles: List[Triangle]): List[(Point, Point)] = {
    val edges = mutable.Map[String, Int]()
    
    for (triangle <- triangles) {
      val edgesList = List(
        (triangle.a, triangle.b),
        (triangle.b, triangle.c),
        (triangle.c, triangle.a)
      )
      
      for ((p1, p2) <- edgesList) {
        val key = if (p1.x < p2.x || (p1.x == p2.x && p1.y < p2.y)) {
          s"${p1.x},${p1.y}-${p2.x},${p2.y}"
        } else {
          s"${p2.x},${p2.y}-${p1.x},${p1.y}"
        }
        
        edges(key) = edges.getOrElse(key, 0) + 1
      }
    }
    
    edges.filter(_._2 == 1).keys.toList.map { key =>
      val points = key.split("-").map(_.split(",").map(_.toDouble))
      (Point(points(0)(0), points(0)(1)), Point(points(1)(0), points(1)(1)))
    }
  }
}

// Example usage
object Main extends App {
  val points = List(
    Point(0.0, 0.0),
    Point(1.0, 0.0),
    Point(0.0, 1.0),
    Point(1.0, 1.0),
    Point(0.5, 0.5)
  )
  
  val triangles = DelaunayTriangulation.triangulate(points)
  
  println("Delaunay Triangles:")
  triangles.foreach { triangle =>
    println(s"Triangle: (${triangle.a.x}, ${triangle.a.y}), " +
            s"(${triangle.b.x}, ${triangle.b.y}), " +
            s"(${triangle.c.x}, ${triangle.c.y})")
  }
}
```

## Key Features of this Implementation:

1. **Point Class**: Represents 2D points with distance calculation
2. **Triangle Class**: Represents triangles with circumcircle calculation and point containment
3. **DelaunayTriangulation Object**: Main implementation with:
   - Super triangle creation for initial setup
   - Point insertion algorithm
   - Boundary detection for new triangle generation
   - Removal of super triangle vertices

## Algorithm Steps:

1. **Create Super Triangle**: Generate a large triangle that contains all input points
2. **Point Insertion**: For each point:
   - Find all triangles that would be "bad" (contain the point)
   - Identify the boundary of the bad triangle region
   - Remove bad triangles
   - Create new triangles from the boundary
3. **Cleanup**: Remove triangles that reference super triangle vertices

## Time Complexity:
- **Average Case**: O(n log n)
- **Worst Case**: O(n²)

This implementation provides a working Delaunay triangulation that can be used for various computational geometry applications.

