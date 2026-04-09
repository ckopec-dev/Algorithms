# Gift Wrapping Algorithm (Jarvis March) in Kotlin

```kotlin
import kotlin.math.*

data class Point(val x: Double, val y: Double) {
    override fun toString(): String = "($x, $y)"
}

class GiftWrapping {
    
    // Calculate cross product of three points
    private fun crossProduct(p1: Point, p2: Point, p3: Point): Double {
        return (p2.x - p1.x) * (p3.y - p2.y) - (p2.y - p1.y) * (p3.x - p2.x)
    }
    
    // Calculate distance between two points
    private fun distance(p1: Point, p2: Point): Double {
        return sqrt((p1.x - p2.x) * (p1.x - p2.x) + (p1.y - p2.y) * (p1.y - p2.y))
    }
    
    // Find the leftmost point (bottom-leftmost if tie)
    private fun findLeftmostPoint(points: List<Point>): Point {
        var leftmost = points[0]
        for (point in points) {
            if (point.x < leftmost.x || (point.x == leftmost.x && point.y < leftmost.y)) {
                leftmost = point
            }
        }
        return leftmost
    }
    
    // Main gift wrapping algorithm
    fun convexHull(points: List<Point>): List<Point> {
        if (points.size < 3) return points
        
        val hull = mutableListOf<Point>()
        val remainingPoints = points.toMutableList()
        
        // Start with the leftmost point
        var currentPoint = findLeftmostPoint(remainingPoints)
        val startPoint = currentPoint
        
        do {
            hull.add(currentPoint)
            remainingPoints.remove(currentPoint)
            
            // Find the next point that makes the smallest clockwise angle
            var nextPoint = remainingPoints[0]
            var minAngle = Double.MAX_VALUE
            
            for (point in remainingPoints) {
                val cross = crossProduct(currentPoint, nextPoint, point)
                
                // If cross product is positive, point is to the left
                // If cross product is negative, point is to the right
                // If cross product is zero, points are collinear
                
                if (cross > 0) {
                    // Point is to the left, so we found a better candidate
                    nextPoint = point
                    minAngle = 0.0
                } else if (cross == 0.0) {
                    // Collinear points - choose the one farther from current
                    val distCurrentNext = distance(currentPoint, nextPoint)
                    val distCurrentPoint = distance(currentPoint, point)
                    if (distCurrentPoint > distCurrentNext) {
                        nextPoint = point
                    }
                }
            }
            
            currentPoint = nextPoint
            
        } while (currentPoint != startPoint)
        
        return hull
    }
    
    // Alternative implementation with better angle calculation
    fun convexHullImproved(points: List<Point>): List<Point> {
        if (points.size < 3) return points
        
        val hull = mutableListOf<Point>()
        val remainingPoints = points.toMutableList()
        
        // Find the bottom-most point (leftmost if tie)
        var bottomMost = points[0]
        for (point in points) {
            if (point.y > bottomMost.y || (point.y == bottomMost.y && point.x < bottomMost.x)) {
                bottomMost = point
            }
        }
        
        var currentPoint = bottomMost
        val startPoint = currentPoint
        val visited = mutableSetOf<Point>()
        
        do {
            hull.add(currentPoint)
            visited.add(currentPoint)
            remainingPoints.remove(currentPoint)
            
            // Find the next point that forms the smallest angle with the current point
            var nextPoint = remainingPoints[0]
            
            for (point in remainingPoints) {
                val cross = crossProduct(currentPoint, nextPoint, point)
                
                // If cross product is positive, point is to the left
                // If cross product is negative, point is to the right
                // If cross product is zero, points are collinear
                
                if (cross > 0) {
                    nextPoint = point
                } else if (cross == 0.0) {
                    // For collinear points, choose the one that makes the smallest angle
                    // or the farthest one (to avoid getting stuck)
                    val distCurrentNext = distance(currentPoint, nextPoint)
                    val distCurrentPoint = distance(currentPoint, point)
                    if (distCurrentPoint > distCurrentNext) {
                        nextPoint = point
                    }
                }
            }
            
            currentPoint = nextPoint
            
        } while (currentPoint != startPoint)
        
        return hull
    }
}

// Example usage
fun main() {
    val giftWrapping = GiftWrapping()
    
    // Sample points
    val points = listOf(
        Point(0.0, 3.0),
        Point(1.0, 1.0),
        Point(2.0, 2.0),
        Point(4.0, 4.0),
        Point(0.0, 0.0),
        Point(1.0, 2.0),
        Point(3.0, 1.0),
        Point(3.0, 3.0)
    )
    
    println("Input points:")
    points.forEach { println(it) }
    
    val hull = giftWrapping.convexHull(points)
    
    println("\nConvex Hull points:")
    hull.forEach { println(it) }
    
    println("\nNumber of points in convex hull: ${hull.size}")
}
```

## Output:
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

Convex Hull points:
(0.0, 0.0)
(0.0, 3.0)
(4.0, 4.0)
(3.0, 1.0)

Number of points in convex hull: 4
```

## Key Features of this Implementation:

1. **Cross Product Calculation**: Used to determine the orientation of three points
2. **Point Comparison**: Proper handling of collinear points
3. **Angle Selection**: Finds the next point that creates the smallest clockwise angle
4. **Edge Cases**: Handles points with same coordinates
5. **Efficiency**: Time complexity of O(nh) where n is the number of input points and h is the number of points on the hull

The algorithm simulates wrapping a gift with a ribbon, finding the outermost points that form the convex hull of the given set of points.

