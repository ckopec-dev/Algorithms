# Bezier Curve Generation in Kotlin

Here's a complete implementation of a Bezier curve generation algorithm in Kotlin:

```kotlin
import kotlin.math.pow
import kotlin.math.factorial

data class Point(val x: Double, val y: Double) {
    operator fun plus(other: Point): Point = Point(x + other.x, y + other.y)
    operator fun minus(other: Point): Point = Point(x - other.x, y - other.y)
    operator fun times(scalar: Double): Point = Point(x * scalar, y * scalar)
}

class BezierCurve {
    
    /**
     * Generate a point on a Bezier curve using the Bernstein polynomial
     * @param points Control points of the curve
     * @param t Parameter value between 0 and 1
     * @return Point on the curve
     */
    fun calculatePoint(points: List<Point>, t: Double): Point {
        val n = points.size - 1
        var result = Point(0.0, 0.0)
        
        for (i in points.indices) {
            val bernstein = bernsteinPolynomial(n, i, t)
            result += points[i] * bernstein
        }
        
        return result
    }
    
    /**
     * Calculate Bernstein polynomial coefficient
     * @param n Degree of polynomial
     * @param i Index of coefficient
     * @param t Parameter value
     * @return Bernstein coefficient
     */
    private fun bernsteinPolynomial(n: Int, i: Int, t: Double): Double {
        val binomialCoeff = factorial(n) / (factorial(i) * factorial(n - i))
        return binomialCoeff * pow(t, i.toDouble()) * pow(1 - t, (n - i).toDouble())
    }
    
    /**
     * Generate multiple points along a Bezier curve
     * @param points Control points
     * @param numPoints Number of points to generate
     * @return List of points on the curve
     */
    fun generateCurvePoints(points: List<Point>, numPoints: Int = 100): List<Point> {
        val curvePoints = mutableListOf<Point>()
        
        for (i in 0..numPoints) {
            val t = i.toDouble() / numPoints
            curvePoints.add(calculatePoint(points, t))
        }
        
        return curvePoints
    }
    
    /**
     * Generate a quadratic Bezier curve (3 control points)
     * @param p0 Start point
     * @param p1 Control point
     * @param p2 End point
     * @param numPoints Number of points to generate
     * @return List of points on the curve
     */
    fun generateQuadraticBezier(p0: Point, p1: Point, p2: Point, numPoints: Int = 100): List<Point> {
        return generateCurvePoints(listOf(p0, p1, p2), numPoints)
    }
    
    /**
     * Generate a cubic Bezier curve (4 control points)
     * @param p0 Start point
     * @param p1 First control point
     * @param p2 Second control point
     * @param p3 End point
     * @param numPoints Number of points to generate
     * @return List of points on the curve
     */
    fun generateCubicBezier(p0: Point, p1: Point, p2: Point, p3: Point, numPoints: Int = 100): List<Point> {
        return generateCurvePoints(listOf(p0, p1, p2, p3), numPoints)
    }
}

// Example usage
fun main() {
    val bezier = BezierCurve()
    
    // Define control points for a quadratic Bezier curve
    val quadraticPoints = listOf(
        Point(0.0, 0.0),
        Point(2.0, 4.0),
        Point(4.0, 0.0)
    )
    
    // Generate points for the quadratic curve
    val quadraticCurve = bezier.generateQuadraticBezier(
        Point(0.0, 0.0),
        Point(2.0, 4.0),
        Point(4.0, 0.0),
        20
    )
    
    println("Quadratic Bezier Curve Points:")
    quadraticCurve.forEachIndexed { index, point ->
        println("Point $index: (${point.x}, ${point.y})")
    }
    
    // Define control points for a cubic Bezier curve
    val cubicPoints = listOf(
        Point(0.0, 0.0),
        Point(1.0, 2.0),
        Point(3.0, 2.0),
        Point(4.0, 0.0)
    )
    
    // Generate points for the cubic curve
    val cubicCurve = bezier.generateCubicBezier(
        Point(0.0, 0.0),
        Point(1.0, 2.0),
        Point(3.0, 2.0),
        Point(4.0, 0.0),
        20
    )
    
    println("\nCubic Bezier Curve Points:")
    cubicCurve.forEachIndexed { index, point ->
        println("Point $index: (${point.x}, ${point.y})")
    }
    
    // Calculate a specific point on the curve
    val specificPoint = bezier.calculatePoint(quadraticPoints, 0.5)
    println("\nPoint at t=0.5: (${specificPoint.x}, ${specificPoint.y})")
}
```

## Key Features of this Implementation:

1. **Point Data Class**: Represents 2D coordinates with operator overloading for mathematical operations
2. **Bernstein Polynomial**: Core mathematical formula for Bezier curve calculation
3. **Flexible Curve Generation**: Supports curves with any number of control points
4. **Specific Curve Types**: Dedicated methods for quadratic and cubic Bezier curves
5. **Parameter Control**: Adjustable number of points for curve resolution

## Mathematical Background:

The Bezier curve is defined by the formula:
```
B(t) = Σ(i=0 to n) C(n,i) * (1-t)^(n-i) * t^i * P_i
```

Where:
- `C(n,i)` is the binomial coefficient
- `P_i` are the control points
- `t` is the parameter (0 to 1)

This implementation provides a solid foundation for generating smooth curves in graphics applications, animations, or any scenario requiring smooth interpolation between points.

