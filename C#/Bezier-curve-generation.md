# Bezier Curve Generation Algorithm in C#

```csharp
using System;
using System.Collections.Generic;
using System.Drawing;

public class BezierCurve
{
    /// <summary>
    /// Generates a Bezier curve using the De Casteljau algorithm
    /// </summary>
    /// <param name="controlPoints">List of control points</param>
    /// <param name="numPoints">Number of points to generate</param>
    /// <returns>List of points representing the Bezier curve</returns>
    public static List<PointF> GenerateBezierCurve(List<PointF> controlPoints, int numPoints)
    {
        if (controlPoints == null || controlPoints.Count < 2)
            throw new ArgumentException("At least 2 control points are required");

        List<PointF> curvePoints = new List<PointF>();
        
        // For each point on the curve
        for (int i = 0; i <= numPoints; i++)
        {
            double t = (double)i / numPoints;
            PointF point = DeCasteljau(controlPoints, t);
            curvePoints.Add(point);
        }
        
        return curvePoints;
    }
    
    /// <summary>
    /// De Casteljau algorithm implementation for Bezier curve calculation
    /// </summary>
    /// <param name="points">Control points</param>
    /// <param name="t">Parameter value between 0 and 1</param>
    /// <returns>Point on the curve at parameter t</returns>
    private static PointF DeCasteljau(List<PointF> points, double t)
    {
        int n = points.Count - 1;
        PointF[] currentPoints = new PointF[points.Count];
        
        // Copy initial points
        Array.Copy(points.ToArray(), currentPoints, points.Count);
        
        // De Casteljau recursion
        for (int i = 1; i <= n; i++)
        {
            for (int j = 0; j <= n - i; j++)
            {
                currentPoints[j].X = (float)(currentPoints[j].X * (1 - t) + currentPoints[j + 1].X * t);
                currentPoints[j].Y = (float)(currentPoints[j].Y * (1 - t) + currentPoints[j + 1].Y * t);
            }
        }
        
        return currentPoints[0];
    }
    
    /// <summary>
    /// Alternative implementation using mathematical formula for quadratic Bezier curves
    /// </summary>
    /// <param name="p0">Start point</param>
    /// <param name="p1">Control point</param>
    /// <param name="p2">End point</param>
    /// <param name="numPoints">Number of points to generate</param>
    /// <returns>List of points representing the quadratic Bezier curve</returns>
    public static List<PointF> GenerateQuadraticBezier(PointF p0, PointF p1, PointF p2, int numPoints)
    {
        List<PointF> curvePoints = new List<PointF>();
        
        for (int i = 0; i <= numPoints; i++)
        {
            double t = (double)i / numPoints;
            
            // Quadratic Bezier formula: B(t) = (1-t)²P0 + 2(1-t)tP1 + t²P2
            float x = (float)(Math.Pow(1 - t, 2) * p0.X + 2 * (1 - t) * t * p1.X + Math.Pow(t, 2) * p2.X);
            float y = (float)(Math.Pow(1 - t, 2) * p0.Y + 2 * (1 - t) * t * p1.Y + Math.Pow(t, 2) * p2.Y);
            
            curvePoints.Add(new PointF(x, y));
        }
        
        return curvePoints;
    }
    
    /// <summary>
    /// Alternative implementation using mathematical formula for cubic Bezier curves
    /// </summary>
    /// <param name="p0">Start point</param>
    /// <param name="p1">First control point</param>
    /// <param name="p2">Second control point</param>
    /// <param name="p3">End point</param>
    /// <param name="numPoints">Number of points to generate</param>
    /// <returns>List of points representing the cubic Bezier curve</returns>
    public static List<PointF> GenerateCubicBezier(PointF p0, PointF p1, PointF p2, PointF p3, int numPoints)
    {
        List<PointF> curvePoints = new List<PointF>();
        
        for (int i = 0; i <= numPoints; i++)
        {
            double t = (double)i / numPoints;
            
            // Cubic Bezier formula: B(t) = (1-t)³P0 + 3(1-t)²tP1 + 3(1-t)t²P2 + t³P3
            float x = (float)(Math.Pow(1 - t, 3) * p0.X + 
                              3 * Math.Pow(1 - t, 2) * t * p1.X + 
                              3 * (1 - t) * Math.Pow(t, 2) * p2.X + 
                              Math.Pow(t, 3) * p3.X);
            
            float y = (float)(Math.Pow(1 - t, 3) * p0.Y + 
                              3 * Math.Pow(1 - t, 2) * t * p1.Y + 
                              3 * (1 - t) * Math.Pow(t, 2) * p2.Y + 
                              Math.Pow(t, 3) * p3.Y);
            
            curvePoints.Add(new PointF(x, y));
        }
        
        return curvePoints;
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        // Example 1: Using De Casteljau algorithm with 4 control points (cubic)
        List<PointF> controlPoints = new List<PointF>
        {
            new PointF(0, 0),
            new PointF(100, 200),
            new PointF(200, 100),
            new PointF(300, 300)
        };
        
        List<PointF> curve = BezierCurve.GenerateBezierCurve(controlPoints, 100);
        
        Console.WriteLine("Generated cubic Bezier curve points:");
        foreach (PointF point in curve)
        {
            Console.WriteLine($"({point.X:F2}, {point.Y:F2})");
        }
        
        // Example 2: Using quadratic Bezier curve
        Console.WriteLine("\nQuadratic Bezier curve:");
        List<PointF> quadraticCurve = BezierCurve.GenerateQuadraticBezier(
            new PointF(0, 300),
            new PointF(150, 100),
            new PointF(300, 300),
            50
        );
        
        foreach (PointF point in quadraticCurve)
        {
            Console.WriteLine($"({point.X:F2}, {point.Y:F2})");
        }
    }
}
```

## Key Features of this Implementation:

1. **De Casteljau Algorithm**: The main implementation uses the numerically stable De Casteljau algorithm for generating Bezier curves of any degree.

2. **Multiple Curve Types**: Supports quadratic and cubic Bezier curves with direct mathematical formulas.

3. **Flexible Control**: Accepts any number of control points and generates smooth curves.

4. **Efficient**: Uses iterative approach with O(n²) complexity for n control points.

5. **Error Handling**: Includes proper validation and error handling.

## Mathematical Background:

- **Quadratic Bezier**: B(t) = (1-t)²P₀ + 2(1-t)tP₁ + t²P₂
- **Cubic Bezier**: B(t) = (1-t)³P₀ + 3(1-t)²tP₁ + 3(1-t)t²P₂ + t³P₃

The algorithm generates smooth curves by interpolating between control points using the Bernstein polynomial basis functions.

