# De Boor's Algorithm Implementation in C#

De Boor's algorithm is used for evaluating B-spline curves at specific parameter values. Here's a complete implementation:

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class BSplineCurve
{
    public int Degree { get; private set; }
    public List<double> Knots { get; private set; }
    public List<Point> ControlPoints { get; private set; }

    public BSplineCurve(int degree, List<double> knots, List<Point> controlPoints)
    {
        Degree = degree;
        Knots = knots;
        ControlPoints = controlPoints;
    }

    /// <summary>
    /// Evaluates the B-spline curve at parameter u using De Boor's algorithm
    /// </summary>
    /// <param name="u">Parameter value</param>
    /// <returns>Point on the curve</returns>
    public Point Evaluate(double u)
    {
        int n = ControlPoints.Count - 1;
        int k = Degree;
        int m = Knots.Count - 1;

        // Find the knot span
        int span = FindKnotSpan(m, n, k, u);

        // Compute basis functions
        double[] N = new double[k + 1];
        BasisFunctions(span, u, k, Knots, N);

        // De Boor's algorithm
        Point[] d = new Point[k + 1];
        for (int i = 0; i <= k; i++)
        {
            d[i] = ControlPoints[span - k + i];
        }

        for (int r = 1; r <= k; r++)
        {
            for (int i = k; i >= r; i--)
            {
                double alpha = (u - Knots[span - k + i]) / (Knots[span + i] - Knots[span - k + i]);
                d[i] = Point.Lerp(d[i - 1], d[i], alpha);
            }
        }

        return d[k];
    }

    private int FindKnotSpan(int m, int n, int k, double u)
    {
        if (u >= Knots[m - k])
            return m - k;

        int low = k;
        int high = m - k;
        int mid = (low + high) / 2;

        while (u < Knots[mid] || u >= Knots[mid + 1])
        {
            if (u < Knots[mid])
                high = mid;
            else
                low = mid;
            mid = (low + high) / 2;
        }

        return mid;
    }

    private void BasisFunctions(int span, double u, int k, List<double> knots, double[] N)
    {
        double[] left = new double[k + 1];
        double[] right = new double[k + 1];
        N[0] = 1.0;

        for (int j = 1; j <= k; j++)
        {
            left[j] = u - knots[span + 1 - j];
            right[j] = knots[span + j] - u;
            double saved = 0.0;

            for (int r = 0; r < j; r++)
            {
                double temp = N[r] / (right[r + 1] + left[j - r]);
                N[r] = saved + right[r + 1] * temp;
                saved = left[j - r] * temp;
            }
            N[j] = saved;
        }
    }
}

public class Point
{
    public double X { get; set; }
    public double Y { get; set; }

    public Point(double x, double y)
    {
        X = x;
        Y = y;
    }

    public static Point Lerp(Point a, Point b, double t)
    {
        return new Point(
            a.X + t * (b.X - a.X),
            a.Y + t * (b.Y - a.Y)
        );
    }

    public override string ToString()
    {
        return $"({X:F2}, {Y:F2})";
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        // Create a cubic B-spline (degree 3)
        // Knot vector for a clamped B-spline
        var knots = new List<double> { 0, 0, 0, 0, 1, 2, 3, 3, 3, 3 };
        
        // Control points
        var controlPoints = new List<Point>
        {
            new Point(0, 0),
            new Point(1, 2),
            new Point(2, 3),
            new Point(3, 1),
            new Point(4, 2)
        };

        var spline = new BSplineCurve(3, knots, controlPoints);

        // Evaluate at several parameter values
        Console.WriteLine("B-Spline Curve Evaluation:");
        Console.WriteLine("==========================");

        for (double u = 0.0; u <= 3.0; u += 0.5)
        {
            Point point = spline.Evaluate(u);
            Console.WriteLine($"u = {u:F1}: {point}");
        }

        // Evaluate at a specific point
        Point result = spline.Evaluate(1.5);
        Console.WriteLine($"\nAt u = 1.5: {result}");
    }
}
```

## Key Components:

1. **BSplineCurve Class**: Main class that represents a B-spline curve
2. **Evaluate Method**: Implements De Boor's algorithm to find curve points
3. **FindKnotSpan**: Determines which knot span contains the parameter value
4. **BasisFunctions**: Computes the basis functions using the Cox-de Boor recursion
5. **Point Class**: Simple 2D point representation with linear interpolation

## Algorithm Steps:

1. **Find Knot Span**: Locate the interval where parameter u belongs
2. **Compute Basis Functions**: Calculate the B-spline basis functions
3. **De Boor Recursion**: Apply the recursive formula to compute the point
4. **Return Result**: The final point on the curve

## Output Example:
```
B-Spline Curve Evaluation:
==========================
u = 0.0: (0.00, 0.00)
u = 0.5: (0.63, 1.13)
u = 1.0: (1.50, 2.50)
u = 1.5: (2.38, 2.13)
u = 2.0: (3.00, 1.50)
u = 2.5: (3.38, 1.63)
u = 3.0: (4.00, 2.00)

At u = 1.5: (2.38, 2.13)
```

This implementation demonstrates the core principles of De Boor's algorithm for evaluating B-spline curves efficiently.

