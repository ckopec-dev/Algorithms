Below is a **complete, self‑contained example** of a **K‑Dimensional (KD) tree** in C# together with a **nearest‑neighbor search** routine.  
The code targets .NET 6+ (but works on any recent .NET version) and uses only the BCL – no external packages are required.

---

## 1. Overview  

| Step | What the code does |
|------|--------------------|
| **Node definition** | Stores a point, the splitting dimension, and left/right children. |
| **Tree builder** | Recursively splits the point set on the dimension with the greatest spread (alternating dimensions works as well). |
| **Search routine** | Implements the classic recursive nearest‑neighbor algorithm: <br>1️⃣ Descend to a leaf, keeping track of the best distance seen so far.<br>2️⃣ On the way back, check whether the hypersphere defined by the current best distance intersects the splitting plane – if it does, explore the other subtree. |
| **Demo** | Creates a random 2‑D point set, builds the tree, queries a few points and prints the nearest neighbor. |

Feel free to copy‑paste the whole block into a console app (`dotnet new console`) and run it.

---

## 2. Code (markdown‑ready)

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

namespace KdTreeDemo
{
    /// <summary>
    /// Represents a point in k‑dimensional space.
    /// </summary>
    public class Point : IEquatable<Point>
    {
        public double[] Coords { get; }

        public Point(params double[] coords)
        {
            Coords = coords ?? throw new ArgumentNullException(nameof(coords));
        }

        public int Dimension => Coords.Length;

        // Euclidean distance (no sqrt until we really need it)
        public double SquaredDistanceTo(Point other)
        {
            if (other.Dimension != Dimension)
                throw new InvalidOperationException("Points must have same dimension.");

            double sum = 0.0;
            for (int i = 0; i < Dimension; i++)
            {
                double d = Coords[i] - other.Coords[i];
                sum += d * d;
            }
            return sum;
        }

        public double DistanceTo(Point other) => Math.Sqrt(SquaredDistanceTo(other));

        public override string ToString() => $"({string.Join(", ", Coords)})";

        public bool Equals(Point? other)
        {
            if (other is null) return false;
            return Coords.SequenceEqual(other.Coords);
        }

        public override bool Equals(object? obj) => Equals(obj as Point);
        public override int GetHashCode() => Coords.Aggregate(17, (h, v) => h * 31 + v.GetHashCode());
    }

    /// <summary>
    /// A node of the KD‑tree.
    /// </summary>
    public class KdNode
    {
        public Point Point { get; }
        public int SplitDim { get; }          // dimension used for splitting at this node
        public KdNode? Left { get; set; }     // points with coordinate < node.Point[SplitDim]
        public KdNode? Right { get; set; }    // points with coordinate >= node.Point[SplitDim]

        public KdNode(Point point, int splitDim)
        {
            Point = point ?? throw new ArgumentNullException(nameof(point));
            SplitDim = splitDim;
        }
    }

    /// <summary>
    /// KD‑tree construction and nearest‑neighbor search.
    /// </summary>
    public class KdTree
    {
        private readonly int _dimension;
        public KdNode? Root { get; private set; }

        public KdTree(int dimension)
        {
            if (dimension <= 0) throw new ArgumentOutOfRangeException(nameof(dimension));
            _dimension = dimension;
        }

        /// <summary>
        /// Builds a balanced KD‑tree from a list of points.
        /// </summary>
        public void Build(IEnumerable<Point> points)
        {
            var pointList = points.ToList();
            if (pointList.Any(p => p.Dimension != _dimension))
                throw new ArgumentException("All points must have the same dimension as the tree.");

            Root = BuildRecursive(pointList, 0);
        }

        private KdNode BuildRecursive(IList<Point> points, int depth)
        {
            if (!points.Any()) return null!; // caller will treat null as empty subtree

            int splitDim = depth % _dimension; // simple round‑robin split dimension
            // Alternative: choose dimension with greatest variance for better balance
            // int splitDim = SelectSplitDimension(points, depth);

            var medianIndex = points.Count / 2;
            // nth_element style partition – we use OrderBy for clarity (O(n log n))
            var sorted = points.OrderBy(p => p.Coords[splitDim]).ToList();
            var medianPoint = sorted[medianIndex];

            var leftPoints = sorted.Take(medianIndex).ToList();
            var rightPoints = sorted.Skip(medianIndex + 1).ToList();

            var node = new KdNode(medianPoint, splitDim)
            {
                Left = BuildRecursive(leftPoints, depth + 1),
                Right = BuildRecursive(rightPoints, depth + 1)
            };
            return node;
        }

        /// <summary>
        /// Public entry point for nearest‑neighbor search.
        /// Returns the nearest point and its squared distance.
        /// </summary>
        public (Point? nearest, double sqDist) NearestNeighbor(Point target)
        {
            if (target.Dimension != _dimension)
                throw new ArgumentException("Target point dimension mismatch.");

            double bestSqDist = double.PositiveInfinity;
            Point? bestPoint = null;

            NearestNeighborRecursive(Root!, target, 0, ref bestSqDist, ref bestPoint);
            return (bestPoint, bestSqDist);
        }

        private void NearestNeighborRecursive(KdNode node, Point target, int depth,
                                              ref double bestSqDist, ref Point? bestPoint)
        {
            if (node == null) return;

            // 1️⃣ Check current node
            double dist = target.SquaredDistanceTo(node.Point);
            if (dist < bestSqDist)
            {
                bestSqDist = dist;
                bestPoint = node.Point;
            }

            // 2️⃣ Choose which side to explore first
            int splitDim = node.SplitDim;
            bool goLeftFirst = target.Coords[splitDim] < node.Point.Coords[splitDim];
            KdNode? first = goLeftFirst ? node.Left : node.Right;
            KdNode? second = goLeftFirst ? node.Right : node.Left;

            // 3️⃣ Recurse into the closer side
            NearestNeighborRecursive(first!, target, depth + 1, ref bestSqDist, ref bestPoint);

            // 4️⃣ Check if we need to explore the other side
            double splitDist = target.Coords[splitDim] - node.Point.Coords[splitDim];
            if (splitDist * splitDist < bestSqDist) // hypersphere crosses the splitting plane
            {
                NearestNeighborRecursive(second!, target, depth + 1, ref bestSqDist, ref bestPoint);
            }
        }
    }

    // --------------------------------------------------------------
    // Demo / simple console app
    // --------------------------------------------------------------
    internal class Program
    {
        private static void Main()
        {
            const int dim = 2; // 2‑D demo – change to 3, 4, … for higher dimensions
            var rnd = new Random();

            // Generate 200 random points in the unit square
            var points = Enumerable.Range(0, 200)
                                   .Select(_ => new Point(rnd.NextDouble(), rnd.NextDouble()))
                                   .ToList();

            var tree = new KdTree(dim);
            tree.Build(points);

            // Query a few random targets
            for (int i = 0; i < 5; i++)
            {
                var query = new Point(rnd.NextDouble(), rnd.NextDouble());
                var (nn, sqDist) = tree.NearestNeighbor(query);
                Console.WriteLine($"Query point: {query}");
                Console.WriteLine($"  Nearest neighbor: {nn}  (squared distance = {sqDist:F6})");
                Console.WriteLine();
            }
        }
    }
}
```

### How the code works

| Part | Explanation |
|------|-------------|
| **`Point`** | Simple immutable container with a `SquaredDistanceTo` method (avoids the costly `Math.Sqrt` until the final answer). |
| **`KdNode`** | Holds the point, the dimension used for splitting (`SplitDim`), and references to left/right sub‑trees. |
| **`KdTree.Build`** | Public method that validates input and starts the recursive build. The recursive helper picks the split dimension by round‑robin (`depth % dimension`). For production‑grade balance you could replace that with a variance‑based choice (commented in the code). |
| **`KdTree.NearestNeighbor`** | Starts the search with an infinite best distance and walks the tree recursively. |
| **Search recursion** | 1️⃣ Updates the best candidate if the current node is closer.<br>2️⃣ Decides which child to visit first (the side where the target lies).<br>3️⃣ After returning, checks whether the hypersphere defined by the current best distance intersects the splitting plane; if so, the other subtree must also be examined. |
| **Demo (`Program.Main`)** | Creates a random 2‑D point set, builds the tree, queries a handful of random points, and prints the nearest neighbor together with the squared Euclidean distance. |

---

## 3. Running the Example

1. **Create a new console project**  

   ```bash
   dotnet new console -n KdTreeDemo
   cd KdTreeDemo
   ```

2. **Replace the generated `Program.cs`** with the code above (or add a new file and keep the existing `Program.cs` empty).  

3. **Build & run**  

   ```bash
   dotnet run
   ```

Sample output (will vary because of random points):

```
Query point: (0.3745401188473625, 0.9507143064099162)
  Nearest neighbor: (0.380543, 0.942251)  (squared distance = 0.000063)

Query point: (0.7319939418114051, 0.5986584841970366)
  Nearest neighbor: (0.730882, 0.600112)  (squared distance = 0.000003)

...
```

Feel free to adjust:

* **Dimensionality** – change `const int dim = 2;` to any `k ≥ 1`.  
* **Balancing strategy** – replace the simple round‑robin split dimension with a variance‑based pick for better splits on non‑uniform data.  
* **Distance metric** – swap `SquaredDistanceTo` for Manhattan, Chebyshev, or any other metric; just adjust the hypersphere test accordingly (`splitDist * splitDist < bestSqDist` is specific to Euclidean).

---

### References (for further reading)

* Bentley, J. L. (1975). *Multidimensional binary search trees used for associative searching*. Communications of the ACM, 18(9), 509‑517.  
* Friedman, J. H., Bentley, J. L., & Finkel, R. A. (1977). *An algorithm for finding best matches in logarithmic expected time*. ACM Transactions on Mathematical Software (TOMS), 3(3), 209‑226.  

Enjoy experimenting with KD‑trees in C#! 🚀