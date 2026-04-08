Below is a **complete, self‑contained example** of the **Andrew monotone‑chain convex‑hull algorithm** written in **Visual Basic .NET** (VB.NET).  
The code defines a `Point` structure, a `ConvexHull` function that returns the hull vertices in counter‑clockwise order, and a small console test that prints the result.

---  

## 1. How the algorithm works (brief)

1. **Sort** the input points lexicographically (by X, then Y).  
2. Build the **lower hull** by scanning the sorted list left‑to‑right, discarding points that would cause a non‑counter‑clockwise turn.  
3. Build the **upper hull** by scanning the sorted list right‑to‑left with the same rule.  
4. Concatenate lower + upper (excluding the duplicate end points) → the convex hull.

The core orientation test uses the **cross product**:  

```
cross(O, A, B) = (A.X‑O.X)*(B.Y‑O.Y) – (A.Y‑O.Y)*(B.X‑O.X)
```

* `> 0` → counter‑clockwise turn  
* `< 0` → clockwise turn  
* `= 0` → collinear (we treat collinear points as “non‑left turn” and discard the middle one to keep the hull minimal).

---  

## 2. VB.NET implementation  

```vb
' ------------------------------------------------------------
' ConvexHull.vb  –  Andrew monotone chain algorithm (VB.NET)
' ------------------------------------------------------------
Imports System
Imports System.Collections.Generic

Module ConvexHullDemo

    ''' <summary>
    ''' Simple 2‑D point with integer coordinates.
    ''' </summary>
    Public Structure Point
        Implements IComparable(Of Point)

        Public Property X As Integer
        Public Property Y As Integer

        Public Sub New(x As Integer, y As Integer)
            Me.X = x
            Me.Y = y
        End Sub

        ''' <summary>
        ''' Lexicographic ordering: first by X, then by Y.
        ''' </summary>
        Public Function CompareTo(other As Point) As Integer _
            Implements IComparable(Of Point).CompareTo
            If Me.X <> other.X Then Return Me.X.CompareTo(other.X)
            Return Me.Y.CompareTo(other.Y)
        End Function

        Public Overrides Function ToString() As String
            Return $"({X},{Y})"
        End Function
    End Structure

    ''' <summary>
    ''' Returns the signed area (cross product) of OA × OB.
    ''' Positive → O‑A‑B makes a left turn (counter‑clockwise).
    ''' </summary>
    Private Function Cross(o As Point, a As Point, b As Point) As Long
        Return (CLng(a.X) - o.X) * (CLng(b.Y) - o.Y) -
               (CLng(a.Y) - o.Y) * (CLng(b.X) - o.X)
    End Function

    ''' <summary>
    ''' Computes the convex hull of a set of points.
    ''' Returns the hull vertices in counter‑clockwise order,
    ''' starting with the left‑most lowest point.
    ''' </summary>
    ''' <param name="points">Input point collection (may contain duplicates).</param>
    ''' <returns>List of points forming the convex hull.</returns>
    Public Function ConvexHull(points As IEnumerable(Of Point)) As List(Of Point)
        ' Remove duplicates and sort
        Dim sorted = points.Distinct().OrderBy(Function(p) p).ToList()
        If sorted.Count <= 1 Then Return New List(Of Point)(sorted)

        Dim lower As New List(Of Point)
        Dim upper As New List(Of Point)

        ' ---- lower hull ----
        For Each p In sorted
            While lower.Count >= 2 AndAlso
                  Cross(lower(lower.Count - 2), lower(lower.Count - 1), p) <= 0
                lower.RemoveAt(lower.Count - 1)
            End While
            lower.Add(p)
        Next

        ' ---- upper hull ----
        For i As Integer = sorted.Count - 1 To 0 Step -1
            Dim p = sorted(i)
            While upper.Count >= 2 AndAlso
                  Cross(upper(upper.Count - 2), upper(upper.Count - 1), p) <= 0
                upper.RemoveAt(upper.Count - 1)
            End While
            upper.Add(p)
        Next

        ' Concatenate lower and upper, removing the last point of each
        ' because it is repeated at the beginning of the other list.
        lower.RemoveAt(lower.Count - 1)
        upper.RemoveAt(upper.Count - 1)
        lower.AddRange(upper)

        Return lower
    End Function

    ''' <summary>
    ''' Demo: generate a random set of points, compute hull, print both.
    ''' </summary>
    Sub Main()
        Dim rnd As New Random(0)
        Dim pts As New List(Of Point)

        ' Create 20 random points in a 0‑100 square
        For i As Integer = 0 To 19
            pts.Add(New Point(rnd.Next(0, 101), rnd.Next(0, 101)))
        Next

        Console.WriteLine("Input points:")
        For Each p In pts
            Console.Write(p.ToString() & " ")
        Next
        Console.WriteLine()
        Console.WriteLine()

        Dim hull As List(Of Point) = ConvexHull(pts)

        Console.WriteLine("Convex hull (CCW):")
        For Each p In hull
            Console.Write(p.ToString() & " ")
        Next
        Console.WriteLine()
        Console.WriteLine($"Hull size: {hull.Count}")
    End Sub

End Module
```

### How to run

1. **Create a new VB.NET Console App** (e.g., via Visual Studio or `dotnet new console -lang VB`).  
2. Replace the generated `Module1.vb` (or add a new file) with the code above.  
3. Build and run:  

```bash
dotnet run
```

You’ll see something like:

```
Input points:
(23,84) (88,12) (75,66) ( ... )
...

Convex hull (CCW):
(12,5) (94,3) (99,87) (2,78) 
Hull size: 4
```

(The exact points will differ because of the random seed.)

---  

### Remarks & Possible Tweaks  

* **Collinear points** – The implementation discards intermediate collinear points (`<= 0` in the while‑condition). Change to `< 0` if you want to keep all boundary points.  
* **Floating‑point coordinates** – Replace `Integer` with `Double` and adjust the `Cross` function to return `Double`.  
* **Performance** – Sorting dominates the runtime: **O(n log n)**. The hull construction itself is linear.  
* **Usage** – The `ConvexHull` function can be called from any VB.NET project (WinForms, WPF, ASP.NET, etc.) by passing an `IEnumerable(Of Point)`.

Feel free to copy‑paste the snippet, adapt the `Point` type to your needs, and integrate the hull computation into larger geometric pipelines. Happy coding!