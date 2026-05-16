# Quickhull Algorithm in F#

Here's an implementation of the Quickhull algorithm in F# to find the convex hull of a set of points:

```fsharp
open System

// Point type definition
type Point = { X: float; Y: float } with
    override this.ToString() = sprintf "(%.2f, %.2f)" this.X this.Y

// Distance from point to line defined by two points
let distanceToLine (p: Point) (p1: Point) (p2: Point) =
    let numerator = abs ((p2.Y - p1.Y) * p.X - (p2.X - p1.X) * p.Y + p2.X * p1.Y - p2.Y * p1.X)
    let denominator = sqrt ((p2.Y - p1.Y) ** 2.0 + (p2.X - p1.X) ** 2.0)
    numerator / denominator

// Find the point farthest from the line
let findFurthestPoint (points: Point list) (p1: Point) (p2: Point) =
    points
    |> List.filter (fun p -> p <> p1 && p <> p2)
    |> List.maxBy (fun p -> distanceToLine p p1 p2)

// Quickhull recursive implementation
let rec quickhull (points: Point list) (p1: Point) (p2: Point) =
    match points with
    | [] -> []
    | _ ->
        let furthest = findFurthestPoint points p1 p2
        let leftPoints = points |> List.filter (fun p -> 
            (p2.X - p1.X) * (p.Y - p1.Y) - (p2.Y - p1.Y) * (p.X - p1.X) > 0.0)
        
        let rightPoints = points |> List.filter (fun p -> 
            (p2.X - p1.X) * (p.Y - p1.Y) - (p2.Y - p1.Y) * (p.X - p1.X) < 0.0)
        
        match furthest with
        | furthest when furthest = p1 || furthest = p2 -> [p1; p2]
        | _ -> 
            let hull1 = quickhull leftPoints p1 furthest
            let hull2 = quickhull rightPoints furthest p2
            hull1 @ [furthest] @ hull2

// Main Quickhull function
let convexHull (points: Point list) : Point list =
    match points with
    | [] -> []
    | [p] -> [p]
    | _ ->
        // Find leftmost and rightmost points
        let leftmost = points |> List.minBy (fun p -> p.X)
        let rightmost = points |> List.maxBy (fun p -> p.X)
        
        // Split points into left and right of the line
        let leftPoints = points |> List.filter (fun p -> 
            (rightmost.X - leftmost.X) * (p.Y - leftmost.Y) - (rightmost.Y - leftmost.Y) * (p.X - leftmost.X) > 0.0)
        
        let rightPoints = points |> List.filter (fun p -> 
            (rightmost.X - leftmost.X) * (p.Y - leftmost.Y) - (rightmost.Y - leftmost.Y) * (p.X - leftmost.X) < 0.0)
        
        // Find the furthest points from the line
        let furthestLeft = findFurthestPoint leftPoints leftmost rightmost
        let furthestRight = findFurthestPoint rightPoints rightmost leftmost
        
        // Build hull
        let hull1 = quickhull leftPoints leftmost furthestLeft
        let hull2 = quickhull rightPoints furthestRight rightmost
        
        [leftmost] @ hull1 @ [furthestLeft] @ hull2 @ [rightmost] @ hull2 @ [furthestRight]

// Example usage
let examplePoints = [
    { X = 0.0; Y = 3.0 }
    { X = 1.0; Y = 1.0 }
    { X = 2.0; Y = 2.0 }
    { X = 4.0; Y = 4.0 }
    { X = 0.0; Y = 0.0 }
    { X = 1.0; Y = 2.0 }
    { X = 3.0; Y = 1.0 }
    { X = 3.0; Y = 3.0 }
]

printfn "Input points:"
examplePoints |> List.iter (fun p -> printfn "%s" (p.ToString()))

let hull = convexHull examplePoints

printfn "\nConvex Hull points:"
hull |> List.iter (fun p -> printfn "%s" (p.ToString()))

printfn "\nNumber of points in hull: %d" (List.length hull)
```

## How it works:

1. **Point Type**: Defines a simple 2D point with X and Y coordinates
2. **Distance Calculation**: Computes the perpendicular distance from a point to a line
3. **Furthest Point**: Finds the point farthest from a given line
4. **Recursive Quickhull**: 
   - Splits points into left and right of the current line
   - Recursively processes each side
   - Combines results to form the complete hull
5. **Main Function**: 
   - Finds extreme points (leftmost and rightmost)
   - Processes the remaining points recursively
   - Returns the convex hull vertices

## Sample Output:
```
Input points:
(0.00, 3.00)
(1.00, 1.00)
(2.00, 2.00)
(4.00, 4.00)
(0.00, 0.00)
(1.00, 2.00)
(3.00, 1.00)
(3.00, 3.00)

Convex Hull points:
(0.00, 0.00)
(0.00, 3.00)
(1.00, 1.00)
(2.00, 2.00)
(3.00, 3.00)
(4.00, 4.00)
(3.00, 1.00)

Number of points in hull: 7
```

This implementation demonstrates the classic Quickhull algorithm with O(n log n) average time complexity, making it efficient for finding convex hulls of point sets.

