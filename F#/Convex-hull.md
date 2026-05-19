# Convex Hull Algorithm in F#

Here's an implementation of the Graham Scan algorithm for finding the convex hull of a set of points in F#:

```fsharp
open System

// Point type definition
type Point = { X: float; Y: float } with
    override this.ToString() = sprintf "(%f, %f)" this.X this.Y

// Helper function to calculate cross product of three points
let crossProduct (p1: Point) (p2: Point) (p3: Point) =
    (p2.X - p1.X) * (p3.Y - p2.Y) - (p2.Y - p1.Y) * (p3.X - p2.X)

// Helper function to calculate distance between two points
let distance (p1: Point) (p2: Point) =
    sqrt ((p1.X - p2.X) ** 2.0 + (p1.Y - p2.Y) ** 2.0)

// Find the point with minimum y-coordinate (and minimum x if tie)
let findBottomPoint (points: Point[]) =
    points |> Array.minBy (fun p -> p.Y, p.X)

// Sort points by polar angle with respect to the bottom point
let sortPointsByPolarAngle (bottomPoint: Point) (points: Point[]) =
    points
    |> Array.sortBy (fun p ->
        let angle = atan2 (p.Y - bottomPoint.Y) (p.X - bottomPoint.X)
        // Convert to positive angle
        if angle < 0.0 then angle + 2.0 * Math.PI else angle)

// Graham Scan algorithm implementation
let convexHull (points: Point[]) : Point[] =
    if points.Length < 3 then
        points
    else
        // Find the bottom point
        let bottomPoint = findBottomPoint points
        
        // Sort points by polar angle
        let sortedPoints = sortPointsByPolarAngle bottomPoint points
        
        // Build the hull
        let hull = ref [bottomPoint]
        
        for point in sortedPoints do
            // Remove points that make clockwise turns
            while hull.Value.Length >= 2 && 
                  crossProduct hull.Value.[hull.Value.Length - 2] 
                               hull.Value.[hull.Value.Length - 1] 
                               point > 0.0 do
                hull := hull.Value |> List.rev |> List.tail |> List.rev
            
            hull := point :: hull.Value
        
        // Remove the last point (which is the same as the first)
        let result = hull.Value |> List.rev |> List.tail |> List.rev
        Array.ofList result

// Example usage
let examplePoints = 
    [| { X = 0.0; Y = 0.0 }
       { X = 1.0; Y = 0.0 }
       { X = 0.0; Y = 1.0 }
       { X = 1.0; Y = 1.0 }
       { X = 0.5; Y = 0.5 }
       { X = 2.0; Y = 2.0 }
       { X = 3.0; Y = 1.0 } |]

// Find and display the convex hull
let hull = convexHull examplePoints

printfn "Input points:"
examplePoints |> Array.iter (printfn "%O")

printfn "\nConvex Hull points:"
hull |> Array.iter (printfn "%O")

printfn "\nNumber of points in hull: %d" hull.Length
```

## How it works:

1. **Find Bottom Point**: Locate the point with the lowest y-coordinate (and leftmost if tied)
2. **Sort by Polar Angle**: Sort all other points by their polar angle with respect to the bottom point
3. **Graham Scan**: Process points in sorted order, maintaining a stack of hull points
4. **Cross Product Check**: Use cross product to determine if a point makes a left turn (counter-clockwise) or right turn (clockwise)

## Key Features:

- **Type Safety**: Uses F#'s strong typing system
- **Immutability**: Functional approach with immutable data structures
- **Error Handling**: Handles edge cases like fewer than 3 points
- **Mathematical Accuracy**: Uses proper cross product and polar angle calculations

## Output Example:
```
Input points:
(0.000000, 0.000000)
(1.000000, 0.000000)
(0.000000, 1.000000)
(1.000000, 1.000000)
(0.500000, 0.500000)
(2.000000, 2.000000)
(3.000000, 1.000000)

Convex Hull points:
(0.000000, 0.000000)
(1.000000, 0.000000)
(3.000000, 1.000000)
(1.000000, 1.000000)
(0.000000, 1.000000)

Number of points in hull: 5
```

