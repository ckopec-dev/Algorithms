```fsharp
// Gift Wrapping Algorithm (Jarvis March) implementation in F#

open System

// Point type definition
type Point = { X: float; Y: float } with
    override this.ToString() = sprintf "(%f, %f)" this.X this.Y

// Calculate cross product of three points to determine orientation
let crossProduct (p1: Point) (p2: Point) (p3: Point) =
    (p2.X - p1.X) * (p3.Y - p2.Y) - (p2.Y - p1.Y) * (p3.X - p2.X)

// Determine if three points make a counterclockwise turn
let isCounterClockwise (p1: Point) (p2: Point) (p3: Point) =
    crossProduct p1 p2 p3 > 0.0

// Find the leftmost point (lowest x-coordinate, or lowest y if tie)
let findLeftmostPoint (points: Point[]) =
    points |> Array.minBy (fun p -> p.X, p.Y)

// Gift Wrapping Algorithm implementation
let giftWrapping (points: Point[]) : Point[] =
    if points.Length < 3 then
        points
    else
        // Find the leftmost point as starting point
        let start = findLeftmostPoint points
        let mutable hull = [start]
        let mutable current = start
        
        // Continue until we return to the starting point
        while true do
            let mutable next = points.[0]
            
            // Find the point that makes the largest counter-clockwise turn
            for i in 0 .. points.Length - 1 do
                if current = points.[i] then
                    ()
                elif next = current || isCounterClockwise current next points.[i] then
                    next <- points.[i]
            
            // If we've completed the hull
            if next = start then
                break
            
            hull <- next :: hull
            current <- next
        
        // Reverse to get clockwise order (standard for convex hull)
        List.rev hull |> List.toArray

// Example usage
let examplePoints = 
    [| { X = 0.0; Y = 3.0 }
       { X = 1.0; Y = 1.0 }
       { X = 2.0; Y = 2.0 }
       { X = 4.0; Y = 4.0 }
       { X = 0.0; Y = 0.0 }
       { X = 1.0; Y = 2.0 }
       { X = 3.0; Y = 1.0 }
       { X = 3.0; Y = 3.0 } |]

// Run the algorithm
let convexHull = giftWrapping examplePoints

// Display results
printfn "Input points:"
examplePoints |> Array.iter (fun p -> printfn "%s" (p.ToString()))
printfn "\nConvex Hull Points:"
convexHull |> Array.iter (fun p -> printfn "%s" (p.ToString()))
```

This implementation demonstrates the Gift Wrapping algorithm (also known as Jarvis March) for finding the convex hull of a set of points. The algorithm works by:

1. Finding the leftmost point to start
2. Repeatedly finding the next point that makes the largest counter-clockwise turn
3. Continuing until returning to the starting point

Key features:
- Uses cross product to determine orientation of three points
- Handles edge cases (fewer than 3 points)
- Returns points in clockwise order (standard convex hull convention)
- Clean F# functional style with immutable data structures

The time complexity is O(nh) where n is the number of input points and h is the number of points on the hull.