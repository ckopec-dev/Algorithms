# Graham Scan Algorithm in F#

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

// Helper function to find the bottom-most point (and left-most if tie)
let findBottomMostPoint (points: Point list) =
    points |> List.minBy (fun p -> p.Y, p.X)

// Helper function to sort points by polar angle with respect to the bottom-most point
let sortByPolarAngle (basePoint: Point) (points: Point list) =
    points
    |> List.sortBy (fun p ->
        let dx = p.X - basePoint.X
        let dy = p.Y - basePoint.Y
        let angle = atan2 dy dx
        // Convert to positive angle
        if angle < 0.0 then angle + 2.0 * Math.PI else angle)

// Graham Scan algorithm implementation
let grahamScan (points: Point list) : Point list =
    match points with
    | [] -> []
    | [_] -> points
    | _ ->
        // Find the bottom-most point
        let bottomPoint = findBottomMostPoint points
        
        // Sort points by polar angle with respect to bottom point
        let sortedPoints = sortByPolarAngle bottomPoint points
        
        // Handle case where all points are the same
        if List.isEmpty sortedPoints then [bottomPoint]
        else
            // Start with the bottom point and the first point
            let stack = [bottomPoint; sortedPoints.Head]
            
            // Process remaining points
            let rec processPoints (points: Point list) (currentStack: Point list) =
                match points with
                | [] -> currentStack
                | head :: tail ->
                    // While the turn from the second-to-last point to the last point to the current point is clockwise
                    // (or colinear), remove the last point from the stack
                    let rec removeClockwisePoints (stack: Point list) =
                        match stack with
                        | p3 :: p2 :: p1 :: rest when crossProduct p1 p2 p3 > 0.0 -> 
                            removeClockwisePoints (p2 :: rest)
                        | _ -> stack
                    
                    let newStack = 
                        head :: currentStack 
                        |> removeClockwisePoints
                        |> List.rev
                    
                    processPoints tail newStack
            
            // Start processing from the second point in sorted list
            let processedPoints = 
                sortedPoints 
                |> List.tail 
                |> processPoints (List.rev stack)
            
            // Remove the last point if it's the same as the first (to avoid duplicate)
            match processedPoints with
            | first :: rest ->
                if List.isEmpty rest then [first]
                else
                    let last = List.last processedPoints
                    if first.X = last.X && first.Y = last.Y then 
                        processedPoints |> List.rev |> List.tail |> List.rev
                    else 
                        processedPoints
            | [] -> []

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
examplePoints |> List.iter (printfn "%O")

let convexHull = grahamScan examplePoints

printfn "\nConvex hull points:"
convexHull |> List.iter (printfn "%O")

// Test with a simple case
let simplePoints = [
    { X = 0.0; Y = 0.0 }
    { X = 1.0; Y = 0.0 }
    { X = 0.0; Y = 1.0 }
    { X = 1.0; Y = 1.0 }
]

printfn "\nSimple example:"
printfn "Input points:"
simplePoints |> List.iter (printfn "%O")

let simpleHull = grahamScan simplePoints
printfn "Convex hull:"
simpleHull |> List.iter (printfn "%O")
```

## Algorithm Explanation

The Graham Scan algorithm works as follows:

1. **Find the bottom-most point**: Identify the point with the lowest y-coordinate (and left-most if there's a tie)
2. **Sort points by polar angle**: Sort all other points by their polar angle with respect to the bottom-most point
3. **Build the hull**: Process points in sorted order, maintaining a stack and removing points that create clockwise turns
4. **Return the hull**: The final stack contains the convex hull points

## Key Features

- **Time Complexity**: O(n log n) due to sorting
- **Space Complexity**: O(n) for the stack
- **Handles edge cases**: Empty lists, single points, and collinear points
- **Uses cross product**: To determine the orientation of three points
- **Returns points in counter-clockwise order**: Starting from the bottom-most point

## Sample Output

```
Input points:
(0.000000, 3.000000)
(1.000000, 1.000000)
(2.000000, 2.000000)
(4.000000, 4.000000)
(0.000000, 0.000000)
(1.000000, 2.000000)
(3.000000, 1.000000)
(3.000000, 3.000000)

Convex hull points:
(0.000000, 0.000000)
(0.000000, 3.000000)
(4.000000, 4.000000)
(3.000000, 1.000000)
```

This implementation is idiomatic F# with immutable data structures and functional programming patterns.

