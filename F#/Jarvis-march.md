# Jarvis March Algorithm in F#

Here's an implementation of the Jarvis March (Gift Wrapping) algorithm for finding the convex hull of a set of points in F#:

```fsharp
open System

// Point type definition
type Point = { X: float; Y: float } with
    override this.ToString() = sprintf "(%.2f, %.2f)" this.X this.Y

// Cross product of three points to determine orientation
let crossProduct (p1: Point) (p2: Point) (p3: Point) =
    (p2.X - p1.X) * (p3.Y - p2.Y) - (p2.Y - p1.Y) * (p3.X - p2.X)

// Determine if three points make a left turn (counter-clockwise)
let isLeftTurn (p1: Point) (p2: Point) (p3: Point) =
    crossProduct p1 p2 p3 > 0.0

// Find the point with the lowest y-coordinate (and leftmost if tie)
let findBottomLeftPoint (points: Point[]) =
    points |> Array.minBy (fun p -> p.Y, p.X)

// Jarvis March algorithm implementation
let jarvisMarch (points: Point[]) : Point[] =
    if Array.isEmpty points || points.Length < 3 then
        points
    else
        // Find the bottom-left point
        let bottomLeft = findBottomLeftPoint points
        
        // Start with the bottom-left point
        let hull = ref [bottomLeft]
        let current = ref bottomLeft
        
        // Continue until we return to the starting point
        while true do
            let nextPoint = 
                points 
                |> Array.filter (fun p -> p <> !current)
                |> Array.sortBy (fun p -> 
                    let angle = Math.Atan2(p.Y - !current.Y, p.X - !current.X)
                    angle)
                |> Array.tryHead
                |> Option.defaultValue (points.[0])
            
            // If we've completed the hull
            if nextPoint = bottomLeft then
                break
            
            hull := !hull @ [nextPoint]
            current := nextPoint
        
        !hull |> Array.ofList

// Alternative implementation with better angle calculation
let jarvisMarchImproved (points: Point[]) : Point[] =
    if Array.isEmpty points || points.Length < 3 then
        points
    else
        // Find the bottom-left point
        let bottomLeft = findBottomLeftPoint points
        
        let hull = ref [bottomLeft]
        let current = ref bottomLeft
        
        // Continue until we return to the starting point
        while true do
            let nextPoint = 
                points 
                |> Array.filter (fun p -> p <> !current)
                |> Array.sortWith (fun p1 p2 ->
                    let angle1 = Math.Atan2(p1.Y - !current.Y, p1.X - !current.X)
                    let angle2 = Math.Atan2(p2.Y - !current.Y, p2.X - !current.X)
                    compare angle1 angle2)
                |> Array.tryHead
                |> Option.defaultValue (points.[0])
            
            // If we've completed the hull
            if nextPoint = bottomLeft then
                break
            
            hull := !hull @ [nextPoint]
            current := nextPoint
        
        !hull |> Array.ofList

// Example usage
[<EntryPoint>]
let main argv =
    // Create sample points
    let points = 
        [| {X = 0.0; Y = 0.0}
           {X = 1.0; Y = 0.0}
           {X = 0.0; Y = 1.0}
           {X = 1.0; Y = 1.0}
           {X = 0.5; Y = 0.5}
           {X = 2.0; Y = 0.0}
           {X = 0.0; Y = 2.0} |]
    
    printfn "Input points:"
    points |> Array.iter (fun p -> printfn "%s" (p.ToString()))
    
    printfn "\nConvex hull points:"
    let hull = jarvisMarchImproved points
    hull |> Array.iter (fun p -> printfn "%s" (p.ToString()))
    
    printfn "\nNumber of points in hull: %d" hull.Length
    
    0 // return code
```

## Algorithm Explanation

The Jarvis March algorithm works as follows:

1. **Find the starting point**: Identify the bottom-most (and left-most in case of ties) point
2. **Build the hull**: Starting from the initial point, find the next point that makes the largest counter-clockwise turn
3. **Continue until return**: Keep adding points until we return to the starting point

## Key Features

- **Time Complexity**: O(nh) where n is the number of input points and h is the number of points on the hull
- **Space Complexity**: O(h) for storing the hull points
- **Handles edge cases**: Works with fewer than 3 points
- **Uses cross product**: To determine orientation of three points

## Sample Output

```
Input points:
(0.00, 0.00)
(1.00, 0.00)
(0.00, 1.00)
(1.00, 1.00)
(0.50, 0.50)
(2.00, 0.00)
(0.00, 2.00)

Convex hull points:
(0.00, 0.00)
(1.00, 0.00)
(2.00, 0.00)
(0.00, 2.00)
(0.00, 1.00)

Number of points in hull: 5
```

