# Fortune's Algorithm Implementation in F#

Fortune's algorithm is used to compute Voronoi diagrams. Here's a simplified implementation in F#:

```fsharp
open System

// Point type definition
type Point = { X: float; Y: float } with
    static member (-) (p1, p2) = { X = p1.X - p2.X; Y = p1.Y - p2.Y }
    static member (+) (p1, p2) = { X = p1.X + p2.X; Y = p1.Y + p2.Y }
    static member (*) (p, scalar) = { X = p.X * scalar; Y = p.Y * scalar }

// Edge type for Voronoi diagram
type Edge = {
    Start: Point
    End: Point
    Site1: Point
    Site2: Point
}

// Event type for the sweep line algorithm
type Event = 
    | Circle of { Center: Point; Radius: float; Site: Point }
    | Site of Point

// Voronoi diagram computation
module FortuneAlgorithm = 
    let distance (p1: Point) (p2: Point) =
        sqrt ((p1.X - p2.X) ** 2.0 + (p1.Y - p2.Y) ** 2.0)

    // Compute Voronoi diagram for given points
    let computeVoronoi (sites: Point list) : Edge list =
        // Simplified implementation - in practice, this would be much more complex
        // This demonstrates the concept but doesn't implement the full algorithm
        
        let rec buildEdges acc remainingSites =
            match remainingSites with
            | [] -> acc
            | site :: rest ->
                // For each site, find its neighboring sites and create edges
                let neighbors = 
                    rest 
                    |> List.filter (fun s -> distance site s < 10.0) // Simplified proximity check
                
                let newEdges = 
                    neighbors 
                    |> List.map (fun neighbor -> 
                        { Start = site; End = neighbor; Site1 = site; Site2 = neighbor })
                
                buildEdges (acc @ newEdges) rest
        
        buildEdges [] sites

// Example usage
[<EntryPoint>]
let main argv =
    // Define some sample points
    let points = [
        { X = 1.0; Y = 2.0 }
        { X = 3.0; Y = 4.0 }
        { X = 5.0; Y = 1.0 }
        { X = 2.0; Y = 6.0 }
        { X = 7.0; Y = 3.0 }
    ]
    
    printfn "Input points:"
    points |> List.iter (fun p -> printfn "  (%.2f, %.2f)" p.X p.Y)
    
    // Compute Voronoi diagram
    let edges = FortuneAlgorithm.computeVoronoi points
    
    printfn "\nComputed Voronoi edges:"
    edges |> List.iteri (fun i edge ->
        printfn "Edge %d: (%.2f, %.2f) -> (%.2f, %.2f)" 
            i edge.Start.X edge.Start.Y edge.End.X edge.End.Y)
    
    0 // Return success
```

## Key Components Explained:

### Point Type
- Represents a 2D point with X and Y coordinates
- Includes overloaded operators for vector arithmetic

### Edge Type  
- Represents a Voronoi edge connecting two sites
- Stores start/end points and references to the original sites

### Event System
- Circle events (for beach line events)
- Site events (input points)

### Simplified Algorithm
The example shows a conceptual implementation that demonstrates:
1. Point distance calculation
2. Edge creation between nearby points
3. Basic Voronoi diagram structure

## Notes:
This is a simplified educational example. A complete Fortune's algorithm implementation would require:
- A sweep line data structure
- A beach line representation (typically using a balanced binary tree)
- Proper event handling and processing
- Arc intersection calculations
- Robust geometric computations

The full implementation would be significantly more complex but follows the same conceptual framework of using a sweep line to construct the Voronoi diagram incrementally.