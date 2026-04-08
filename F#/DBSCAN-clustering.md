# DBSCAN Clustering Algorithm in F#

Here's a complete implementation of the DBSCAN clustering algorithm in F#:

```fsharp
open System
open System.Collections.Generic

// Point type representing a data point in n-dimensional space
type Point = {
    Coordinates: float[]
    ClusterId: int option
    IsVisited: bool
}

// DBSCAN algorithm implementation
type DBSCAN<'T> = {
    Eps: float
    MinPts: int
    Points: Point[]
}

// Helper functions
let distance (p1: Point) (p2: Point) : float =
    Array.zip p1.Coordinates p2.Coordinates
    |> Array.map (fun (x, y) -> (x - y) ** 2.0)
    |> Array.sum
    |> sqrt

let findNeighbors (dbscan: DBSCAN<'T>) (point: Point) : Point list =
    dbscan.Points
    |> Array.filter (fun p -> distance point p <= dbscan.Eps)
    |> Array.toList

let expandCluster (dbscan: DBSCAN<'T>) (point: Point) (clusterId: int) : DBSCAN<'T> =
    let mutable dbscan = dbscan
    let mutable pointsToProcess = [point]
    
    while pointsToProcess <> [] do
        let currentPoint = List.head pointsToProcess
        pointsToProcess <- List.tail pointsToProcess
        
        // If point is already visited, skip it
        if not currentPoint.IsVisited then
            // Mark as visited
            let updatedPoints = 
                dbscan.Points 
                |> Array.map (fun p -> 
                    if p = currentPoint then { p with IsVisited = true } else p)
            dbscan <- { dbscan with Points = updatedPoints }
            
            // Find neighbors
            let neighbors = findNeighbors dbscan currentPoint
            
            // If point is a core point, add neighbors to processing queue
            if List.length neighbors >= dbscan.MinPts then
                pointsToProcess <- pointsToProcess @ neighbors
                
                // Assign cluster to neighbors that aren't already assigned
                let updatedPoints = 
                    dbscan.Points 
                    |> Array.map (fun p -> 
                        if List.contains p neighbors && p.ClusterId.IsNone then 
                            { p with ClusterId = Some clusterId } 
                        else p)
                dbscan <- { dbscan with Points = updatedPoints }
    
    dbscan

// Main DBSCAN algorithm
let dbscan (points: Point[]) (eps: float) (minPts: int) : Point[] =
    let dbscan = {
        Eps = eps
        MinPts = minPts
        Points = points
    }
    
    let mutable clusterId = 0
    let mutable updatedPoints = points
    
    // Process each point
    for i = 0 to updatedPoints.Length - 1 do
        let currentPoint = updatedPoints.[i]
        
        // If point is not visited and not noise
        if not currentPoint.IsVisited then
            let neighbors = findNeighbors dbscan currentPoint
            
            // If point is a core point
            if List.length neighbors >= dbscan.MinPts then
                clusterId <- clusterId + 1
                
                // Create new point array with updated cluster assignment
                let updatedPointsWithCluster = 
                    updatedPoints 
                    |> Array.map (fun p -> 
                        if p = currentPoint then 
                            { p with ClusterId = Some clusterId; IsVisited = true } 
                        else p)
                
                // Expand cluster
                let updatedDbscan = { dbscan with Points = updatedPointsWithCluster }
                let expandedDbscan = expandCluster updatedDbscan currentPoint clusterId
                updatedPoints <- expandedDbscan.Points
            else
                // Mark as noise
                let updatedPointsWithNoise = 
                    updatedPoints 
                    |> Array.map (fun p -> 
                        if p = currentPoint then 
                            { p with IsVisited = true } 
                        else p)
                updatedPoints <- updatedPointsWithNoise
    
    updatedPoints

// Example usage
let example() =
    // Create sample data points (2D coordinates)
    let points = [
        [| 1.0; 1.0 |]  // Point 1
        [| 1.1; 1.1 |]  // Point 2
        [| 1.2; 1.2 |]  // Point 3
        [| 10.0; 10.0 |] // Point 4
        [| 10.1; 10.1 |] // Point 5
        [| 10.2; 10.2 |] // Point 6
        [| 20.0; 20.0 |] // Point 7
        [| 20.1; 20.1 |] // Point 8
        [| 20.2; 20.2 |] // Point 9
        [| 20.3; 20.3 |] // Point 10
    ]
    
    // Convert to Point type
    let pointObjects = 
        points 
        |> Array.map (fun coords -> 
            { Coordinates = coords; ClusterId = None; IsVisited = false })
    
    // Apply DBSCAN clustering
    let clusteredPoints = dbscan pointObjects 1.5 2
    
    // Print results
    printfn "DBSCAN Clustering Results:"
    printfn "=========================="
    
    clusteredPoints 
    |> Array.iteri (fun i point -> 
        match point.ClusterId with
        | Some clusterId -> 
            printfn "Point %d: [%A] -> Cluster %d" 
                (i + 1) point.Coordinates clusterId
        | None -> 
            printfn "Point %d: [%A] -> Noise" 
                (i + 1) point.Coordinates)
    
    // Count points per cluster
    let clusters = 
        clusteredPoints 
        |> Array.filter (fun p -> p.ClusterId.IsSome)
        |> Array.groupBy (fun p -> p.ClusterId.Value)
        |> Array.map (fun (clusterId, points) -> (clusterId, Array.length points))
    
    printfn "\nCluster Summary:"
    Array.iter (fun (clusterId, count) -> 
        printfn "Cluster %d: %d points" clusterId count) clusters

// Run the example
example()
```

## Key Features of this Implementation:

1. **Point Type**: Represents data points with coordinates, cluster assignment, and visited status
2. **Distance Calculation**: Euclidean distance between points
3. **Neighbor Finding**: Identifies all points within the epsilon radius
4. **Cluster Expansion**: Expands clusters using the DBSCAN algorithm logic
5. **Noise Detection**: Points that don't belong to any cluster are marked as noise

## How it Works:

1. **Core Points**: Points with at least `MinPts` neighbors within `Eps` distance
2. **Border Points**: Points that are neighbors of core points but don't have enough neighbors themselves
3. **Noise Points**: Points that are neither core nor border points
4. **Cluster Growth**: Core points expand their clusters by connecting to directly reachable points

## Usage:

The algorithm takes:
- An array of points with coordinates
- Epsilon (`eps`) - maximum distance for neighborhood
- Minimum points (`minPts`) - minimum neighbors required for core point

This implementation demonstrates the fundamental DBSCAN algorithm in F# with proper functional programming patterns and clear separation of concerns.

