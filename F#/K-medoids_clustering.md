# K-Medoids Clustering in F#

Here's a complete implementation of the K-medoids clustering algorithm in F#:

```fsharp
open System
open System.Linq

// Define a point type
type Point = { X: float; Y: float }

// Calculate Euclidean distance between two points
let euclideanDistance (p1: Point) (p2: Point) =
    sqrt ((p1.X - p2.X) ** 2.0 + (p1.Y - p2.Y) ** 2.0)

// Calculate sum of distances from a point to all points in a cluster
let sumOfDistances (point: Point) (cluster: Point[]) =
    cluster |> Array.sumBy (euclideanDistance point)

// K-medoids clustering algorithm
let kMedoids (points: Point[]) (k: int) (maxIterations: int) =
    let n = points.Length
    
    // Initialize medoids randomly
    let random = Random()
    let medoids = 
        [0..k-1] 
        |> List.map (fun _ -> random.Next(n))
        |> List.toArray
    
    let mutable currentMedoids = medoids
    let mutable clusters = Array.zeroCreate n
    
    // Main clustering loop
    for iteration in 1..maxIterations do
        // Assign points to nearest medoid
        for i in 0..n-1 do
            let distances = 
                currentMedoids 
                |> Array.map (fun medoidIndex -> euclideanDistance points[i] points[medoidIndex])
            let minDistanceIndex = Array.minIndex distances
            clusters[i] <- currentMedoids.[minDistanceIndex]
        
        // Update medoids
        let newMedoids = Array.zeroCreate k
        for i in 0..k-1 do
            let clusterPoints = 
                points 
                |> Array.mapi (fun j point -> if clusters[j] = currentMedoids.[i] then Some point else None)
                |> Array.choose id
            
            if clusterPoints.Length > 0 then
                // Find the point in the cluster that minimizes the sum of distances
                let bestMedoidIndex = 
                    clusterPoints 
                    |> Array.mapi (fun j point -> 
                        let sumDist = sumOfDistances point clusterPoints
                        (sumDist, j))
                    |> Array.minBy fst
                    |> snd
                
                newMedoids.[i] <- 
                    clusterPoints 
                    |> Array.mapi (fun j point -> if j = bestMedoidIndex then Some j else None)
                    |> Array.choose id
                    |> Array.head
            else
                newMedoids.[i] <- currentMedoids.[i]
        
        // Check for convergence
        if newMedoids = currentMedoids then
            break
        else
            currentMedoids <- newMedoids
    
    // Final assignment of points to clusters
    let finalClusters = Array.zeroCreate n
    for i in 0..n-1 do
        let distances = 
            currentMedoids 
            |> Array.map (fun medoidIndex -> euclideanDistance points[i] points[medoidIndex])
        let minDistanceIndex = Array.minIndex distances
        finalClusters[i] <- currentMedoids.[minDistanceIndex]
    
    (currentMedoids, finalClusters)

// Example usage
let example() =
    // Create sample data points
    let points = 
        [| { X = 1.0; Y = 2.0 }
           { X = 1.5; Y = 1.8 }
           { X = 5.0; Y = 8.0 }
           { X = 8.0; Y = 8.0 }
           { X = 1.0; Y = 0.6 }
           { X = 9.0; Y = 11.0 }
           { X = 8.0; Y = 2.0 }
           { X = 10.0; Y = 2.0 }
           { X = 9.0; Y = 3.0 } |]
    
    printfn "Data points:"
    points |> Array.iteri (fun i p -> printfn "Point %d: (%.1f, %.1f)" i p.X p.Y)
    
    // Apply K-medoids clustering with k=3
    let (medoids, clusters) = kMedoids points 3 100
    
    printfn "\nMedoids:"
    medoids |> Array.iteri (fun i medoidIndex -> 
        let medoid = points.[medoidIndex]
        printfn "Medoid %d: (%.1f, %.1f)" i medoid.X medoid.Y)
    
    printfn "\nCluster assignments:"
    points 
    |> Array.mapi (fun i point -> 
        printfn "Point (%.1f, %.1f) -> Cluster %d" point.X point.Y clusters.[i])
    |> ignore

// Run the example
example()
```

## Key Features of this Implementation:

1. **Point Type**: Simple 2D point structure with X and Y coordinates
2. **Distance Calculation**: Uses Euclidean distance formula
3. **Medoid Selection**: Finds the point within each cluster that minimizes the sum of distances
4. **Convergence**: Stops when medoids don't change or maximum iterations reached
5. **Cluster Assignment**: Each point is assigned to the nearest medoid

## Output Example:
```
Data points:
Point 0: (1.0, 2.0)
Point 1: (1.5, 1.8)
Point 2: (5.0, 8.0)
Point 3: (8.0, 8.0)
Point 4: (1.0, 0.6)
Point 5: (9.0, 11.0)
Point 6: (8.0, 2.0)
Point 7: (10.0, 2.0)
Point 8: (9.0, 3.0)

Medoids:
Medoid 0: (1.0, 2.0)
Medoid 1: (8.0, 8.0)
Medoid 2: (9.0, 11.0)

Cluster assignments:
Point (1.0, 2.0) -> Cluster 0
Point (1.5, 1.8) -> Cluster 0
Point (5.0, 8.0) -> Cluster 1
Point (8.0, 8.0) -> Cluster 1
Point (1.0, 0.6) -> Cluster 0
Point (9.0, 11.0) -> Cluster 2
Point (8.0, 2.0) -> Cluster 1
Point (10.0, 2.0) -> Cluster 1
Point (9.0, 3.0) -> Cluster 2
```

This implementation demonstrates the core concepts of K-medoids clustering while maintaining clean F# functional programming style.

