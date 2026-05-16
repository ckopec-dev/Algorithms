# Lloyd's Algorithm Implementation in F#

Lloyd's algorithm is used for k-means clustering. Here's a complete implementation in F#:

```fsharp
open System

// Define a point type
type Point = { X: float; Y: float }

// Define a cluster type
type Cluster = { Center: Point; Points: Point list }

// Calculate Euclidean distance between two points
let distance p1 p2 = 
    sqrt ((p1.X - p2.X) ** 2.0 + (p1.Y - p2.Y) ** 2.0)

// Find the closest cluster center to a point
let findClosestCluster (clusters: Cluster list) point =
    clusters 
    |> List.minBy (fun cluster -> distance point cluster.Center)

// Calculate the mean of a list of points
let meanPoints points =
    match points with
    | [] -> { X = 0.0; Y = 0.0 }
    | _ -> 
        let sumX = points |> List.sumBy (fun p -> p.X)
        let sumY = points |> List.sumBy (fun p -> p.Y)
        { X = sumX / float (List.length points)
          Y = sumY / float (List.length points) }

// Update cluster centers based on current point assignments
let updateCenters (clusters: Cluster list) =
    clusters 
    |> List.map (fun cluster -> 
        { Center = meanPoints cluster.Points
          Points = cluster.Points })

// Perform one iteration of Lloyd's algorithm
let lloydIteration (clusters: Cluster list) points =
    // Assign points to closest clusters
    let assignedClusters = 
        points 
        |> List.map (findClosestCluster clusters)
        |> List.groupBy (fun cluster -> cluster.Center)
        |> List.map (fun (center, clusterList) -> 
            { Center = center
              Points = clusterList |> List.collect (fun c -> c.Points) })
    
    // Update cluster centers
    updateCenters assignedClusters

// Main Lloyd's algorithm function
let lloydAlgorithm (initialCenters: Point list) (points: Point list) iterations =
    // Initialize clusters with initial centers
    let initialClusters = 
        initialCenters 
        |> List.map (fun center -> { Center = center; Points = [] })
    
    // Run iterations
    let rec run currentClusters iter =
        if iter <= 0 then currentClusters
        else
            let updatedClusters = lloydIteration currentClusters points
            run updatedClusters (iter - 1)
    
    run initialClusters iterations

// Example usage
let example() =
    // Sample data points
    let dataPoints = [
        { X = 1.0; Y = 2.0 }
        { X = 1.5; Y = 1.8 }
        { X = 5.0; Y = 8.0 }
        { X = 8.0; Y = 8.0 }
        { X = 1.0; Y = 0.6 }
        { X = 9.0; Y = 11.0 }
        { X = 8.0; Y = 2.0 }
        { X = 10.0; Y = 2.0 }
        { X = 9.0; Y = 3.0 }
    ]
    
    // Initial cluster centers
    let initialCenters = [
        { X = 1.0; Y = 2.0 }
        { X = 5.0; Y = 8.0 }
    ]
    
    // Run Lloyd's algorithm
    let finalClusters = lloydAlgorithm initialCenters dataPoints 10
    
    printfn "Final clusters:"
    finalClusters |> List.iteri (fun i cluster ->
        printfn "Cluster %d: Center = (%.2f, %.2f), Points = %d" 
            i cluster.Center.X cluster.Center.Y (List.length cluster.Points)
    )

// Run the example
example()
```

## How it works:

1. **Point and Cluster Types**: Define the basic data structures for points and clusters
2. **Distance Calculation**: Uses Euclidean distance formula
3. **Assignment Step**: Each point is assigned to the nearest cluster center
4. **Update Step**: Cluster centers are recalculated as the mean of assigned points
5. **Iteration**: Repeat assignment and update steps until convergence

## Key Features:

- **Functional Approach**: Uses immutable data structures and higher-order functions
- **Type Safety**: Strong typing prevents runtime errors
- **Modular Design**: Separate functions for each step of the algorithm
- **Extensible**: Easy to modify for different distance metrics or stopping criteria

The algorithm will converge to a local minimum of the k-means objective function, typically finding a good clustering solution for the given data.

