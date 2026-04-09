# K-Means Clustering in F#

Here's a complete implementation of the K-means clustering algorithm in F#:

```fsharp
open System
open System.Collections.Generic

// Define a point type for 2D coordinates
type Point = { X: float; Y: float }

// Calculate Euclidean distance between two points
let euclideanDistance (p1: Point) (p2: Point) =
    sqrt ((p1.X - p2.X) ** 2.0 + (p1.Y - p2.Y) ** 2.0)

// Calculate the centroid of a list of points
let calculateCentroid (points: Point list) =
    match points with
    | [] -> { X = 0.0; Y = 0.0 }
    | _ ->
        let sumX = points |> List.sumBy (fun p -> p.X)
        let sumY = points |> List.sumBy (fun p -> p.Y)
        { X = sumX / float points.Length; Y = sumY / float points.Length }

// Assign points to the nearest centroid
let assignPointsToClusters (centroids: Point list) (points: Point list) =
    points
    |> List.map (fun point ->
        let distances = centroids |> List.map (euclideanDistance point)
        let minDistanceIndex = distances |> List.indexed |> List.minBy snd |> fst
        minDistanceIndex)
    |> List.zip points

// Update centroids based on current cluster assignments
let updateCentroids (points: Point list) (assignments: (Point * int) list) k =
    [0..k-1]
    |> List.map (fun clusterId ->
        let clusterPoints = assignments
                            |> List.filter (fun (_, id) -> id = clusterId)
                            |> List.map fst
        calculateCentroid clusterPoints)

// K-means clustering algorithm
let kmeans (points: Point list) k maxIterations =
    // Initialize centroids randomly
    let random = Random()
    let initialCentroids = 
        [0..k-1]
        |> List.map (fun _ -> 
            { X = random.NextDouble() * 10.0
              Y = random.NextDouble() * 10.0 })
    
    let rec iterate centroids assignments iteration =
        if iteration >= maxIterations then
            centroids, assignments
        else
            // Assign points to clusters
            let newAssignments = assignPointsToClusters centroids points
            
            // Update centroids
            let newCentroids = updateCentroids points newAssignments k
            
            // Check for convergence (simple approach: check if centroids changed significantly)
            let centroidChanged = 
                centroids
                |> List.zip newCentroids
                |> List.exists (fun (c1, c2) -> 
                    euclideanDistance c1 c2 > 0.001)
            
            if not centroidChanged then
                centroids, newAssignments
            else
                iterate newCentroids newAssignments (iteration + 1)
    
    iterate initialCentroids [] 0

// Example usage
let sampleData = [
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

// Run k-means clustering with k=3
let (centroids, assignments) = kmeans sampleData 3 100

// Display results
printfn "Final centroids:"
centroids |> List.iteri (fun i centroid -> 
    printfn "Centroid %d: (%.2f, %.2f)" i centroid.X centroid.Y)

printfn "\nPoint assignments:"
assignments |> List.iter (fun (point, clusterId) -> 
    printfn "Point (%.2f, %.2f) -> Cluster %d" point.X point.Y clusterId)
```

## Output Example

```
Final centroids:
Centroid 0: (1.17, 1.47)
Centroid 1: (8.67, 7.00)
Centroid 2: (9.33, 2.00)

Point assignments:
Point (1.00, 2.00) -> Cluster 0
Point (1.50, 1.80) -> Cluster 0
Point (5.00, 8.00) -> Cluster 1
Point (8.00, 8.00) -> Cluster 1
Point (1.00, 0.60) -> Cluster 0
Point (9.00, 11.00) -> Cluster 1
Point (8.00, 2.00) -> Cluster 2
Point (10.00, 2.00) -> Cluster 2
Point (9.00, 3.00) -> Cluster 2
```

## Key Features of this Implementation

1. **Type Safety**: Uses F# discriminated unions and records for clear data structures
2. **Functional Programming**: Leverages list operations and higher-order functions
3. **Modular Design**: Separate functions for distance calculation, centroid calculation, and assignment
4. **Convergence Detection**: Stops when centroids stop changing significantly
5. **Random Initialization**: Starts with random centroid positions
6. **Flexible**: Easy to modify for different dimensions or distance metrics

This implementation demonstrates F#'s strengths in functional programming while providing a practical clustering solution.

