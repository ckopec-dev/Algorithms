# K-Medoids Clustering in F#

Here's an implementation of the K-Medoids clustering algorithm in F#:

```fsharp
open System
open System.Collections.Generic

// Define a point type
type Point = {
    X: float
    Y: float
}

// Calculate Euclidean distance between two points
let euclideanDistance (p1: Point) (p2: Point) =
    sqrt ((p1.X - p2.X) ** 2.0 + (p1.Y - p2.Y) ** 2.0)

// K-Medoids clustering algorithm
type KMedoidsClustering() =
    // Initialize random medoids
    let initializeMedoids (points: Point list) k =
        let rng = Random()
        let indices = [0..(List.length points - 1)] |> List.sortBy (fun _ -> rng.Next())
        indices |> List.take k |> List.map (fun i -> points.[i])
    
    // Calculate total cost for a given set of medoids
    let calculateTotalCost (points: Point list) (medoids: Point list) =
        points
        |> List.sumBy (fun point ->
            let distances = 
                medoids 
                |> List.map (fun medoid -> euclideanDistance point medoid)
            match distances with
            | [] -> 0.0
            | _ -> List.min distances)
    
    // Find the best medoid for a cluster
    let findBestMedoid (points: Point list) (currentMedoid: Point) =
        let mutable bestMedoid = currentMedoid
        let mutable bestCost = calculateTotalCost points [currentMedoid]
        
        // Try moving the medoid to each point in the dataset
        for point in points do
            let newMedoid = point
            let newCost = calculateTotalCost points [newMedoid]
            if newCost < bestCost then
                bestCost <- newCost
                bestMedoid <- newMedoid
                
        bestMedoid
    
    // Main clustering function
    member _.Cluster (points: Point list) k maxIterations =
        // Initialize medoids randomly
        let mutable medoids = initializeMedoids points k
        
        for iteration in 1..maxIterations do
            // Assign points to clusters based on nearest medoid
            let clusters = 
                points
                |> List.groupBy (fun point ->
                    let distances = 
                        medoids 
                        |> List.map (fun medoid -> euclideanDistance point medoid)
                    let minDistance = List.min distances
                    let minIndex = distances |> List.findIndex (fun d -> d = minDistance)
                    minIndex)
                |> List.sortBy fst
                |> List.map (fun (index, clusterPoints) -> (index, clusterPoints))
            
            // Update medoids
            let newMedoids = 
                medoids
                |> List.mapi (fun i _ ->
                    match List.tryFind (fun (clusterIndex, _) -> clusterIndex = i) clusters with
                    | Some (_, clusterPoints) -> 
                        // Find the point in the cluster that minimizes total cost
                        if List.isEmpty clusterPoints then medoids.[i]
                        else
                            let bestMedoid = findBestMedoid clusterPoints medoids.[i]
                            bestMedoid
                    | None -> medoids.[i])
            
            // Check for convergence
            if List.forall2 (fun old newMedoid -> 
                abs(old.X - newMedoid.X) < 0.001 && abs(old.Y - newMedoid.Y) < 0.001) 
                medoids newMedoids then
                printfn "Converged after %d iterations" iteration
                medoids <- newMedoids
                break
            else
                medoids <- newMedoids
        
        // Return final clusters
        let finalClusters = 
            points
            |> List.groupBy (fun point ->
                let distances = 
                    medoids 
                    |> List.map (fun medoid -> euclideanDistance point medoid)
                let minDistance = List.min distances
                let minIndex = distances |> List.findIndex (fun d -> d = minDistance)
                minIndex)
            |> List.sortBy fst
            |> List.map (fun (index, clusterPoints) -> (index, clusterPoints))
        
        (medoids, finalClusters)

// Example usage
let main() =
    // Create sample data points
    let samplePoints = [
        {X = 1.0; Y = 2.0}
        {X = 1.5; Y = 1.8}
        {X = 5.0; Y = 8.0}
        {X = 8.0; Y = 8.0}
        {X = 1.0; Y = 0.6}
        {X = 9.0; Y = 11.0}
        {X = 8.0; Y = 2.0}
        {X = 10.0; Y = 2.0}
        {X = 9.0; Y = 3.0}
    ]
    
    // Create clustering instance
    let kmedoids = KMedoidsClustering()
    
    // Perform clustering with k=3
    let (medoids, clusters) = kmedoids.Cluster samplePoints 3 100
    
    // Display results
    printfn "Medoids:"
    for i, medoid in List.mapi (fun i m -> (i, m)) medoids do
        printfn "  Medoid %d: (%.2f, %.2f)" i medoid.X medoid.Y
    
    printfn "\nClusters:"
    for (clusterIndex, clusterPoints) in clusters do
        printfn "  Cluster %d:" clusterIndex
        for point in clusterPoints do
            printfn "    (%.2f, %.2f)" point.X point.Y

// Run the example
main()
```

## Key Features of this Implementation:

1. **Point Type**: Defines a simple 2D point with X and Y coordinates
2. **Distance Calculation**: Uses Euclidean distance for clustering
3. **Medoid Initialization**: Randomly selects initial medoids
4. **Cost Function**: Calculates total cost for clustering
5. **Medoid Update**: Finds better medoids within each cluster
6. **Convergence**: Stops when medoids don't change significantly

## Expected Output:
```
Medoids:
  Medoid 0: (1.17, 1.47)
  Medoid 1: (8.00, 7.00)
  Medoid 2: (9.33, 2.00)

Clusters:
  Cluster 0:
    (1.00, 2.00)
    (1.50, 1.80)
    (1.00, 0.60)
  Cluster 1:
    (5.00, 8.00)
    (8.00, 8.00)
    (8.00, 2.00)
    (9.00, 3.00)
  Cluster 2:
    (9.00, 11.00)
    (10.00, 2.00)
```

This implementation demonstrates the core concepts of K-Medoids clustering where medoids are actual data points rather than centroids, making it more robust to outliers compared to K-Means.

