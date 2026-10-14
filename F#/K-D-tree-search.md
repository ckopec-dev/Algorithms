```fsharp
// K-D Tree Search Implementation in F#

type Point = { X: float; Y: float }

type KDNode = 
    | Leaf of Point
    | Branch of Point * KDNode * KDNode

type KDTree = {
    Root: KDNode option
    Dimensions: int
}

// Helper function to calculate Euclidean distance
let distance p1 p2 = 
    sqrt ((p1.X - p2.X) ** 2.0 + (p1.Y - p2.Y) ** 2.0)

// Function to build a simple 2D KD-Tree from a list of points
let rec buildKDTree points depth = 
    match points with
    | [] -> None
    | [point] -> Some (Leaf point)
    | _ ->
        let axis = depth % 2  // Alternate between X and Y axes
        let sortedPoints = 
            match axis with
            | 0 -> List.sortBy (fun p -> p.X) points  // Sort by X coordinate
            | _ -> List.sortBy (fun p -> p.Y) points  // Sort by Y coordinate
        
        let medianIndex = List.length sortedPoints / 2
        let medianPoint = sortedPoints.[medianIndex]
        
        let leftPoints = sortedPoints |> List.take medianIndex
        let rightPoints = sortedPoints |> List.skip (medianIndex + 1)
        
        let leftChild = buildKDTree leftPoints (depth + 1)
        let rightChild = buildKDTree rightPoints (depth + 1)
        
        Some (Branch(medianPoint, leftChild.Value, rightChild.Value))

// K-D Tree search function
let kdtreeSearch tree target maxDistance =
    let rec search node currentBest distanceSoFar =
        match node with
        | Leaf point ->
            let dist = distance target point
            if dist < distanceSoFar then
                Some (point, dist)
            else
                currentBest
            
        | Branch(point, left, right) ->
            let dist = distance target point
            let newBest = 
                match currentBest with
                | Some (_, bestDist) when dist < bestDist -> Some (point, dist)
                | _ -> currentBest
            
            // Determine which subtree to search first based on target
            let (firstChild, secondChild) =
                if target.X <= point.X then
                    (left, right)
                else
                    (right, left)
            
            // Search the first child
            let bestFirst = 
                match firstChild with
                | Some child -> search child newBest distanceSoFar
                | None -> currentBest
            
            // Check if we need to search the second child
            let distanceToSplitPlane = 
                if target.X <= point.X then 
                    point.X - target.X 
                else 
                    target.X - point.X
            
            let bestSecond = 
                if distanceToSplitPlane < distanceSoFar then
                    match secondChild with
                    | Some child -> search child bestFirst distanceSoFar
                    | None -> bestFirst
                else
                    bestFirst
            
            bestSecond
    
    match tree.Root with
    | Some root -> search root None maxDistance
    | None -> None

// Example usage
let examplePoints = [
    { X = 2.0; Y = 3.0 }
    { X = 5.0; Y = 4.0 }
    { X = 9.0; Y = 6.0 }
    { X = 4.0; Y = 7.0 }
    { X = 8.0; Y = 1.0 }
    { X = 7.0; Y = 2.0 }
]

// Build the KD-Tree
let tree = {
    Root = buildKDTree examplePoints 0
    Dimensions = 2
}

// Search for the closest point to (6.0, 5.0) with max distance of 10.0
let result = kdtreeSearch tree { X = 6.0; Y = 5.0 } 10.0

match result with
| Some (point, dist) -> 
    printfn "Closest point: (%.2f, %.2f) at distance %.2f" point.X point.Y dist
| None -> 
    printfn "No point found within the specified distance"

// Search for the closest point to (1.0, 1.0)
let result2 = kdtreeSearch tree { X = 1.0; Y = 1.0 } 10.0

match result2 with
| Some (point, dist) -> 
    printfn "Closest point: (%.2f, %.2f) at distance %.2f" point.X point.Y dist
| None -> 
    printfn "No point found within the specified distance"
```

This F# implementation demonstrates:

1. **Point and KDNode Types**: Defines a 2D point structure and a K-D tree node that can be either a leaf or branch
2. **KDTree Type**: Contains the root of the tree and dimension information
3. **Distance Calculation**: Euclidean distance function between two points
4. **Build Function**: Creates a balanced KD-Tree from a list of points by recursively splitting along alternating axes
5. **Search Function**: Implements K-D tree search that finds the closest point to a target within a maximum distance
6. **Example Usage**: Shows how to build a tree and perform searches

The algorithm efficiently narrows down the search space by using the properties of K-D trees, where at each node, we determine which subtree to explore first based on the target point's position relative to the splitting hyperplane.