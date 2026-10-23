```fsharp
// Traveling Salesman Problem (TSP) Solution in F#

open System

// Define a point type for cities
type Point = { X: float; Y: float }

// Calculate Euclidean distance between two points
let distance (p1: Point) (p2: Point) =
    sqrt ((p1.X - p2.X) ** 2.0 + (p1.Y - p2.Y) ** 2.0)

// Generate all permutations of a list
let rec permute = function
    | [] -> [[]]
    | x :: xs ->
        List.collect (fun p ->
            List.init (List.length p + 1) (fun i ->
                List.insertAt i x p)
        ) (permute xs)

// Insert element at specific index
let rec insertAt index item list =
    match index, list with
    | 0, _ -> item :: list
    | _, [] -> [item]
    | _, head :: tail ->
        if index = 0 then
            item :: list
        else
            head :: (insertAt (index - 1) item tail)

// Calculate total distance for a tour
let totalDistance (cities: Point[]) (tour: int[]) =
    let mutable total = 0.0
    for i in 0 .. tour.Length - 2 do
        total <- total + distance cities[tour.[i]] cities[tour.[i + 1]]
    // Return to starting city
    total <- total + distance cities[tour.[tour.Length - 1]] cities[tour.[0]]
    total

// Brute force approach for small TSP instances
let solveTSPBruteForce (cities: Point[]) =
    let n = cities.Length
    if n <= 1 then
        [||]
    else
        // Generate all possible tours (starting from city 0)
        let allPermutations = 
            permute [1..n-1] 
            |> List.map (fun p -> 0 :: p)
        
        // Find the tour with minimum distance
        let bestTour = 
            allPermutations
            |> List.minBy (fun tour ->
                let tourArray = Array.ofList tour
                totalDistance cities tourArray)
        
        Array.ofList bestTour

// Alternative: Nearest Neighbor heuristic for larger instances
let solveTSPNearestNeighbor (cities: Point[]) =
    let n = cities.Length
    if n <= 1 then
        [||]
    else
        let visited = Array.create n false
        let tour = Array.zeroCreate n
        let mutable current = 0
        let mutable index = 0
        
        // Start with first city
        tour.[index] <- current
        visited.[current] <- true
        index <- index + 1
        
        // Find nearest unvisited city at each step
        for i in 0 .. n - 2 do
            let mutable minDist = Double.MaxValue
            let mutable nextCity = -1
            
            for j in 0 .. n - 1 do
                if not visited.[j] && j <> current then
                    let dist = distance cities[current] cities[j]
                    if dist < minDist then
                        minDist <- dist
                        nextCity <- j
            
            current <- nextCity
            tour.[index] <- current
            visited.[current] <- true
            index <- index + 1
        
        tour

// Example usage
let main() =
    // Define sample cities (coordinates)
    let cities = 
        [| { X = 0.0; Y = 0.0 }   // City 0
           { X = 1.0; Y = 2.0 }   // City 1
           { X = 3.0; Y = 1.0 }   // City 2
           { X = 5.0; Y = 3.0 }   // City 3
           { X = 2.0; Y = 4.0 }   // City 4
        |]
    
    printfn "Cities:"
    for i in 0 .. cities.Length - 1 do
        printfn "City %d: (%.1f, %.1f)" i cities.[i].X cities.[i].Y
    
    printfn "\n--- Brute Force Solution ---"
    let bruteForceTour = solveTSPBruteForce cities
    let bruteForceDistance = totalDistance cities bruteForceTour
    printfn "Best tour: %A" bruteForceTour
    printfn "Total distance: %.2f" bruteForceDistance
    
    printfn "\n--- Nearest Neighbor Solution ---"
    let nnTour = solveTSPNearestNeighbor cities
    let nnDistance = totalDistance cities nnTour
    printfn "Tour: %A" nnTour
    printfn "Total distance: %.2f" nnDistance

// Run the example
main()
```

This F# implementation demonstrates two approaches to solving the Traveling Salesman Problem:

## Key Features:

1. **Point Type**: Represents cities with X,Y coordinates
2. **Distance Calculation**: Uses Euclidean distance formula
3. **Brute Force Method**: Finds optimal solution by checking all permutations (only practical for small instances)
4. **Nearest Neighbor Heuristic**: Approximate solution for larger instances
5. **Tour Validation**: Calculates total tour distance including return to starting city

## Algorithm Comparison:

- **Brute Force**: 
  - Time complexity: O(n!)
  - Guarantees optimal solution
  - Only practical for n ≤ 10

- **Nearest Neighbor**:
  - Time complexity: O(n²)
  - Fast approximation
  - Good for larger instances

The code includes sample data with 5 cities and demonstrates both approaches, showing the trade-off between solution quality and computational efficiency.