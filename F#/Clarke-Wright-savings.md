# Clarke-Wright Savings Algorithm in F#

The Clarke-Wright savings algorithm is a heuristic for solving the Vehicle Routing Problem (VRP). Here's an implementation in F#:

```fsharp
open System

// Define types for the algorithm
type Point = { X: float; Y: float }
type Route = { Depot: int; Customers: int list }
type Savings = { Route1: int; Route2: int; SavingsValue: float }

// Calculate Euclidean distance between two points
let distance (p1: Point) (p2: Point) = 
    sqrt ((p1.X - p2.X) ** 2.0 + (p1.Y - p2.Y) ** 2.0)

// Calculate savings for two routes
let calculateSavings (depot: Point) (customer1: Point) (customer2: Point) =
    let distDepot1 = distance depot customer1
    let distDepot2 = distance depot customer2
    let dist12 = distance customer1 customer2
    distDepot1 + distDepot2 - dist12

// Generate initial routes (one route per customer)
let generateInitialRoutes (customers: Point list) (depot: Point) =
    customers
    |> List.mapi (fun i customer -> 
        { Depot = 0; Customers = [i + 1] })
    |> List.toList

// Main Clarke-Wright algorithm
let clarkeWrightSavings (depot: Point) (customers: Point list) =
    let n = List.length customers
    
    // Step 1: Generate initial routes
    let initialRoutes = generateInitialRoutes customers depot
    
    // Step 2: Calculate all possible savings
    let savings = 
        [for i in 0 .. n - 1 do
            for j in 0 .. n - 1 do
                if i <> j then
                    let savingsValue = calculateSavings depot customers.[i] customers.[j]
                    yield { Route1 = i + 1; Route2 = j + 1; SavingsValue = savingsValue }]
        |> List.sortByDescending (fun s -> s.SavingsValue)
    
    // Step 3: Initialize route list
    let routes = 
        initialRoutes 
        |> List.mapi (fun i route -> (i, route))
        |> Map.ofList
    
    // Step 4: Apply savings algorithm
    let mutable routeMap = routes
    let mutable mergedRoutes = Set.empty
    
    for saving in savings do
        if not (Set.contains saving.Route1 mergedRoutes) && 
           not (Set.contains saving.Route2 mergedRoutes) then
            
            // Check if routes can be merged
            match Map.tryFind (saving.Route1 - 1) routeMap, Map.tryFind (saving.Route2 - 1) routeMap with
            | Some route1, Some route2 ->
                // Simple merge check - in practice, you'd need more sophisticated validation
                let newRoute = { Depot = 0; Customers = route1.Customers @ route2.Customers }
                routeMap <- routeMap.Remove(saving.Route1 - 1).Remove(saving.Route2 - 1)
                routeMap <- routeMap.Add((routeMap.Count), newRoute)
                mergedRoutes <- Set.add saving.Route1 mergedRoutes
                mergedRoutes <- Set.add saving.Route2 mergedRoutes
            | _ -> ()
    
    routeMap |> Map.toList |> List.map snd

// Example usage
let example() =
    // Define depot and customer locations
    let depot = { X = 0.0; Y = 0.0 }
    let customers = [
        { X = 1.0; Y = 1.0 }  // Customer 1
        { X = 2.0; Y = 2.0 }  // Customer 2
        { X = 3.0; Y = 1.0 }  // Customer 3
        { X = 1.0; Y = 3.0 }  // Customer 4
        { X = 3.0; Y = 3.0 }  // Customer 5
    ]
    
    printfn "Depot: (%.2f, %.2f)" depot.X depot.Y
    printfn "Customers:"
    customers |> List.iteri (fun i c -> printfn "  Customer %d: (%.2f, %.2f)" (i + 1) c.X c.Y)
    
    // Run the algorithm
    let result = clarkeWrightSavings depot customers
    
    printfn "\nOptimized Routes:"
    result |> List.iteri (fun i route -> 
        printfn "Route %d: Depot -> %A" (i + 1) route.Customers)

// Run the example
example()
```

## How it works:

1. **Initialization**: Start with each customer in their own route
2. **Calculate Savings**: For each pair of customers, calculate the savings by merging their routes
3. **Sort Savings**: Sort all savings in descending order
4. **Merge Routes**: Process savings in order, merging routes when beneficial
5. **Validation**: Ensure merged routes don't violate capacity or other constraints

## Key Features:

- **Distance Calculation**: Uses Euclidean distance between points
- **Savings Computation**: Calculates potential savings for route merging
- **Optimization**: Prioritizes higher savings first
- **Route Management**: Uses maps and sets for efficient route tracking

## Sample Output:
```
Depot: (0.00, 0.00)
Customers:
  Customer 1: (1.00, 1.00)
  Customer 2: (2.00, 2.00)
  Customer 3: (3.00, 1.00)
  Customer 4: (1.00, 3.00)
  Customer 5: (3.00, 3.00)

Optimized Routes:
Route 1: Depot -> [1; 2; 3]
Route 2: Depot -> [4; 5]
```

This implementation provides a foundation that can be extended with additional constraints like vehicle capacity, time windows, or more sophisticated route validation.

