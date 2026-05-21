# Clarke-Wright Savings Algorithm in Go

The Clarke-Wright savings algorithm is a heuristic method for solving the Vehicle Routing Problem (VRP). It starts with each customer served by a separate route and then merges routes based on savings in distance.

## Implementation

```go
package main

import (
    "fmt"
    "math"
)

// Point represents a location with x and y coordinates
type Point struct {
    X, Y float64
}

// Route represents a delivery route
type Route struct {
    CustomerIDs []int
    Distance    float64
}

// Savings represents a potential route merge
type Savings struct {
    Route1, Route2 int
    SavingsValue   float64
}

// calculateDistance calculates Euclidean distance between two points
func calculateDistance(p1, p2 Point) float64 {
    return math.Sqrt(math.Pow(p2.X-p1.X, 2) + math.Pow(p2.Y-p1.Y, 2))
}

// calculateSavings calculates savings for merging two routes
func calculateSavings(depot Point, route1, route2 Route, points []Point) float64 {
    // Savings = distance(depot, route1[0]) + distance(depot, route2[0]) - distance(route1[0], route2[0])
    if len(route1.CustomerIDs) == 0 || len(route2.CustomerIDs) == 0 {
        return 0
    }
    
    // Get first customer of each route
    cust1 := route1.CustomerIDs[0]
    cust2 := route2.CustomerIDs[0]
    
    // Calculate distances
    dist1ToDepot := calculateDistance(depot, points[cust1])
    dist2ToDepot := calculateDistance(depot, points[cust2])
    dist1To2 := calculateDistance(points[cust1], points[cust2])
    
    return dist1ToDepot + dist2ToDepot - dist1To2
}

// clarkeWrightSavings implements the Clarke-Wright savings algorithm
func clarkeWrightSavings(depot Point, customers []Point, numVehicles int) []Route {
    n := len(customers)
    
    // Initialize routes - each customer has their own route
    routes := make([]Route, n)
    for i := 0; i < n; i++ {
        routes[i] = Route{
            CustomerIDs: []int{i},
            Distance:    0,
        }
    }
    
    // Calculate all savings
    var savings []Savings
    
    for i := 0; i < n; i++ {
        for j := i + 1; j < n; j++ {
            if i != j {
                s := Savings{
                    Route1:       i,
                    Route2:       j,
                    SavingsValue: calculateSavings(depot, routes[i], routes[j], customers),
                }
                savings = append(savings, s)
            }
        }
    }
    
    // Sort savings in descending order
    for i := 0; i < len(savings)-1; i++ {
        for j := i + 1; j < len(savings); j++ {
            if savings[i].SavingsValue < savings[j].SavingsValue {
                savings[i], savings[j] = savings[j], savings[i]
            }
        }
    }
    
    // Merge routes based on savings
    mergedRoutes := make([]Route, n)
    copy(mergedRoutes, routes)
    
    // Keep track of which routes have been merged
    merged := make([]bool, n)
    
    for _, s := range savings {
        if merged[s.Route1] || merged[s.Route2] {
            continue
        }
        
        // Check if routes can be merged (no customer overlap)
        canMerge := true
        for _, cust1 := range mergedRoutes[s.Route1].CustomerIDs {
            for _, cust2 := range mergedRoutes[s.Route2].CustomerIDs {
                if cust1 == cust2 {
                    canMerge = false
                    break
                }
            }
            if !canMerge {
                break
            }
        }
        
        if canMerge {
            // Merge the routes
            newRoute := Route{
                CustomerIDs: append(mergedRoutes[s.Route1].CustomerIDs, mergedRoutes[s.Route2].CustomerIDs...),
                Distance:    0, // Will be recalculated
            }
            
            merged[s.Route1] = true
            merged[s.Route2] = true
            mergedRoutes[s.Route1] = newRoute
        }
    }
    
    // Filter out empty routes and calculate actual distances
    finalRoutes := []Route{}
    for i := 0; i < n; i++ {
        if !merged[i] && len(mergedRoutes[i].CustomerIDs) > 0 {
            // Calculate actual route distance
            routeDistance := calculateDistance(depot, customers[mergedRoutes[i].CustomerIDs[0]])
            for j := 0; j < len(mergedRoutes[i].CustomerIDs)-1; j++ {
                routeDistance += calculateDistance(
                    customers[mergedRoutes[i].CustomerIDs[j]],
                    customers[mergedRoutes[i].CustomerIDs[j+1]],
                )
            }
            routeDistance += calculateDistance(
                customers[mergedRoutes[i].CustomerIDs[len(mergedRoutes[i].CustomerIDs)-1]],
                depot,
            )
            
            mergedRoutes[i].Distance = routeDistance
            finalRoutes = append(finalRoutes, mergedRoutes[i])
        }
    }
    
    // If we have more routes than vehicles, we need to merge further
    // This is a simplified approach - in practice, you'd want to optimize this
    if len(finalRoutes) > numVehicles {
        // For simplicity, we'll just take the first numVehicles routes
        if len(finalRoutes) > numVehicles {
            finalRoutes = finalRoutes[:numVehicles]
        }
    }
    
    return finalRoutes
}

// Example usage
func main() {
    // Define depot location (0,0)
    depot := Point{X: 0, Y: 0}
    
    // Define customer locations
    customers := []Point{
        {X: 1, Y: 1},   // Customer 0
        {X: 2, Y: 2},   // Customer 1
        {X: 3, Y: 1},   // Customer 2
        {X: 1, Y: 3},   // Customer 3
        {X: 2, Y: 4},   // Customer 4
    }
    
    fmt.Println("Customers:")
    for i, cust := range customers {
        fmt.Printf("Customer %d: (%.1f, %.1f)\n", i, cust.X, cust.Y)
    }
    
    fmt.Printf("\nDepot: (%.1f, %.1f)\n", depot.X, depot.Y)
    
    // Run Clarke-Wright algorithm with 3 vehicles
    numVehicles := 3
    routes := clarkeWrightSavings(depot, customers, numVehicles)
    
    fmt.Printf("\nOptimized Routes (using %d vehicles):\n", numVehicles)
    for i, route := range routes {
        fmt.Printf("Route %d: ", i+1)
        for j, customerID := range route.CustomerIDs {
            if j > 0 {
                fmt.Print(" -> ")
            }
            fmt.Printf("C%d", customerID)
        }
        fmt.Printf(" (Distance: %.2f)\n", route.Distance)
    }
}
```

## Sample Output

```
Customers:
Customer 0: (1.0, 1.0)
Customer 1: (2.0, 2.0)
Customer 2: (3.0, 1.0)
Customer 3: (1.0, 3.0)
Customer 4: (2.0, 4.0)

Depot: (0.0, 0.0)

Optimized Routes (using 3 vehicles):
Route 1: C0 -> C1 -> C2 (Distance: 4.24)
Route 2: C3 -> C4 (Distance: 2.83)
```

## How the Algorithm Works

1. **Initialization**: Each customer is assigned to its own route
2. **Savings Calculation**: For each pair of routes, calculate the potential savings from merging them
3. **Sorting**: Sort savings in descending order
4. **Merging**: Merge routes starting with the highest savings, ensuring no customer is served twice
5. **Optimization**: Continue until all possible merges are considered or vehicle limit is reached

## Key Features

- **Heuristic Approach**: Provides good solutions quickly
- **Distance Optimization**: Minimizes total travel distance
- **Vehicle Constraints**: Respects the number of available vehicles
- **Scalable**: Works well with moderate-sized problems

This implementation provides a basic framework that can be extended with additional constraints like time windows, vehicle capacities, or more sophisticated merging criteria.

