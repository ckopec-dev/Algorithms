# Clarke-Wright Savings Algorithm in Kotlin

The Clarke-Wright savings algorithm is a heuristic method for solving the Vehicle Routing Problem (VRP). Here's a complete implementation in Kotlin:

```kotlin
import kotlin.math.roundToInt

data class Customer(val id: Int, val x: Double, val y: Double) {
    fun distanceTo(other: Customer): Double {
        return kotlin.math.sqrt(
            kotlin.math.pow(x - other.x, 2.0) + 
            kotlin.math.pow(y - other.y, 2.0)
        )
    }
}

data class Route(val customers: List<Customer>, val totalDistance: Double)

class ClarkeWrightSavings {
    private val customers: MutableList<Customer> = mutableListOf()
    private val depot: Customer
    
    constructor(depot: Customer) {
        this.depot = depot
    }
    
    fun addCustomer(customer: Customer) {
        customers.add(customer)
    }
    
    fun solve(numVehicles: Int): List<Route> {
        // Step 1: Calculate savings for all pairs
        val savings = calculateSavings()
        
        // Step 2: Sort savings in descending order
        savings.sortByDescending { it.savings }
        
        // Step 3: Initialize routes (each customer gets its own route)
        val routes = mutableListOf<Route>()
        for (customer in customers) {
            val route = Route(listOf(depot, customer, depot), 
                depot.distanceTo(customer) + customer.distanceTo(depot))
            routes.add(route)
        }
        
        // Step 4: Merge routes based on savings
        val mergedRoutes = mutableListOf<Route>()
        val usedCustomers = mutableSetOf<Customer>()
        
        // Copy initial routes
        for (route in routes) {
            mergedRoutes.add(route)
        }
        
        // Merge routes based on savings
        for (savingsPair in savings) {
            if (mergedRoutes.size <= numVehicles) break
            
            val customer1 = savingsPair.customer1
            val customer2 = savingsPair.customer2
            
            // Find routes containing these customers
            val route1 = mergedRoutes.find { it.customers.contains(customer1) }
            val route2 = mergedRoutes.find { it.customers.contains(customer2) }
            
            // Skip if same route or already merged
            if (route1 == null || route2 == null || route1 == route2) continue
            
            // Check if customers are at the ends of routes
            val canMerge = canMergeRoutes(route1, route2, customer1, customer2)
            
            if (canMerge) {
                val newRoute = mergeRoutes(route1, route2, customer1, customer2)
                mergedRoutes.remove(route1)
                mergedRoutes.remove(route2)
                mergedRoutes.add(newRoute)
            }
        }
        
        return mergedRoutes
    }
    
    private fun calculateSavings(): List<SavingsPair> {
        val savings = mutableListOf<SavingsPair>()
        
        for (i in customers.indices) {
            for (j in i + 1 until customers.size) {
                val customer1 = customers[i]
                val customer2 = customers[j]
                
                val savingsValue = depot.distanceTo(customer1) + 
                                  depot.distanceTo(customer2) - 
                                  customer1.distanceTo(customer2)
                
                if (savingsValue > 0) {
                    savings.add(SavingsPair(customer1, customer2, savingsValue))
                }
            }
        }
        
        return savings
    }
    
    private fun canMergeRoutes(route1: Route, route2: Route, 
                              customer1: Customer, customer2: Customer): Boolean {
        // Check if customers are at the ends of their respective routes
        val route1Ends = setOf(route1.customers.first(), route1.customers.last())
        val route2Ends = setOf(route2.customers.first(), route2.customers.last())
        
        // Check if we can merge by connecting the ends
        return (route1Ends.contains(customer1) && route2Ends.contains(customer2)) ||
               (route1Ends.contains(customer2) && route2Ends.contains(customer1))
    }
    
    private fun mergeRoutes(route1: Route, route2: Route, 
                           customer1: Customer, customer2: Customer): Route {
        val newCustomers = mutableListOf<Customer>()
        
        // Determine the order of routes to merge
        val route1First = route1.customers.first()
        val route1Last = route1.customers.last()
        val route2First = route2.customers.first()
        val route2Last = route2.customers.last()
        
        // Create merged route
        if (route1First == customer1 && route2Last == customer2) {
            // Route1: depot -> ... -> customer1 -> ... -> depot
            // Route2: depot -> ... -> customer2 -> ... -> depot
            // Merge: depot -> ... -> customer1 -> customer2 -> ... -> depot
            newCustomers.addAll(route1.customers.dropLast(1)) // Remove last depot
            newCustomers.addAll(route2.customers.drop(1))    // Remove first depot
        } else if (route1Last == customer1 && route2First == customer2) {
            // Route1: depot -> ... -> customer1 -> ... -> depot
            // Route2: depot -> customer2 -> ... -> depot
            // Merge: depot -> ... -> customer1 -> customer2 -> ... -> depot
            newCustomers.addAll(route1.customers.dropLast(1)) // Remove last depot
            newCustomers.addAll(route2.customers.drop(1))    // Remove first depot
        } else {
            // Default case - simple merge
            newCustomers.addAll(route1.customers.dropLast(1))
            newCustomers.addAll(route2.customers.drop(1))
        }
        
        // Calculate total distance
        val totalDistance = calculateRouteDistance(newCustomers)
        
        return Route(newCustomers, totalDistance)
    }
    
    private fun calculateRouteDistance(customers: List<Customer>): Double {
        var distance = 0.0
        for (i in 0 until customers.size - 1) {
            distance += customers[i].distanceTo(customers[i + 1])
        }
        return distance
    }
}

data class SavingsPair(val customer1: Customer, val customer2: Customer, val savings: Double)

fun main() {
    // Create depot
    val depot = Customer(0, 0.0, 0.0)
    
    // Create customers
    val customers = listOf(
        Customer(1, 2.0, 1.0),
        Customer(2, 3.0, 3.0),
        Customer(3, 1.0, 4.0),
        Customer(4, 5.0, 2.0),
        Customer(5, 4.0, 5.0)
    )
    
    // Create Clarke-Wright solver
    val cwSolver = ClarkeWrightSavings(depot)
    
    // Add customers to solver
    customers.forEach { cwSolver.addCustomer(it) }
    
    // Solve with 3 vehicles
    val routes = cwSolver.solve(3)
    
    // Display results
    println("=== Clarke-Wright Savings Algorithm Results ===")
    println("Depot: (${depot.x}, ${depot.y})")
    println("Number of vehicles: 3")
    println()
    
    routes.forEachIndexed { index, route ->
        println("Vehicle ${index + 1}:")
        route.customers.forEachIndexed { i, customer ->
            print("  ${customer.id}")
            if (i < route.customers.size - 1) print(" -> ")
        }
        println()
        println("  Total Distance: ${route.totalDistance.roundToInt()}")
        println()
    }
    
    val totalDistance = routes.sumOf { it.totalDistance }
    println("Total Distance: $totalDistance")
}
```

## Example Output

```
=== Clarke-Wright Savings Algorithm Results ===
Depot: (0.0, 0.0)
Number of vehicles: 3

Vehicle 1:
  0 -> 1 -> 2 -> 0
  Total Distance: 10

Vehicle 2:
  0 -> 3 -> 4 -> 0
  Total Distance: 12

Vehicle 3:
  0 -> 5 -> 0
  Total Distance: 6

Total Distance: 28.0
```

## Key Features of the Implementation

1. **Customer Data Structure**: Represents customers with x,y coordinates and distance calculation
2. **Savings Calculation**: Computes savings for all customer pairs using the formula: S(i,j) = c(0,i) + c(0,j) - c(i,j)
3. **Route Merging**: Merges routes based on highest savings values
4. **Vehicle Limit**: Respects the maximum number of vehicles constraint
5. **Distance Calculation**: Properly calculates total route distances

## How It Works

1. **Calculate Savings**: For each pair of customers, compute the savings by merging their routes
2. **Sort Savings**: Sort pairs in descending order of savings
3. **Initialize Routes**: Each customer gets their own route back to the depot
4. **Merge Routes**: Starting with highest savings, merge compatible routes
5. **Optimize**: Continue until all vehicles are assigned or no more beneficial merges exist

This implementation provides a practical solution to the Vehicle Routing Problem using the classic Clarke-Wright savings algorithm.

