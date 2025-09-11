# Clarke–Wright Savings Algorithm Implementation

The Clarke–Wright savings algorithm is a heuristic for solving the Vehicle Routing Problem (VRP). It works by calculating savings from merging routes and then greedily combining routes to minimize total distance.

```python
import numpy as np
import matplotlib.pyplot as plt
from itertools import combinations

def calculate_savings(distances, depot_index=0):
    """
    Calculate savings for all pairs of customers
    
    Args:
        distances: 2D array of distances between all points
        depot_index: index of the depot (default 0)
    
    Returns:
        savings: list of tuples (savings_value, customer1, customer2)
    """
    n = len(distances)
    savings = []
    
    # Calculate savings for all pairs of customers
    for i in range(1, n):
        for j in range(i+1, n):
            # Savings = distance(depot,i) + distance(depot,j) - distance(i,j)
            saving = distances[depot_index][i] + distances[depot_index][j] - distances[i][j]
            savings.append((saving, i, j))
    
    # Sort by savings in descending order
    savings.sort(reverse=True)
    return savings

def is_valid_merge(route1, route2, capacity, demands):
    """
    Check if two routes can be merged without exceeding capacity
    
    Args:
        route1, route2: lists of customer indices
        capacity: vehicle capacity
        demands: list of customer demands
    
    Returns:
        bool: True if merge is valid
    """
    total_demand = sum(demands[i] for i in route1 + route2)
    return total_demand <= capacity

def merge_routes(route1, route2):
    """
    Merge two routes by connecting them through the depot
    
    Args:
        route1, route2: lists of customer indices
    
    Returns:
        merged_route: combined route
    """
    # Create a new route that goes depot -> route1 -> route2 -> depot
    return [0] + route1 + route2 + [0]

def clarke_wright_savings(distances, demands, capacity, depot_index=0):
    """
    Implement Clarke-Wright savings algorithm
    
    Args:
        distances: 2D array of distances between all points
        demands: list of customer demands
        capacity: vehicle capacity
        depot_index: index of the depot
    
    Returns:
        routes: list of optimal routes
    """
    n = len(distances)
    
    # Step 1: Calculate savings
    savings = calculate_savings(distances, depot_index)
    
    # Step 2: Initialize individual routes (each customer has its own route)
    routes = [[i] for i in range(1, n)]  # Each customer gets their own route
    
    # Step 3: Greedily merge routes based on savings
    merged = [False] * (n - 1)  # Track which routes have been merged
    
    for saving, i, j in savings:
        # Check if both routes exist and can be merged
        if not merged[i-1] and not merged[j-1]:
            # Get the actual routes
            route_i = routes[i-1]
            route_j = routes[j-1]
            
            # Check capacity constraint
            if is_valid_merge(route_i, route_j, capacity, demands):
                # Merge the routes
                new_route = merge_routes(route_i, route_j)
                routes[i-1] = new_route  # Replace first route
                routes[j-1] = []  # Mark second route as merged
                merged[j-1] = True  # Mark as merged
    
    # Filter out empty routes and add depot to each route
    final_routes = []
    for route in routes:
        if route:  # Only non-empty routes
            # Ensure route starts and ends at depot
            if route[0] != depot_index:
                route.insert(0, depot_index)
            if route[-1] != depot_index:
                route.append(depot_index)
            final_routes.append(route)
    
    return final_routes

def calculate_total_distance(routes, distances):
    """
    Calculate total distance for all routes
    
    Args:
        routes: list of routes
        distances: 2D array of distances
    
    Returns:
        total_distance: sum of all route distances
    """
    total = 0
    for route in routes:
        for i in range(len(route) - 1):
            total += distances[route[i]][route[i+1]]
    return total

# Example usage
if __name__ == "__main__":
    # Example with 6 points (1 depot + 5 customers)
    # Distance matrix (symmetric)
    distances = np.array([
        [0,   10,  15,  20,  25,  30],   # Depot
        [10,  0,   35,  25,  30,  35],   # Customer 1
        [15,  35,  0,   30,  35,  40],   # Customer 2
        [20,  25,  30,  0,   40,  45],   # Customer 3
        [25,  30,  35,  40,  0,   50],   # Customer 4
        [30,  35,  40,  45,  50,  0]    # Customer 5
    ])
    
    # Customer demands (depot has 0 demand)
    demands = [0, 10, 15, 20, 25, 30]
    
    # Vehicle capacity
    capacity = 50
    
    print("Distance Matrix:")
    print(distances)
    print("\nCustomer Demands:", demands)
    print("Vehicle Capacity:", capacity)
    
    # Run Clarke-Wright algorithm
    routes = clarke_wright_savings(distances, demands, capacity)
    
    print("\nOptimal Routes:")
    total_distance = 0
    for i, route in enumerate(routes):
        distance = calculate_total_distance([route], distances)
        total_distance += distance
        print(f"Route {i+1}: {' -> '.join(map(str, route))} (Distance: {distance})")
    
    print(f"\nTotal Distance: {total_distance}")
    
    # Visualize the routes
    plt.figure(figsize=(10, 8))
    
    # Plot customers
    for i in range(1, len(distances)):
        plt.scatter(i, 0, s=100, c='blue', marker='o', label=f'Customer {i}' if i == 1 else "")
        plt.annotate(f'C{i}', (i, 0), xytext=(5, 5), textcoords='offset points')
    
    # Plot depot
    plt.scatter(0, 0, s=200, c='red', marker='s', label='Depot')
    plt.annotate('D', (0, 0), xytext=(5, 5), textcoords='offset points')
    
    # Draw routes
    colors = ['green', 'orange', 'purple', 'brown', 'pink']
    for i, route in enumerate(routes):
        if len(route) > 2:  # Only draw routes with more than just depot
            x_coords = [j for j in route]
            y_coords = [0] * len(route)
            plt.plot(x_coords, y_coords, color=colors[i % len(colors)], linewidth=2, marker='o')
    
    plt.title('Clarke-Wright Savings Algorithm - Vehicle Routes')
    plt.xlabel('Customer Index')
    plt.ylabel('Location')
    plt.legend()
    plt.grid(True, alpha=0.3)
    plt.show()
```

## How the Algorithm Works:

1. **Calculate Savings**: For each pair of customers (i,j), calculate savings using:
   ```
   S(i,j) = C(0,i) + C(0,j) - C(i,j)
   ```
   where C(i,j) is the distance between points i and j, and 0 is the depot.

2. **Sort Savings**: Sort all savings in descending order.

3. **Greedy Merging**: Go through savings in descending order and merge routes if:
   - Both routes haven't been merged yet
   - The combined route doesn't exceed vehicle capacity

4. **Final Routes**: Return the final set of routes that minimize total distance.

## Key Features:

- Handles capacity constraints
- Works with symmetric distance matrices
- Returns multiple routes for a multi-vehicle VRP
- Includes visualization capability
- Provides total distance calculation

This implementation demonstrates the core logic of the Clarke-Wright algorithm while being easily extensible for different problem instances.

