# Traveling Salesman Problem (TSP) - Python Implementation

Here's a complete implementation of the TSP problem using different approaches:

## 1. Brute Force Approach

```python
import itertools
import math

def distance(city1, city2):
    """Calculate Euclidean distance between two cities"""
    return math.sqrt((city1[0] - city2[0])**2 + (city1[1] - city2[1])**2)

def total_distance(tour, cities):
    """Calculate total distance of a tour"""
    total = 0
    for i in range(len(tour)):
        total += distance(cities[tour[i]], cities[tour[i-1]])
    return total

def tsp_brute_force(cities):
    """Solve TSP using brute force approach"""
    n = len(cities)
    if n <= 1:
        return 0, []
    
    # Generate all possible permutations
    min_distance = float('inf')
    best_tour = []
    
    for perm in itertools.permutations(range(n)):
        if perm[0] == 0:  # Start from city 0
            dist = total_distance(perm, cities)
            if dist < min_distance:
                min_distance = dist
                best_tour = list(perm)
    
    return min_distance, best_tour

# Example usage
cities = [(0, 0), (1, 2), (3, 1), (5, 3), (2, 4)]
distance, tour = tsp_brute_force(cities)
print(f"Minimum distance: {distance:.2f}")
print(f"Best tour: {tour}")
```

## 2. Nearest Neighbor Heuristic

```python
def tsp_nearest_neighbor(cities):
    """Solve TSP using nearest neighbor heuristic"""
    n = len(cities)
    if n <= 1:
        return 0, []
    
    unvisited = set(range(1, n))  # Start from city 0
    current_city = 0
    tour = [0]
    total_distance = 0
    
    while unvisited:
        nearest_city = min(unvisited, key=lambda city: distance(cities[current_city], cities[city]))
        total_distance += distance(cities[current_city], cities[nearest_city])
        current_city = nearest_city
        tour.append(current_city)
        unvisited.remove(current_city)
    
    # Return to starting city
    total_distance += distance(cities[current_city], cities[0])
    tour.append(0)
    
    return total_distance, tour

# Example usage
distance, tour = tsp_nearest_neighbor(cities)
print(f"Nearest neighbor distance: {distance:.2f}")
print(f"Tour: {tour}")
```

## 3. Dynamic Programming Approach (Held-Karp)

```python
def tsp_dynamic_programming(cities):
    """Solve TSP using dynamic programming (Held-Karp algorithm)"""
    n = len(cities)
    if n <= 1:
        return 0, []
    
    # Precompute distances
    dist = [[0] * n for _ in range(n)]
    for i in range(n):
        for j in range(n):
            dist[i][j] = distance(cities[i], cities[j])
    
    # dp[mask][i] = minimum cost to visit all cities in mask and end at city i
    dp = [[float('inf')] * n for _ in range(1 << n)]
    parent = [[-1] * n for _ in range(1 << n)]
    
    # Base case: start from city 0
    dp[1][0] = 0
    
    # Fill DP table
    for mask in range(1 << n):
        for u in range(n):
            if not (mask & (1 << u)):
                continue
            for v in range(n):
                if mask & (1 << v):
                    continue
                new_mask = mask | (1 << v)
                new_cost = dp[mask][u] + dist[u][v]
                if new_cost < dp[new_mask][v]:
                    dp[new_mask][v] = new_cost
                    parent[new_mask][v] = u
    
    # Find minimum cost to return to start
    final_mask = (1 << n) - 1
    min_cost = float('inf')
    last_city = -1
    
    for i in range(1, n):
        cost = dp[final_mask][i] + dist[i][0]
        if cost < min_cost:
            min_cost = cost
            last_city = i
    
    # Reconstruct tour
    tour = []
    mask = final_mask
    current = last_city
    while current != -1:
        tour.append(current)
        next_city = parent[mask][current]
        mask ^= (1 << current)
        current = next_city
    
    tour.reverse()
    tour = [0] + tour  # Add starting city
    
    return min_cost, tour

# Example usage
distance, tour = tsp_dynamic_programming(cities)
print(f"Dynamic programming distance: {distance:.2f}")
print(f"Tour: {tour}")
```

## 4. Complete Example with Visualization

```python
import matplotlib.pyplot as plt
import numpy as np

def plot_tour(cities, tour, title):
    """Plot the TSP tour"""
    plt.figure(figsize=(10, 8))
    
    # Plot cities
    x = [city[0] for city in cities]
    y = [city[1] for city in cities]
    plt.scatter(x, y, c='red', s=100, zorder=5)
    
    # Label cities
    for i, (cx, cy) in enumerate(cities):
        plt.annotate(f'{i}', (cx, cy), xytext=(5, 5), textcoords='offset points')
    
    # Plot tour
    tour_x = [cities[i][0] for i in tour]
    tour_y = [cities[i][1] for i in tour]
    plt.plot(tour_x, tour_y, 'b-', linewidth=2, marker='o', markersize=8)
    
    plt.title(title)
    plt.xlabel('X coordinate')
    plt.ylabel('Y coordinate')
    plt.grid(True, alpha=0.3)
    plt.show()

# Example with more cities
cities_example = [(60, 200), (180, 200), (80, 180), (140, 180),
                  (20, 160), (100, 160), (200, 160), (140, 140),
                  (40, 120), (100, 120), (180, 100), (60, 80),
                  (120, 80), (180, 60), (20, 40), (100, 40)]

# Solve using different methods
print("TSP Solutions:")
print("=" * 40)

# Brute force (for small instances)
if len(cities_example) <= 8:
    distance, tour = tsp_brute_force(cities_example)
    print(f"Brute Force: {distance:.2f}")

# Nearest neighbor
distance, tour = tsp_nearest_neighbor(cities_example)
print(f"Nearest Neighbor: {distance:.2f}")

# Dynamic programming
distance, tour = tsp_dynamic_programming(cities_example)
print(f"Dynamic Programming: {distance:.2f}")

# Uncomment to visualize (requires matplotlib)
# plot_tour(cities_example, tour, "TSP Solution")
```

## Key Features of This Implementation:

1. **Multiple Algorithms**: Brute force, nearest neighbor, and dynamic programming
2. **Distance Calculation**: Euclidean distance between cities
3. **Flexible Input**: Accepts list of (x, y) coordinates
4. **Visualization**: Optional plotting functionality
5. **Performance Comparison**: Shows different approaches and their trade-offs

## Time Complexity:
- **Brute Force**: O(n!)
- **Nearest Neighbor**: O(n²)
- **Dynamic Programming**: O(n²2ⁿ)

The brute force approach is only practical for small instances (n ≤ 10), while the nearest neighbor provides a quick approximation, and dynamic programming gives optimal solutions for moderate-sized problems.

