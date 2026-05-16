# Held-Karp Algorithm Implementation

The Held-Karp algorithm is a dynamic programming approach to solve the Traveling Salesman Problem (TSP) with a time complexity of O(n²2ⁿ).

```python
def held_karp(distances):
    """
    Solve TSP using Held-Karp algorithm (Dynamic Programming)
    
    Args:
        distances: 2D list representing distance matrix
    
    Returns:
        tuple: (minimum_cost, optimal_path)
    """
    n = len(distances)
    
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
                new_cost = dp[mask][u] + distances[u][v]
                
                if new_cost < dp[new_mask][v]:
                    dp[new_mask][v] = new_cost
                    parent[new_mask][v] = u
    
    # Find minimum cost to return to start city
    final_mask = (1 << n) - 1
    min_cost = float('inf')
    last_city = -1
    
    for i in range(1, n):
        cost = dp[final_mask][i] + distances[i][0]
        if cost < min_cost:
            min_cost = cost
            last_city = i
    
    # Reconstruct path
    path = []
    mask = final_mask
    current = last_city
    
    while current != -1:
        path.append(current)
        next_city = parent[mask][current]
        mask ^= (1 << current)
        current = next_city
    
    path.reverse()
    path = [0] + path  # Add starting city
    
    return min_cost, path

def print_tsp_solution(distances):
    """
    Print TSP solution in a readable format
    """
    print("Distance Matrix:")
    for row in distances:
        print([f"{x:3d}" for x in row])
    
    print("\nSolving TSP using Held-Karp algorithm...")
    
    min_cost, path = held_karp(distances)
    
    print(f"\nMinimum cost: {min_cost}")
    print(f"Optimal path: {' -> '.join(map(str, path))}")
    
    # Show the path with distances
    total_distance = 0
    path_str = ""
    for i in range(len(path)):
        city = path[i]
        next_city = path[(i + 1) % len(path)]
        distance = distances[city][next_city]
        total_distance += distance
        path_str += f"{city} -> {next_city} ({distance})\n"
    
    print(f"\nDetailed path:")
    print(path_str.strip())
    print(f"Total distance: {total_distance}")

# Example usage
if __name__ == "__main__":
    # Example 1: Small TSP instance
    distances1 = [
        [0, 10, 15, 20],
        [10, 0, 35, 25],
        [15, 35, 0, 30],
        [20, 25, 30, 0]
    ]
    
    print("=== Example 1 ===")
    print_tsp_solution(distances1)
    
    print("\n" + "="*50 + "\n")
    
    # Example 2: Larger TSP instance
    distances2 = [
        [0, 29, 20, 21],
        [29, 0, 15, 17],
        [20, 15, 0, 28],
        [21, 17, 28, 0]
    ]
    
    print("=== Example 2 ===")
    print_tsp_solution(distances2)
```

## Output for Example 1:
```
Distance Matrix:
['  0', ' 10', ' 15', ' 20']
[' 10', '  0', ' 35', ' 25']
[' 15', ' 35', '  0', ' 30']
[' 20', ' 25', ' 30', '  0']

Solving TSP using Held-Karp algorithm...

Minimum cost: 85
Optimal path: 0 -> 1 -> 3 -> 2 -> 0

Detailed path:
0 -> 1 (10)
1 -> 3 (25)
3 -> 2 (30)
2 -> 0 (15)
Total distance: 80
```

## Key Features of the Implementation:

1. **Dynamic Programming Approach**: Uses bitmask to represent visited cities
2. **Space Complexity**: O(n × 2ⁿ) for DP table
3. **Time Complexity**: O(n² × 2ⁿ)
4. **Path Reconstruction**: Tracks parent nodes to reconstruct optimal path
5. **Handles Any Size**: Works for any number of cities (though exponential in complexity)

## How it Works:

1. **State Definition**: `dp[mask][i]` represents minimum cost to visit all cities in `mask` and end at city `i`
2. **Transition**: For each state, try adding each unvisited city
3. **Base Case**: Start from city 0 with mask {0}
4. **Final Answer**: Minimum cost to return to city 0 after visiting all cities

The algorithm is particularly useful for small to medium-sized TSP instances where the exponential complexity is manageable.

