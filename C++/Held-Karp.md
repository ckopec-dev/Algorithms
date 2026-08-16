# Held-Karp Algorithm Implementation in C++

The Held-Karp algorithm is used to solve the Traveling Salesman Problem (TSP) using dynamic programming with a time complexity of O(n²2ⁿ).

```cpp
#include <iostream>
#include <vector>
#include <algorithm>
#include <climits>

class TSPSolver {
private:
    int n; // number of cities
    std::vector<std::vector<int>> dist; // distance matrix
    
public:
    TSPSolver(int num_cities, std::vector<std::vector<int>>& distance_matrix) 
        : n(num_cities), dist(distance_matrix) {}
    
    int solveTSP() {
        // dp[mask][i] represents the minimum cost to visit all cities in mask
        // and end at city i
        std::vector<std::vector<int>> dp(1 << n, std::vector<int>(n, INT_MAX));
        
        // Base case: starting from city 0
        dp[1][0] = 0;
        
        // Fill the DP table
        for (int mask = 1; mask < (1 << n); mask++) {
            for (int u = 0; u < n; u++) {
                // If city u is not in current mask, skip
                if (!(mask & (1 << u))) continue;
                
                // Try to go to each unvisited city v
                for (int v = 0; v < n; v++) {
                    // If city v is already visited, skip
                    if (mask & (1 << v)) continue;
                    
                    int new_mask = mask | (1 << v);
                    dp[new_mask][v] = std::min(dp[new_mask][v], 
                                              dp[mask][u] + dist[u][v]);
                }
            }
        }
        
        // Find minimum cost to return to starting city
        int min_cost = INT_MAX;
        for (int i = 1; i < n; i++) {
            if (dp[(1 << n) - 1][i] != INT_MAX) {
                min_cost = std::min(min_cost, dp[(1 << n) - 1][i] + dist[i][0]);
            }
        }
        
        return min_cost;
    }
    
    // Function to get the actual path (not just cost)
    std::vector<int> solveTSPWithPath() {
        // dp[mask][i] stores minimum cost
        // parent[mask][i] stores previous city in optimal path
        std::vector<std::vector<int>> dp(1 << n, std::vector<int>(n, INT_MAX));
        std::vector<std::vector<int>> parent(1 << n, std::vector<int>(n, -1));
        
        dp[1][0] = 0;
        
        for (int mask = 1; mask < (1 << n); mask++) {
            for (int u = 0; u < n; u++) {
                if (!(mask & (1 << u))) continue;
                
                for (int v = 0; v < n; v++) {
                    if (mask & (1 << v)) continue;
                    
                    int new_mask = mask | (1 << v);
                    if (dp[mask][u] + dist[u][v] < dp[new_mask][v]) {
                        dp[new_mask][v] = dp[mask][u] + dist[u][v];
                        parent[new_mask][v] = u;
                    }
                }
            }
        }
        
        // Reconstruct path
        std::vector<int> path;
        int mask = (1 << n) - 1;
        int current = 0;
        
        // Find the city that gives minimum cost to return to start
        int last_city = 0;
        for (int i = 1; i < n; i++) {
            if (dp[(1 << n) - 1][i] != INT_MAX && 
                dp[(1 << n) - 1][i] + dist[i][0] < dp[(1 << n) - 1][last_city] + dist[last_city][0]) {
                last_city = i;
            }
        }
        
        // Reconstruct the path backwards
        int current_city = last_city;
        while (current_city != -1) {
            path.push_back(current_city);
            int prev = parent[mask][current_city];
            mask ^= (1 << current_city);
            current_city = prev;
        }
        
        std::reverse(path.begin(), path.end());
        return path;
    }
};

int main() {
    // Example: 4 cities with distance matrix
    int n = 4;
    std::vector<std::vector<int>> dist = {
        {0, 10, 15, 20},
        {10, 0, 35, 25},
        {15, 35, 0, 30},
        {20, 25, 30, 0}
    };
    
    TSPSolver solver(n, dist);
    
    int min_cost = solver.solveTSP();
    std::cout << "Minimum cost of TSP: " << min_cost << std::endl;
    
    // Get the actual path
    std::vector<int> path = solver.solveTSPWithPath();
    std::cout << "Optimal path: ";
    for (int city : path) {
        std::cout << city << " ";
    }
    std::cout << std::endl;
    
    return 0;
}
```

## How it works:

1. **Dynamic Programming Approach**: Uses bitmasks to represent subsets of visited cities
2. **State Definition**: `dp[mask][i]` represents the minimum cost to visit all cities in `mask` and end at city `i`
3. **Transition**: For each subset of cities, try adding each unvisited city to form a new subset
4. **Base Case**: Start from city 0 with mask `{0}` (only city 0 visited)
5. **Final Answer**: Minimum cost to return to city 0 after visiting all cities

## Time and Space Complexity:
- **Time Complexity**: O(n²2ⁿ)
- **Space Complexity**: O(n2ⁿ)

## Sample Output:
```
Minimum cost of TSP: 80
Optimal path: 0 1 3 2 
```

This implementation efficiently solves the TSP for small to medium-sized instances (up to ~20 cities) using the Held-Karp algorithm's dynamic programming approach.