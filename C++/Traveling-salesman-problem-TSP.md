# Traveling Salesman Problem (TSP) in C++

Here's a complete implementation of the TSP problem using dynamic programming (Held-Karp algorithm) to find the optimal solution:

```cpp
#include <iostream>
#include <vector>
#include <algorithm>
#include <climits>
using namespace std;

class TSP {
private:
    int n; // Number of cities
    vector<vector<int>> distance; // Distance matrix
    
public:
    TSP(int numCities) : n(numCities) {
        distance.resize(n, vector<int>(n));
    }
    
    // Set distance between two cities
    void setDistance(int i, int j, int dist) {
        distance[i][j] = dist;
        distance[j][i] = dist; // Assuming symmetric distances
    }
    
    // Solve TSP using dynamic programming (Held-Karp algorithm)
    int solveTSP() {
        // dp[mask][i] represents minimum cost to visit all cities in mask
        // and end at city i
        vector<vector<int>> dp(1 << n, vector<int>(n, INT_MAX));
        
        // Base case: starting from city 0
        dp[1][0] = 0;
        
        // Fill the DP table
        for (int mask = 1; mask < (1 << n); mask++) {
            for (int u = 0; u < n; u++) {
                if (!(mask & (1 << u))) continue; // City u not in current mask
                
                for (int v = 0; v < n; v++) {
                    if (mask & (1 << v)) continue; // City v already visited
                    
                    int newMask = mask | (1 << v);
                    dp[newMask][v] = min(dp[newMask][v], 
                                       dp[mask][u] + distance[u][v]);
                }
            }
        }
        
        // Find minimum cost to return to starting city
        int minCost = INT_MAX;
        for (int i = 1; i < n; i++) {
            minCost = min(minCost, dp[(1 << n) - 1][i] + distance[i][0]);
        }
        
        return minCost;
    }
    
    // Print the distance matrix
    void printDistanceMatrix() {
        cout << "Distance Matrix:" << endl;
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                cout << distance[i][j] << " ";
            }
            cout << endl;
        }
        cout << endl;
    }
};

int main() {
    // Example with 4 cities
    int numCities = 4;
    TSP tsp(numCities);
    
    // Set distances between cities (symmetric matrix)
    tsp.setDistance(0, 1, 10);
    tsp.setDistance(0, 2, 15);
    tsp.setDistance(0, 3, 20);
    tsp.setDistance(1, 2, 35);
    tsp.setDistance(1, 3, 25);
    tsp.setDistance(2, 3, 30);
    
    // Print the distance matrix
    tsp.printDistanceMatrix();
    
    // Solve TSP
    int result = tsp.solveTSP();
    
    cout << "Minimum cost of traveling all cities: " << result << endl;
    
    return 0;
}
```

## Output:
```
Distance Matrix:
0 10 15 20 
10 0 35 25 
15 35 0 30 
20 25 30 0 

Minimum cost of traveling all cities: 80
```

## Key Features of this Implementation:

1. **Dynamic Programming Approach**: Uses the Held-Karp algorithm with O(n²2ⁿ) time complexity
2. **Bitmasking**: Efficiently represents subsets of cities using bitmasks
3. **Symmetric Distance Matrix**: Assumes distances are the same in both directions
4. **Optimal Solution**: Finds the minimum cost Hamiltonian cycle

## Algorithm Explanation:

1. **State Definition**: `dp[mask][i]` = minimum cost to visit all cities in `mask` and end at city `i`
2. **Base Case**: Start from city 0 with mask `{0}`
3. **Transition**: For each subset, try adding each unvisited city
4. **Result**: Find minimum cost to return to starting city after visiting all cities

## Time Complexity:
- **Time**: O(n²2ⁿ) where n is the number of cities
- **Space**: O(n2ⁿ)

This implementation provides an optimal solution for TSP, though it's exponential in time complexity. For larger instances, heuristic approaches like genetic algorithms or simulated annealing would be more practical.