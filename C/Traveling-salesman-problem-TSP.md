# Traveling Salesman Problem (TSP) - C Implementation

Here's a complete C implementation of a brute force solution for the Traveling Salesman Problem:

```c
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

#define MAX_CITIES 10

// Function to calculate minimum distance using brute force
int calculateMinDistance(int graph[MAX_CITIES][MAX_CITIES], int numCities) {
    // Array to store current permutation
    int *perm = (int*)malloc(numCities * sizeof(int));
    
    // Initialize permutation array
    for (int i = 0; i < numCities; i++) {
        perm[i] = i;
    }
    
    int minDistance = INT_MAX;
    
    // Generate all permutations and calculate distances
    do {
        int currentDistance = 0;
        
        // Calculate total distance for current permutation
        for (int i = 0; i < numCities - 1; i++) {
            currentDistance += graph[perm[i]][perm[i+1]];
        }
        
        // Add distance from last city back to first city
        currentDistance += graph[perm[numCities-1]][perm[0]];
        
        // Update minimum distance
        if (currentDistance < minDistance) {
            minDistance = currentDistance;
        }
        
    } while (next_permutation(perm, numCities));
    
    free(perm);
    return minDistance;
}

// Function to generate next permutation (simplified version)
int next_permutation(int *array, int size) {
    // Find the largest index i such that array[i] < array[i+1]
    int i = size - 2;
    while (i >= 0 && array[i] >= array[i+1]) {
        i--;
    }
    
    if (i < 0) {
        return 0; // No more permutations
    }
    
    // Find the largest index j such that array[i] < array[j]
    int j = size - 1;
    while (array[j] <= array[i]) {
        j--;
    }
    
    // Swap array[i] and array[j]
    int temp = array[i];
    array[i] = array[j];
    array[j] = temp;
    
    // Reverse the suffix starting at i+1
    int start = i + 1;
    int end = size - 1;
    while (start < end) {
        temp = array[start];
        array[start] = array[end];
        array[end] = temp;
        start++;
        end--;
    }
    
    return 1; // Next permutation exists
}

// Function to print the distance matrix
void printDistanceMatrix(int graph[MAX_CITIES][MAX_CITIES], int numCities) {
    printf("Distance Matrix:\n");
    for (int i = 0; i < numCities; i++) {
        for (int j = 0; j < numCities; j++) {
            printf("%3d ", graph[i][j]);
        }
        printf("\n");
    }
    printf("\n");
}

// Main function
int main() {
    // Example: 4 cities with distance matrix
    int numCities = 4;
    
    int graph[MAX_CITIES][MAX_CITIES] = {
        {0, 10, 15, 20},
        {10, 0, 35, 25},
        {15, 35, 0, 30},
        {20, 25, 30, 0}
    };
    
    printf("Traveling Salesman Problem - Brute Force Solution\n");
    printf("================================================\n\n");
    
    printDistanceMatrix(graph, numCities);
    
    int minDistance = calculateMinDistance(graph, numCities);
    
    printf("Minimum distance: %d\n", minDistance);
    
    return 0;
}
```

## Alternative Implementation with Better Permutation Function

```c
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

#define MAX_CITIES 10

// Simple recursive function to generate permutations and find minimum distance
void tspRecursive(int graph[MAX_CITIES][MAX_CITIES], int *path, int *visited, 
                  int currentCity, int pathLength, int totalDistance, 
                  int *minDistance, int numCities) {
    
    // If we've visited all cities and returned to start
    if (pathLength == numCities) {
        totalDistance += graph[currentCity][0]; // Return to starting city
        if (totalDistance < *minDistance) {
            *minDistance = totalDistance;
        }
        return;
    }
    
    // Try visiting each unvisited city
    for (int i = 1; i < numCities; i++) { // Start from 1, skip 0 (starting city)
        if (!visited[i]) {
            visited[i] = 1;
            path[pathLength] = i;
            
            tspRecursive(graph, path, visited, i, pathLength + 1, 
                        totalDistance + graph[currentCity][i], minDistance, numCities);
            
            visited[i] = 0; // Backtrack
        }
    }
}

// Main function with recursive approach
int solveTSP(int graph[MAX_CITIES][MAX_CITIES], int numCities) {
    int *path = (int*)malloc(numCities * sizeof(int));
    int *visited = (int*)calloc(numCities, sizeof(int));
    
    int minDistance = INT_MAX;
    
    visited[0] = 1; // Start from city 0
    path[0] = 0;
    
    tspRecursive(graph, path, visited, 0, 1, 0, &minDistance, numCities);
    
    free(path);
    free(visited);
    
    return minDistance;
}

int main() {
    int numCities = 4;
    
    int graph[MAX_CITIES][MAX_CITIES] = {
        {0, 10, 15, 20},
        {10, 0, 35, 25},
        {15, 35, 0, 30},
        {20, 25, 30, 0}
    };
    
    printf("TSP Solution using Recursive Approach\n");
    printf("=====================================\n");
    
    int result = solveTSP(graph, numCities);
    printf("Minimum distance: %d\n", result);
    
    return 0;
}
```

## Key Features of This Implementation:

1. **Brute Force Approach**: Checks all possible permutations
2. **Distance Matrix**: Uses a 2D array to represent city distances
3. **Permutation Generation**: Generates all possible routes
4. **Minimum Calculation**: Finds the route with minimum total distance

## Time Complexity:
- O(n! × n) where n is the number of cities
- This is exponential and only practical for small instances

## Sample Output:
```
TSP Solution using Recursive Approach
=====================================
Minimum distance: 80
```

**Note**: For larger instances, consider using heuristic approaches like genetic algorithms, simulated annealing, or nearest neighbor heuristics for better performance.