# Knapsack Problem Algorithm in C

Here's a complete implementation of the 0/1 Knapsack problem using dynamic programming in C:

```c
#include <stdio.h>
#include <stdlib.h>

// Function to find maximum of two integers
int max(int a, int b) {
    return (a > b) ? a : b;
}

// 0/1 Knapsack problem solution using dynamic programming
int knapsack(int capacity, int weights[], int values[], int n) {
    // Create a 2D array to store results of subproblems
    int** dp = (int**)malloc((n + 1) * sizeof(int*));
    for (int i = 0; i <= n; i++) {
        dp[i] = (int*)malloc((capacity + 1) * sizeof(int));
    }
    
    // Build the table dp[][] in bottom up manner
    for (int i = 0; i <= n; i++) {
        for (int w = 0; w <= capacity; w++) {
            // Base case: no items or no capacity
            if (i == 0 || w == 0) {
                dp[i][w] = 0;
            }
            // If weight of current item is more than capacity, skip it
            else if (weights[i - 1] > w) {
                dp[i][w] = dp[i - 1][w];
            }
            // Otherwise, take maximum of including or excluding the item
            else {
                dp[i][w] = max(dp[i - 1][w], 
                              values[i - 1] + dp[i - 1][w - weights[i - 1]]);
            }
        }
    }
    
    // Store the result and free memory
    int result = dp[n][capacity];
    
    // Free allocated memory
    for (int i = 0; i <= n; i++) {
        free(dp[i]);
    }
    free(dp);
    
    return result;
}

// Function to print the items included in the knapsack
void printKnapsackItems(int capacity, int weights[], int values[], int n) {
    int** dp = (int**)malloc((n + 1) * sizeof(int*));
    for (int i = 0; i <= n; i++) {
        dp[i] = (int*)malloc((capacity + 1) * sizeof(int));
    }
    
    // Fill the dp table
    for (int i = 0; i <= n; i++) {
        for (int w = 0; w <= capacity; w++) {
            if (i == 0 || w == 0) {
                dp[i][w] = 0;
            } else if (weights[i - 1] > w) {
                dp[i][w] = dp[i - 1][w];
            } else {
                dp[i][w] = max(dp[i - 1][w], 
                              values[i - 1] + dp[i - 1][w - weights[i - 1]]);
            }
        }
    }
    
    // Backtrack to find which items were included
    printf("Items included in knapsack:\n");
    int w = capacity;
    for (int i = n; i > 0 && w >= 0; i--) {
        // If value is different from above row, item was included
        if (dp[i][w] != dp[i - 1][w]) {
            printf("Item %d (Weight: %d, Value: %d)\n", i, weights[i - 1], values[i - 1]);
            w = w - weights[i - 1];
        }
    }
    
    // Free memory
    for (int i = 0; i <= n; i++) {
        free(dp[i]);
    }
    free(dp);
}

int main() {
    // Example values
    int values[] = {60, 100, 120};
    int weights[] = {10, 20, 30};
    int capacity = 50;
    int n = sizeof(values) / sizeof(values[0]);
    
    printf("Knapsack Problem Example\n");
    printf("========================\n");
    printf("Values:     ");
    for (int i = 0; i < n; i++) {
        printf("%d ", values[i]);
    }
    printf("\nWeights:    ");
    for (int i = 0; i < n; i++) {
        printf("%d ", weights[i]);
    }
    printf("\nCapacity: %d\n\n", capacity);
    
    // Solve knapsack problem
    int max_value = knapsack(capacity, weights, values, n);
    printf("Maximum value that can be obtained: %d\n", max_value);
    
    // Print which items were included
    printKnapsackItems(capacity, weights, values, n);
    
    return 0;
}
```

## Output:
```
Knapsack Problem Example
========================
Values:     60 100 120 
Weights:    10 20 30 
Capacity: 50

Maximum value that can be obtained: 220
Items included in knapsack:
Item 3 (Weight: 30, Value: 120)
Item 2 (Weight: 20, Value: 100)
```

## Algorithm Explanation:

1. **Dynamic Programming Approach**: Uses a 2D table `dp[i][w]` where `i` represents the first `i` items and `w` represents the current capacity.

2. **Recurrence Relation**:
   - If weight of item `i-1` > current capacity `w`: `dp[i][w] = dp[i-1][w]`
   - Otherwise: `dp[i][w] = max(dp[i-1][w], values[i-1] + dp[i-1][w-weights[i-1]])`

3. **Time Complexity**: O(n × capacity)
4. **Space Complexity**: O(n × capacity)

The algorithm efficiently solves the classic 0/1 Knapsack problem by building up solutions to subproblems and using optimal substructure property.

