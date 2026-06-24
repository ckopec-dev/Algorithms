# Matrix Chain Multiplication in C

Here's a complete implementation of the Matrix Chain Multiplication algorithm using dynamic programming:

```c
#include <stdio.h>
#include <limits.h>

// Function to calculate minimum number of multiplications
int matrixChainMultiplication(int p[], int n) {
    // Create a 2D table to store results of subproblems
    int dp[n][n];
    
    // Initialize the diagonal elements as 0 (no multiplication needed for single matrix)
    for (int i = 1; i < n; i++) {
        dp[i][i] = 0;
    }
    
    // L is chain length
    for (int L = 2; L < n; L++) {
        for (int i = 1; i < n - L + 1; i++) {
            int j = i + L - 1;
            dp[i][j] = INT_MAX;
            
            // Try all possible divisions
            for (int k = i; k <= j - 1; k++) {
                // Cost of multiplying left and right subchains
                int cost = dp[i][k] + dp[k + 1][j] + p[i - 1] * p[k] * p[j];
                
                if (cost < dp[i][j]) {
                    dp[i][j] = cost;
                }
            }
        }
    }
    
    return dp[1][n - 1];
}

// Function to print the optimal parenthesization
void printOptimalParentheses(int dp[][10], int i, int j, char *names) {
    if (i == j) {
        printf("%c", names[i - 1]);
        return;
    }
    
    printf("(");
    printOptimalParentheses(dp, i, dp[i][j], names);
    printf(" x ");
    printOptimalParentheses(dp, dp[i][j] + 1, j, names);
    printf(")");
}

int main() {
    // Example: Matrices A1(10x100), A2(100x5), A3(5x50)
    int dimensions[] = {10, 100, 5, 50};
    int n = sizeof(dimensions) / sizeof(dimensions[0]);
    
    printf("Matrix dimensions: ");
    for (int i = 0; i < n; i++) {
        printf("%d ", dimensions[i]);
    }
    printf("\n");
    
    // Calculate minimum multiplications
    int minMultiplications = matrixChainMultiplication(dimensions, n);
    
    printf("Minimum number of scalar multiplications needed: %d\n", minMultiplications);
    
    // Example with more matrices
    printf("\n--- Another Example ---\n");
    int dimensions2[] = {40, 20, 30, 10, 30};
    int n2 = sizeof(dimensions2) / sizeof(dimensions2[0]);
    
    printf("Matrix dimensions: ");
    for (int i = 0; i < n2; i++) {
        printf("%d ", dimensions2[i]);
    }
    printf("\n");
    
    int minMultiplications2 = matrixChainMultiplication(dimensions2, n2);
    printf("Minimum number of scalar multiplications needed: %d\n", minMultiplications2);
    
    return 0;
}
```

## How it works:

1. **Input**: Array `p[]` where matrix i has dimensions `p[i-1] × p[i]`
2. **Algorithm**: Uses dynamic programming to find the optimal way to parenthesize matrices
3. **Time Complexity**: O(n³)
4. **Space Complexity**: O(n²)

## Example Output:
```
Matrix dimensions: 10 100 5 50 
Minimum number of scalar multiplications needed: 7500

--- Another Example ---
Matrix dimensions: 40 20 30 10 30 
Minimum number of scalar multiplications needed: 26000
```

## Key Concepts:

- **Dynamic Programming**: Builds solution from smaller subproblems
- **Optimal Substructure**: Optimal solution contains optimal solutions to subproblems
- **Overlapping Subproblems**: Same subproblems are solved multiple times

The algorithm determines the most efficient way to multiply a chain of matrices by finding the optimal parenthesization that minimizes the total number of scalar multiplications.