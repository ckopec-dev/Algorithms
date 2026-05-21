# Dynamic Programming Example: Fibonacci Sequence

Here's a C implementation of the Fibonacci sequence using dynamic programming with memoization:

```c
#include <stdio.h>
#include <stdlib.h>

// Function to calculate Fibonacci number using dynamic programming
int fibonacci_dp(int n) {
    // Handle edge cases
    if (n <= 0) return 0;
    if (n == 1) return 1;
    
    // Create array to store computed values
    int *dp = (int*)malloc((n + 1) * sizeof(int));
    
    // Base cases
    dp[0] = 0;
    dp[1] = 1;
    
    // Fill the array using dynamic programming
    for (int i = 2; i <= n; i++) {
        dp[i] = dp[i-1] + dp[i-2];
    }
    
    int result = dp[n];
    free(dp);  // Free allocated memory
    return result;
}

// Alternative implementation using space-optimized approach
int fibonacci_optimized(int n) {
    if (n <= 0) return 0;
    if (n == 1) return 1;
    
    int prev2 = 0;  // F(0)
    int prev1 = 1;  // F(1)
    int current;
    
    for (int i = 2; i <= n; i++) {
        current = prev1 + prev2;
        prev2 = prev1;
        prev1 = current;
    }
    
    return current;
}

int main() {
    int n = 10;
    
    printf("Fibonacci sequence (first %d numbers):\n", n);
    
    for (int i = 0; i < n; i++) {
        printf("F(%d) = %d\n", i, fibonacci_dp(i));
    }
    
    printf("\nUsing optimized approach:\n");
    printf("F(10) = %d\n", fibonacci_optimized(10));
    
    return 0;
}
```

## Output:
```
Fibonacci sequence (first 10 numbers):
F(0) = 0
F(1) = 1
F(2) = 1
F(3) = 2
F(4) = 3
F(5) = 5
F(6) = 8
F(7) = 13
F(8) = 21
F(9) = 34

Using optimized approach:
F(10) = 55
```

## Key Dynamic Programming Concepts Demonstrated:

1. **Overlapping Subproblems**: The same Fibonacci numbers are calculated multiple times
2. **Optimal Substructure**: F(n) = F(n-1) + F(n-2)
3. **Memoization**: Storing previously computed results to avoid redundant calculations
4. **Bottom-up Approach**: Building solutions from smaller subproblems

## Time and Space Complexity:
- **Time Complexity**: O(n) - each subproblem is solved only once
- **Space Complexity**: O(n) for the first approach, O(1) for the optimized version

This example shows how dynamic programming transforms an exponential time complexity problem into a linear time solution by avoiding redundant calculations.

