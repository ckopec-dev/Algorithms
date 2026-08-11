# Dynamic Programming Example: Fibonacci Sequence

Here's a C++ implementation of the Fibonacci sequence using dynamic programming with memoization:

```cpp
#include <iostream>
#include <vector>
using namespace std;

// Memoization approach - Top-down dynamic programming
int fibonacciMemo(int n, vector<int>& dp) {
    // Base cases
    if (n <= 1) {
        return n;
    }
    
    // If already computed, return stored result
    if (dp[n] != -1) {
        return dp[n];
    }
    
    // Compute and store the result
    dp[n] = fibonacciMemo(n - 1, dp) + fibonacciMemo(n - 2, dp);
    return dp[n];
}

// Tabulation approach - Bottom-up dynamic programming
int fibonacciTab(int n) {
    if (n <= 1) {
        return n;
    }
    
    // Create DP array
    vector<int> dp(n + 1, 0);
    
    // Base cases
    dp[0] = 0;
    dp[1] = 1;
    
    // Fill the DP table
    for (int i = 2; i <= n; i++) {
        dp[i] = dp[i - 1] + dp[i - 2];
    }
    
    return dp[n];
}

int main() {
    int n = 10;
    
    // Using memoization approach
    vector<int> dpMemo(n + 1, -1);
    cout << "Fibonacci(" << n << ") using Memoization: " 
         << fibonacciMemo(n, dpMemo) << endl;
    
    // Using tabulation approach
    cout << "Fibonacci(" << n << ") using Tabulation: " 
         << fibonacciTab(n) << endl;
    
    return 0;
}
```

## Output:
```
Fibonacci(10) using Memoization: 55
Fibonacci(10) using Tabulation: 55
```

## Key Dynamic Programming Concepts Demonstrated:

1. **Overlapping Subproblems**: Fibonacci has many repeated calculations
2. **Optimal Substructure**: F(n) = F(n-1) + F(n-2)
3. **Memoization**: Storing previously computed results to avoid recomputation
4. **Tabulation**: Building solution from bottom-up using a table

## Time and Space Complexity:
- **Memoization**: O(n) time, O(n) space
- **Tabulation**: O(n) time, O(n) space

This example shows how dynamic programming optimizes recursive solutions by eliminating redundant calculations through either memoization or tabulation techniques.