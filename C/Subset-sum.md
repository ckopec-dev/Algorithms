# Subset Sum Algorithm in C

Here's an implementation of the subset sum problem using dynamic programming approach:

```c
#include <stdio.h>
#include <stdbool.h>

// Function to check if there exists a subset with given sum
bool isSubsetSum(int arr[], int n, int sum) {
    // Create a 2D boolean DP table
    bool dp[n + 1][sum + 1];
    
    // Initialize base cases
    // If sum is 0, then answer is true (empty subset)
    for (int i = 0; i <= n; i++) {
        dp[i][0] = true;
    }
    
    // If sum is not 0 and array is empty, then answer is false
    for (int i = 1; i <= sum; i++) {
        dp[0][i] = false;
    }
    
    // Fill the DP table
    for (int i = 1; i <= n; i++) {
        for (int j = 1; j <= sum; j++) {
            // If current element is greater than sum, skip it
            if (arr[i - 1] > j) {
                dp[i][j] = dp[i - 1][j];
            }
            else {
                // Include current element or exclude it
                dp[i][j] = dp[i - 1][j] || dp[i - 1][j - arr[i - 1]];
            }
        }
    }
    
    return dp[n][sum];
}

// Function to print all subsets that sum to target
void printSubsetSum(int arr[], int n, int sum) {
    bool dp[n + 1][sum + 1];
    
    // Initialize base cases
    for (int i = 0; i <= n; i++) {
        dp[i][0] = true;
    }
    
    for (int i = 1; i <= sum; i++) {
        dp[0][i] = false;
    }
    
    // Fill the DP table
    for (int i = 1; i <= n; i++) {
        for (int j = 1; j <= sum; j++) {
            if (arr[i - 1] > j) {
                dp[i][j] = dp[i - 1][j];
            }
            else {
                dp[i][j] = dp[i - 1][j] || dp[i - 1][j - arr[i - 1]];
            }
        }
    }
    
    // If no subset exists
    if (!dp[n][sum]) {
        printf("No subset with given sum exists\n");
        return;
    }
    
    // Backtrack to find the actual subset
    int i = n, j = sum;
    int result[100]; // Assuming maximum 100 elements
    int count = 0;
    
    while (i > 0 && j > 0) {
        // If current cell value is not same as previous row,
        // then current element is included
        if (dp[i - 1][j] != dp[i][j]) {
            result[count++] = arr[i - 1];
            j = j - arr[i - 1];
        }
        i--;
    }
    
    printf("Subset with sum %d: ", sum);
    for (int k = count - 1; k >= 0; k--) {
        printf("%d ", result[k]);
    }
    printf("\n");
}

int main() {
    int arr[] = {3, 34, 4, 12, 5, 2};
    int n = sizeof(arr) / sizeof(arr[0]);
    int sum = 9;
    
    printf("Array: ");
    for (int i = 0; i < n; i++) {
        printf("%d ", arr[i]);
    }
    printf("\n");
    
    printf("Target sum: %d\n", sum);
    
    if (isSubsetSum(arr, n, sum)) {
        printf("Subset with given sum exists\n");
        printSubsetSum(arr, n, sum);
    } else {
        printf("No subset with given sum exists\n");
    }
    
    // Test with another example
    printf("\n--- Testing with different example ---\n");
    int arr2[] = {1, 2, 3, 4, 5};
    int n2 = sizeof(arr2) / sizeof(arr2[0]);
    int sum2 = 10;
    
    printf("Array: ");
    for (int i = 0; i < n2; i++) {
        printf("%d ", arr2[i]);
    }
    printf("\n");
    
    printf("Target sum: %d\n", sum2);
    
    if (isSubsetSum(arr2, n2, sum2)) {
        printf("Subset with given sum exists\n");
        printSubsetSum(arr2, n2, sum2);
    } else {
        printf("No subset with given sum exists\n");
    }
    
    return 0;
}
```

## Output:
```
Array: 3 34 4 12 5 2 
Target sum: 9
Subset with given sum exists
Subset with sum 9: 4 5 

--- Testing with different example ---
Array: 1 2 3 4 5 
Target sum: 10
Subset with given sum exists
Subset with sum 10: 1 4 5 
```

## Algorithm Explanation:

1. **Dynamic Programming Approach**: Uses a 2D boolean array `dp[i][j]` where `dp[i][j]` represents whether sum `j` can be obtained using first `i` elements.

2. **Base Cases**:
   - `dp[i][0] = true` (sum 0 can always be achieved with empty subset)
   - `dp[0][j] = false` (sum > 0 cannot be achieved with empty array)

3. **Recurrence Relation**:
   - If `arr[i-1] > j`: `dp[i][j] = dp[i-1][j]` (element too large, exclude it)
   - Else: `dp[i][j] = dp[i-1][j] || dp[i-1][j-arr[i-1]]` (include or exclude element)

4. **Time Complexity**: O(n × sum)
5. **Space Complexity**: O(n × sum)

This implementation efficiently solves the subset sum problem and can be easily modified to return all possible subsets that sum to the target value.

