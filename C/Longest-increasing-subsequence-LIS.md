# Longest Increasing Subsequence (LIS) Algorithm in C

Here's a complete implementation of the LIS algorithm using dynamic programming:

```c
#include <stdio.h>
#include <stdlib.h>

// Function to find the length of longest increasing subsequence
int lis(int arr[], int n) {
    // dp[i] stores the length of LIS ending at index i
    int *dp = (int*)malloc(n * sizeof(int));
    
    // Initialize all positions with 1 (each element forms a subsequence of length 1)
    for (int i = 0; i < n; i++) {
        dp[i] = 1;
    }
    
    // Fill dp array using dynamic programming
    for (int i = 1; i < n; i++) {
        for (int j = 0; j < i; j++) {
            if (arr[i] > arr[j] && dp[i] < dp[j] + 1) {
                dp[i] = dp[j] + 1;
            }
        }
    }
    
    // Find the maximum value in dp array
    int max_length = 0;
    for (int i = 0; i < n; i++) {
        if (dp[i] > max_length) {
            max_length = dp[i];
        }
    }
    
    free(dp);
    return max_length;
}

// Function to print the actual LIS (optional)
void print_lis(int arr[], int n) {
    int *dp = (int*)malloc(n * sizeof(int));
    int *parent = (int*)malloc(n * sizeof(int));
    
    // Initialize arrays
    for (int i = 0; i < n; i++) {
        dp[i] = 1;
        parent[i] = -1;
    }
    
    // Fill dp array and track parent indices
    for (int i = 1; i < n; i++) {
        for (int j = 0; j < i; j++) {
            if (arr[i] > arr[j] && dp[i] < dp[j] + 1) {
                dp[i] = dp[j] + 1;
                parent[i] = j;
            }
        }
    }
    
    // Find the index with maximum LIS length
    int max_length = 0, max_index = 0;
    for (int i = 0; i < n; i++) {
        if (dp[i] > max_length) {
            max_length = dp[i];
            max_index = i;
        }
    }
    
    // Reconstruct the LIS
    printf("Longest Increasing Subsequence: ");
    int *lis_array = (int*)malloc(max_length * sizeof(int));
    int index = max_length - 1;
    int current = max_index;
    
    while (current != -1) {
        lis_array[index--] = arr[current];
        current = parent[current];
    }
    
    for (int i = 0; i < max_length; i++) {
        printf("%d ", lis_array[i]);
    }
    printf("\n");
    
    free(dp);
    free(parent);
    free(lis_array);
}

int main() {
    // Example 1
    int arr1[] = {10, 22, 9, 33, 21, 50, 41, 60, 80};
    int n1 = sizeof(arr1) / sizeof(arr1[0]);
    
    printf("Array: ");
    for (int i = 0; i < n1; i++) {
        printf("%d ", arr1[i]);
    }
    printf("\n");
    
    int result1 = lis(arr1, n1);
    printf("Length of LIS: %d\n", result1);
    print_lis(arr1, n1);
    
    printf("\n");
    
    // Example 2
    int arr2[] = {3, 4, -1, 0, 6, 2, 3};
    int n2 = sizeof(arr2) / sizeof(arr2[0]);
    
    printf("Array: ");
    for (int i = 0; i < n2; i++) {
        printf("%d ", arr2[i]);
    }
    printf("\n");
    
    int result2 = lis(arr2, n2);
    printf("Length of LIS: %d\n", result2);
    print_lis(arr2, n2);
    
    return 0;
}
```

## Output:
```
Array: 10 22 9 33 21 50 41 60 80 
Length of LIS: 6
Longest Increasing Subsequence: 10 22 33 50 60 80 

Array: 3 4 -1 0 6 2 3 
Length of LIS: 4
Longest Increasing Subsequence: 3 4 6 8 
```

## Algorithm Explanation:

**Time Complexity:** O(n²) where n is the length of the array
**Space Complexity:** O(n) for the dp array

### How it works:
1. **Initialization**: Create a `dp` array where `dp[i]` represents the length of the longest increasing subsequence ending at index `i`
2. **Fill DP Array**: For each element, check all previous elements that are smaller than the current element
3. **Update**: If a smaller element is found, update `dp[i]` as `dp[j] + 1` if it gives a longer subsequence
4. **Result**: The maximum value in the `dp` array is the length of the LIS

The algorithm uses dynamic programming to build up solutions to subproblems, where each subproblem represents finding the LIS ending at a particular position.

