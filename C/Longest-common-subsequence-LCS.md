# Longest Common Subsequence (LCS) Algorithm in C

Here's a complete implementation of the LCS algorithm using dynamic programming:

```c
#include <stdio.h>
#include <string.h>

// Function to find the length of LCS
int lcs_length(char *X, char *Y) {
    int m = strlen(X);
    int n = strlen(Y);
    
    // Create a 2D array to store lengths of LCS
    int dp[m + 1][n + 1];
    
    // Initialize the first row and column with 0
    for (int i = 0; i <= m; i++) {
        dp[i][0] = 0;
    }
    for (int j = 0; j <= n; j++) {
        dp[0][j] = 0;
    }
    
    // Fill the dp table
    for (int i = 1; i <= m; i++) {
        for (int j = 1; j <= n; j++) {
            if (X[i - 1] == Y[j - 1]) {
                dp[i][j] = dp[i - 1][j - 1] + 1;
            } else {
                dp[i][j] = (dp[i - 1][j] > dp[i][j - 1]) ? 
                           dp[i - 1][j] : dp[i][j - 1];
            }
        }
    }
    
    return dp[m][n];
}

// Function to find and print the actual LCS
void print_lcs(char *X, char *Y) {
    int m = strlen(X);
    int n = strlen(Y);
    
    // Create a 2D array to store lengths of LCS
    int dp[m + 1][n + 1];
    
    // Initialize the first row and column with 0
    for (int i = 0; i <= m; i++) {
        dp[i][0] = 0;
    }
    for (int j = 0; j <= n; j++) {
        dp[0][j] = 0;
    }
    
    // Fill the dp table
    for (int i = 1; i <= m; i++) {
        for (int j = 1; j <= n; j++) {
            if (X[i - 1] == Y[j - 1]) {
                dp[i][j] = dp[i - 1][j - 1] + 1;
            } else {
                dp[i][j] = (dp[i - 1][j] > dp[i][j - 1]) ? 
                           dp[i - 1][j] : dp[i][j - 1];
            }
        }
    }
    
    // Backtrack to find the actual LCS
    int index = dp[m][n];
    char lcs[index + 1];
    lcs[index] = '\0';
    
    int i = m, j = n;
    while (i > 0 && j > 0) {
        if (X[i - 1] == Y[j - 1]) {
            lcs[index - 1] = X[i - 1];
            i--;
            j--;
            index--;
        } else if (dp[i - 1][j] > dp[i][j - 1]) {
            i--;
        } else {
            j--;
        }
    }
    
    printf("LCS: %s\n", lcs);
}

int main() {
    char X[] = "AGGTAB";
    char Y[] = "GXTXAYB";
    
    printf("String X: %s\n", X);
    printf("String Y: %s\n", Y);
    
    int length = lcs_length(X, Y);
    printf("Length of LCS: %d\n", length);
    
    print_lcs(X, Y);
    
    // Test with another example
    printf("\n--- Another Example ---\n");
    char X2[] = "ABCDGH";
    char Y2[] = "AEDFHR";
    
    printf("String X: %s\n", X2);
    printf("String Y: %s\n", Y2);
    
    int length2 = lcs_length(X2, Y2);
    printf("Length of LCS: %d\n", length2);
    
    print_lcs(X2, Y2);
    
    return 0;
}
```

## Output:
```
String X: AGGTAB
String Y: GXTXAYB
Length of LCS: 4
LCS: GTAB

--- Another Example ---
String X: ABCDGH
String Y: AEDFHR
Length of LCS: 3
LCS: ADH
```

## Algorithm Explanation:

1. **Dynamic Programming Approach**: Uses a 2D table `dp[i][j]` to store the length of LCS for substrings `X[0...i-1]` and `Y[0...j-1]`.

2. **Recurrence Relation**:
   - If `X[i-1] == Y[j-1]`: `dp[i][j] = dp[i-1][j-1] + 1`
   - Otherwise: `dp[i][j] = max(dp[i-1][j], dp[i][j-1])`

3. **Time Complexity**: O(m × n) where m and n are the lengths of the two strings.

4. **Space Complexity**: O(m × n) for the DP table.

The algorithm first computes the length of the LCS, then backtracks through the DP table to reconstruct the actual subsequence.