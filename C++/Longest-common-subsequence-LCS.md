# Longest Common Subsequence (LCS) Algorithm in C++

## Problem Description
Given two sequences, find the length of the longest subsequence present in both of them. A subsequence is a sequence that appears in the same relative order, but not necessarily contiguous.

## C++ Implementation

```cpp
#include <iostream>
#include <vector>
#include <algorithm>
using namespace std;

// Function to find the length of LCS
int lcsLength(string X, string Y) {
    int m = X.length();
    int n = Y.length();
    
    // Create a 2D DP table
    vector<vector<int>> dp(m + 1, vector<int>(n + 1, 0));
    
    // Fill the DP table
    for (int i = 1; i <= m; i++) {
        for (int j = 1; j <= n; j++) {
            if (X[i - 1] == Y[j - 1]) {
                dp[i][j] = dp[i - 1][j - 1] + 1;
            } else {
                dp[i][j] = max(dp[i - 1][j], dp[i][j - 1]);
            }
        }
    }
    
    return dp[m][n];
}

// Function to find the actual LCS string
string lcsString(string X, string Y) {
    int m = X.length();
    int n = Y.length();
    
    // Create DP table
    vector<vector<int>> dp(m + 1, vector<int>(n + 1, 0));
    
    // Fill the DP table
    for (int i = 1; i <= m; i++) {
        for (int j = 1; j <= n; j++) {
            if (X[i - 1] == Y[j - 1]) {
                dp[i][j] = dp[i - 1][j - 1] + 1;
            } else {
                dp[i][j] = max(dp[i - 1][j], dp[i][j - 1]);
            }
        }
    }
    
    // Reconstruct the LCS string
    string lcs = "";
    int i = m, j = n;
    
    while (i > 0 && j > 0) {
        if (X[i - 1] == Y[j - 1]) {
            lcs = X[i - 1] + lcs;  // Prepend character
            i--;
            j--;
        } else if (dp[i - 1][j] > dp[i][j - 1]) {
            i--;
        } else {
            j--;
        }
    }
    
    return lcs;
}

// Main function to demonstrate LCS
int main() {
    string X = "ABCDGH";
    string Y = "AEDFHR";
    
    cout << "String 1: " << X << endl;
    cout << "String 2: " << Y << endl;
    
    int length = lcsLength(X, Y);
    cout << "Length of LCS: " << length << endl;
    
    string lcs = lcsString(X, Y);
    cout << "LCS String: " << lcs << endl;
    
    // Another example
    cout << "\n--- Another Example ---" << endl;
    string str1 = "AGGTAB";
    string str2 = "GXTXAYB";
    
    cout << "String 1: " << str1 << endl;
    cout << "String 2: " << str2 << endl;
    
    int length2 = lcsLength(str1, str2);
    cout << "Length of LCS: " << length2 << endl;
    
    string lcs2 = lcsString(str1, str2);
    cout << "LCS String: " << lcs2 << endl;
    
    return 0;
}
```

## Output
```
String 1: ABCDGH
String 2: AEDFHR
Length of LCS: 3
LCS String: ADH

--- Another Example ---
String 1: AGGTAB
String 2: GXTXAYB
Length of LCS: 4
LCS String: GTAB
```

## Algorithm Explanation

### Approach: Dynamic Programming
1. **Create a DP table** of size (m+1) × (n+1) where m and n are lengths of the two strings
2. **Fill the table** using the recurrence relation:
   - If characters match: `dp[i][j] = dp[i-1][j-1] + 1`
   - If characters don't match: `dp[i][j] = max(dp[i-1][j], dp[i][j-1])`
3. **Return** the value at `dp[m][n]` for the length
4. **Reconstruct** the actual LCS by backtracking through the DP table

### Time Complexity: O(m × n)
### Space Complexity: O(m × n)

## Key Points
- The algorithm uses dynamic programming to avoid recomputing overlapping subproblems
- The space complexity can be optimized to O(min(m,n)) if only the length is needed
- This is a classic dynamic programming problem with applications in bioinformatics, file comparison, and more