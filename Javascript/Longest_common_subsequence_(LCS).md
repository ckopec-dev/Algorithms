# Longest Common Subsequence (LCS) Algorithm in JavaScript

```javascript
function lcsLength(str1, str2) {
    const m = str1.length;
    const n = str2.length;
    
    // Create a 2D array to store lengths of LCS
    const dp = Array(m + 1).fill().map(() => Array(n + 1).fill(0));
    
    // Fill the dp table
    for (let i = 1; i <= m; i++) {
        for (let j = 1; j <= n; j++) {
            if (str1[i - 1] === str2[j - 1]) {
                dp[i][j] = dp[i - 1][j - 1] + 1;
            } else {
                dp[i][j] = Math.max(dp[i - 1][j], dp[i][j - 1]);
            }
        }
    }
    
    return dp[m][n];
}

function lcsString(str1, str2) {
    const m = str1.length;
    const n = str2.length;
    
    // Create a 2D array to store lengths of LCS
    const dp = Array(m + 1).fill().map(() => Array(n + 1).fill(0));
    
    // Fill the dp table
    for (let i = 1; i <= m; i++) {
        for (let j = 1; j <= n; j++) {
            if (str1[i - 1] === str2[j - 1]) {
                dp[i][j] = dp[i - 1][j - 1] + 1;
            } else {
                dp[i][j] = Math.max(dp[i - 1][j], dp[i][j - 1]);
            }
        }
    }
    
    // Backtrack to find the actual LCS string
    let lcs = '';
    let i = m;
    let j = n;
    
    while (i > 0 && j > 0) {
        if (str1[i - 1] === str2[j - 1]) {
            lcs = str1[i - 1] + lcs;
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

// Example usage
const string1 = "ABCDGH";
const string2 = "AEDFHR";

console.log("String 1:", string1);
console.log("String 2:", string2);
console.log("Length of LCS:", lcsLength(string1, string2));
console.log("LCS String:", lcsString(string1, string2));

// Another example
const str1 = "AGGTAB";
const str2 = "GXTXAYB";

console.log("\nString 1:", str1);
console.log("String 2:", str2);
console.log("Length of LCS:", lcsLength(str1, str2));
console.log("LCS String:", lcsString(str1, str2));

// Test with empty strings
console.log("\nEmpty strings:");
console.log("Length of LCS:", lcsLength("", ""));
console.log("LCS String:", lcsString("", ""));
```

## Output:
```
String 1: ABCDGH
String 2: AEDFHR
Length of LCS: 3
LCS String: ADH

String 1: AGGTAB
String 2: GXTXAYB
Length of LCS: 4
LCS String: GTAB

Empty strings:
Length of LCS: 0
LCS String: 
```

## How it works:

1. **Dynamic Programming Approach**: Uses a 2D table `dp[i][j]` to store the length of LCS for substrings `str1[0...i-1]` and `str2[0...j-1]`

2. **Recurrence Relation**:
   - If `str1[i-1] === str2[j-1]`: `dp[i][j] = dp[i-1][j-1] + 1`
   - Otherwise: `dp[i][j] = max(dp[i-1][j], dp[i][j-1])`

3. **Backtracking**: To reconstruct the actual LCS string, we trace back through the dp table from `dp[m][n]` to `dp[0][0]`

## Time Complexity: O(m × n)
## Space Complexity: O(m × n)

Where m and n are the lengths of the two input strings.

