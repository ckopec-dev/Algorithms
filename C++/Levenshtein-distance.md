# Levenshtein Distance Algorithm in C++

The Levenshtein distance is a string metric that measures the difference between two sequences. It's the minimum number of single-character edits (insertions, deletions, or substitutions) required to change one word into another.

## C++ Implementation

```cpp
#include <iostream>
#include <vector>
#include <algorithm>
#include <string>

class LevenshteinDistance {
public:
    static int calculate(const std::string& str1, const std::string& str2) {
        int m = str1.length();
        int n = str2.length();
        
        // Create a 2D DP table
        std::vector<std::vector<int>> dp(m + 1, std::vector<int>(n + 1, 0));
        
        // Initialize base cases
        for (int i = 0; i <= m; i++) {
            dp[i][0] = i;
        }
        
        for (int j = 0; j <= n; j++) {
            dp[0][j] = j;
        }
        
        // Fill the DP table
        for (int i = 1; i <= m; i++) {
            for (int j = 1; j <= n; j++) {
                if (str1[i - 1] == str2[j - 1]) {
                    // Characters match, no operation needed
                    dp[i][j] = dp[i - 1][j - 1];
                } else {
                    // Take minimum of three operations
                    dp[i][j] = 1 + std::min({
                        dp[i - 1][j],     // deletion
                        dp[i][j - 1],     // insertion
                        dp[i - 1][j - 1]  // substitution
                    });
                }
            }
        }
        
        return dp[m][n];
    }
    
    // Alternative implementation with space optimization
    static int calculateOptimized(const std::string& str1, const std::string& str2) {
        int m = str1.length();
        int n = str2.length();
        
        // Use only two rows instead of full matrix
        std::vector<int> prev(n + 1, 0);
        std::vector<int> curr(n + 1, 0);
        
        // Initialize first row
        for (int j = 0; j <= n; j++) {
            prev[j] = j;
        }
        
        // Fill the matrix row by row
        for (int i = 1; i <= m; i++) {
            curr[0] = i;
            
            for (int j = 1; j <= n; j++) {
                if (str1[i - 1] == str2[j - 1]) {
                    curr[j] = prev[j - 1];
                } else {
                    curr[j] = 1 + std::min({
                        prev[j],      // deletion
                        curr[j - 1],  // insertion
                        prev[j - 1]   // substitution
                    });
                }
            }
            
            // Swap vectors
            prev = curr;
        }
        
        return prev[n];
    }
};

// Example usage
int main() {
    // Test cases
    std::vector<std::pair<std::string, std::string>> testCases = {
        {"kitten", "sitting"},
        {"saturday", "sunday"},
        {"hello", "world"},
        {"", "abc"},
        {"abc", ""},
        {"same", "same"}
    };
    
    std::cout << "Levenshtein Distance Examples:\n";
    std::cout << "================================\n";
    
    for (const auto& testCase : testCases) {
        const std::string& str1 = testCase.first;
        const std::string& str2 = testCase.second;
        
        int distance = LevenshteinDistance::calculate(str1, str2);
        int distanceOpt = LevenshteinDistance::calculateOptimized(str1, str2);
        
        std::cout << "String 1: \"" << str1 << "\"\n";
        std::cout << "String 2: \"" << str2 << "\"\n";
        std::cout << "Levenshtein Distance: " << distance << "\n";
        std::cout << "Optimized version: " << distanceOpt << "\n";
        std::cout << "------------------------\n";
    }
    
    // Detailed example with step-by-step explanation
    std::cout << "\nDetailed Example: kitten -> sitting\n";
    std::cout << "=====================================\n";
    std::cout << "kitten → sitten (substitute 'k' with 's')\n";
    std::cout << "sitten → sittin (delete 'e')\n";
    std::cout << "sittin → sitting (insert 'g')\n";
    std::cout << "Total operations: 3\n";
    
    return 0;
}
```

## Output Example

```
Levenshtein Distance Examples:
================================
String 1: "kitten"
String 2: "sitting"
Levenshtein Distance: 3
Optimized version: 3
------------------------
String 1: "saturday"
String 2: "sunday"
Levenshtein Distance: 3
Optimized version: 3
------------------------
String 1: "hello"
String 2: "world"
Levenshtein Distance: 4
Optimized version: 4
------------------------
String 1: ""
String 2: "abc"
Levenshtein Distance: 3
Optimized version: 3
------------------------
String 1: "abc"
String 2: ""
Levenshtein Distance: 3
Optimized version: 3
------------------------
String 1: "same"
String 2: "same"
Levenshtein Distance: 0
Optimized version: 0
------------------------

Detailed Example: kitten -> sitting
=====================================
kitten → sitten (substitute 'k' with 's')
sitten → sittin (delete 'e')
sittin → sitting (insert 'g')
Total operations: 3
```

## Algorithm Explanation

1. **Dynamic Programming Approach**: Uses a 2D table where `dp[i][j]` represents the minimum edit distance between the first `i` characters of `str1` and the first `j` characters of `str2`.

2. **Base Cases**: 
   - `dp[i][0] = i` (deleting all characters from str1)
   - `dp[0][j] = j` (inserting all characters to get str2)

3. **Recurrence Relation**:
   - If characters match: `dp[i][j] = dp[i-1][j-1]`
   - If characters don't match: `dp[i][j] = 1 + min(dp[i-1][j], dp[i][j-1], dp[i-1][j-1])`

4. **Time Complexity**: O(m × n) where m and n are the lengths of the strings
5. **Space Complexity**: O(m × n) for basic version, O(min(m,n)) for optimized version

This implementation provides both a standard and space-optimized version of the Levenshtein distance algorithm.

