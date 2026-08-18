# Knuth-Morris-Pratt (KMP) Algorithm Implementation

## Overview
The KMP algorithm is used for string pattern matching. It preprocesses the pattern to create a "failure function" that helps avoid unnecessary comparisons when a mismatch occurs.

## C++ Implementation

```cpp
#include <iostream>
#include <vector>
#include <string>
using namespace std;

class KMP {
private:
    vector<int> lps; // Longest Proper Prefix which is also Suffix
    
    void computeLPSArray(string pattern) {
        int M = pattern.length();
        lps.resize(M);
        lps[0] = 0; // First element is always 0
        
        int len = 0; // Length of previous longest prefix suffix
        int i = 1;
        
        while (i < M) {
            if (pattern[i] == pattern[len]) {
                len++;
                lps[i] = len;
                i++;
            } else {
                if (len != 0) {
                    // Fall back using previously computed values
                    len = lps[len - 1];
                } else {
                    // No proper prefix suffix found
                    lps[i] = 0;
                    i++;
                }
            }
        }
    }
    
public:
    // Search for pattern in text and return all starting positions
    vector<int> search(string text, string pattern) {
        vector<int> result;
        
        if (pattern.empty()) return result;
        
        int N = text.length();
        int M = pattern.length();
        
        // Preprocess pattern to create LPS array
        computeLPSArray(pattern);
        
        int i = 0; // Index for text
        int j = 0; // Index for pattern
        
        while (i < N) {
            if (pattern[j] == text[i]) {
                i++;
                j++;
            }
            
            if (j == M) {
                // Pattern found at index i-j
                result.push_back(i - j);
                j = lps[j - 1]; // Continue searching for overlapping patterns
            } else if (i < N && pattern[j] != text[i]) {
                if (j != 0) {
                    // Use previously computed LPS value
                    j = lps[j - 1];
                } else {
                    // No match, move to next character in text
                    i++;
                }
            }
        }
        
        return result;
    }
    
    // Simple search function that returns first occurrence
    int searchFirst(string text, string pattern) {
        vector<int> positions = search(text, pattern);
        return positions.empty() ? -1 : positions[0];
    }
};

// Example usage
int main() {
    KMP kmp;
    
    // Test case 1
    string text1 = "ABABDABACDABABCABCABCABCABC";
    string pattern1 = "ABABCABCABCABC";
    
    cout << "Text: " << text1 << endl;
    cout << "Pattern: " << pattern1 << endl;
    
    vector<int> positions = kmp.search(text1, pattern1);
    
    if (positions.empty()) {
        cout << "Pattern not found in text" << endl;
    } else {
        cout << "Pattern found at positions: ";
        for (int pos : positions) {
            cout << pos << " ";
        }
        cout << endl;
    }
    
    cout << "\n--- Another Example ---" << endl;
    
    // Test case 2
    string text2 = "AABAACAADAABAABA";
    string pattern2 = "AABA";
    
    cout << "Text: " << text2 << endl;
    cout << "Pattern: " << pattern2 << endl;
    
    int firstPos = kmp.searchFirst(text2, pattern2);
    if (firstPos != -1) {
        cout << "First occurrence at position: " << firstPos << endl;
    } else {
        cout << "Pattern not found" << endl;
    }
    
    return 0;
}
```

## How it works:

### 1. **LPS Array Construction**
- For each position in the pattern, we compute the length of the longest proper prefix that is also a suffix
- This helps us determine how much to shift the pattern when a mismatch occurs

### 2. **Search Process**
- Compare characters from text and pattern
- When characters match, advance both pointers
- When they don't match:
  - If we're not at the beginning of the pattern, use LPS array to determine where to resume comparison
  - Otherwise, move to next character in text

## Sample Output:
```
Text: ABABDABACDABABCABCABCABCABC
Pattern: ABABCABCABCABC
Pattern found at positions: 10 

--- Another Example ---
Text: AABAACAADAABAABA
Pattern: AABA
First occurrence at position: 0
```

## Time Complexity:
- **Preprocessing**: O(M) where M is the length of pattern
- **Searching**: O(N) where N is the length of text
- **Overall**: O(N + M)

## Space Complexity: O(M) for the LPS array

This implementation handles overlapping patterns and efficiently avoids redundant comparisons, making it more efficient than naive string matching algorithms.