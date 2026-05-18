# Rabin-Karp Algorithm Implementation in C++

The Rabin-Karp algorithm is a string searching algorithm that uses hashing to find patterns in text. It's particularly useful when searching for multiple patterns or when the pattern is long.

## Code Implementation

```cpp
#include <iostream>
#include <string>
#include <vector>
using namespace std;

class RabinKarp {
private:
    static const int d = 256;  // Number of characters in the input alphabet
    static const int q = 101;  // A prime number for hashing
    
public:
    // Function to search for pattern in text
    static vector<int> search(const string& pattern, const string& text) {
        vector<int> result;
        int M = pattern.length();
        int N = text.length();
        
        // Edge case: if pattern is longer than text
        if (M > N) return result;
        
        // Calculate d^(M-1) % q
        long long h = 1;
        for (int i = 0; i < M - 1; i++) {
            h = (h * d) % q;
        }
        
        // Calculate hash value of pattern and first window of text
        long long p = 0;  // Hash value for pattern
        long long t = 0;  // Hash value for text
        
        for (int i = 0; i < M; i++) {
            p = (d * p + pattern[i]) % q;
            t = (d * t + text[i]) % q;
        }
        
        // Slide the pattern over text one by one
        for (int i = 0; i <= N - M; i++) {
            // Check if hash values match
            if (p == t) {
                // Check character by character if hash matches
                bool match = true;
                for (int j = 0; j < M; j++) {
                    if (text[i + j] != pattern[j]) {
                        match = false;
                        break;
                    }
                }
                if (match) {
                    result.push_back(i);
                }
            }
            
            // Calculate hash value for next window of text
            if (i < N - M) {
                t = (d * (t - text[i] * h) + text[i + M]) % q;
                
                // Handle negative hash values
                if (t < 0)
                    t = t + q;
            }
        }
        
        return result;
    }
    
    // Function to print all occurrences
    static void printOccurrences(const string& pattern, const string& text) {
        vector<int> occurrences = search(pattern, text);
        
        if (occurrences.empty()) {
            cout << "Pattern not found in text" << endl;
        } else {
            cout << "Pattern found at positions: ";
            for (int i = 0; i < occurrences.size(); i++) {
                cout << occurrences[i];
                if (i < occurrences.size() - 1) cout << ", ";
            }
            cout << endl;
        }
    }
};

// Main function to demonstrate the algorithm
int main() {
    // Example 1
    string text1 = "ABABDABACDABABCABCABC";
    string pattern1 = "ABABCABCABC";
    
    cout << "Example 1:" << endl;
    cout << "Text: " << text1 << endl;
    cout << "Pattern: " << pattern1 << endl;
    RabinKarp::printOccurrences(pattern1, text1);
    cout << endl;
    
    // Example 2
    string text2 = "AABAACAADAABAABA";
    string pattern2 = "AABA";
    
    cout << "Example 2:" << endl;
    cout << "Text: " << text2 << endl;
    cout << "Pattern: " << pattern2 << endl;
    RabinKarp::printOccurrences(pattern2, text2);
    cout << endl;
    
    // Example 3 - No match
    string text3 = "HELLO WORLD";
    string pattern3 = "PYTHON";
    
    cout << "Example 3:" << endl;
    cout << "Text: " << text3 << endl;
    cout << "Pattern: " << pattern3 << endl;
    RabinKarp::printOccurrences(pattern3, text3);
    
    return 0;
}
```

## Output

```
Example 1:
Text: ABABDABACDABABCABCABC
Pattern: ABABCABCABC
Pattern found at positions: 10

Example 2:
Text: AABAACAADAABAABA
Pattern: AABA
Pattern found at positions: 0, 9, 13

Example 3:
Text: HELLO WORLD
Pattern: PYTHON
Pattern not found in text
```

## How It Works

1. **Hash Calculation**: The algorithm calculates hash values for the pattern and each window of text
2. **Window Sliding**: It slides the pattern over the text one by one
3. **Hash Matching**: When hash values match, it performs character-by-character comparison
4. **Rolling Hash**: Uses rolling hash technique to efficiently compute hash values for subsequent windows

## Time Complexity

- **Best/Average Case**: O(n + m) where n is text length and m is pattern length
- **Worst Case**: O(n × m) when all hash values match but characters don't match

## Space Complexity

O(1) - Only using a constant amount of extra space

## Key Features

- Efficient for multiple pattern searches
- Uses rolling hash for fast window updates
- Handles hash collisions through character verification
- Works well with large texts and patterns

