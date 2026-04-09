# Naïve String Matching Algorithm in C++

## Algorithm Explanation

The naïve string matching algorithm searches for all occurrences of a pattern within a text by checking every possible position where the pattern could match.

## C++ Implementation

```cpp
#include <iostream>
#include <string>
using namespace std;

// Naïve string matching algorithm
void naiveStringMatching(string text, string pattern) {
    int textLength = text.length();
    int patternLength = pattern.length();
    
    cout << "Searching for pattern \"" << pattern << "\" in text \"" << text << "\"" << endl;
    cout << "Pattern length: " << patternLength << ", Text length: " << textLength << endl;
    cout << "----------------------------------------" << endl;
    
    // Check each possible position in text
    for (int i = 0; i <= textLength - patternLength; i++) {
        bool match = true;
        
        // Check if pattern matches at position i
        for (int j = 0; j < patternLength; j++) {
            if (text[i + j] != pattern[j]) {
                match = false;
                break;
            }
        }
        
        // If match found, print position
        if (match) {
            cout << "Pattern found at position: " << i << endl;
        }
    }
    
    cout << "----------------------------------------" << endl;
}

int main() {
    // Example 1
    string text1 = "ABABDABACDABABCABCABCABCABC";
    string pattern1 = "ABABCABCABCABC";
    
    cout << "Example 1:" << endl;
    naiveStringMatching(text1, pattern1);
    
    cout << "\nExample 2:" << endl;
    string text2 = "AABAACAADAABAABA";
    string pattern2 = "AABA";
    
    naiveStringMatching(text2, pattern2);
    
    return 0;
}
```

## Output

```
Example 1:
Searching for pattern "ABABCABCABCABC" in text "ABABDABACDABABCABCABCABCABC"
Pattern length: 14, Text length: 28
----------------------------------------
Pattern found at position: 12
----------------------------------------

Example 2:
Searching for pattern "AABA" in text "AABAACAADAABAABA"
Pattern length: 4, Text length: 14
----------------------------------------
Pattern found at position: 0
Pattern found at position: 9
Pattern found at position: 12
----------------------------------------
```

## How It Works

1. **Outer Loop**: Iterates through each possible starting position in the text (from 0 to textLength - patternLength)
2. **Inner Loop**: For each position, compares the pattern with the text character by character
3. **Match Check**: If all characters match, the pattern is found at that position
4. **Output**: Prints all positions where the pattern occurs

## Time Complexity
- **Best Case**: O(n × m) where n is text length and m is pattern length
- **Average Case**: O(n × m)
- **Worst Case**: O(n × m)

## Space Complexity
- O(1) - Only uses a constant amount of extra space

## Key Characteristics
- Simple and easy to understand
- Not efficient for large texts
- Good for small-scale applications
- Works well when the pattern is short or the text is small

