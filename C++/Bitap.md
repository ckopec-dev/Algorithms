# Bitap Algorithm Implementation in C++

The Bitap algorithm is a string searching algorithm that uses bitwise operations to find occurrences of a pattern in a text. It's particularly efficient for small patterns.

## C++ Implementation

```cpp
#include <iostream>
#include <string>
#include <vector>
#include <bitset>

class BitapSearch {
private:
    std::string pattern;
    std::string text;
    int patternLength;
    int textLength;
    
    // Create the bitap table for the pattern
    std::vector<int> createBitapTable() {
        std::vector<int> table(256, 0); // Assuming ASCII characters
        
        for (int i = 0; i < patternLength; i++) {
            table[pattern[i]] |= (1 << i);
        }
        
        return table;
    }
    
public:
    BitapSearch(const std::string& pat, const std::string& txt) 
        : pattern(pat), text(txt), patternLength(pat.length()), textLength(txt.length()) {}
    
    // Bitap search algorithm
    std::vector<int> search() {
        std::vector<int> matches;
        
        if (patternLength == 0 || textLength == 0) {
            return matches;
        }
        
        // Create bitap table
        std::vector<int> bitapTable = createBitapTable();
        
        // Initialize the state
        int state = 0;
        int result = (1 << (patternLength - 1)); // The last bit position
        
        // Process each character in the text
        for (int i = 0; i < textLength; i++) {
            // Update the state using bitwise operations
            state = ((state << 1) | 1) & bitapTable[text[i]];
            
            // Check if we found a match
            if (state & result) {
                matches.push_back(i - patternLength + 1);
            }
        }
        
        return matches;
    }
    
    // Print all matches
    void printMatches() {
        std::vector<int> matches = search();
        
        if (matches.empty()) {
            std::cout << "No matches found." << std::endl;
        } else {
            std::cout << "Matches found at positions: ";
            for (size_t i = 0; i < matches.size(); i++) {
                std::cout << matches[i];
                if (i < matches.size() - 1) {
                    std::cout << ", ";
                }
            }
            std::cout << std::endl;
        }
    }
};

// Alternative implementation with error tolerance (k-mismatches)
class BitapSearchWithErrors {
private:
    std::string pattern;
    std::string text;
    int patternLength;
    int textLength;
    int maxErrors;
    
public:
    BitapSearchWithErrors(const std::string& pat, const std::string& txt, int maxErr = 0) 
        : pattern(pat), text(txt), patternLength(pat.length()), textLength(txt.length()), maxErrors(maxErr) {}
    
    std::vector<int> search() {
        std::vector<int> matches;
        
        if (patternLength == 0 || textLength == 0) {
            return matches;
        }
        
        // Create bitap table
        std::vector<int> bitapTable(256, 0);
        for (int i = 0; i < patternLength; i++) {
            bitapTable[pattern[i]] |= (1 << i);
        }
        
        // Initialize state array
        std::vector<int> states(maxErrors + 1, 0);
        int result = (1 << (patternLength - 1));
        
        for (int i = 0; i < textLength; i++) {
            // Update states
            for (int j = maxErrors; j >= 0; j--) {
                if (j == 0) {
                    states[j] = ((states[j] << 1) | 1) & bitapTable[text[i]];
                } else {
                    states[j] = ((states[j] << 1) | 1) & bitapTable[text[i]] | 
                               ((states[j-1] << 1) | 1) & bitapTable[text[i]];
                }
                
                // Check for match
                if (j == 0 && (states[j] & result)) {
                    matches.push_back(i - patternLength + 1);
                }
            }
        }
        
        return matches;
    }
};

int main() {
    // Example 1: Basic Bitap search
    std::cout << "=== Basic Bitap Search ===" << std::endl;
    std::string text1 = "This is a simple text for testing Bitap algorithm";
    std::string pattern1 = "Bitap";
    
    BitapSearch searcher1(pattern1, text1);
    std::cout << "Text: " << text1 << std::endl;
    std::cout << "Pattern: " << pattern1 << std::endl;
    searcher1.printMatches();
    
    std::cout << "\n=== Another Example ===" << std::endl;
    std::string text2 = "ABABDABACDABABCABCABCABCABC";
    std::string pattern2 = "ABCABC";
    
    BitapSearch searcher2(pattern2, text2);
    std::cout << "Text: " << text2 << std::endl;
    std::cout << "Pattern: " << pattern2 << std::endl;
    searcher2.printMatches();
    
    // Example 3: Show bit operations
    std::cout << "\n=== Bit Operations Demo ===" << std::endl;
    std::string demoPattern = "abc";
    std::string demoText = "ababcabc";
    
    std::cout << "Pattern: " << demoPattern << std::endl;
    std::cout << "Text: " << demoText << std::endl;
    
    // Manual bit table creation
    std::vector<int> bitTable(256, 0);
    for (int i = 0; i < demoPattern.length(); i++) {
        bitTable[demoPattern[i]] |= (1 << i);
    }
    
    std::cout << "Bit table for pattern '" << demoPattern << "':" << std::endl;
    for (int i = 0; i < 256; i++) {
        if (bitTable[i] != 0) {
            std::cout << "Character '" << (char)i << "': " << std::bitset<8>(bitTable[i]) << std::endl;
        }
    }
    
    return 0;
}
```

## How It Works

### Key Concepts:

1. **Bit Table Creation**: For each character in the pattern, we create a bitmask representing its positions
2. **State Tracking**: We maintain a state that represents how much of the pattern we've matched
3. **Bitwise Operations**: Use left shifts and bitwise AND/OR operations to efficiently update matching states

### Algorithm Steps:

1. Create a bit table mapping each character to its pattern positions
2. Initialize the state to 0
3. For each character in text:
   - Update state using bitwise operations
   - Check if pattern is fully matched
   - Record match position if found

### Time Complexity:
- **Best Case**: O(n) where n is text length
- **Average Case**: O(n) 
- **Worst Case**: O(nm) where m is pattern length

### Space Complexity:
- O(k) where k is the size of character set (typically 256 for ASCII)

This implementation efficiently finds all occurrences of a pattern in text using bitwise operations, making it particularly fast for small patterns and large texts.

