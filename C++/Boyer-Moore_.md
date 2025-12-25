# Boyer-Moore Algorithm Implementation in C++

```cpp
#include <iostream>
#include <vector>
#include <string>
using namespace std;

class BoyerMoore {
private:
    vector<int> badCharTable;
    int textLength;
    int patternLength;
    
    // Build bad character table
    void buildBadCharTable(const string& pattern) {
        // Initialize all entries as -1
        for (int i = 0; i < 256; i++) {
            badCharTable[i] = -1;
        }
        
        // Fill the actual value of last occurrence of each character
        for (int i = 0; i < patternLength; i++) {
            badCharTable[pattern[i]] = i;
        }
    }
    
public:
    BoyerMoore(const string& pattern) {
        patternLength = pattern.length();
        badCharTable.resize(256);
        buildBadCharTable(pattern);
    }
    
    // Search for pattern in text
    vector<int> search(const string& text, const string& pattern) {
        vector<int> occurrences;
        textLength = text.length();
        
        if (patternLength > textLength) {
            return occurrences; // Pattern longer than text
        }
        
        int shift = 0; // Shift of the pattern with respect to text
        
        while (shift <= (textLength - patternLength)) {
            int j = patternLength - 1;
            
            // Keep reducing index j of pattern while characters of pattern 
            // and text are matching at this shift shift
            while (j >= 0 && pattern[j] == text[shift + j]) {
                j--;
            }
            
            // If pattern is found at current shift
            if (j < 0) {
                occurrences.push_back(shift);
                // Shift pattern so that next character in text aligns with last 
                // occurrence of that character in pattern
                shift += (shift + patternLength < textLength) ? 
                         patternLength - badCharTable[text[shift + patternLength]] : 1;
            } else {
                // Shift pattern so that bad character in text aligns with last 
                // occurrence of that character in pattern
                shift += max(1, j - badCharTable[text[shift + j]]);
            }
        }
        
        return occurrences;
    }
};

// Example usage
int main() {
    string text = "ABAAABCDABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABC

