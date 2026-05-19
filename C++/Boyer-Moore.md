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
    
    // Preprocessing: Create bad character table
    void buildBadCharTable(const string& pattern) {
        // Initialize all entries as -1 (not found)
        for (int i = 0; i < 256; i++) {
            badCharTable[i] = -1;
        }
        
        // Fill the bad character table with last occurrence positions
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
            int j = patternLength - 1; // Start from the end of pattern
            
            // Keep reducing index j of pattern while characters of 
            // pattern and text are matching at this shift
            while (j >= 0 && text[shift + j] == pattern[j]) {
                j--;
            }
            
            // If pattern is found at current shift
            if (j < 0) {
                occurrences.push_back(shift);
                // Shift pattern so that next character in text aligns with last 
                // character of pattern
                shift += (shift + patternLength < textLength) ? 
                         patternLength - badCharTable[text[shift + patternLength]] : 1;
            } else {
                // Shift pattern so that bad character in text aligns with 
                // last occurrence of that character in pattern
                shift += max(1, j - badCharTable[text[shift + j]]);
            }
        }
        
        return occurrences;
    }
};

// Example usage
int main() {
    string text = "ABAAABCDABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABC

