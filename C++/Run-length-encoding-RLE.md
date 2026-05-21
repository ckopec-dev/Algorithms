# Run-Length Encoding (RLE) in C++

Here's a complete implementation of the Run-Length Encoding algorithm in C++:

```cpp
#include <iostream>
#include <string>
#include <vector>
using namespace std;

class RLE {
public:
    // Encode function - compresses the input string
    static string encode(const string& input) {
        if (input.empty()) return "";
        
        string encoded = "";
        char currentChar = input[0];
        int count = 1;
        
        for (int i = 1; i < input.length(); i++) {
            if (input[i] == currentChar) {
                count++;
            } else {
                // Append count and character to result
                if (count > 1) {
                    encoded += to_string(count);
                }
                encoded += currentChar;
                
                // Reset for new character
                currentChar = input[i];
                count = 1;
            }
        }
        
        // Handle the last sequence
        if (count > 1) {
            encoded += to_string(count);
        }
        encoded += currentChar;
        
        return encoded;
    }
    
    // Decode function - decompresses the RLE string
    static string decode(const string& encoded) {
        string decoded = "";
        int i = 0;
        
        while (i < encoded.length()) {
            // Extract the count (if present)
            int count = 0;
            while (i < encoded.length() && isdigit(encoded[i])) {
                count = count * 10 + (encoded[i] - '0');
                i++;
            }
            
            // If no count was found, count is 1
            if (count == 0) {
                count = 1;
            }
            
            // Get the character to repeat
            if (i < encoded.length()) {
                char ch = encoded[i];
                for (int j = 0; j < count; j++) {
                    decoded += ch;
                }
                i++;
            }
        }
        
        return decoded;
    }
};

// Example usage
int main() {
    // Test cases
    vector<string> testCases = {
        "AAAABBBCCDAA",
        "AAAAAAAAAA",
        "ABC",
        "AABBCC",
        "aabbccdd"
    };
    
    cout << "Run-Length Encoding Example\n";
    cout << "==========================\n\n";
    
    for (const string& test : testCases) {
        string encoded = RLE::encode(test);
        string decoded = RLE::decode(encoded);
        
        cout << "Original:    \"" << test << "\"\n";
        cout << "Encoded:     \"" << encoded << "\"\n";
        cout << "Decoded:     \"" << decoded << "\"\n";
        cout << "Match:       " << (test == decoded ? "YES" : "NO") << "\n\n";
    }
    
    return 0;
}
```

## Output:
```
Run-Length Encoding Example
==========================

Original:    "AAAABBBCCDAA"
Encoded:     "4A3B2CD2A"
Decoded:     "AAAABBBCCDAA"
Match:       YES

Original:    "AAAAAAAAAA"
Encoded:     "10A"
Decoded:     "AAAAAAAAAA"
Match:       YES

Original:    "ABC"
Encoded:     "ABC"
Decoded:     "ABC"
Match:       YES

Original:    "AABBCC"
Encoded:     "2A2B2C"
Decoded:     "AABBCC"
Match:       YES

Original:    "aabbccdd"
Encoded:     "2a2b2c2d"
Decoded:     "aabbccdd"
Match:       YES
```

## How it works:

1. **Encoding Process:**
   - Traverse the input string character by character
   - Count consecutive occurrences of the same character
   - If count > 1, append the count followed by the character
   - If count = 1, just append the character

2. **Decoding Process:**
   - Parse the encoded string character by character
   - Extract numbers (counts) when encountered
   - Repeat the following character the specified number of times
   - Handle single characters (count = 1) appropriately

## Time and Space Complexity:
- **Time Complexity:** O(n) where n is the length of the input string
- **Space Complexity:** O(n) for storing the encoded/decoded result

This implementation handles edge cases like empty strings, single characters, and sequences with counts greater than 9.

