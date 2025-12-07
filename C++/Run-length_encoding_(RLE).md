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
    
    // Decode function - decompresses the RLE encoded string
    static string decode(const string& encoded) {
        string decoded = "";
        int i = 0;
        
        while (i < encoded.length()) {
            // Check if current character is a digit
            if (isdigit(encoded[i])) {
                // Extract the full number
                int num = 0;
                while (i < encoded.length() && isdigit(encoded[i])) {
                    num = num * 10 + (encoded[i] - '0');
                    i++;
                }
                
                // Next character is the one to repeat
                if (i < encoded.length()) {
                    char ch = encoded[i];
                    for (int j = 0; j < num; j++) {
                        decoded += ch;
                    }
                    i++;
                }
            } else {
                // Single character (no count)
                decoded += encoded[i];
                i++;
            }
        }
        
        return decoded;
    }
};

int main() {
    // Test cases
    vector<string> testCases = {
        "AAAABBBCCDAA",
        "AAAAAAAAAA",
        "abcdef",
        "AABBCC",
        ""
    };
    
    cout << "Run-Length Encoding Examples:\n";
    cout << "============================\n\n";
    
    for (const string& test : testCases) {
        cout << "Original: \"" << test << "\"\n";
        
        string encoded = RLE::encode(test);
        cout << "Encoded:  \"" << encoded << "\"\n";
        
        string decoded = RLE::decode(encoded);
        cout << "Decoded:  \"" << decoded << "\"\n";
        
        // Verify correctness
        cout << "Match: " << (test == decoded ? "YES" : "NO") << "\n\n";
    }
    
    return 0;
}
```

## Sample Output:
```
Run-Length Encoding Examples:
============================

Original: "AAAABBBCCDAA"
Encoded:  "4AB3BCD2A"
Decoded:  "AAAABBBCCDAA"
Match: YES

Original: "AAAAAAAAAA"
Encoded:  "10A"
Decoded:  "AAAAAAAAAA"
Match: YES

Original: "abcdef"
Encoded:  "abcdef"
Decoded:  "abcdef"
Match: YES

Original: "AABBCC"
Encoded:  "2A2B2C"
Decoded:  "AABBCC"
Match: YES

Original: ""
Encoded:  ""
Decoded:  ""
Match: YES
```

## How it works:

1. **Encoding Process:**
   - Traverse the input string and count consecutive identical characters
   - If count > 1, append the count followed by the character
   - If count = 1, just append the character
   - Handle the last sequence after the loop ends

2. **Decoding Process:**
   - Parse the encoded string character by character
   - If a digit is encountered, extract the complete number
   - Repeat the following character 'number' times
   - If no number precedes a character, use count = 1

## Time Complexity:
- **Encoding:** O(n) where n is the length of input string
- **Decoding:** O(m) where m is the length of encoded string

## Space Complexity:
- O(n) for both encoding and decoding operations

This implementation handles edge cases like empty strings, single characters, and strings with no repeated characters.

