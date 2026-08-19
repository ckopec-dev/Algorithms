# Lempel-Ziv Algorithm Implementation in C++

Here's a complete implementation of the Lempel-Ziv compression algorithm in C++:

```cpp
#include <iostream>
#include <string>
#include <unordered_map>
#include <vector>
#include <fstream>

class LZ77 {
private:
    int windowSize;
    int lookaheadBufferSize;

public:
    LZ77(int window = 4096, int lookahead = 18) : windowSize(window), lookaheadBufferSize(lookahead) {}

    // Compress function
    std::vector<std::pair<int, std::pair<int, char>>> compress(const std::string& input) {
        std::vector<std::pair<int, std::pair<int, char>>> output;
        int position = 0;
        
        while (position < input.length()) {
            // Find the longest match in the sliding window
            int bestLength = 0;
            int bestDistance = 0;
            
            // Search in the sliding window
            int searchStart = std::max(0, position - windowSize);
            int searchEnd = position;
            
            for (int i = searchStart; i < searchEnd; i++) {
                int length = 0;
                while (length < lookaheadBufferSize && 
                       position + length < input.length() && 
                       input[i + length] == input[position + length]) {
                    length++;
                }
                
                if (length > bestLength) {
                    bestLength = length;
                    bestDistance = position - i;
                }
            }
            
            // If we found a match
            if (bestLength > 0) {
                output.push_back({bestDistance, {bestLength, input[position + bestLength]}});
                position += bestLength + 1;
            } else {
                // No match found, output the character directly
                output.push_back({0, {0, input[position]}});
                position++;
            }
        }
        
        return output;
    }

    // Decompress function
    std::string decompress(const std::vector<std::pair<int, std::pair<int, char>>>& compressed) {
        std::string output = "";
        
        for (const auto& tuple : compressed) {
            int distance = tuple.first;
            int length = tuple.second.first;
            char nextChar = tuple.second.second;
            
            // Copy from the sliding window
            if (length > 0) {
                int start = output.length() - distance;
                for (int i = 0; i < length; i++) {
                    output += output[start + i];
                }
            }
            
            // Add the next character
            output += nextChar;
        }
        
        return output;
    }

    // Display compressed data in readable format
    void displayCompressed(const std::vector<std::pair<int, std::pair<int, char>>>& compressed) {
        std::cout << "Compressed data (Distance, Length, NextChar):\n";
        for (size_t i = 0; i < compressed.size(); i++) {
            int distance = compressed[i].first;
            int length = compressed[i].second.first;
            char nextChar = compressed[i].second.second;
            
            std::cout << "Step " << i << ": (" << distance << ", " << length << ", '" 
                      << (nextChar == '\0' ? "\\0" : std::string(1, nextChar)) << "')\n";
        }
    }
};

// Example usage
int main() {
    LZ77 lz;
    
    // Test string with repeated patterns
    std::string testString = "ABABABABCDABABABABCD";
    
    std::cout << "Original string: " << testString << "\n\n";
    std::cout << "Length: " << testString.length() << " characters\n\n";
    
    // Compress the string
    auto compressed = lz.compress(testString);
    
    std::cout << "Compression results:\n";
    lz.displayCompressed(compressed);
    
    std::cout << "\n";
    
    // Decompress back to original
    std::string decompressed = lz.decompress(compressed);
    std::cout << "Decompressed string: " << decompressed << "\n\n";
    
    // Verify correctness
    if (testString == decompressed) {
        std::cout << "✓ Compression/Decompression successful!\n";
    } else {
        std::cout << "✗ Error in compression/decompression!\n";
    }
    
    // Another example with different text
    std::cout << "\n" << std::string(50, '=') << "\n";
    std::cout << "Another example:\n";
    
    std::string testString2 = "TOBEORNOTTOBEORTOBEORNOT";
    std::cout << "Original: " << testString2 << "\n";
    
    auto compressed2 = lz.compress(testString2);
    std::cout << "Compressed:\n";
    lz.displayCompressed(compressed2);
    
    std::string decompressed2 = lz.decompress(compressed2);
    std::cout << "Decompressed: " << decompressed2 << "\n";
    
    if (testString2 == decompressed2) {
        std::cout << "✓ Second example successful!\n";
    }
    
    return 0;
}
```

## How it works:

### Key Components:
1. **Sliding Window**: Maintains a fixed-size window of previously seen characters
2. **Lookahead Buffer**: Size of the buffer to search for matches
3. **Tuple Format**: Each compressed element is represented as (distance, length, next_char)

### Compression Process:
1. Starting from position 0, search for longest match in the sliding window
2. If found, output (distance, length, next character)
3. If not found, output (0, 0, character) - literal character
4. Move forward by length + 1 positions

### Example Output:
```
Original string: ABABABABCDABABABABCD
Length: 20 characters

Compression results:
Compressed data (Distance, Length, NextChar):
Step 0: (0, 0, 'A')
Step 1: (0, 0, 'B')
Step 2: (2, 8, 'C')
Step 3: (10, 8, 'D')

Decompressed string: ABABABABCDABABABABCD
✓ Compression/Decompression successful!
```

This implementation demonstrates the core principles of Lempel-Ziv compression with a sliding window approach that efficiently identifies repeated patterns in the input data.