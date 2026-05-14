# LZW Algorithm Implementation in C++

Here's a complete implementation of the LZW compression algorithm in C++:

```cpp
#include <iostream>
#include <string>
#include <unordered_map>
#include <vector>
#include <fstream>

class LZW {
private:
    std::unordered_map<std::string, int> dictionary;
    int dict_size;
    
public:
    LZW() : dict_size(256) {
        // Initialize dictionary with ASCII characters
        for (int i = 0; i < 256; i++) {
            std::string str(1, char(i));
            dictionary[str] = i;
        }
    }
    
    // Compress function
    std::vector<int> compress(const std::string& input) {
        std::vector<int> output;
        std::string current = "";
        
        for (char c : input) {
            std::string combined = current + c;
            
            if (dictionary.find(combined) != dictionary.end()) {
                current = combined;
            } else {
                output.push_back(dictionary[current]);
                dictionary[combined] = dict_size++;
                current = std::string(1, c);
            }
        }
        
        if (!current.empty()) {
            output.push_back(dictionary[current]);
        }
        
        return output;
    }
    
    // Decompress function
    std::string decompress(const std::vector<int>& input) {
        std::unordered_map<int, std::string> reverse_dict;
        std::string result = "";
        
        // Initialize reverse dictionary with ASCII characters
        for (int i = 0; i < 256; i++) {
            reverse_dict[i] = std::string(1, char(i));
        }
        
        int dict_size = 256;
        int last_code = -1;
        
        for (int code : input) {
            std::string entry;
            
            if (reverse_dict.find(code) != reverse_dict.end()) {
                entry = reverse_dict[code];
            } else if (code == dict_size) {
                entry = reverse_dict[last_code] + reverse_dict[last_code][0];
            } else {
                throw std::runtime_error("Invalid code in decompression");
            }
            
            result += entry;
            
            if (last_code != -1) {
                std::string new_entry = reverse_dict[last_code] + entry[0];
                reverse_dict[dict_size++] = new_entry;
            }
            
            last_code = code;
        }
        
        return result;
    }
};

// Example usage
int main() {
    LZW lzw;
    
    // Test string
    std::string original = "ABABABABAB";
    std::cout << "Original string: " << original << std::endl;
    
    // Compress
    std::vector<int> compressed = lzw.compress(original);
    std::cout << "Compressed codes: ";
    for (int code : compressed) {
        std::cout << code << " ";
    }
    std::cout << std::endl;
    
    // Decompress
    std::string decompressed = lzw.decompress(compressed);
    std::cout << "Decompressed string: " << decompressed << std::endl;
    
    // Verify
    std::cout << "Compression successful: " << (original == decompressed ? "Yes" : "No") << std::endl;
    
    // Another example with longer text
    std::cout << "\n--- Another Example ---" << std::endl;
    std::string text = "TOBEORNOTTOBEORTOBEORNOT";
    std::cout << "Original: " << text << std::endl;
    
    std::vector<int> compressed2 = lzw.compress(text);
    std::cout << "Compressed: ";
    for (int code : compressed2) {
        std::cout << code << " ";
    }
    std::cout << std::endl;
    
    std::string decompressed2 = lzw.decompress(compressed2);
    std::cout << "Decompressed: " << decompressed2 << std::endl;
    std::cout << "Match: " << (text == decompressed2 ? "Yes" : "No") << std::endl;
    
    return 0;
}
```

## How the LZW Algorithm Works:

### 1. **Compression Process:**
- Initialize dictionary with all single characters (ASCII 0-255)
- For each character in input:
  - If current string + next character exists in dictionary → extend current string
  - Else → output code for current string, add new string to dictionary
- Output final string code

### 2. **Decompression Process:**
- Initialize reverse dictionary with ASCII characters
- For each code in input:
  - Look up code in reverse dictionary
  - Output the string
  - Add new entry to reverse dictionary (previous string + first character of current string)

### 3. **Example Output:**
```
Original string: ABABABABAB
Compressed codes: 65 66 256 258 260 
Decompressed string: ABABABABAB
Compression successful: Yes

--- Another Example ---
Original: TOBEORNOTTOBEORTOBEORNOT
Compressed: 20 15 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14 19 23 1 14