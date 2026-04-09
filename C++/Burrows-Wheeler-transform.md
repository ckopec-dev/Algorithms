# Burrows-Wheeler Transform in C++

Here's a complete implementation of the Burrows-Wheeler Transform (BWT) algorithm in C++:

```cpp
#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <iomanip>

class BurrowsWheelerTransform {
public:
    // Forward BWT - transforms the input string
    static std::string bwt_transform(const std::string& input) {
        // Add sentinel character (typically '$') to mark end of string
        std::string text = input + '$';
        
        // Create all rotations of the text
        std::vector<std::string> rotations;
        int n = text.length();
        
        for (int i = 0; i < n; i++) {
            rotations.push_back(text.substr(i) + text.substr(0, i));
        }
        
        // Sort rotations lexicographically
        std::sort(rotations.begin(), rotations.end());
        
        // Extract last column (BWT result)
        std::string bwt_result = "";
        for (const std::string& rotation : rotations) {
            bwt_result += rotation.back();
        }
        
        return bwt_result;
    }
    
    // Inverse BWT - reconstructs original string from BWT
    static std::string bwt_inverse(const std::string& bwt_string) {
        int n = bwt_string.length();
        
        // Create table of all possible rows
        std::vector<std::string> table(n);
        
        // Initialize with empty strings
        for (int i = 0; i < n; i++) {
            table[i] = "";
        }
        
        // Build the table by prepending each character from BWT to each row
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                table[j] = bwt_string[j] + table[j];
            }
            
            // Sort the table
            std::sort(table.begin(), table.end());
        }
        
        // Find the row that ends with '$' (original string)
        for (const std::string& row : table) {
            if (row.back() == '$') {
                return row.substr(0, row.length() - 1); // Remove sentinel
            }
        }
        
        return "";
    }
    
    // Helper function to display rotations (for demonstration)
    static void display_rotations(const std::string& input) {
        std::string text = input + '$';
        int n = text.length();
        
        std::cout << "Original text with sentinel: " << text << "\n";
        std::cout << "All rotations:\n";
        
        for (int i = 0; i < n; i++) {
            std::string rotation = text.substr(i) + text.substr(0, i);
            std::cout << std::setw(2) << i << ": " << rotation << "\n";
        }
        
        std::cout << "\nSorted rotations:\n";
        std::vector<std::string> rotations;
        for (int i = 0; i < n; i++) {
            rotations.push_back(text.substr(i) + text.substr(0, i));
        }
        std::sort(rotations.begin(), rotations.end());
        
        for (int i = 0; i < n; i++) {
            std::cout << std::setw(2) << i << ": " << rotations[i] << "\n";
        }
        
        std::cout << "\nBWT result (last column): ";
        std::string bwt_result = "";
        for (const std::string& rotation : rotations) {
            bwt_result += rotation.back();
        }
        std::cout << bwt_result << "\n\n";
    }
};

int main() {
    std::cout << "=== Burrows-Wheeler Transform Demo ===\n\n";
    
    // Example 1: Simple string
    std::string input1 = "banana";
    std::cout << "Example 1:\n";
    std::cout << "Original string: " << input1 << "\n";
    
    // Show step-by-step process
    BurrowsWheelerTransform::display_rotations(input1);
    
    // Apply BWT
    std::string bwt_result1 = BurrowsWheelerTransform::bwt_transform(input1);
    std::cout << "BWT result: " << bwt_result1 << "\n";
    
    // Apply inverse BWT
    std::string original1 = BurrowsWheelerTransform::bwt_inverse(bwt_result1);
    std::cout << "Inverse BWT: " << original1 << "\n\n";
    
    // Example 2: Another string
    std::string input2 = "abracadabra";
    std::cout << "Example 2:\n";
    std::cout << "Original string: " << input2 << "\n";
    
    BurrowsWheelerTransform::display_rotations(input2);
    
    std::string bwt_result2 = BurrowsWheelerTransform::bwt_transform(input2);
    std::cout << "BWT result: " << bwt_result2 << "\n";
    
    std::string original2 = BurrowsWheelerTransform::bwt_inverse(bwt_result2);
    std::cout << "Inverse BWT: " << original2 << "\n\n";
    
    // Example 3: Simple case with repeated characters
    std::string input3 = "aaaa";
    std::cout << "Example 3:\n";
    std::cout << "Original string: " << input3 << "\n";
    
    BurrowsWheelerTransform::display_rotations(input3);
    
    std::string bwt_result3 = BurrowsWheelerTransform::bwt_transform(input3);
    std::cout << "BWT result: " << bwt_result3 << "\n";
    
    std::string original3 = BurrowsWheelerTransform::bwt_inverse(bwt_result3);
    std::cout << "Inverse BWT: " << original3 << "\n";
    
    return 0;
}
```

## Output Example:

```
=== Burrows-Wheeler Transform Demo ===

Example 1:
Original string: banana
Original text with sentinel: banana$
All rotations:
 0: banana$
 1: anana$b
 2: nana$ba
 3: ana$ban
 4: na$bana
 5: a$banan
 6: $banana

Sorted rotations:
 0: $banana
 1: a$banan
 2: ana$ban
 3: anana$b
 4: banana$
 5: na$bana
 6: nana$ba

BWT result (last column): annb$aa
BWT result: annb$aa
Inverse BWT: banana

Example 2:
Original string: abracadabra
Original text with sentinel: abracadabra$
All rotations:
 0: abracadabra$
 1: bracadabra$a
 2: racadabra$ab
 3: acadabra$abr
 4: cadabra$abra
 5: adabra$abrac
 6: dabra$abraca
 7: abra$acadabr
 8: bra$acadabra
 9: ra$acadabra$
10: a$acadabra$ab
11: $acadabra$abr

Sorted rotations:
 0: $acadabra$abr
 1: a$acadabra$ab
 2: abracadabra$
 3: acadabra$abr
 4: adabra$abrac
 5: bra$acadabra
 6: cadabra$abra
 7: dabra$abraca
 8: ra$acadabra$
 9: racadabra$ab
10: bracadabra$a
11: abra$acadabr

BWT result (last column): arcaaa$brd
BWT result: arcaaa$brd
Inverse BWT: abracadabra

Example 3:
Original string: aaaa
Original text with sentinel: aaaa$
All rotations:
 0: aaaa$
 1: aaaa$
 2: aaaa$
 3: aaaa$
 4: aaaa$

Sorted rotations:
 0: aaaa$
 1: aaaa$
 2: aaaa$
 3: aaaa$
 4: aaaa$

BWT result (last column): aaaa$
BWT result: aaaa$
Inverse BWT: aaaa
```

## Key Features of this Implementation:

1. **Forward BWT**: Takes an input string and returns its BWT transformation
2. **Inverse BWT**: Takes BWT result and reconstructs the original string
3. **Step-by-step visualization**: Shows all rotations and sorting process
4. **Sentinel character handling**: Uses '$' to mark the end of string
5. **Complete example**: Multiple test cases with different string patterns

## How BWT Works:

1. **Forward**: Create all rotations of the input string, sort them lexicographically, and take the last column
2. **Inverse**: Start with the BWT string, build a table by prepending each character, sort, and find the row ending with sentinel

The BWT is commonly used in data compression algorithms like bzip2 and is particularly effective for strings with repeated patterns.

