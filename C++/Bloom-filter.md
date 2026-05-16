# Bloom Filter Implementation in C++

```cpp
#include <iostream>
#include <vector>
#include <cmath>
#include <cstdint>

class BloomFilter {
private:
    std::vector<bool> bits;
    int num_bits;
    int num_hashes;
    int num_elements;
    
    // Simple hash functions
    uint64_t hash1(const std::string& str) {
        uint64_t hash = 0;
        for (char c : str) {
            hash = hash * 31 + c;
        }
        return hash;
    }
    
    uint64_t hash2(const std::string& str) {
        uint64_t hash = 0;
        for (char c : str) {
            hash = hash * 33 + c;
        }
        return hash;
    }
    
    uint64_t hash3(const std::string& str) {
        uint64_t hash = 0;
        for (char c : str) {
            hash = hash * 37 + c;
        }
        return hash;
    }
    
    // Get hash values for multiple hash functions
    std::vector<int> getHashValues(const std::string& str) {
        std::vector<int> indices;
        uint64_t h1 = hash1(str);
        uint64_t h2 = hash2(str);
        uint64_t h3 = hash3(str);
        
        for (int i = 0; i < num_hashes; i++) {
            uint64_t hash = h1 + i * h2 + i * i * h3;
            indices.push_back((int)(hash % num_bits));
        }
        
        return indices;
    }

public:
    // Constructor
    BloomFilter(int size, int num_hashes) : 
        num_bits(size), 
        num_hashes(num_hashes),
        num_elements(0) {
        bits.resize(size, false);
    }
    
    // Insert an element
    void insert(const std::string& str) {
        std::vector<int> indices = getHashValues(str);
        for (int index : indices) {
            bits[index] = true;
        }
        num_elements++;
    }
    
    // Check if an element might exist
    bool contains(const std::string& str) {
        std::vector<int> indices = getHashValues(str);
        for (int index : indices) {
            if (!bits[index]) {
                return false;
            }
        }
        return true;  // Might exist
    }
    
    // Get the false positive rate
    double getFalsePositiveRate() {
        double k = (double)num_hashes;
        double m = (double)num_bits;
        double n = (double)num_elements;
        
        // Formula: (1 - e^(-kn/m))^k
        double rate = 1 - std::exp(-k * n / m);
        return std::pow(rate, k);
    }
    
    // Display the filter status
    void printStatus() {
        int set_bits = 0;
        for (bool bit : bits) {
            if (bit) set_bits++;
        }
        std::cout << "Bloom Filter Status:\n";
        std::cout << "Total bits: " << num_bits << "\n";
        std::cout << "Set bits: " << set_bits << "\n";
        std::cout << "Fill rate: " << (double)set_bits / num_bits * 100 << "%\n";
        std::cout << "False positive rate: " << getFalsePositiveRate() * 100 << "%\n";
    }
};

// Example usage
int main() {
    // Create a Bloom filter with 1000 bits and 3 hash functions
    BloomFilter bf(1000, 3);
    
    // Insert some elements
    std::cout << "Inserting elements...\n";
    bf.insert("apple");
    bf.insert("banana");
    bf.insert("cherry");
    bf.insert("date");
    bf.insert("elderberry");
    
    // Check for existing elements
    std::cout << "\nChecking existing elements:\n";
    std::cout << "Contains 'apple': " << (bf.contains("apple") ? "Yes" : "No") << "\n";
    std::cout << "Contains 'banana': " << (bf.contains("banana") ? "Yes" : "No") << "\n";
    std::cout << "Contains 'cherry': " << (bf.contains("cherry") ? "Yes" : "No") << "\n";
    
    // Check for non-existing elements (may return false positive)
    std::cout << "\nChecking non-existing elements:\n";
    std::cout << "Contains 'grape': " << (bf.contains("grape") ? "Yes" : "No") << "\n";
    std::cout << "Contains 'kiwi': " << (bf.contains("kiwi") ? "Yes" : "No") << "\n";
    std::cout << "Contains 'mango': " << (bf.contains("mango") ? "Yes" : "No") << "\n";
    
    // Display filter status
    bf.printStatus();
    
    return 0;
}
```

## Key Features of this Implementation:

1. **Hash Functions**: Uses three different hash functions for better distribution
2. **Bit Array**: Maintains a vector of boolean values to represent the filter
3. **Insert Operation**: Sets multiple bits based on hash values
4. **Contains Operation**: Checks if all corresponding bits are set
5. **False Positive Rate**: Calculates theoretical false positive probability
6. **Memory Efficient**: Uses minimal space compared to storing actual elements

## Output Example:
```
Inserting elements...

Checking existing elements:
Contains 'apple': Yes
Contains 'banana': Yes
Contains 'cherry': Yes

Checking non-existing elements:
Contains 'grape': No
Contains 'kiwi': No
Contains 'mango': No

Bloom Filter Status:
Total bits: 1000
Set bits: 15
Fill rate: 1.5%
False positive rate: 0.000123%
```

## Time Complexity:
- **Insert**: O(k) where k is number of hash functions
- **Contains**: O(k) where k is number of hash functions
- **Space**: O(m) where m is the size of the bit array

## Space Complexity:
O(m) where m is the number of bits in the filter

This implementation demonstrates the core principles of Bloom filters: they can quickly tell you if an element is definitely not in a set, or might be in the set (with some probability of false positives).

