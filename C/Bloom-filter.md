# Bloom Filter Implementation in C

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

// Bloom Filter structure
typedef struct {
    int size;           // Size of bit array
    int hash_count;     // Number of hash functions
    char* bit_array;    // Bit array to store elements
} BloomFilter;

// Simple hash function 1
uint32_t hash1(const char* str) {
    uint32_t hash = 5381;
    int c;
    
    while ((c = *str++))
        hash = ((hash << 5) + hash) + c;
    
    return hash;
}

// Simple hash function 2
uint32_t hash2(const char* str) {
    uint32_t hash = 0;
    int c;
    
    while ((c = *str++))
        hash = hash * 31 + c;
    
    return hash;
}

// Initialize Bloom Filter
BloomFilter* bloom_filter_init(int size, int hash_count) {
    BloomFilter* bf = (BloomFilter*)malloc(sizeof(BloomFilter));
    if (!bf) return NULL;
    
    bf->size = size;
    bf->hash_count = hash_count;
    bf->bit_array = (char*)calloc(size, sizeof(char));
    
    if (!bf->bit_array) {
        free(bf);
        return NULL;
    }
    
    return bf;
}

// Add element to Bloom Filter
void bloom_filter_add(BloomFilter* bf, const char* str) {
    if (!bf || !str) return;
    
    uint32_t hash1_val = hash1(str);
    uint32_t hash2_val = hash2(str);
    
    for (int i = 0; i < bf->hash_count; i++) {
        // Generate multiple hash values using linear combination
        uint32_t hash = (hash1_val + i * hash2_val) % bf->size;
        int byte_index = hash / 8;
        int bit_index = hash % 8;
        
        // Set the bit
        bf->bit_array[byte_index] |= (1 << bit_index);
    }
}

// Check if element might exist in Bloom Filter
int bloom_filter_contains(BloomFilter* bf, const char* str) {
    if (!bf || !str) return 0;
    
    uint32_t hash1_val = hash1(str);
    uint32_t hash2_val = hash2(str);
    
    for (int i = 0; i < bf->hash_count; i++) {
        // Generate multiple hash values using linear combination
        uint32_t hash = (hash1_val + i * hash2_val) % bf->size;
        int byte_index = hash / 8;
        int bit_index = hash % 8;
        
        // Check if the bit is set
        if (!(bf->bit_array[byte_index] & (1 << bit_index))) {
            return 0; // Element definitely not present
        }
    }
    
    return 1; // Element might be present (false positive possible)
}

// Free Bloom Filter memory
void bloom_filter_free(BloomFilter* bf) {
    if (bf) {
        free(bf->bit_array);
        free(bf);
    }
}

// Example usage
int main() {
    // Create a Bloom Filter with 1024 bits and 3 hash functions
    BloomFilter* bf = bloom_filter_init(1024, 3);
    
    if (!bf) {
        printf("Failed to create Bloom Filter\n");
        return 1;
    }
    
    // Add some elements
    const char* elements[] = {"apple", "banana", "cherry", "date", "elderberry"};
    int num_elements = sizeof(elements) / sizeof(elements[0]);
    
    printf("Adding elements to Bloom Filter:\n");
    for (int i = 0; i < num_elements; i++) {
        bloom_filter_add(bf, elements[i]);
        printf("Added: %s\n", elements[i]);
    }
    
    // Test membership
    printf("\nTesting membership:\n");
    const char* test_elements[] = {"apple", "grape", "banana", "kiwi"};
    int num_tests = sizeof(test_elements) / sizeof(test_elements[0]);
    
    for (int i = 0; i < num_tests; i++) {
        if (bloom_filter_contains(bf, test_elements[i])) {
            printf("'%s' might be present\n", test_elements[i]);
        } else {
            printf("'%s' is definitely not present\n", test_elements[i]);
        }
    }
    
    // Clean up
    bloom_filter_free(bf);
    
    return 0;
}
```

## Key Features of this Implementation:

1. **Bit Array**: Uses a character array to store bits efficiently
2. **Multiple Hash Functions**: Implements two different hash functions for better distribution
3. **Linear Hashing**: Uses linear combination to generate multiple hash values from the same input
4. **Memory Management**: Proper allocation and deallocation of memory
5. **Error Handling**: Checks for NULL pointers and memory allocation failures

## How it Works:

1. **Initialization**: Creates a bit array of specified size with given number of hash functions
2. **Adding Elements**: Each element is hashed multiple times and corresponding bits are set to 1
3. **Membership Testing**: For a query, all hash functions are applied and if any bit is 0, the element is definitely not present; if all bits are 1, it might be present (false positive possible)

## Time Complexity:
- Insert: O(k) where k is number of hash functions
- Lookup: O(k) where k is number of hash functions

## Space Complexity: O(m) where m is the size of bit array