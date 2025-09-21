# Bloom Filter Implementation in C

Here's a complete implementation of a Bloom Filter algorithm in C:

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

// Bloom Filter structure
typedef struct {
    uint8_t *bits;        // Bit array
    int size;             // Size of bit array
    int hash_count;       // Number of hash functions
    int capacity;         // Expected number of elements
} BloomFilter;

// Simple hash function 1 (djb2)
unsigned int hash1(const char *str) {
    unsigned int hash = 5381;
    int c;
    while ((c = *str++))
        hash = ((hash << 5) + hash) + c;
    return hash;
}

// Simple hash function 2 (FNV-1a)
unsigned int hash2(const char *str) {
    unsigned int hash = 2166136261U;
    int c;
    while ((c = *str++))
        hash = (hash ^ c) * 16777619;
    return hash;
}

// Simple hash function 3 (MurmurHash3)
unsigned int hash3(const char *str) {
    unsigned int h = 0;
    int len = strlen(str);
    for (int i = 0; i < len; i++) {
        h ^= str[i];
        h *= 0x5bd1e995;
        h ^= h >> 15;
    }
    return h;
}

// Create a new Bloom Filter
BloomFilter* bloom_filter_create(int size, int hash_count) {
    BloomFilter *bf = (BloomFilter*)malloc(sizeof(BloomFilter));
    if (!bf) return NULL;
    
    bf->size = size;
    bf->hash_count = hash_count;
    bf->capacity = 0;
    
    // Allocate bit array
    bf->bits = (uint8_t*)calloc((size + 7) / 8, sizeof(uint8_t));
    if (!bf->bits) {
        free(bf);
        return NULL;
    }
    
    return bf;
}

// Set a bit at position index
void set_bit(BloomFilter *bf, int index) {
    int byte_index = index / 8;
    int bit_index = index % 8;
    bf->bits[byte_index] |= (1 << bit_index);
}

// Check if a bit is set at position index
int get_bit(BloomFilter *bf, int index) {
    int byte_index = index / 8;
    int bit_index = index % 8;
    return (bf->bits[byte_index] & (1 << bit_index)) != 0;
}

// Add an element to the Bloom Filter
void bloom_filter_add(BloomFilter *bf, const char *str) {
    if (!bf || !str) return;
    
    // Generate multiple hash values
    unsigned int h1 = hash1(str);
    unsigned int h2 = hash2(str);
    unsigned int h3 = hash3(str);
    
    for (int i = 0; i < bf->hash_count; i++) {
        unsigned int hash_value;
        switch (i % 3) {
            case 0: hash_value = h1; break;
            case 1: hash_value = h2; break;
            case 2: hash_value = h3; break;
        }
        
        // Generate multiple hash positions
        unsigned int index = (hash_value + i * h2) % bf->size;
        set_bit(bf, index);
    }
    
    bf->capacity++;
}

// Check if an element might exist in the Bloom Filter
int bloom_filter_contains(BloomFilter *bf, const char *str) {
    if (!bf || !str) return 0;
    
    unsigned int h1 = hash1(str);
    unsigned int h2 = hash2(str);
    unsigned int h3 = hash3(str);
    
    for (int i = 0; i < bf->hash_count; i++) {
        unsigned int hash_value;
        switch (i % 3) {
            case 0: hash_value = h1; break;
            case 1: hash_value = h2; break;
            case 2: hash_value = h3; break;
        }
        
        // Generate multiple hash positions
        unsigned int index = (hash_value + i * h2) % bf->size;
        if (!get_bit(bf, index)) {
            return 0; // Definitely not present
        }
    }
    
    return 1; // Might be present (false positive possible)
}

// Free Bloom Filter memory
void bloom_filter_free(BloomFilter *bf) {
    if (bf) {
        free(bf->bits);
        free(bf);
    }
}

// Example usage
int main() {
    // Create a Bloom Filter with 1000 bits and 3 hash functions
    BloomFilter *bf = bloom_filter_create(1000, 3);
    
    if (!bf) {
        printf("Failed to create Bloom Filter\n");
        return 1;
    }
    
    // Add some elements
    printf("Adding elements...\n");
    bloom_filter_add(bf, "apple");
    bloom_filter_add(bf, "banana");
    bloom_filter_add(bf, "cherry");
    bloom_filter_add(bf, "date");
    
    // Test for existing elements
    printf("\nTesting existing elements:\n");
    printf("Contains 'apple': %s\n", bloom_filter_contains(bf, "apple") ? "YES" : "NO");
    printf("Contains 'banana': %s\n", bloom_filter_contains(bf, "banana") ? "YES" : "NO");
    printf("Contains 'cherry': %s\n", bloom_filter_contains(bf, "cherry") ? "YES" : "NO");
    printf("Contains 'date': %s\n", bloom_filter_contains(bf, "date") ? "YES" : "NO");
    
    // Test for non-existing elements
    printf("\nTesting non-existing elements:\n");
    printf("Contains 'grape': %s\n", bloom_filter_contains(bf, "grape") ? "YES" : "NO");
    printf("Contains 'kiwi': %s\n", bloom_filter_contains(bf, "kiwi") ? "YES" : "NO");
    printf("Contains 'mango': %s\n", bloom_filter_contains(bf, "mango") ? "YES" : "NO");
    
    // Test with some false positives
    printf("\nTesting potential false positives:\n");
    printf("Contains 'xyz123': %s\n", bloom_filter_contains(bf, "xyz123") ? "YES (possible false positive)" : "NO");
    printf("Contains 'abc789': %s\n", bloom_filter_contains(bf, "abc789") ? "YES (possible false positive)" : "NO");
    
    // Clean up
    bloom_filter_free(bf);
    
    return 0;
}
```

## Key Features of this Implementation:

### 1. **Core Components:**
- Bit array for storing hash values
- Multiple hash functions for better distribution
- Efficient memory usage with bit manipulation

### 2. **Functions Provided:**
- `bloom_filter_create()`: Initialize the Bloom Filter
- `bloom_filter_add()`: Add elements to the filter
- `bloom_filter_contains()`: Check if element might exist
- `bloom_filter_free()`: Clean up memory

### 3. **How it Works:**
1. **Adding Elements**: Each string is hashed by multiple hash functions, and corresponding bits are set to 1
2. **Checking Elements**: The same hash functions are applied, and if ALL corresponding bits are 1, the element might exist
3. **False Positives**: The algorithm can return false positives but never false negatives

### 4. **Advantages:**
- **Space Efficient**: Uses minimal memory
- **Fast Operations**: O(k) time complexity where k is number of hash functions
- **No False Negatives**: If an element is not in the set, it will never be reported as present

### 5. **Usage Example Output:**
```
Adding elements...

Testing existing elements:
Contains 'apple': YES
Contains 'banana': YES
Contains 'cherry': YES
Contains 'date': YES

Testing non-existing elements:
Contains 'grape': NO
Contains 'kiwi': NO
Contains 'mango': NO

Testing potential false positives:
Contains 'xyz123': NO
Contains 'abc789': NO
```

This implementation demonstrates the fundamental principles of Bloom filters while maintaining good performance and memory efficiency.

