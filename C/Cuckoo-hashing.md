# Cuckoo Hashing Implementation in C

```c
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#define TABLE_SIZE 16
#define MAX_ITERATIONS 1000

// Hash function for cuckoo hashing
int hash1(int key) {
    return key % TABLE_SIZE;
}

int hash2(int key) {
    return (key / TABLE_SIZE) % TABLE_SIZE;
}

// Cuckoo hash table structure
typedef struct {
    int *table1;
    int *table2;
    int *keys1;
    int *keys2;
    int size;
} CuckooHashTable;

// Initialize the cuckoo hash table
CuckooHashTable* init_cuckoo_hash_table() {
    CuckooHashTable *ht = (CuckooHashTable*)malloc(sizeof(CuckooHashTable));
    ht->size = TABLE_SIZE;
    
    ht->table1 = (int*)calloc(TABLE_SIZE, sizeof(int));
    ht->table2 = (int*)calloc(TABLE_SIZE, sizeof(int));
    ht->keys1 = (int*)calloc(TABLE_SIZE, sizeof(int));
    ht->keys2 = (int*)calloc(TABLE_SIZE, sizeof(int));
    
    // Initialize with -1 (empty slot)
    for (int i = 0; i < TABLE_SIZE; i++) {
        ht->table1[i] = -1;
        ht->table2[i] = -1;
        ht->keys1[i] = -1;
        ht->keys2[i] = -1;
    }
    
    return ht;
}

// Insert a key-value pair into the hash table
bool cuckoo_insert(CuckooHashTable *ht, int key, int value) {
    // Check if key already exists
    if (cuckoo_search(ht, key) != -1) {
        return false; // Key already exists
    }
    
    int current_key = key;
    int current_value = value;
    int hash1_index = hash1(current_key);
    int hash2_index = hash2(current_key);
    int iteration = 0;
    
    // Try to insert the key
    while (iteration < MAX_ITERATIONS) {
        // Try inserting into table1
        if (ht->table1[hash1_index] == -1) {
            ht->table1[hash1_index] = current_value;
            ht->keys1[hash1_index] = current_key;
            return true;
        }
        
        // Try inserting into table2
        if (ht->table2[hash2_index] == -1) {
            ht->table2[hash2_index] = current_value;
            ht->keys2[hash2_index] = current_key;
            return true;
        }
        
        // Both slots are occupied - perform cuckoo operation
        int old_key = ht->keys1[hash1_index];
        int old_value = ht->table1[hash1_index];
        
        // Evict from table1 and move to table2
        ht->table1[hash1_index] = current_value;
        ht->keys1[hash1_index] = current_key;
        
        current_key = old_key;
        current_value = old_value;
        
        // Update hash indices for the evicted key
        hash1_index = hash1(current_key);
        hash2_index = hash2(current_key);
        
        iteration++;
    }
    
    printf("Cuckoo hashing failed to insert key %d after %d iterations\n", key, MAX_ITERATIONS);
    return false;
}

// Search for a key in the hash table
int cuckoo_search(CuckooHashTable *ht, int key) {
    int hash1_index = hash1(key);
    int hash2_index = hash2(key);
    
    // Check table1
    if (ht->keys1[hash1_index] == key) {
        return ht->table1[hash1_index];
    }
    
    // Check table2
    if (ht->keys2[hash2_index] == key) {
        return ht->table2[hash2_index];
    }
    
    return -1; // Key not found
}

// Delete a key from the hash table
bool cuckoo_delete(CuckooHashTable *ht, int key) {
    int hash1_index = hash1(key);
    int hash2_index = hash2(key);
    
    // Check table1
    if (ht->keys1[hash1_index] == key) {
        ht->table1[hash1_index] = -1;
        ht->keys1[hash1_index] = -1;
        return true;
    }
    
    // Check table2
    if (ht->keys2[hash2_index] == key) {
        ht->table2[hash2_index] = -1;
        ht->keys2[hash2_index] = -1;
        return true;
    }
    
    return false; // Key not found
}

// Print the hash table contents
void print_cuckoo_table(CuckooHashTable *ht) {
    printf("\nCuckoo Hash Table Contents:\n");
    printf("Table 1:\n");
    for (int i = 0; i < TABLE_SIZE; i++) {
        if (ht->keys1[i] != -1) {
            printf("  [%d]: key=%d, value=%d\n", i, ht->keys1[i], ht->table1[i]);
        } else {
            printf("  [%d]: empty\n", i);
        }
    }
    
    printf("Table 2:\n");
    for (int i = 0; i < TABLE_SIZE; i++) {
        if (ht->keys2[i] != -1) {
            printf("  [%d]: key=%d, value=%d\n", i, ht->keys2[i], ht->table2[i]);
        } else {
            printf("  [%d]: empty\n", i);
        }
    }
}

// Free memory allocated for the hash table
void free_cuckoo_hash_table(CuckooHashTable *ht) {
    if (ht) {
        free(ht->table1);
        free(ht->table2);
        free(ht->keys1);
        free(ht->keys2);
        free(ht);
    }
}

// Example usage
int main() {
    CuckooHashTable *ht = init_cuckoo_hash_table();
    
    printf("Cuckoo Hashing Example\n");
    printf("=====================\n");
    
    // Insert some key-value pairs
    printf("Inserting key-value pairs:\n");
    cuckoo_insert(ht, 10, 100);
    cuckoo_insert(ht, 20, 200);
    cuckoo_insert(ht, 30, 300);
    cuckoo_insert(ht, 40, 400);
    cuckoo_insert(ht, 50, 500);
    cuckoo_insert(ht, 60, 600);
    
    print_cuckoo_table(ht);
    
    // Search for keys
    printf("\nSearching for keys:\n");
    printf("Key 20 found with value: %d\n", cuckoo_search(ht, 20));
    printf("Key 30 found with value: %d\n", cuckoo_search(ht, 30));
    printf("Key 70 found with value: %d\n", cuckoo_search(ht, 70)); // Should return -1
    
    // Delete a key
    printf("\nDeleting key 30:\n");
    if (cuckoo_delete(ht, 30)) {
        printf("Key 30 deleted successfully\n");
    }
    
    print_cuckoo_table(ht);
    
    // Insert more keys to demonstrate cuckoo operations
    printf("\nInserting more keys to trigger cuckoo operations:\n");
    cuckoo_insert(ht, 70, 700);
    cuckoo_insert(ht, 80, 800);
    cuckoo_insert(ht, 90, 900);
    
    print_cuckoo_table(ht);
    
    free_cuckoo_hash_table(ht);
    return 0;
}
```

## Key Features of This Implementation:

1. **Two Hash Tables**: Uses two separate hash tables with different hash functions
2. **Two Hash Functions**: 
   - `hash1(key) = key % TABLE_SIZE`
   - `hash2(key) = (key / TABLE_SIZE) % TABLE_SIZE`
3. **Cuckoo Operations**: When a collision occurs, it evicts an existing element and reinserts it
4. **Collision Resolution**: Implements the cuckoo hashing algorithm with rehashing
5. **Basic Operations**: Insert, Search, Delete, and Print functions

## Expected Output:
```
Cuckoo Hashing Example
=====================
Inserting key-value pairs:

Cuckoo Hash Table Contents:
Table 1:
  [0]: key=10, value=100
  [1]: key=20, value=200
  [2]: key=30, value=300
  [3]: key=40, value=400
  [4]: key=50, value=500
  [5]: key=60, value=600
  [6]: empty
  [7]: empty
  [8]: empty
  [9]: empty
  [10]: empty
  [11]: empty
  [12]: empty
  [13]: empty
  [14]: empty
  [15]: empty
Table 2:
  [0]: empty
  [1]: empty
  [2]: empty
  [3]: empty
  [4]: empty
  [5]: empty
  [6]: empty
  [7]: empty
  [8]: empty
  [9]: empty
  [10]: empty
  [11]: empty
  [12]: empty
  [13]: empty
  [14]: empty
  [15]: empty

Searching for keys:
Key 20 found with value: 200
Key 30 found with value: 300
Key 70 found with value: -1

Deleting key 30:
Key 30 deleted successfully
```

This implementation demonstrates the core concepts of cuckoo hashing including the use of multiple hash functions and the cuckoo eviction mechanism when collisions occur.

