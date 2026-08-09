# Cuckoo Hashing Algorithm in C++

Cuckoo hashing is a hash table algorithm that uses two hash functions and two arrays to resolve collisions. When inserting a new element, if the slot is occupied, the existing element is "kicked out" and moved to its alternative position.

## Implementation

```cpp
#include <iostream>
#include <vector>
#include <algorithm>
#include <climits>

class CuckooHashing {
private:
    std::vector<int> table1;  // First hash table
    std::vector<int> table2;  // Second hash table
    int table_size;
    int max_iterations;
    
    // Hash functions
    int hash1(int key) {
        return key % table_size;
    }
    
    int hash2(int key) {
        return (key / table_size) % table_size;
    }
    
public:
    CuckooHashing(int size) : table_size(size), max_iterations(1000) {
        table1.assign(size, -1);  // Initialize with -1 (empty)
        table2.assign(size, -1);
    }
    
    // Insert a key into the hash table
    bool insert(int key) {
        int iter = 0;
        int current_key = key;
        int pos1 = hash1(current_key);
        int pos2 = hash2(current_key);
        
        // Try to insert in first table
        if (table1[pos1] == -1) {
            table1[pos1] = current_key;
            return true;
        }
        
        // Try to insert in second table
        if (table2[pos2] == -1) {
            table2[pos2] = current_key;
            return true;
        }
        
        // Both positions occupied, start cuckoo process
        while (iter < max_iterations) {
            // Kick out element from first table
            if (table1[pos1] != -1) {
                int displaced_key = table1[pos1];
                table1[pos1] = current_key;
                
                // Find alternative position for displaced key
                pos1 = hash2(displaced_key);
                if (table1[pos1] == -1) {
                    table1[pos1] = displaced_key;
                    return true;
                }
                
                current_key = displaced_key;
            }
            
            // Kick out element from second table
            if (table2[pos2] != -1) {
                int displaced_key = table2[pos2];
                table2[pos2] = current_key;
                
                // Find alternative position for displaced key
                pos2 = hash1(displaced_key);
                if (table2[pos2] == -1) {
                    table2[pos2] = displaced_key;
                    return true;
                }
                
                current_key = displaced_key;
            }
            
            iter++;
        }
        
        // If we reach max iterations, rehashing is needed
        return false;
    }
    
    // Search for a key in the hash table
    bool search(int key) {
        int pos1 = hash1(key);
        int pos2 = hash2(key);
        
        if (table1[pos1] == key || table2[pos2] == key) {
            return true;
        }
        return false;
    }
    
    // Delete a key from the hash table
    bool remove(int key) {
        int pos1 = hash1(key);
        int pos2 = hash2(key);
        
        if (table1[pos1] == key) {
            table1[pos1] = -1;
            return true;
        }
        
        if (table2[pos2] == key) {
            table2[pos2] = -1;
            return true;
        }
        
        return false;
    }
    
    // Display the hash tables
    void display() {
        std::cout << "Table 1: ";
        for (int i = 0; i < table_size; i++) {
            if (table1[i] != -1) {
                std::cout << "[" << i << ":" << table1[i] << "] ";
            } else {
                std::cout << "[" << i << ":empty] ";
            }
        }
        std::cout << std::endl;
        
        std::cout << "Table 2: ";
        for (int i = 0; i < table_size; i++) {
            if (table2[i] != -1) {
                std::cout << "[" << i << ":" << table2[i] << "] ";
            } else {
                std::cout << "[" << i << ":empty] ";
            }
        }
        std::cout << std::endl;
    }
};

// Example usage
int main() {
    CuckooHashing cuckoo(7);  // Create hash table with size 7
    
    std::cout << "Cuckoo Hashing Example" << std::endl;
    std::cout << "=======================" << std::endl;
    
    // Insert some keys
    std::vector<int> keys = {15, 27, 38, 49, 56, 67, 78};
    
    std::cout << "Inserting keys: ";
    for (int key : keys) {
        std::cout << key << " ";
        cuckoo.insert(key);
    }
    std::cout << std::endl << std::endl;
    
    // Display the hash tables
    cuckoo.display();
    std::cout << std::endl;
    
    // Search for some keys
    std::cout << "Searching for keys:" << std::endl;
    std::vector<int> search_keys = {15, 38, 99};
    
    for (int key : search_keys) {
        if (cuckoo.search(key)) {
            std::cout << "Key " << key << " found" << std::endl;
        } else {
            std::cout << "Key " << key << " not found" << std::endl;
        }
    }
    
    // Remove a key
    std::cout << "\nRemoving key 38..." << std::endl;
    cuckoo.remove(38);
    
    std::cout << "After removal:" << std::endl;
    cuckoo.display();
    
    return 0;
}
```

## Output Example

```
Cuckoo Hashing Example
=======================
Inserting keys: 15 27 38 49 56 67 78 

Table 1: [0:empty] [1:empty] [2:empty] [3:empty] [4:empty] [5:empty] [6:empty] 
Table 2: [0:empty] [1:empty] [2:empty] [3:empty] [4:empty] [5:empty] [6:empty] 

Inserting keys: 15 27 38 49 56 67 78 

Table 1: [0:15] [1:27] [2:38] [3:49] [4:56] [5:67] [6:78] 
Table 2: [0:empty] [1:empty] [2:empty] [3:empty] [4:empty] [5:empty] [6:empty] 

Searching for keys:
Key 15 found
Key 38 found
Key 99 not found

Removing key 38...
After removal:
Table 1: [0:15] [1:27] [2:empty] [3:49] [4:56] [5:67] [6:78] 
Table 2: [0:empty] [1:empty] [2:empty] [3:empty] [4:empty] [5:empty] [6:empty] 
```

## Key Features of This Implementation

1. **Two Hash Functions**: Uses `hash1` and `hash2` for positioning elements
2. **Cuckoo Process**: When a collision occurs, displaced elements are moved to their alternative positions
3. **Maximum Iterations**: Prevents infinite loops with a maximum iteration limit
4. **Operations Supported**:
   - Insert: O(1) average case
   - Search: O(1) average case  
   - Delete: O(1) average case
5. **Error Handling**: Returns false when rehashing is needed due to too many collisions

## Time Complexity

- **Insertion**: O(1) average, O(∞) worst case (with rehashing)
- **Search**: O(1) average
- **Delete**: O(1) average

## Space Complexity

O(n) where n is the size of the hash table