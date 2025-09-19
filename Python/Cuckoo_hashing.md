# Cuckoo Hashing Algorithm Implementation

Cuckoo hashing is a hash table algorithm that uses two hash functions and two arrays to resolve collisions. When inserting a new element, if the slot is occupied, the existing element is "kicked out" and moved to its alternative location.

```python
class CuckooHash:
    def __init__(self, capacity=8):
        self.capacity = capacity
        self.hash1 = lambda x: hash(x) % self.capacity
        self.hash2 = lambda x: (hash(x) * 2654435761) % self.capacity  # Knuth's multiplicative hash
        self.table1 = [None] * self.capacity
        self.table2 = [None] * self.capacity
        self.size = 0
    
    def _insert_recursive(self, key, table_num, depth=0):
        """
        Recursively insert elements, kicking out existing elements when necessary
        """
        if depth > 2 * self.capacity:  # Prevent infinite loop
            return False
        
        if table_num == 1:
            index = self.hash1(key)
            if self.table1[index] is None:
                self.table1[index] = key
                return True
            else:
                old_key = self.table1[index]
                self.table1[index] = key
                return self._insert_recursive(old_key, 2, depth + 1)
        else:
            index = self.hash2(key)
            if self.table2[index] is None:
                self.table2[index] = key
                return True
            else:
                old_key = self.table2[index]
                self.table2[index] = key
                return self._insert_recursive(old_key, 1, depth + 1)
    
    def insert(self, key):
        """
        Insert a key into the cuckoo hash table
        """
        if self.search(key):
            return False  # Key already exists
        
        if self._insert_recursive(key, 1):
            self.size += 1
            return True
        else:
            # Rehashing needed - this is a simplified version
            print(f"Rehashing required for key {key}")
            return False
    
    def search(self, key):
        """
        Search for a key in the cuckoo hash table
        """
        index1 = self.hash1(key)
        index2 = self.hash2(key)
        
        if self.table1[index1] == key or self.table2[index2] == key:
            return True
        return False
    
    def delete(self, key):
        """
        Delete a key from the cuckoo hash table
        """
        index1 = self.hash1(key)
        index2 = self.hash2(key)
        
        if self.table1[index1] == key:
            self.table1[index1] = None
            self.size -= 1
            return True
        elif self.table2[index2] == key:
            self.table2[index2] = None
            self.size -= 1
            return True
        return False
    
    def display(self):
        """
        Display the contents of both tables
        """
        print("Table 1:", self.table1)
        print("Table 2:", self.table2)
        print(f"Size: {self.size}")

# Example usage
if __name__ == "__main__":
    # Create a cuckoo hash table
    ch = CuckooHash(8)
    
    print("=== Cuckoo Hash Table Demo ===")
    
    # Insert some keys
    keys_to_insert = [10, 20, 30, 40, 50, 60, 70]
    
    print("\nInserting keys:", keys_to_insert)
    for key in keys_to_insert:
        result = ch.insert(key)
        print(f"Inserted {key}: {'Success' if result else 'Failed'}")
        ch.display()
        print("-" * 30)
    
    # Search for keys
    print("\nSearching for keys:")
    search_keys = [10, 25, 30, 75]
    for key in search_keys:
        found = ch.search(key)
        print(f"Key {key} found: {found}")
    
    # Delete a key
    print(f"\nDeleting key 30:")
    deleted = ch.delete(30)
    print(f"Delete result: {'Success' if deleted else 'Failed'}")
    ch.display()
```

## Key Features of this Implementation:

1. **Two Hash Functions**: Uses different hash functions for each table
2. **Recursive Insertion**: When a collision occurs, the existing element is kicked out and moved to its alternative location
3. **Collision Resolution**: Handles collisions through the "cuckoo" process
4. **Search Operation**: Efficient O(1) average time complexity
5. **Memory Management**: Tracks table size and handles empty slots

## Time Complexities:

- **Insert**: O(1) average, O(∞) worst case (with rehashing)
- **Search**: O(1) average
- **Delete**: O(1) average

## Advantages:

- No chaining or probing required
- Constant time lookups in average case
- Simple implementation for basic use cases

## Disadvantages:

- Potential for infinite loops during insertion (requires rehashing)
- May need rehashing when table gets too full
- More complex than simple hash tables with chaining

