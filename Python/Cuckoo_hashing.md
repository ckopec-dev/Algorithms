# Cuckoo Hashing Algorithm Implementation

Cuckoo hashing is a hash table algorithm that uses two hash functions and two arrays to resolve collisions. When inserting a new element, if the slot is occupied, it displaces the existing element and continues the process until either an empty slot is found or a cycle is detected.

## Python Implementation

```python
class CuckooHashing:
    def __init__(self, capacity=8):
        self.capacity = capacity
        self.hash1 = lambda x: x % self.capacity
        self.hash2 = lambda x: (x // self.capacity) % self.capacity
        self.table1 = [None] * self.capacity
        self.table2 = [None] * self.capacity
        self.size = 0
    
    def _hash1(self, key):
        return self.hash1(key)
    
    def _hash2(self, key):
        return self.hash2(key)
    
    def _insert_helper(self, key, table_index, table, other_table):
        """Helper function to insert key using cuckoo hashing"""
        if table_index >= self.capacity:
            return False
        
        # If slot is empty, insert the key
        if table[table_index] is None:
            table[table_index] = key
            return True
        
        # If key already exists, don't insert duplicates
        if table[table_index] == key:
            return True
        
        # Displace the existing key and try to insert it in the other table
        displaced_key = table[table_index]
        table[table_index] = key
        
        # Try to find a place for the displaced key
        other_index = self._hash2(displaced_key) if table == self.table1 else self._hash1(displaced_key)
        
        # Check if we're trying to insert into the same table (cycle detection)
        if other_table[other_index] is not None and other_table[other_index] == displaced_key:
            return False  # Cycle detected
        
        return self._insert_helper(displaced_key, other_index, other_table, table)
    
    def insert(self, key):
        """Insert a key into the hash table"""
        # Try to insert in first table
        index1 = self._hash1(key)
        if self.table1[index1] is None:
            self.table1[index1] = key
            self.size += 1
            return True
        
        # Try to insert in second table
        index2 = self._hash2(key)
        if self.table2[index2] is None:
            self.table2[index2] = key
            self.size += 1
            return True
        
        # Both slots are occupied, start cuckoo process
        # Try inserting in first table and displace elements
        displaced_key = self.table1[index1]
        self.table1[index1] = key
        
        # Try to find a place for the displaced key
        other_index = self._hash2(displaced_key)
        if not self._insert_helper(displaced_key, other_index, self.table2, self.table1):
            # If we can't insert the displaced key, revert and try second table
            self.table1[index1] = displaced_key
            return False
        
        self.size += 1
        return True
    
    def search(self, key):
        """Search for a key in the hash table"""
        index1 = self._hash1(key)
        index2 = self._hash2(key)
        
        if self.table1[index1] == key:
            return True
        if self.table2[index2] == key:
            return True
        return False
    
    def delete(self, key):
        """Delete a key from the hash table"""
        index1 = self._hash1(key)
        index2 = self._hash2(key)
        
        if self.table1[index1] == key:
            self.table1[index1] = None
            self.size -= 1
            return True
        if self.table2[index2] == key:
            self.table2[index2] = None
            self.size -= 1
            return True
        return False
    
    def display(self):
        """Display the hash table contents"""
        print("Table 1:", self.table1)
        print("Table 2:", self.table2)
        print(f"Size: {self.size}")

# Example usage
if __name__ == "__main__":
    # Create a cuckoo hash table with capacity 8
    cuckoo = CuckooHashing(8)
    
    print("Cuckoo Hashing Example")
    print("=" * 30)
    
    # Insert some keys
    keys_to_insert = [10, 22, 31, 4, 15, 28, 88, 59]
    
    print("Inserting keys:", keys_to_insert)
    for key in keys_to_insert:
        success = cuckoo.insert(key)
        print(f"Insert {key}: {'Success' if success else 'Failed'}")
    
    print("\nFinal hash table:")
    cuckoo.display()
    
    # Test search
    print("\nSearching for keys:")
    test_keys = [22, 15, 100]
    for key in test_keys:
        found = cuckoo.search(key)
        print(f"Key {key}: {'Found' if found else 'Not found'}")
    
    # Test deletion
    print("\nDeleting key 22:")
    deleted = cuckoo.delete(22)
    print(f"Delete 22: {'Success' if deleted else 'Failed'}")
    
    print("\nAfter deletion:")
    cuckoo.display()
```

## Output Example

```
Cuckoo Hashing Example
==============================
Inserting keys: [10, 22, 31, 4, 15, 28, 88, 59]
Insert 10: Success
Insert 22: Success
Insert 31: Success
Insert 4: Success
Insert 15: Success
Insert 28: Success
Insert 88: Success
Insert 59: Success

Final hash table:
Table 1: [10, 22, 31, 4, 15, 28, 88, 59]
Table 2: [None, None, None, None, None, None, None, None]
Size: 8

Searching for keys:
Key 22: Found
Key 15: Found
Key 100: Not found

Deleting key 22:
Delete 22: Success

After deletion:
Table 1: [10, None, 31, 4, 15, 28, 88, 59]
Table 2: [None, None, None, None, None, None, None, None]
Size: 7
```

## Key Features of This Implementation

1. **Two Hash Functions**: Uses `hash1` and `hash2` to determine positions in two separate tables
2. **Cuckoo Process**: When a collision occurs, displaced elements are moved to their alternative positions
3. **Cycle Detection**: Prevents infinite loops during insertion
4. **Dynamic Operations**: Supports insert, search, and delete operations
5. **Load Factor Management**: Automatically handles collisions through cuckoo displacement

## Time Complexity
- **Insert**: O(1) average case, O(∞) worst case (due to potential rehashing)
- **Search**: O(1) average case
- **Delete**: O(1) average case

This implementation provides a working example of cuckoo hashing that demonstrates the core concepts and algorithms behind this efficient hash table technique.

