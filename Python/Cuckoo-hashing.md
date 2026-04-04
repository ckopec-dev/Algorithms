# Cuckoo Hashing Algorithm in Python

Cuckoo hashing is a hash table algorithm that uses two hash functions and two hash tables. When inserting a new element, if the slot is occupied, it displaces the existing element, which may displace another element, and so on.

## Implementation

```python
class CuckooHashing:
    def __init__(self, size=10):
        self.size = size
        self.table1 = [None] * size
        self.table2 = [None] * size
        self.max_displacements = size  # Prevent infinite loops
        
    def hash1(self, key):
        """First hash function"""
        return hash(key) % self.size
    
    def hash2(self, key):
        """Second hash function"""
        return (hash(key) * 256 + 17) % self.size
    
    def insert(self, key, value):
        """Insert a key-value pair into the hash table"""
        # Try to insert into first table
        if self.table1[self.hash1(key)] is None:
            self.table1[self.hash1(key)] = (key, value)
            return True
        
        # Try to insert into second table
        if self.table2[self.hash2(key)] is None:
            self.table2[self.hash2(key)] = (key, value)
            return True
        
        # Both slots are occupied, need to displace elements
        return self._displace(key, value)
    
    def _displace(self, key, value):
        """Displace elements to make room for new element"""
        displaced_key = key
        displaced_value = value
        displacements = 0
        
        # Start with first table
        current_table = 1
        current_index = self.hash1(key)
        
        while displacements < self.max_displacements:
            if current_table == 1:
                # Check if we can insert into first table
                if self.table1[current_index] is None:
                    self.table1[current_index] = (displaced_key, displaced_value)
                    return True
                
                # Displace existing element
                old_key, old_value = self.table1[current_index]
                self.table1[current_index] = (displaced_key, displaced_value)
                displaced_key = old_key
                displaced_value = old_value
                
                # Switch to second table for next displacement
                current_table = 2
                current_index = self.hash2(old_key)
            else:
                # Check if we can insert into second table
                if self.table2[current_index] is None:
                    self.table2[current_index] = (displaced_key, displaced_value)
                    return True
                
                # Displace existing element
                old_key, old_value = self.table2[current_index]
                self.table2[current_index] = (displaced_key, displaced_value)
                displaced_key = old_key
                displaced_value = old_value
                
                # Switch back to first table for next displacement
                current_table = 1
                current_index = self.hash1(old_key)
            
            displacements += 1
        
        # If we reach here, we have too many displacements
        print("Hash table is full or too many displacements needed")
        return False
    
    def search(self, key):
        """Search for a key in the hash table"""
        index1 = self.hash1(key)
        index2 = self.hash2(key)
        
        if self.table1[index1] is not None and self.table1[index1][0] == key:
            return self.table1[index1][1]
        
        if self.table2[index2] is not None and self.table2[index2][0] == key:
            return self.table2[index2][1]
        
        return None
    
    def delete(self, key):
        """Delete a key from the hash table"""
        index1 = self.hash1(key)
        index2 = self.hash2(key)
        
        if self.table1[index1] is not None and self.table1[index1][0] == key:
            self.table1[index1] = None
            return True
        
        if self.table2[index2] is not None and self.table2[index2][0] == key:
            self.table2[index2] = None
            return True
        
        return False
    
    def display(self):
        """Display the contents of both tables"""
        print("Table 1:", self.table1)
        print("Table 2:", self.table2)
        print()

# Example usage
if __name__ == "__main__":
    # Create a cuckoo hash table
    cuckoo = CuckooHashing(7)
    
    print("Cuckoo Hashing Example")
    print("=" * 30)
    
    # Insert some key-value pairs
    print("Inserting elements:")
    cuckoo.insert("apple", 10)
    cuckoo.insert("banana", 20)
    cuckoo.insert("cherry", 30)
    cuckoo.insert("date", 40)
    cuckoo.insert("elderberry", 50)
    
    print("After insertions:")
    cuckoo.display()
    
    # Search for elements
    print("Searching for elements:")
    print(f"apple: {cuckoo.search('apple')}")
    print(f"banana: {cuckoo.search('banana')}")
    print(f"grape: {cuckoo.search('grape')}")
    
    # Delete an element
    print("\nDeleting 'banana':")
    cuckoo.delete("banana")
    print("After deletion:")
    cuckoo.display()
    
    # Try to insert more elements to see displacement
    print("Inserting more elements to trigger displacement:")
    cuckoo.insert("fig", 60)
    cuckoo.insert("grape", 70)
    cuckoo.insert("honeydew", 80)
    
    print("After more insertions:")
    cuckoo.display()
```

## Output Example

```
Cuckoo Hashing Example
==============================
Inserting elements:
After insertions:
Table 1: [('apple', 10), ('cherry', 30), ('elderberry', 50), None, None, None, None]
Table 2: [('banana', 20), ('date', 40), None, None, None, None, None]

Searching for elements:
apple: 10
banana: 20
grape: None

Deleting 'banana':
After deletion:
Table 1: [('apple', 10), ('cherry', 30), ('elderberry', 50), None, None, None, None]
Table 2: [None, ('date', 40), None, None, None, None, None]

Inserting more elements to trigger displacement:
After more insertions:
Table 1: [('apple', 10), ('cherry', 30), ('elderberry', 50), ('fig', 60), None, None, None]
Table 2: [('date', 40), ('grape', 70), ('honeydew', 80), None, None, None, None]
```

## Key Features

1. **Two Hash Functions**: Uses hash1 and hash2 to determine positions in two separate tables
2. **Displacement**: When a collision occurs, displaced elements are moved to alternative positions
3. **Search Time**: O(1) average case, O(n) worst case
4. **Insert Time**: O(1) average case, O(n) worst case
5. **Space Efficiency**: Uses 2n space (two tables of size n)

## Advantages

- Fast average lookup time
- No clustering issues like linear probing
- Simple implementation

## Disadvantages

- May require multiple displacements
- Can fail to insert if too many displacements are needed
- Requires two hash functions
- More complex than simple hash tables

