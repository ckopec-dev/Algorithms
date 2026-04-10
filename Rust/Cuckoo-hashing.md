# Cuckoo Hashing in Rust

Here's a complete implementation of the Cuckoo Hashing algorithm in Rust:

```rust
use std::collections::HashMap;

pub struct CuckooHashTable<K, V> {
    table1: Vec<Option<(K, V)>>,
    table2: Vec<Option<(K, V)>>,
    capacity: usize,
    size: usize,
}

impl<K, V> CuckooHashTable<K, V>
where
    K: std::hash::Hash + Eq + Clone,
    V: Clone,
{
    pub fn new(capacity: usize) -> Self {
        let mut table1 = Vec::with_capacity(capacity);
        let mut table2 = Vec::with_capacity(capacity);
        
        table1.resize_with(capacity, || None);
        table2.resize_with(capacity, || None);
        
        CuckooHashTable {
            table1,
            table2,
            capacity,
            size: 0,
        }
    }
    
    // Simple hash function for demonstration
    fn hash1(&self, key: &K) -> usize {
        use std::hash::{Hash, Hasher};
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        key.hash(&mut hasher);
        (hasher.finish() % self.capacity as u64) as usize
    }
    
    // Second hash function
    fn hash2(&self, key: &K) -> usize {
        use std::hash::{Hash, Hasher};
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        key.hash(&mut hasher);
        (hasher.finish() / 2) % self.capacity as u64) as usize
    }
    
    // Insert a key-value pair
    pub fn insert(&mut self, key: K, value: V) -> Result<(), String> {
        // Check if key already exists
        if self.get(&key).is_some() {
            return Err("Key already exists".to_string());
        }
        
        // Try to insert the key-value pair
        let result = self.insert_helper(key, value);
        
        if result.is_ok() {
            self.size += 1;
        }
        
        result
    }
    
    fn insert_helper(&mut self, key: K, value: V) -> Result<(), String> {
        let mut current_key = key;
        let mut current_value = value;
        let mut table_index = 0; // Start with table1
        
        // Maximum number of iterations to avoid infinite loops
        let mut max_iterations = self.capacity;
        
        while max_iterations > 0 {
            match table_index {
                0 => {
                    let hash = self.hash1(&current_key);
                    if let Some(ref mut entry) = self.table1[hash] {
                        // Evict existing entry
                        let (evicted_key, evicted_value) = entry.take().unwrap();
                        *entry = Some((current_key, current_value));
                        current_key = evicted_key;
                        current_value = evicted_value;
                    } else {
                        self.table1[hash] = Some((current_key, current_value));
                        return Ok(());
                    }
                }
                1 => {
                    let hash = self.hash2(&current_key);
                    if let Some(ref mut entry) = self.table2[hash] {
                        // Evict existing entry
                        let (evicted_key, evicted_value) = entry.take().unwrap();
                        *entry = Some((current_key, current_value));
                        current_key = evicted_key;
                        current_value = evicted_value;
                    } else {
                        self.table2[hash] = Some((current_key, current_value));
                        return Ok(());
                    }
                }
                _ => unreachable!(),
            }
            
            table_index = 1 - table_index; // Switch between tables
            max_iterations -= 1;
        }
        
        // If we reach here, we have a cycle - need to rehash
        Err("Cuckoo hash table is full or has cycle".to_string())
    }
    
    // Get a value by key
    pub fn get(&self, key: &K) -> Option<&V> {
        let hash1 = self.hash1(key);
        let hash2 = self.hash2(key);
        
        if let Some((k, v)) = &self.table1[hash1] {
            if k == key {
                return Some(v);
            }
        }
        
        if let Some((k, v)) = &self.table2[hash2] {
            if k == key {
                return Some(v);
            }
        }
        
        None
    }
    
    // Remove a key-value pair
    pub fn remove(&mut self, key: &K) -> Option<V> {
        let hash1 = self.hash1(key);
        let hash2 = self.hash2(key);
        
        // Check table1
        if let Some((k, v)) = &self.table1[hash1] {
            if k == key {
                self.table1[hash1] = None;
                self.size -= 1;
                return Some(v.clone());
            }
        }
        
        // Check table2
        if let Some((k, v)) = &self.table2[hash2] {
            if k == key {
                self.table2[hash2] = None;
                self.size -= 1;
                return Some(v.clone());
            }
        }
        
        None
    }
    
    // Get the size of the hash table
    pub fn len(&self) -> usize {
        self.size
    }
    
    // Check if the hash table is empty
    pub fn is_empty(&self) -> bool {
        self.size == 0
    }
    
    // Get the capacity of the hash table
    pub fn capacity(&self) -> usize {
        self.capacity
    }
}

// Example usage
fn main() {
    let mut cuckoo_table = CuckooHashTable::<String, i32>::new(8);
    
    // Insert some key-value pairs
    println!("Inserting key-value pairs...");
    cuckoo_table.insert("apple".to_string(), 10).unwrap();
    cuckoo_table.insert("banana".to_string(), 20).unwrap();
    cuckoo_table.insert("cherry".to_string(), 30).unwrap();
    cuckoo_table.insert("date".to_string(), 40).unwrap();
    
    println!("Table size: {}", cuckoo_table.len());
    
    // Retrieve values
    println!("Value for 'apple': {:?}", cuckoo_table.get(&"apple".to_string()));
    println!("Value for 'banana': {:?}", cuckoo_table.get(&"banana".to_string()));
    
    // Remove an entry
    println!("Removing 'banana'...");
    let removed = cuckoo_table.remove(&"banana".to_string());
    println!("Removed value: {:?}", removed);
    println!("Table size after removal: {}", cuckoo_table.len());
    
    // Try to get removed entry
    println!("Value for 'banana' after removal: {:?}", cuckoo_table.get(&"banana".to_string()));
    
    // Test with numeric keys
    let mut numeric_table = CuckooHashTable::<i32, String>::new(4);
    numeric_table.insert(1, "one".to_string()).unwrap();
    numeric_table.insert(2, "two".to_string()).unwrap();
    numeric_table.insert(3, "three".to_string()).unwrap();
    
    println!("Numeric table:");
    println!("Value for 1: {:?}", numeric_table.get(&1));
    println!("Value for 2: {:?}", numeric_table.get(&2));
    println!("Value for 3: {:?}", numeric_table.get(&3));
}
```

## Key Features of this Implementation:

1. **Two Hash Tables**: Uses two separate hash tables to store key-value pairs
2. **Two Hash Functions**: Implements two different hash functions for better distribution
3. **Cuckoo Eviction**: When a collision occurs, it evicts the existing entry and places it in the other table
4. **Cycle Detection**: Prevents infinite loops during insertion
5. **Generic Types**: Works with any hashable key type and any value type
6. **Basic Operations**: Insert, get, remove, size checking

## Time Complexity:
- **Insert**: O(1) average, O(∞) worst case (due to potential rehashing)
- **Search**: O(1) average
- **Delete**: O(1) average

## Space Complexity:
- O(n) where n is the number of elements stored

This implementation provides a working example of Cuckoo Hashing that demonstrates the core concepts while maintaining good performance characteristics for most practical use cases.

