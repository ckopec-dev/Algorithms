# Cuckoo Hashing Algorithm in JavaScript

Cuckoo hashing is a hash table algorithm that uses two hash functions and two arrays to resolve collisions. When a collision occurs, it "kicks out" the existing element and places it in its alternative location.

```javascript
class CuckooHash {
    constructor(size = 16) {
        this.size = size;
        this.table1 = new Array(size).fill(null);
        this.table2 = new Array(size).fill(null);
        this.maxIterations = 100; // Prevent infinite loops
    }

    // Two hash functions
    hash1(key) {
        return Math.abs(hashCode(key)) % this.size;
    }

    hash2(key) {
        return Math.abs(hashCode(key) * 31) % this.size;
    }

    // Simple hash function for demonstration
    hashCode(str) {
        let hash = 0;
        for (let i = 0; i < str.length; i++) {
            const char = str.charCodeAt(i);
            hash = ((hash << 5) - hash) + char;
            hash = hash & hash; // Convert to 32bit integer
        }
        return hash;
    }

    // Insert a key-value pair
    insert(key, value) {
        let currentKey = key;
        let currentValue = value;
        let iteration = 0;

        while (iteration < this.maxIterations) {
            // Try to insert in table1
            const index1 = this.hash1(currentKey);
            if (this.table1[index1] === null) {
                this.table1[index1] = { key: currentKey, value: currentValue };
                return true;
            }

            // If table1 is full, kick out the existing element
            const temp = this.table1[index1];
            this.table1[index1] = { key: currentKey, value: currentValue };

            // Try to insert the kicked out element in table2
            currentKey = temp.key;
            currentValue = temp.value;

            const index2 = this.hash2(currentKey);
            if (this.table2[index2] === null) {
                this.table2[index2] = { key: currentKey, value: currentValue };
                return true;
            }

            // If table2 is also full, kick out the existing element
            const temp2 = this.table2[index2];
            this.table2[index2] = { key: currentKey, value: currentValue };

            currentKey = temp2.key;
            currentValue = temp2.value;

            iteration++;
        }

        console.log("Cuckoo hashing failed - maximum iterations reached");
        return false;
    }

    // Search for a key
    search(key) {
        const index1 = this.hash1(key);
        if (this.table1[index1] && this.table1[index1].key === key) {
            return this.table1[index1].value;
        }

        const index2 = this.hash2(key);
        if (this.table2[index2] && this.table2[index2].key === key) {
            return this.table2[index2].value;
        }

        return null;
    }

    // Delete a key
    delete(key) {
        const index1 = this.hash1(key);
        if (this.table1[index1] && this.table1[index1].key === key) {
            this.table1[index1] = null;
            return true;
        }

        const index2 = this.hash2(key);
        if (this.table2[index2] && this.table2[index2].key === key) {
            this.table2[index2] = null;
            return true;
        }

        return false;
    }

    // Display the hash tables
    display() {
        console.log("Table 1:", this.table1);
        console.log("Table 2:", this.table2);
    }
}

// Example usage:
const cuckoo = new CuckooHash(8);

console.log("Inserting elements:");
cuckoo.insert("apple", 5);
cuckoo.insert("banana", 3);
cuckoo.insert("cherry", 8);
cuckoo.insert("date", 2);
cuckoo.insert("elderberry", 7);

console.log("\nHash tables after insertion:");
cuckoo.display();

console.log("\nSearching for elements:");
console.log("apple:", cuckoo.search("apple"));     // Should return 5
console.log("banana:", cuckoo.search("banana"));   // Should return 3
console.log("grape:", cuckoo.search("grape"));     // Should return null

console.log("\nDeleting 'banana':");
cuckoo.delete("banana");
console.log("After deletion:");
cuckoo.display();

console.log("\nSearching after deletion:");
console.log("banana:", cuckoo.search("banana"));   // Should return null
```

## How Cuckoo Hashing Works:

1. **Two Tables**: Uses two hash tables (table1 and table2)
2. **Two Hash Functions**: Each key is hashed using both functions to determine possible locations
3. **Insertion Process**:
   - Try to place the element in its primary location
   - If occupied, kick out the existing element and try to place it in its alternative location
   - Continue this process until either placement succeeds or maximum iterations are reached
4. **Collision Resolution**: Instead of chaining or open addressing, it uses the "kicking" mechanism
5. **Search**: Check both tables using both hash functions

## Time Complexity:
- **Insert**: O(1) average case, O(∞) worst case (with high probability)
- **Search**: O(1) average case
- **Delete**: O(1) average case

## Space Complexity: O(n)

This implementation demonstrates the core principles of cuckoo hashing with two tables and two hash functions, showing how elements are moved between tables during insertion to resolve collisions.

