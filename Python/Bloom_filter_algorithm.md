# Bloom Filter Implementation in Python

A Bloom filter is a probabilistic data structure that tests whether an element is a member of a set. It can tell you if an element *might* be in the set or if it's *definitely not* in the set.

```python
import hashlib
import math

class BloomFilter:
    def __init__(self, capacity, error_rate):
        """
        Initialize Bloom Filter
        
        Args:
            capacity: Expected number of elements
            error_rate: Desired false positive rate (0 < error_rate < 1)
        """
        self.capacity = capacity
        self.error_rate = error_rate
        
        # Calculate optimal parameters
        self.bit_array_size = self._get_size()
        self.hash_count = self._get_hash_count()
        
        # Initialize bit array
        self.bit_array = [0] * self.bit_array_size
    
    def _get_size(self):
        """Calculate optimal bit array size"""
        m = -(self.capacity * math.log(self.error_rate)) / (math.log(2) ** 2)
        return int(m)
    
    def _get_hash_count(self):
        """Calculate optimal number of hash functions"""
        k = (self.bit_array_size * math.log(2)) / self.capacity
        return int(k)
    
    def _hash(self, item, seed):
        """Generate hash with given seed"""
        hash_obj = hashlib.md5((str(item) + str(seed)).encode())
        return int(hash_obj.hexdigest(), 16) % self.bit_array_size
    
    def add(self, item):
        """Add an item to the Bloom filter"""
        for i in range(self.hash_count):
            index = self._hash(item, i)
            self.bit_array[index] = 1
    
    def contains(self, item):
        """Check if item might be in the set"""
        for i in range(self.hash_count):
            index = self._hash(item, i)
            if self.bit_array[index] == 0:
                return False
        return True
    
    def get_bit_array_size(self):
        """Return the size of the bit array"""
        return self.bit_array_size

# Example usage
if __name__ == "__main__":
    # Create a Bloom filter for 1000 items with 1% error rate
    bf = BloomFilter(1000, 0.01)
    
    # Add some items
    words = ["apple", "banana", "cherry", "date", "elderberry"]
    for word in words:
        bf.add(word)
        print(f"Added: {word}")
    
    print(f"\nBloom Filter size: {bf.get_bit_array_size()} bits")
    print(f"Number of hash functions: {bf.hash_count}")
    
    # Test membership
    test_words = ["apple", "fig", "grape", "banana"]
    
    print("\nTesting membership:")
    for word in test_words:
        if bf.contains(word):
            print(f"'{word}' might be in the set")
        else:
            print(f"'{word}' is definitely not in the set")
    
    # Demonstrate false positives
    print("\nFalse positive demonstration:")
    false_positives = 0
    for i in range(100):
        test_word = f"test_{i}"
        if bf.contains(test_word):
            false_positives += 1
    
    print(f"False positives out of 100 tests: {false_positives}")
```

## Key Features:

- **Space Efficient**: Uses only a bit array and multiple hash functions
- **Probabilistic**: Can have false positives but never false negatives
- **Fast Operations**: Both add and lookup are O(k) where k is number of hash functions
- **Configurable**: Size and error rate can be adjusted

## How it works:

1. **Initialization**: Calculate optimal bit array size and number of hash functions
2. **Add Operation**: Hash the item with multiple hash functions and set corresponding bits to 1
3. **Lookup Operation**: Hash the item with same hash functions and check if all corresponding bits are 1

## Time Complexity:
- Add: O(k) where k is number of hash functions
- Lookup: O(k) where k is number of hash functions

## Space Complexity:
- O(m) where m is the size of the bit array

The Bloom filter trades accuracy for space efficiency and speed, making it ideal for applications like web crawlers, databases, and network routers where memory is limited but false positive rates can be tolerated.

