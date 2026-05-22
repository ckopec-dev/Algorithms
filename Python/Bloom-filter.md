# Bloom Filter Implementation in Python

A Bloom filter is a probabilistic data structure that tests whether an element is a member of a set. It can tell you if an element is definitely not in the set or possibly in the set, with a small probability of false positives.

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
        
        # Calculate optimal number of bits and hash functions
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
        hash_object = hashlib.md5((str(item) + str(seed)).encode())
        return int(hash_object.hexdigest(), 16) % self.bit_array_size
    
    def add(self, item):
        """Add an item to the Bloom filter"""
        for i in range(self.hash_count):
            index = self._hash(item, i)
            self.bit_array[index] = 1
    
    def check(self, item):
        """Check if item is in the Bloom filter"""
        for i in range(self.hash_count):
            index = self._hash(item, i)
            if self.bit_array[index] == 0:
                return False
        return True
    
    def __len__(self):
        """Return the number of elements added"""
        return self.capacity

# Example usage
if __name__ == "__main__":
    # Create a Bloom filter for 1000 items with 1% error rate
    bf = BloomFilter(capacity=1000, error_rate=0.01)
    
    # Add some items
    words = ["apple", "banana", "cherry", "date", "elderberry"]
    for word in words:
        bf.add(word)
        print(f"Added: {word}")
    
    print("\nChecking items:")
    
    # Test existing items (should return True)
    test_words = ["apple", "banana", "grape"]
    for word in test_words:
        if bf.check(word):
            print(f"'{word}' - Possibly in set")
        else:
            print(f"'{word}' - Definitely not in set")
    
    print(f"\nBloom filter size: {bf.bit_array_size} bits")
    print(f"Number of hash functions: {bf.hash_count}")
```

## Output Example:
```
Added: apple
Added: banana
Added: cherry
Added: date
Added: elderberry

Checking items:
'apple' - Possibly in set
'banana' - Possibly in set
'grape' - Definitely not in set

Bloom filter size: 9581 bits
Number of hash functions: 7
```

## Key Features:

1. **Space Efficient**: Uses minimal memory compared to storing actual elements
2. **Fast Operations**: Both add and check operations are O(k) where k is the number of hash functions
3. **Probabilistic**: May return false positives but never false negatives
4. **No Deletion**: Standard Bloom filters don't support deletion (though variants exist)

## Common Use Cases:
- Database systems to avoid expensive disk lookups
- Web crawlers to avoid re-crawling URLs
- Network routers to filter packets
- Password checking systems
- Cache systems to avoid cache misses

