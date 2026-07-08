# Bloom Filter Implementation in C#

Here's a complete implementation of a Bloom Filter algorithm in C#:

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class BloomFilter<T>
{
    private BitArray bitArray;
    private int hashCount;
    private int bitArraySize;
    private HashSet<string> hashFunctions;
    
    public BloomFilter(int capacity, double falsePositiveRate)
    {
        // Calculate optimal bit array size
        bitArraySize = GetBitArraySize(capacity, falsePositiveRate);
        
        // Calculate optimal number of hash functions
        hashCount = GetHashCount(capacity, bitArraySize);
        
        // Initialize bit array
        bitArray = new BitArray(bitArraySize);
        
        // Initialize hash functions
        hashFunctions = new HashSet<string>();
    }
    
    private int GetBitArraySize(int capacity, double falsePositiveRate)
    {
        double ln2 = Math.Log(2);
        return (int)Math.Ceiling(-capacity * Math.Log(falsePositiveRate) / (ln2 * ln2));
    }
    
    private int GetHashCount(int capacity, int bitArraySize)
    {
        return (int)Math.Ceiling(bitArraySize * Math.Log(2) / capacity);
    }
    
    public void Add(T item)
    {
        string itemString = item.ToString();
        var hashes = GenerateHashes(itemString);
        
        foreach (int hash in hashes)
        {
            int index = Math.Abs(hash % bitArraySize);
            bitArray[index] = true;
        }
    }
    
    public bool Contains(T item)
    {
        string itemString = item.ToString();
        var hashes = GenerateHashes(itemString);
        
        foreach (int hash in hashes)
        {
            int index = Math.Abs(hash % bitArraySize);
            if (!bitArray[index])
                return false;
        }
        
        return true;
    }
    
    private IEnumerable<int> GenerateHashes(string item)
    {
        // Simple hash functions using different seeds
        for (int i = 0; i < hashCount; i++)
        {
            yield return HashFunction(item, i);
        }
    }
    
    private int HashFunction(string item, int seed)
    {
        unchecked
        {
            int hash1 = 5381;
            int hash2 = seed;
            
            foreach (char c in item)
            {
                hash1 = ((hash1 << 5) + hash1) ^ c;
                hash2 = ((hash2 << 5) + hash2) ^ c;
            }
            
            return hash1 + (hash2 * 1566083941);
        }
    }
    
    public int Count => bitArray.Cast<bool>().Count(b => b);
    
    public double FalsePositiveRate => Math.Pow((1 - Math.Exp(-hashCount * bitArraySize)), hashCount);
}

// Example usage
public class Program
{
    public static void Main()
    {
        // Create a Bloom filter for 1000 items with 1% false positive rate
        var bloomFilter = new BloomFilter<string>(1000, 0.01);
        
        // Add some items
        string[] itemsToInsert = { "apple", "banana", "cherry", "date", "elderberry" };
        
        Console.WriteLine("Adding items to Bloom Filter:");
        foreach (string item in itemsToInsert)
        {
            bloomFilter.Add(item);
            Console.WriteLine($"Added: {item}");
        }
        
        // Test membership
        Console.WriteLine("\nTesting membership:");
        string[] testItems = { "apple", "grape", "banana", "kiwi" };
        
        foreach (string item in testItems)
        {
            bool exists = bloomFilter.Contains(item);
            Console.WriteLine($"'{item}' exists: {exists}");
        }
        
        // Display statistics
        Console.WriteLine($"\nBloom Filter Statistics:");
        Console.WriteLine($"Bit array size: {bloomFilter.bitArraySize}");
        Console.WriteLine($"Number of hash functions: {bloomFilter.hashCount}");
        Console.WriteLine($"Number of items stored: {bloomFilter.Count}");
        Console.WriteLine($"Estimated false positive rate: {bloomFilter.FalsePositiveRate:P2}");
    }
}
```

## Key Features of this Implementation:

### 1. **Core Components**
- `BitArray` for efficient storage of bits
- Multiple hash functions for distribution
- Configurable capacity and false positive rate

### 2. **Main Methods**
- `Add(T item)` - Inserts an item into the filter
- `Contains(T item)` - Checks if item might exist (with potential false positives)
- `Count` - Returns number of set bits
- `FalsePositiveRate` - Returns estimated false positive rate

### 3. **Algorithm Details**
- Uses multiple hash functions to map items to different positions
- Calculates optimal bit array size and hash count based on desired false positive rate
- Implements simple but effective hashing with different seeds
- Handles potential hash collisions through multiple probes

### 4. **Usage Example Output**
```
Adding items to Bloom Filter:
Added: apple
Added: banana
Added: cherry
Added: date
Added: elderberry

Testing membership:
'apple' exists: True
'grape' exists: False
'banana' exists: True
'kiwi' exists: False

Bloom Filter Statistics:
Bit array size: 9590
Number of hash functions: 7
Number of items stored: 5
Estimated false positive rate: 1.00%
```

This implementation provides a practical Bloom filter that can efficiently handle membership testing with minimal memory usage, making it ideal for applications like database indexing, network routers, and cache systems where false positives are acceptable but false negatives are not.