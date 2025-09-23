# Reservoir Sampling Algorithm Example

Reservoir sampling is a family of randomized algorithms for randomly choosing a sample of k items from a list S containing n items, where n is either a very large or unknown number.

```python
import random

def reservoir_sampling(stream, k):
    """
    Implements the reservoir sampling algorithm.
    
    Args:
        stream: List of items to sample from
        k: Number of samples to select
    
    Returns:
        List containing k randomly selected items
    """
    # Initialize reservoir with first k elements
    reservoir = stream[:k]
    
    # Process remaining elements
    for i in range(k, len(stream)):
        # Generate a random index between 0 and i (inclusive)
        j = random.randint(0, i)
        
        # If the random index is less than k, replace an element in reservoir
        if j < k:
            reservoir[j] = stream[i]
    
    return reservoir

# Example usage
if __name__ == "__main__":
    # Example 1: Sample from a known list
    data = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    k = 3
    
    print("Original data:", data)
    print("Sample size:", k)
    
    # Get a random sample
    sample = reservoir_sampling(data, k)
    print("Reservoir sample:", sample)
    
    # Example 2: Simulate streaming data
    print("\n--- Streaming Example ---")
    large_stream = list(range(1, 1001))  # 1000 items
    sample_size = 5
    
    print(f"Processing stream of {len(large_stream)} items")
    print(f"Sample size: {sample_size}")
    
    # This works even when we don't know the total size beforehand
    streaming_sample = reservoir_sampling(large_stream, sample_size)
    print("Streaming sample:", streaming_sample)
```

## How it works:

1. **Initialization**: Fill the reservoir with the first k elements from the stream
2. **Processing**: For each subsequent element at position i (starting from k+1):
   - Generate a random number j between 0 and i (inclusive)
   - If j < k, replace the j-th element in the reservoir with the current element
3. **Result**: After processing all elements, the reservoir contains a uniform random sample

## Key Properties:

- **Time Complexity**: O(n) where n is the number of elements
- **Space Complexity**: O(k) for storing the sample
- **Uniformity**: Each element has equal probability (k/n) of being selected
- **Memory Efficient**: Works with streaming data and unknown total size

## Output Example:
```
Original data: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
Sample size: 3
Reservoir sample: [2, 7, 1]

--- Streaming Example ---
Processing stream of 1000 items
Sample size: 5
Streaming sample: [42, 187, 934, 267, 789]
```

