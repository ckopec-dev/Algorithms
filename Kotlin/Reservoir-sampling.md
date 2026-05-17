# Reservoir Sampling Algorithm in Kotlin

Reservoir sampling is a family of algorithms for randomly sampling k items from a large dataset of unknown size. Here's an implementation in Kotlin:

```kotlin
import kotlin.random.Random

class ReservoirSampling<T> {
    
    /**
     * Reservoir sampling algorithm to select k random elements from a stream
     * @param stream The input stream of elements
     * @param k The number of elements to sample
     * @return A list containing k randomly selected elements
     */
    fun reservoirSample(stream: List<T>, k: Int): List<T> {
        if (k <= 0) return emptyList()
        if (stream.isEmpty()) return emptyList()
        
        val reservoir = mutableListOf<T>()
        
        // Fill the reservoir with first k elements
        for (i in stream.indices) {
            if (i < k) {
                reservoir.add(stream[i])
            } else {
                // Generate a random index between 0 and i (inclusive)
                val randomIndex = Random.nextInt(i + 1)
                
                // If the random index is less than k, replace an element in reservoir
                if (randomIndex < k) {
                    reservoir[randomIndex] = stream[i]
                }
            }
        }
        
        return reservoir
    }
    
    /**
     * Alternative implementation for streaming data (when we don't have all data at once)
     */
    fun reservoirSampleStreaming(dataStream: Sequence<T>, k: Int): List<T> {
        if (k <= 0) return emptyList()
        
        val reservoir = mutableListOf<T>()
        var count = 0
        
        for (item in dataStream) {
            count++
            
            if (reservoir.size < k) {
                reservoir.add(item)
            } else {
                val randomIndex = Random.nextInt(count)
                if (randomIndex < k) {
                    reservoir[randomIndex] = item
                }
            }
        }
        
        return reservoir
    }
}

// Example usage
fun main() {
    val reservoirSampler = ReservoirSampling<Int>()
    
    // Example 1: Sampling from a known list
    val largeDataset = (1..1000).toList()
    val sample = reservoirSampler.reservoirSample(largeDataset, 5)
    
    println("Sample of 5 elements from dataset of 1000:")
    println(sample)
    
    // Example 2: Streaming data
    val streamingData = sequence {
        for (i in 1..100) {
            yield(i)
        }
    }
    
    val streamingSample = reservoirSampler.reservoirSampleStreaming(streamingData, 3)
    println("\nSample from streaming data:")
    println(streamingSample)
    
    // Example 3: Edge cases
    println("\nEdge cases:")
    println("Empty list: ${reservoirSampler.reservoirSample(emptyList(), 3)}")
    println("k = 0: ${reservoirSampler.reservoirSample(listOf(1, 2, 3), 0)}")
    println("k > size: ${reservoirSampler.reservoirSample(listOf(1, 2), 5)}")
}
```

## How it works:

1. **Initialization**: Fill the reservoir with the first k elements from the dataset
2. **Sampling**: For each subsequent element at position i (where i ≥ k):
   - Generate a random number between 0 and i (inclusive)
   - If the random number is less than k, replace the element at that random index in the reservoir with the current element
3. **Result**: After processing all elements, the reservoir contains k randomly selected elements

## Key Properties:

- **Time Complexity**: O(n) where n is the size of the dataset
- **Space Complexity**: O(k) for storing the reservoir
- **Unbiased**: Each element has an equal probability of being selected
- **Memory Efficient**: Works with streaming data and unknown dataset size

## Output Example:
```
Sample of 5 elements from dataset of 1000:
[123, 456, 789, 234, 567]

Sample from streaming data:
[42, 87, 15]

Edge cases:
Empty list: []
k = 0: []
k > size: [1, 2]
```

