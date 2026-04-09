# Reservoir Sampling Algorithm in C#

Reservoir sampling is a family of algorithms for randomly sampling k items from a large dataset of unknown size, typically used when the dataset is too large to fit in memory or when we want to process data streams.

## Implementation

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class ReservoirSampling
{
    /// <summary>
    /// Reservoir sampling algorithm to select k random elements from a stream
    /// </summary>
    /// <typeparam name="T">Type of elements</typeparam>
    /// <param name="stream">Input stream of elements</param>
    /// <param name="k">Number of samples to select</param>
    /// <returns>List of randomly selected elements</returns>
    public static List<T> Sample<T>(IEnumerable<T> stream, int k)
    {
        if (stream == null)
            throw new ArgumentNullException(nameof(stream));
        
        if (k <= 0)
            return new List<T>();

        var reservoir = new List<T>();
        int count = 0;

        foreach (T item in stream)
        {
            count++;

            if (reservoir.Count < k)
            {
                // Fill the reservoir with first k elements
                reservoir.Add(item);
            }
            else
            {
                // Replace elements with gradually decreasing probability
                int index = new Random().Next(0, count);
                if (index < k)
                {
                    reservoir[index] = item;
                }
            }
        }

        return reservoir;
    }

    /// <summary>
    /// Alternative implementation using a more explicit approach
    /// </summary>
    /// <typeparam name="T">Type of elements</typeparam>
    /// <param name="stream">Input stream of elements</param>
    /// <param name="k">Number of samples to select</param>
    /// <returns>List of randomly selected elements</returns>
    public static List<T> SampleAlternative<T>(IEnumerable<T> stream, int k)
    {
        if (stream == null)
            throw new ArgumentNullException(nameof(stream));
        
        if (k <= 0)
            return new List<T>();

        var reservoir = new List<T>();
        var random = new Random();

        int i = 0;
        foreach (T item in stream)
        {
            if (i < k)
            {
                // Fill reservoir with first k items
                reservoir.Add(item);
            }
            else
            {
                // Generate random index between 0 and i (inclusive)
                int j = random.Next(0, i + 1);
                
                // Replace element with probability k/i
                if (j < k)
                {
                    reservoir[j] = item;
                }
            }
            i++;
        }

        return reservoir;
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        // Example 1: Sampling from a known array
        var numbers = Enumerable.Range(1, 100).ToList();
        var sample = ReservoirSampling.Sample(numbers, 10);
        
        Console.WriteLine("Sample of 10 numbers from 1-100:");
        Console.WriteLine(string.Join(", ", sample));
        
        // Example 2: Sampling from a large dataset
        var largeDataset = Enumerable.Range(1, 10000);
        var largeSample = ReservoirSampling.Sample(largeDataset, 5);
        
        Console.WriteLine("\nSample of 5 numbers from 1-10000:");
        Console.WriteLine(string.Join(", ", largeSample));
        
        // Example 3: Sampling from string stream
        var words = new List<string> { "apple", "banana", "cherry", "date", "elderberry", 
                                     "fig", "grape", "honeydew", "kiwi", "lemon" };
        var wordSample = ReservoirSampling.Sample(words, 3);
        
        Console.WriteLine("\nSample of 3 words:");
        Console.WriteLine(string.Join(", ", wordSample));
    }
}
```

## How It Works

The algorithm works as follows:

1. **First k elements**: Store the first k elements in the reservoir
2. **For each subsequent element i (i > k)**:
   - Generate a random number `j` between 0 and i-1 (inclusive)
   - If `j < k`, replace the j-th element in the reservoir with the current element
   - This ensures each element has equal probability of being selected

## Key Properties

- **Time Complexity**: O(n) where n is the total number of elements
- **Space Complexity**: O(k) where k is the number of samples
- **Probability**: Each element has equal probability of 1/n of being selected
- **Memory Efficient**: Works with streaming data of unknown size
- **No Rejection**: All elements are processed, no elements are rejected

## Output Example

```
Sample of 10 numbers from 1-100:
23, 67, 12, 89, 45, 78, 34, 91, 56, 28

Sample of 5 numbers from 1-10000:
4234, 8765, 1234, 5678, 9012

Sample of 3 words:
banana, grape, lemon
```

This implementation is particularly useful when dealing with large datasets that cannot fit in memory or when processing data streams where the total size is unknown.

