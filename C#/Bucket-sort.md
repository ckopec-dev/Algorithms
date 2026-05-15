# Bucket Sort Algorithm in C#

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class BucketSort
{
    public static void Sort(float[] array)
    {
        if (array == null || array.Length <= 1)
            return;

        int bucketCount = array.Length;
        List<List<float>> buckets = new List<List<float>>();

        // Create empty buckets
        for (int i = 0; i < bucketCount; i++)
        {
            buckets.Add(new List<float>());
        }

        // Distribute elements into buckets
        foreach (float num in array)
        {
            int bucketIndex = (int)(num * bucketCount);
            if (bucketIndex >= bucketCount)
                bucketIndex = bucketCount - 1;
            buckets[bucketIndex].Add(num);
        }

        // Sort individual buckets and concatenate them
        int index = 0;
        foreach (List<float> bucket in buckets)
        {
            if (bucket.Count > 0)
            {
                bucket.Sort(); // Use built-in sort for each bucket
                foreach (float num in bucket)
                {
                    array[index++] = num;
                }
            }
        }
    }

    // Alternative implementation with custom bucket size
    public static void SortWithCustomBuckets(float[] array, int bucketSize = 10)
    {
        if (array == null || array.Length <= 1)
            return;

        // Find min and max values
        float min = array.Min();
        float max = array.Max();

        // Calculate number of buckets
        int bucketCount = (int)Math.Ceiling((max - min + 1) / (float)bucketSize);

        // Create buckets
        List<List<float>> buckets = new List<List<float>>();
        for (int i = 0; i < bucketCount; i++)
        {
            buckets.Add(new List<float>());
        }

        // Distribute elements into buckets
        foreach (float num in array)
        {
            int bucketIndex = (int)((num - min) / bucketSize);
            if (bucketIndex >= bucketCount)
                bucketIndex = bucketCount - 1;
            buckets[bucketIndex].Add(num);
        }

        // Sort and merge buckets
        int index = 0;
        foreach (List<float> bucket in buckets)
        {
            if (bucket.Count > 0)
            {
                bucket.Sort();
                foreach (float num in bucket)
                {
                    array[index++] = num;
                }
            }
        }
    }
}

// Example usage
class Program
{
    static void Main()
    {
        // Example 1: Basic bucket sort
        float[] array1 = { 0.42f, 0.32f, 0.33f, 0.52f, 0.37f, 0.47f, 0.51f };
        Console.WriteLine("Original array: [" + string.Join(", ", array1) + "]");
        
        BucketSort.Sort(array1);
        Console.WriteLine("Sorted array:   [" + string.Join(", ", array1) + "]");
        
        Console.WriteLine();
        
        // Example 2: Custom bucket size
        float[] array2 = { 0.897f, 0.565f, 0.656f, 0.1234f, 0.665f, 0.3434f };
        Console.WriteLine("Original array: [" + string.Join(", ", array2) + "]");
        
        BucketSort.SortWithCustomBuckets(array2, 3);
        Console.WriteLine("Sorted array:   [" + string.Join(", ", array2) + "]");
    }
}
```

## Output:
```
Original array: [0.42, 0.32, 0.33, 0.52, 0.37, 0.47, 0.51]
Sorted array:   [0.32, 0.33, 0.37, 0.42, 0.47, 0.51, 0.52]

Original array: [0.897, 0.565, 0.656, 0.1234, 0.665, 0.3434]
Sorted array:   [0.1234, 0.3434, 0.565, 0.656, 0.665, 0.897]
```

## Algorithm Explanation:

1. **Bucket Creation**: Create empty buckets based on the range of input values
2. **Distribution**: Place each element into the appropriate bucket
3. **Sorting**: Sort each individual bucket (using built-in sort or another algorithm)
4. **Concatenation**: Combine all sorted buckets to form the final sorted array

## Time Complexity:
- **Best Case**: O(n + k) - when elements are uniformly distributed
- **Average Case**: O(n + k) 
- **Worst Case**: O(n²) - when all elements are placed in one bucket

## Space Complexity: O(n + k)

## Key Features:
- Works well with uniformly distributed data
- Stable sorting algorithm
- Efficient when data is evenly spread across range
- Can be adapted for different bucket sizes

