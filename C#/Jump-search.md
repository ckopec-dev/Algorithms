# Jump Search Algorithm in C#

Jump search is a searching algorithm that works on sorted arrays by jumping ahead by fixed steps and then performing a linear search in the identified block.

## Implementation

```csharp
using System;

public class JumpSearch
{
    /// <summary>
    /// Performs jump search on a sorted array
    /// </summary>
    /// <param name="arr">Sorted array to search</param>
    /// <param name="target">Element to search for</param>
    /// <returns>Index of target element if found, otherwise -1</returns>
    public static int JumpSearchAlgorithm(int[] arr, int target)
    {
        int n = arr.Length;
        
        // Handle edge cases
        if (n == 0) return -1;
        
        // Calculate optimal jump size
        int jumpSize = (int)Math.Sqrt(n);
        
        // Find the block where target may be present
        int prev = 0;
        while (arr[Math.Min(jumpSize, n) - 1] < target)
        {
            prev = jumpSize;
            jumpSize += (int)Math.Sqrt(n);
            
            // If we've gone past the array
            if (prev >= n)
                return -1;
        }
        
        // Linear search in the identified block
        for (int i = prev; i < Math.Min(jumpSize, n); i++)
        {
            if (arr[i] == target)
                return i;
        }
        
        return -1;
    }
    
    /// <summary>
    /// Alternative implementation with clearer step-by-step approach
    /// </summary>
    public static int JumpSearchAlternative(int[] arr, int target)
    {
        int n = arr.Length;
        if (n == 0) return -1;
        
        int step = (int)Math.Sqrt(n);
        int prev = 0;
        
        // Jump through the array
        while (arr[Math.Min(step, n) - 1] < target)
        {
            prev = step;
            step += (int)Math.Sqrt(n);
            
            if (prev >= n)
                return -1;
        }
        
        // Linear search in the block
        for (int i = prev; i < Math.Min(step, n); i++)
        {
            if (arr[i] == target)
                return i;
        }
        
        return -1;
    }
}

// Example usage
class Program
{
    static void Main()
    {
        // Sorted array for testing
        int[] arr = { 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610 };
        int target = 55;
        
        Console.WriteLine("Array: [" + string.Join(", ", arr) + "]");
        Console.WriteLine($"Searching for: {target}");
        
        // Perform jump search
        int result = JumpSearch.JumpSearchAlgorithm(arr, target);
        
        if (result != -1)
        {
            Console.WriteLine($"Element found at index: {result}");
        }
        else
        {
            Console.WriteLine("Element not found in array");
        }
        
        // Test with element not in array
        target = 100;
        Console.WriteLine($"\nSearching for: {target}");
        result = JumpSearch.JumpSearchAlgorithm(arr, target);
        
        if (result != -1)
        {
            Console.WriteLine($"Element found at index: {result}");
        }
        else
        {
            Console.WriteLine("Element not found in array");
        }
        
        // Test with first element
        target = 0;
        Console.WriteLine($"\nSearching for: {target}");
        result = JumpSearch.JumpSearchAlgorithm(arr, target);
        
        if (result != -1)
        {
            Console.WriteLine($"Element found at index: {result}");
        }
        else
        {
            Console.WriteLine("Element not found in array");
        }
    }
}
```

## Output
```
Array: [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610]
Searching for: 55
Element found at index: 10

Searching for: 100
Element not found in array

Searching for: 0
Element found at index: 0
```

## Algorithm Steps

1. **Calculate jump size**: `jumpSize = √n` where n is array length
2. **Jump through array**: Move in blocks of `jumpSize` until finding a block where `arr[jumpSize-1] >= target`
3. **Linear search**: Perform linear search within the identified block
4. **Return result**: Return index if found, otherwise return -1

## Time Complexity
- **Best Case**: O(1) - Element found at first position
- **Average Case**: O(√n)
- **Worst Case**: O(√n)

## Space Complexity
- O(1) - Only uses a constant amount of extra space

## Advantages
- More efficient than linear search for large sorted arrays
- Simpler than binary search implementation
- Works well with arrays that are too large for binary search

## Disadvantages
- Requires sorted array
- Less efficient than binary search in most cases
- Not suitable for very small arrays where linear search might be faster

