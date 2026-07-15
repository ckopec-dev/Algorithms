# Exponential Search Algorithm in C#

Exponential search is a searching algorithm that works by first finding a range where the target element might be located, then performing a binary search within that range.

## Implementation

```csharp
using System;

public class ExponentialSearch
{
    /// <summary>
    /// Performs exponential search on a sorted array
    /// </summary>
    /// <param name="arr">Sorted array to search in</param>
    /// <param name="target">Element to search for</param>
    /// <returns>Index of target element if found, otherwise -1</returns>
    public static int Search(int[] arr, int target)
    {
        // Handle edge case of empty array
        if (arr == null || arr.Length == 0)
            return -1;
        
        // If target is at the first position
        if (arr[0] == target)
            return 0;
        
        // Find range where target may be present
        int bound = 1;
        while (bound < arr.Length && arr[bound] < target)
        {
            bound *= 2;
        }
        
        // Perform binary search in the range [bound/2, min(bound, length-1)]
        return BinarySearch(arr, target, bound / 2, Math.Min(bound, arr.Length - 1));
    }
    
    /// <summary>
    /// Performs binary search in a given range
    /// </summary>
    /// <param name="arr">Array to search in</param>
    /// <param name="target">Element to search for</param>
    /// <param name="left">Left boundary of search range</param>
    /// <param name="right">Right boundary of search range</param>
    /// <returns>Index of target element if found, otherwise -1</returns>
    private static int BinarySearch(int[] arr, int target, int left, int right)
    {
        while (left <= right)
        {
            int mid = left + (right - left) / 2;
            
            if (arr[mid] == target)
                return mid;
            else if (arr[mid] < target)
                left = mid + 1;
            else
                right = mid - 1;
        }
        
        return -1;
    }
}

// Example usage
class Program
{
    static void Main()
    {
        int[] arr = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 15, 20, 25, 30 };
        
        // Test cases
        Console.WriteLine("Array: [" + string.Join(", ", arr) + "]");
        Console.WriteLine();
        
        int[] testValues = { 1, 5, 15, 30, 25, 100 };
        
        foreach (int target in testValues)
        {
            int result = ExponentialSearch.Search(arr, target);
            if (result != -1)
                Console.WriteLine($"Element {target} found at index: {result}");
            else
                Console.WriteLine($"Element {target} not found in array");
        }
    }
}
```

## Output
```
Array: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 15, 20, 25, 30]

Element 1 found at index: 0
Element 5 found at index: 4
Element 15 found at index: 10
Element 30 found at index: 13
Element 25 found at index: 12
Element 100 not found in array
```

## How it works:

1. **Initial Check**: First check if the target is at the first position
2. **Find Range**: Keep doubling the index until we find an element greater than or equal to the target
3. **Binary Search**: Perform binary search in the identified range

## Time Complexity:
- **Best Case**: O(1) - when element is found at the first position
- **Average/Worst Case**: O(log n) - where n is the number of elements

## Space Complexity:
- O(1) - only using a constant amount of extra space

## Advantages:
- Works well for unbounded or very large arrays
- No need to know the array size beforehand
- Efficient for small arrays as well as large ones

## Disadvantages:
- Requires the array to be sorted
- Slightly more overhead than binary search due to the initial exponential search phase