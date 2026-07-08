# Binary Search Algorithm in C#

Here's an implementation of the binary search algorithm in C#:

```csharp
using System;

public class BinarySearchExample
{
    /// <summary>
    /// Performs binary search on a sorted array
    /// </summary>
    /// <param name="array">Sorted array to search in</param>
    /// <param name="target">Element to search for</param>
    /// <returns>Index of target element if found, -1 if not found</returns>
    public static int BinarySearch(int[] array, int target)
    {
        int left = 0;
        int right = array.Length - 1;
        
        while (left <= right)
        {
            // Calculate middle index (avoiding integer overflow)
            int middle = left + (right - left) / 2;
            
            if (array[middle] == target)
            {
                return middle; // Element found, return index
            }
            else if (array[middle] < target)
            {
                left = middle + 1; // Search in right half
            }
            else
            {
                right = middle - 1; // Search in left half
            }
        }
        
        return -1; // Element not found
    }
    
    public static void Main(string[] args)
    {
        // Example usage
        int[] sortedArray = { 2, 5, 8, 12, 16, 23, 38, 45, 56, 67, 78, 89, 99 };
        
        Console.WriteLine("Sorted Array: [" + string.Join(", ", sortedArray) + "]");
        
        // Test cases
        int[] testValues = { 23, 45, 100, 2 };
        
        foreach (int value in testValues)
        {
            int result = BinarySearch(sortedArray, value);
            if (result != -1)
            {
                Console.WriteLine($"Element {value} found at index {result}");
            }
            else
            {
                Console.WriteLine($"Element {value} not found in array");
            }
        }
    }
}
```

## Output:
```
Sorted Array: [2, 5, 8, 12, 16, 23, 38, 45, 56, 67, 78, 89, 99]
Element 23 found at index 5
Element 45 found at index 7
Element 100 not found in array
Element 2 found at index 0
```

## Key Features:

- **Time Complexity**: O(log n)
- **Space Complexity**: O(1) - iterative implementation
- **Precondition**: Array must be sorted
- **Return Value**: Index of found element or -1 if not found
- **Overflow Prevention**: Uses `left + (right - left) / 2` instead of `(left + right) / 2`

## How it works:

1. Initialize `left` and `right` pointers to the start and end of the array
2. Calculate the middle index
3. Compare middle element with target:
   - If equal: return middle index
   - If target is greater: search right half
   - If target is smaller: search left half
4. Repeat until element found or search space is exhausted