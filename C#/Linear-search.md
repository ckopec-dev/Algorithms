# Linear Search Algorithm in C#

## Implementation

```csharp
using System;

public class LinearSearch
{
    /// <summary>
    /// Performs linear search on an array to find the target element
    /// </summary>
    /// <param name="array">The array to search through</param>
    /// <param name="target">The element to search for</param>
    /// <returns>The index of the target element if found, otherwise -1</returns>
    public static int Search(int[] array, int target)
    {
        // Iterate through each element in the array
        for (int i = 0; i < array.Length; i++)
        {
            // If current element matches the target, return its index
            if (array[i] == target)
            {
                return i;
            }
        }
        
        // Target element not found
        return -1;
    }
    
    /// <summary>
    /// Alternative implementation using foreach loop
    /// </summary>
    /// <param name="array">The array to search through</param>
    /// <param name="target">The element to search for</param>
    /// <returns>The index of the target element if found, otherwise -1</returns>
    public static int SearchForeach(int[] array, int target)
    {
        int index = 0;
        foreach (int element in array)
        {
            if (element == target)
            {
                return index;
            }
            index++;
        }
        return -1;
    }
}

// Example usage
class Program
{
    static void Main()
    {
        // Create an example array
        int[] numbers = { 10, 25, 3, 47, 15, 8, 92, 33 };
        
        Console.WriteLine("Array: [" + string.Join(", ", numbers) + "]");
        
        // Test searching for existing element
        int target1 = 47;
        int result1 = LinearSearch.Search(numbers, target1);
        
        if (result1 != -1)
        {
            Console.WriteLine($"Element {target1} found at index: {result1}");
        }
        else
        {
            Console.WriteLine($"Element {target1} not found");
        }
        
        // Test searching for non-existing element
        int target2 = 100;
        int result2 = LinearSearch.Search(numbers, target2);
        
        if (result2 != -1)
        {
            Console.WriteLine($"Element {target2} found at index: {result2}");
        }
        else
        {
            Console.WriteLine($"Element {target2} not found");
        }
    }
}
```

## Output
```
Array: [10, 25, 3, 47, 15, 8, 92, 33]
Element 47 found at index: 3
Element 100 not found
```

## Algorithm Explanation

**Time Complexity:** O(n) - where n is the number of elements in the array
**Space Complexity:** O(1) - constant space usage

The linear search algorithm works by:
1. Starting from the first element of the array
2. Comparing each element with the target value
3. Returning the index if a match is found
4. Continuing until the end of the array is reached
5. Returning -1 if the target element is not found

This algorithm is simple and works on both sorted and unsorted arrays, but for large datasets, more efficient algorithms like binary search should be considered.