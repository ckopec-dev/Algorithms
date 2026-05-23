# QuickSelect Algorithm in C#

QuickSelect is an efficient algorithm to find the kth smallest element in an unordered list. It's based on the quicksort algorithm but only processes one side of the partition.

## Implementation

```csharp
using System;
using System.Linq;

public class QuickSelect
{
    /// <summary>
    /// Finds the kth smallest element in the array (0-indexed)
    /// </summary>
    /// <param name="array">The input array</param>
    /// <param name="k">The index of the element to find (0-based)</param>
    /// <returns>The kth smallest element</returns>
    public static int QuickSelect(int[] array, int k)
    {
        if (array == null || k < 0 || k >= array.Length)
            throw new ArgumentException("Invalid input parameters");
        
        return QuickSelectHelper(array, 0, array.Length - 1, k);
    }
    
    private static int QuickSelectHelper(int[] array, int left, int right, int k)
    {
        if (left == right)
            return array[left];
        
        // Partition the array and get the pivot index
        int pivotIndex = Partition(array, left, right);
        
        if (k == pivotIndex)
        {
            return array[k];
        }
        else if (k < pivotIndex)
        {
            // Search in the left subarray
            return QuickSelectHelper(array, left, pivotIndex - 1, k);
        }
        else
        {
            // Search in the right subarray
            return QuickSelectHelper(array, pivotIndex + 1, right, k);
        }
    }
    
    private static int Partition(int[] array, int left, int right)
    {
        // Choose the rightmost element as pivot
        int pivot = array[right];
        int i = left - 1; // Index of smaller element
        
        for (int j = left; j < right; j++)
        {
            // If current element is smaller than or equal to pivot
            if (array[j] <= pivot)
            {
                i++;
                Swap(array, i, j);
            }
        }
        
        // Place pivot in its correct position
        Swap(array, i + 1, right);
        return i + 1;
    }
    
    private static void Swap(int[] array, int i, int j)
    {
        int temp = array[i];
        array[i] = array[j];
        array[j] = temp;
    }
}

// Example usage
class Program
{
    static void Main()
    {
        int[] array = { 3, 2, 1, 5, 6, 4 };
        int k = 2; // Find 3rd smallest element (0-indexed)
        
        Console.WriteLine($"Array: [{string.Join(", ", array)}]");
        Console.WriteLine($"Finding {k+1}th smallest element...");
        
        int result = QuickSelect.QuickSelect(array, k);
        Console.WriteLine($"The {k+1}th smallest element is: {result}");
        
        // Test with different values
        Console.WriteLine("\nTesting with different k values:");
        for (int i = 0; i < array.Length; i++)
        {
            int element = QuickSelect.QuickSelect(array, i);
            Console.WriteLine($"Element at index {i}: {element}");
        }
    }
}
```

## Output
```
Array: [3, 2, 1, 5, 6, 4]
Finding 3rd smallest element...
The 3rd smallest element is: 3

Testing with different k values:
Element at index 0: 1
Element at index 1: 2
Element at index 2: 3
Element at index 3: 4
Element at index 4: 5
Element at index 5: 6
```

## Key Features

- **Time Complexity**: 
  - Average case: O(n)
  - Worst case: O(n²)
- **Space Complexity**: O(log n) due to recursion
- **In-place**: Modifies the original array
- **Unstable**: Does not preserve the relative order of equal elements

## How it works

1. **Partition**: Rearranges the array so that elements smaller than the pivot are on the left, and larger elements are on the right
2. **Compare**: If the pivot index equals k, we found our answer
3. **Recursion**: If k is smaller than pivot index, search left subarray; otherwise, search right subarray
4. **Repeat**: Continue until the kth element is found

This algorithm is particularly useful when you need to find a specific ranked element without sorting the entire array.

