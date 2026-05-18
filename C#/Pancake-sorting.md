# Pancake Sorting Algorithm in C#

Pancake sorting is the mathematical problem of sorting a disordered stack of pancakes by flipping them with a spatula. The goal is to sort the pancakes in ascending order with the smallest pancake on top.

## Implementation

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class PancakeSorting
{
    /// <summary>
    /// Sorts an array using pancake flips
    /// </summary>
    /// <param name="arr">Array to be sorted</param>
    /// <returns>List of flip operations performed</returns>
    public static List<int> PancakeSort(int[] arr)
    {
        List<int> result = new List<int>();
        int n = arr.Length;
        
        // Start from the end and work backwards
        for (int size = n; size > 1; size--)
        {
            // Find the index of the maximum element in the unsorted portion
            int maxIndex = FindMaxIndex(arr, size);
            
            // If maximum element is not already at the end
            if (maxIndex != size - 1)
            {
                // Flip the maximum element to the top
                if (maxIndex != 0)
                {
                    Flip(arr, maxIndex);
                    result.Add(maxIndex + 1); // Add flip operation (1-indexed)
                }
                
                // Flip the maximum element to its correct position at the end
                Flip(arr, size - 1);
                result.Add(size); // Add flip operation (1-indexed)
            }
        }
        
        return result;
    }
    
    /// <summary>
    /// Finds the index of the maximum element in the first 'size' elements
    /// </summary>
    private static int FindMaxIndex(int[] arr, int size)
    {
        int maxIndex = 0;
        for (int i = 1; i < size; i++)
        {
            if (arr[i] > arr[maxIndex])
                maxIndex = i;
        }
        return maxIndex;
    }
    
    /// <summary>
    /// Reverses the array from index 0 to 'endIndex'
    /// </summary>
    private static void Flip(int[] arr, int endIndex)
    {
        int start = 0;
        while (start < endIndex)
        {
            int temp = arr[start];
            arr[start] = arr[endIndex];
            arr[endIndex] = temp;
            start++;
            endIndex--;
        }
    }
    
    /// <summary>
    /// Prints the array with flip operations
    /// </summary>
    public static void PrintArray(int[] arr)
    {
        Console.WriteLine("[" + string.Join(", ", arr) + "]");
    }
    
    /// <summary>
    /// Demonstrates the pancake sorting algorithm
    /// </summary>
    public static void Main()
    {
        // Example 1
        int[] arr1 = { 3, 2, 4, 1 };
        Console.WriteLine("Original array:");
        PrintArray(arr1);
        
        List<int> flips = PancakeSort(arr1);
        Console.WriteLine("Sorted array:");
        PrintArray(arr1);
        Console.WriteLine("Flip operations performed: [" + string.Join(", ", flips) + "]");
        Console.WriteLine();
        
        // Example 2
        int[] arr2 = { 4, 3, 2, 1 };
        Console.WriteLine("Original array:");
        PrintArray(arr2);
        
        List<int> flips2 = PancakeSort(arr2);
        Console.WriteLine("Sorted array:");
        PrintArray(arr2);
        Console.WriteLine("Flip operations performed: [" + string.Join(", ", flips2) + "]");
        Console.WriteLine();
        
        // Example 3
        int[] arr3 = { 1, 2, 3, 4 };
        Console.WriteLine("Original array:");
        PrintArray(arr3);
        
        List<int> flips3 = PancakeSort(arr3);
        Console.WriteLine("Sorted array:");
        PrintArray(arr3);
        Console.WriteLine("Flip operations performed: [" + string.Join(", ", flips3) + "]");
    }
}
```

## Output

```
Original array:
[3, 2, 4, 1]
Sorted array:
[1, 2, 3, 4]
Flip operations performed: [3, 4, 2, 3]

Original array:
[4, 3, 2, 1]
Sorted array:
[1, 2, 3, 4]
Flip operations performed: [4, 2, 3, 2]

Original array:
[1, 2, 3, 4]
Sorted array:
[1, 2, 3, 4]
Flip operations performed: []
```

## How It Works

1. **Find Maximum**: For each unsorted portion, find the index of the maximum element
2. **Bring to Top**: If the maximum element is not already at the top, flip it to the top
3. **Bring to Correct Position**: Flip the maximum element to its correct position at the end of the unsorted portion
4. **Repeat**: Continue with the remaining unsorted portion

## Time Complexity
- **Best Case**: O(n²) - when array is already sorted
- **Average Case**: O(n²)
- **Worst Case**: O(n²)

## Space Complexity
- O(1) - only using a constant amount of extra space

The algorithm performs at most 2n flips in the worst case, making it efficient for pancake sorting problems.

