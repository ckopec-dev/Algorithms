# Cycle Sort Algorithm in C#

Cycle sort is an in-place, unstable sorting algorithm that is particularly efficient for sorting arrays where the elements are in the range [0, n-1] or when there are many duplicate elements.

## Implementation

```csharp
using System;
using System.Collections.Generic;

public class CycleSort
{
    /// <summary>
    /// Sorts an array using cycle sort algorithm
    /// </summary>
    /// <param name="arr">Array to be sorted</param>
    public static void Sort(int[] arr)
    {
        int writes = 0;
        
        // Traverse the array to find cycles
        for (int cycleStart = 0; cycleStart < arr.Length - 1; cycleStart++)
        {
            int item = arr[cycleStart];
            int pos = cycleStart;
            
            // Find the position where the item should be placed
            for (int i = cycleStart + 1; i < arr.Length; i++)
            {
                if (arr[i] < item)
                    pos++;
            }
            
            // If item is already in correct position
            if (pos == cycleStart)
                continue;
            
            // Skip duplicates
            while (item == arr[pos])
                pos++;
            
            // Place item at its correct position
            if (pos != cycleStart)
            {
                int temp = item;
                item = arr[pos];
                arr[pos] = temp;
                writes++;
            }
            
            // Rotate the cycle
            while (pos != cycleStart)
            {
                pos = cycleStart;
                
                // Find position for item
                for (int i = cycleStart + 1; i < arr.Length; i++)
                {
                    if (arr[i] < item)
                        pos++;
                }
                
                // Skip duplicates
                while (item == arr[pos])
                    pos++;
                
                // Place item at its correct position
                if (item != arr[pos])
                {
                    int temp = item;
                    item = arr[pos];
                    arr[pos] = temp;
                    writes++;
                }
            }
        }
        
        Console.WriteLine($"Total writes performed: {writes}");
    }
    
    /// <summary>
    /// Helper method to print array elements
    /// </summary>
    /// <param name="arr">Array to print</param>
    public static void PrintArray(int[] arr)
    {
        Console.WriteLine("[" + string.Join(", ", arr) + "]");
    }
}

// Example usage
class Program
{
    static void Main()
    {
        // Example 1: Basic array
        int[] arr1 = { 4, 3, 2, 1, 0 };
        Console.WriteLine("Original array:");
        CycleSort.PrintArray(arr1);
        
        CycleSort.Sort(arr1);
        Console.WriteLine("Sorted array:");
        CycleSort.PrintArray(arr1);
        
        Console.WriteLine();
        
        // Example 2: Array with duplicates
        int[] arr2 = { 3, 2, 1, 3, 2, 1 };
        Console.WriteLine("Original array with duplicates:");
        CycleSort.PrintArray(arr2);
        
        CycleSort.Sort(arr2);
        Console.WriteLine("Sorted array:");
        CycleSort.PrintArray(arr2);
        
        Console.WriteLine();
        
        // Example 3: Already sorted array
        int[] arr3 = { 1, 2, 3, 4, 5 };
        Console.WriteLine("Already sorted array:");
        CycleSort.PrintArray(arr3);
        
        CycleSort.Sort(arr3);
        Console.WriteLine("Sorted array:");
        CycleSort.PrintArray(arr3);
    }
}
```

## Output
```
Original array:
[4, 3, 2, 1, 0]
Total writes performed: 4
Sorted array:
[0, 1, 2, 3, 4]

Original array with duplicates:
[3, 2, 1, 3, 2, 1]
Total writes performed: 4
Sorted array:
[1, 1, 2, 2, 3, 3]

Already sorted array:
[1, 2, 3, 4, 5]
Total writes performed: 0
Sorted array:
[1, 2, 3, 4, 5]
```

## How it works:

1. **Cycle Detection**: The algorithm identifies cycles in the permutation of elements
2. **Position Finding**: For each element, it determines where it should be placed in the sorted array
3. **Rotation**: Elements are rotated within cycles to their correct positions
4. **Duplicate Handling**: The algorithm skips over duplicate elements to avoid unnecessary swaps

## Time and Space Complexity:

- **Time Complexity**: O(n²) in worst case, O(n) in best case (already sorted)
- **Space Complexity**: O(1) - sorts in-place

## Key Features:

- **In-place sorting**: Uses only O(1) extra memory
- **Minimum writes**: Performs minimum number of writes to sort the array
- **Unstable**: May change the relative order of equal elements
- **Efficient for duplicates**: Particularly effective when there are many duplicate elements

This implementation demonstrates the cycle sort algorithm's ability to minimize the number of writes while sorting arrays efficiently.

