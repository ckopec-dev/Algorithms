# Comb Sort Algorithm in C#

Comb sort is an improved version of bubble sort that uses a gap sequence to compare elements that are farther apart, making it more efficient than the standard bubble sort.

## Implementation

```csharp
using System;
using System.Linq;

public class CombSort
{
    /// <summary>
    /// Sorts an array using the comb sort algorithm
    /// </summary>
    /// <param name="array">The array to be sorted</param>
    public static void Sort(int[] array)
    {
        if (array == null || array.Length <= 1)
            return;

        int gap = array.Length;
        bool swapped = true;
        
        // Continue until gap becomes 1 and no swaps occur
        while (gap > 1 || swapped)
        {
            // Calculate next gap using shrink factor of 1.3
            gap = (int)(gap / 1.3);
            if (gap < 1)
                gap = 1;
            
            swapped = false;
            
            // Compare elements with current gap
            for (int i = 0; i < array.Length - gap; i++)
            {
                if (array[i] > array[i + gap])
                {
                    // Swap elements
                    Swap(array, i, i + gap);
                    swapped = true;
                }
            }
        }
    }
    
    /// <summary>
    /// Helper method to swap two elements in an array
    /// </summary>
    /// <param name="array">The array containing elements to swap</param>
    /// <param name="i">First index</param>
    /// <param name="j">Second index</param>
    private static void Swap(int[] array, int i, int j)
    {
        int temp = array[i];
        array[i] = array[j];
        array[j] = temp;
    }
    
    /// <summary>
    /// Helper method to print array elements
    /// </summary>
    /// <param name="array">Array to print</param>
    public static void PrintArray(int[] array)
    {
        Console.WriteLine(string.Join(", ", array));
    }
}

// Example usage
class Program
{
    static void Main()
    {
        // Example 1: Unsorted array
        int[] array1 = { 8, 4, 1, 56, 3, -44, 23, -6, 28, 0 };
        Console.WriteLine("Original array:");
        CombSort.PrintArray(array1);
        
        CombSort.Sort(array1);
        Console.WriteLine("Sorted array:");
        CombSort.PrintArray(array1);
        
        Console.WriteLine();
        
        // Example 2: Already sorted array
        int[] array2 = { 1, 2, 3, 4, 5 };
        Console.WriteLine("Already sorted array:");
        CombSort.PrintArray(array2);
        
        CombSort.Sort(array2);
        Console.WriteLine("After sorting:");
        CombSort.PrintArray(array2);
        
        Console.WriteLine();
        
        // Example 3: Reverse sorted array
        int[] array3 = { 5, 4, 3, 2, 1 };
        Console.WriteLine("Reverse sorted array:");
        CombSort.PrintArray(array3);
        
        CombSort.Sort(array3);
        Console.WriteLine("After sorting:");
        CombSort.PrintArray(array3);
    }
}
```

## Output
```
Original array:
8, 4, 1, 56, 3, -44, 23, -6, 28, 0
Sorted array:
-44, -6, 0, 1, 3, 4, 8, 23, 28, 56

Already sorted array:
1, 2, 3, 4, 5
After sorting:
1, 2, 3, 4, 5

Reverse sorted array:
5, 4, 3, 2, 1
After sorting:
1, 2, 3, 4, 5
```

## How Comb Sort Works

1. **Initial Gap**: Start with a gap equal to the array length
2. **Shrink Factor**: Reduce the gap by a factor of 1.3 in each iteration
3. **Comparison**: Compare elements that are 'gap' positions apart
4. **Swap**: If elements are in wrong order, swap them
5. **Continue**: Repeat until gap becomes 1 and no more swaps occur

## Time Complexity
- **Best Case**: O(n log n)
- **Average Case**: O(n²/2^p) where p is the number of increments
- **Worst Case**: O(n²)

## Space Complexity
- **O(1)** - Only uses a constant amount of extra memory

## Key Advantages
- Better than bubble sort for larger datasets
- Simple implementation
- Adaptive to data distribution
- In-place sorting algorithm

