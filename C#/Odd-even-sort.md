# Odd-Even Sort Algorithm in C#

```csharp
using System;
using System.Linq;

public class OddEvenSort
{
    /// <summary>
    /// Performs odd-even sort (also known as brick sort) on an array
    /// </summary>
    /// <param name="array">The array to be sorted</param>
    public static void OddEvenSortAlgorithm(int[] array)
    {
        bool isSorted = false;
        int n = array.Length;
        
        while (!isSorted)
        {
            isSorted = true;
            
            // Perform odd phase
            for (int i = 1; i < n - 1; i += 2)
            {
                if (array[i] > array[i + 1])
                {
                    Swap(array, i, i + 1);
                    isSorted = false;
                }
            }
            
            // Perform even phase
            for (int i = 0; i < n - 1; i += 2)
            {
                if (array[i] > array[i + 1])
                {
                    Swap(array, i, i + 1);
                    isSorted = false;
                }
            }
        }
    }
    
    /// <summary>
    /// Swaps two elements in the array
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
    /// Prints the array elements
    /// </summary>
    /// <param name="array">The array to print</param>
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
        int[] arr1 = { 5, 2, 8, 1, 9, 3, 7, 4, 6 };
        Console.WriteLine("Original array:");
        OddEvenSort.PrintArray(arr1);
        
        OddEvenSort.OddEvenSortAlgorithm(arr1);
        Console.WriteLine("Sorted array:");
        OddEvenSort.PrintArray(arr1);
        
        Console.WriteLine();
        
        // Example 2: Another array
        int[] arr2 = { 10, 7, 3, 1, 9, 5, 2, 8, 4, 6 };
        Console.WriteLine("Original array:");
        OddEvenSort.PrintArray(arr2);
        
        OddEvenSort.OddEvenSortAlgorithm(arr2);
        Console.WriteLine("Sorted array:");
        OddEvenSort.PrintArray(arr2);
    }
}
```

## Output:
```
Original array:
5, 2, 8, 1, 9, 3, 7, 4, 6
Sorted array:
1, 2, 3, 4, 5, 6, 7, 8, 9

Original array:
10, 7, 3, 1, 9, 5, 2, 8, 4, 6
Sorted array:
1, 2, 3, 4, 5, 6, 7, 8, 9, 10
```

## How it works:

1. **Odd-Even Sort** is a variation of Bubble Sort that works in two phases:
   - **Odd phase**: Compare and swap elements at odd indices (1, 3, 5, ...)
   - **Even phase**: Compare and swap elements at even indices (0, 2, 4, ...)

2. The algorithm continues alternating between odd and even phases until no more swaps are needed.

3. **Time Complexity**: O(n²) in worst case, O(n) in best case
4. **Space Complexity**: O(1) - sorts in place

5. This sorting algorithm is particularly useful for parallel processing since odd and even phases can be executed simultaneously.