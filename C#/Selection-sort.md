# Selection Sort Algorithm in C#

## Algorithm Explanation

Selection sort works by repeatedly finding the minimum element from the unsorted portion of the array and placing it at the beginning. The algorithm maintains two subarrays:
- Left subarray: sorted elements
- Right subarray: remaining unsorted elements

## C# Implementation

```csharp
using System;

public class SelectionSort
{
    public static void Sort(int[] arr)
    {
        int n = arr.Length;
        
        // Traverse through all array elements
        for (int i = 0; i < n - 1; i++)
        {
            // Find the minimum element in remaining unsorted array
            int minIndex = i;
            
            for (int j = i + 1; j < n; j++)
            {
                if (arr[j] < arr[minIndex])
                {
                    minIndex = j;
                }
            }
            
            // Swap the found minimum element with the first element
            if (minIndex != i)
            {
                int temp = arr[i];
                arr[i] = arr[minIndex];
                arr[minIndex] = temp;
            }
        }
    }
    
    // Helper method to print the array
    public static void PrintArray(int[] arr)
    {
        foreach (int element in arr)
        {
            Console.Write(element + " ");
        }
        Console.WriteLine();
    }
}

// Example usage
class Program
{
    static void Main()
    {
        int[] arr = { 64, 34, 25, 12, 22, 11, 90 };
        
        Console.WriteLine("Original array:");
        SelectionSort.PrintArray(arr);
        
        SelectionSort.Sort(arr);
        
        Console.WriteLine("Sorted array:");
        SelectionSort.PrintArray(arr);
    }
}
```

## Output
```
Original array:
64 34 25 12 22 11 90 
Sorted array:
11 12 22 25 34 64 90 
```

## Algorithm Complexity

- **Time Complexity**: O(n²) in all cases (best, average, and worst)
- **Space Complexity**: O(1) - sorts in-place
- **Stability**: Not stable (does not preserve relative order of equal elements)

## How it works step by step:

1. Find the minimum element in the entire array and swap it with the first element
2. Find the minimum element in the remaining unsorted array and swap it with the second element
3. Continue this process until the entire array is sorted

The algorithm performs exactly n-1 swaps in the worst case, making it inefficient for large datasets but simple to understand and implement.