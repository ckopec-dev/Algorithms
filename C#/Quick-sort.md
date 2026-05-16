# Quick Sort Algorithm in C#

Here's an implementation of the Quick Sort algorithm in C#:

```csharp
using System;

public class QuickSort
{
    public static void QuickSortAlgorithm(int[] arr, int low, int high)
    {
        if (low < high)
        {
            // Partition the array and get pivot index
            int pivotIndex = Partition(arr, low, high);
            
            // Recursively sort elements before and after partition
            QuickSortAlgorithm(arr, low, pivotIndex - 1);
            QuickSortAlgorithm(arr, pivotIndex + 1, high);
        }
    }
    
    private static int Partition(int[] arr, int low, int high)
    {
        // Choose the rightmost element as pivot
        int pivot = arr[high];
        
        // Index of smaller element (indicates right position of pivot)
        int i = low - 1;
        
        for (int j = low; j < high; j++)
        {
            // If current element is smaller than or equal to pivot
            if (arr[j] <= pivot)
            {
                i++;
                Swap(arr, i, j);
            }
        }
        
        // Place pivot in its correct position
        Swap(arr, i + 1, high);
        return i + 1;
    }
    
    private static void Swap(int[] arr, int i, int j)
    {
        int temp = arr[i];
        arr[i] = arr[j];
        arr[j] = temp;
    }
    
    // Helper method to print array
    public static void PrintArray(int[] arr)
    {
        foreach (int item in arr)
        {
            Console.Write(item + " ");
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
        QuickSort.PrintArray(arr);
        
        QuickSort.QuickSortAlgorithm(arr, 0, arr.Length - 1);
        
        Console.WriteLine("Sorted array:");
        QuickSort.PrintArray(arr);
    }
}
```

## Output:
```
Original array:
64 34 25 12 22 11 90 
Sorted array:
11 12 22 25 34 64 90 
```

## How it works:

1. **Choose a pivot**: Select an element from the array (in this implementation, the last element is chosen)
2. **Partition**: Rearrange the array so that:
   - Elements smaller than the pivot are on the left
   - Elements greater than the pivot are on the right
3. **Recursively sort**: Apply the same process to the sub-arrays on both sides of the pivot
4. **Base case**: When the sub-array has one or zero elements, it's already sorted

## Time Complexity:
- **Best/Average case**: O(n log n)
- **Worst case**: O(n²) - when the pivot is always the smallest or largest element

## Space Complexity:
- O(log n) - due to recursive call stack

