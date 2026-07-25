# Merge Sort Algorithm in C#

```csharp
using System;
using System.Linq;

public class MergeSort
{
    public static void MergeSortAlgorithm(int[] array)
    {
        if (array.Length <= 1)
            return;
        
        int[] tempArray = new int[array.Length];
        MergeSortHelper(array, tempArray, 0, array.Length - 1);
    }
    
    private static void MergeSortHelper(int[] array, int[] tempArray, int left, int right)
    {
        if (left >= right)
            return;
        
        int middle = left + (right - left) / 2;
        
        // Recursively sort both halves
        MergeSortHelper(array, tempArray, left, middle);
        MergeSortHelper(array, tempArray, middle + 1, right);
        
        // Merge the sorted halves
        Merge(array, tempArray, left, middle, right);
    }
    
    private static void Merge(int[] array, int[] tempArray, int left, int middle, int right)
    {
        // Copy data to temp array
        for (int i = left; i <= right; i++)
        {
            tempArray[i] = array[i];
        }
        
        int i = left;      // Initial index of first subarray
        int j = middle + 1; // Initial index of second subarray
        int k = left;      // Initial index of merged array
        
        // Merge the temp arrays back
        while (i <= middle && j <= right)
        {
            if (tempArray[i] <= tempArray[j])
            {
                array[k] = tempArray[i];
                i++;
            }
            else
            {
                array[k] = tempArray[j];
                j++;
            }
            k++;
        }
        
        // Copy remaining elements of left subarray, if any
        while (i <= middle)
        {
            array[k] = tempArray[i];
            i++;
            k++;
        }
        
        // Copy remaining elements of right subarray, if any
        while (j <= right)
        {
            array[k] = tempArray[j];
            j++;
            k++;
        }
    }
    
    // Helper method to print array
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
        int[] arr = { 64, 34, 25, 12, 22, 11, 90 };
        
        Console.WriteLine("Original array:");
        MergeSort.PrintArray(arr);
        
        MergeSort.MergeSortAlgorithm(arr);
        
        Console.WriteLine("Sorted array:");
        MergeSort.PrintArray(arr);
    }
}
```

## Output:
```
Original array:
64, 34, 25, 12, 22, 11, 90
Sorted array:
11, 12, 22, 25, 34, 64, 90
```

## How it works:

1. **Divide**: Split the array into two halves recursively until each subarray has only one element
2. **Conquer**: Merge the sorted subarrays back together in a sorted manner
3. **Combine**: The merging process compares elements from both subarrays and places them in the correct order

## Time Complexity:
- **Best Case**: O(n log n)
- **Average Case**: O(n log n)
- **Worst Case**: O(n log n)

## Space Complexity: O(n)

The algorithm is stable (maintains relative order of equal elements) and works efficiently for large datasets.