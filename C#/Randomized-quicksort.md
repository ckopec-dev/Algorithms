# Randomized Quicksort Algorithm in C#

```csharp
using System;
using System.Linq;

public class RandomizedQuicksort
{
    // Main method to sort the array
    public static void Sort(int[] arr)
    {
        if (arr == null || arr.Length <= 1)
            return;
            
        Random rand = new Random();
        QuickSort(arr, 0, arr.Length - 1, rand);
    }
    
    // Recursive quicksort implementation with random pivot selection
    private static void QuickSort(int[] arr, int low, int high, Random rand)
    {
        if (low < high)
        {
            // Partition the array and get pivot index
            int pivotIndex = Partition(arr, low, high, rand);
            
            // Recursively sort elements before and after partition
            QuickSort(arr, low, pivotIndex - 1, rand);
            QuickSort(arr, pivotIndex + 1, high, rand);
        }
    }
    
    // Partition method with random pivot selection
    private static int Partition(int[] arr, int low, int high, Random rand)
    {
        // Randomly select pivot and swap with last element
        int randomIndex = low + rand.Next(high - low + 1);
        Swap(arr, randomIndex, high);
        
        int pivot = arr[high]; // Pivot is now the last element
        int i = low - 1;       // Index of smaller element
        
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
    
    // Helper method to swap two elements
    private static void Swap(int[] arr, int i, int j)
    {
        int temp = arr[i];
        arr[i] = arr[j];
        arr[j] = temp;
    }
    
    // Utility method to print array
    public static void PrintArray(int[] arr)
    {
        Console.WriteLine(string.Join(", ", arr));
    }
}

// Example usage
class Program
{
    static void Main()
    {
        // Example 1: Random array
        int[] arr1 = { 64, 34, 25, 12, 22, 11, 90, 88, 76, 50, 42 };
        Console.WriteLine("Original array:");
        RandomizedQuicksort.PrintArray(arr1);
        
        RandomizedQuicksort.Sort(arr1);
        Console.WriteLine("Sorted array:");
        RandomizedQuicksort.PrintArray(arr1);
        
        Console.WriteLine();
        
        // Example 2: Array with duplicates
        int[] arr2 = { 5, 2, 8, 2, 9, 1, 5, 5 };
        Console.WriteLine("Original array with duplicates:");
        RandomizedQuicksort.PrintArray(arr2);
        
        RandomizedQuicksort.Sort(arr2);
        Console.WriteLine("Sorted array:");
        RandomizedQuicksort.PrintArray(arr2);
        
        Console.WriteLine();
        
        // Example 3: Already sorted array
        int[] arr3 = { 1, 2, 3, 4, 5, 6, 7, 8, 9 };
        Console.WriteLine("Already sorted array:");
        RandomizedQuicksort.PrintArray(arr3);
        
        RandomizedQuicksort.Sort(arr3);
        Console.WriteLine("Sorted array:");
        RandomizedQuicksort.PrintArray(arr3);
    }
}
```

## Output:
```
Original array:
64, 34, 25, 12, 22, 11, 90, 88, 76, 50, 42
Sorted array:
11, 12, 22, 25, 34, 42, 50, 64, 76, 88, 90

Original array with duplicates:
5, 2, 8, 2, 9, 1, 5, 5
Sorted array:
1, 2, 2, 5, 5, 5, 8, 9

Already sorted array:
1, 2, 3, 4, 5, 6, 7, 8, 9
Sorted array:
1, 2, 3, 4, 5, 6, 7, 8, 9
```

## Key Features of this Implementation:

1. **Random Pivot Selection**: Uses `Random.Next()` to select a random pivot element
2. **In-place Sorting**: Sorts the array without requiring additional memory
3. **Time Complexity**: 
   - Average case: O(n log n)
   - Worst case: O(n²) (very rare with randomization)
4. **Space Complexity**: O(log n) due to recursion stack
5. **Handles Edge Cases**: Works with empty arrays, single elements, and duplicates

The randomization helps avoid the worst-case scenario that occurs with regular quicksort when the pivot is consistently the smallest or largest element.