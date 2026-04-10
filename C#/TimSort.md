# TimSort Algorithm Implementation in C#

```csharp
using System;
using System.Collections.Generic;

public class TimSort
{
    private const int MIN_MERGE = 32;

    public static void TimSortAlgorithm<T>(T[] array) where T : IComparable<T>
    {
        int n = array.Length;
        
        if (n < 2) return;
        
        // Find the size of the smallest subarray to be merged
        int minRun = FindMinRun(n);
        
        // Sort individual subarrays of size minRun
        for (int i = 0; i < n; i += minRun)
        {
            InsertionSort(array, i, Math.Min(i + minRun - 1, n - 1));
        }
        
        // Merge subarrays in bottom-up manner
        for (int size = minRun; size < n; size = 2 * size)
        {
            for (int left = 0; left < n - size; left += 2 * size)
            {
                int mid = left + size - 1;
                int right = Math.Min(left + 2 * size - 1, n - 1);
                
                if (mid < right)
                {
                    Merge(array, left, mid, right);
                }
            }
        }
    }
    
    private static int FindMinRun(int n)
    {
        int run = 0;
        while (n >= MIN_MERGE)
        {
            run |= n & 1;
            n >>= 1;
        }
        return n + run;
    }
    
    private static void InsertionSort<T>(T[] array, int left, int right) where T : IComparable<T>
    {
        for (int i = left + 1; i <= right; i++)
        {
            T key = array[i];
            int j = i - 1;
            
            while (j >= left && array[j].CompareTo(key) > 0)
            {
                array[j + 1] = array[j];
                j--;
            }
            
            array[j + 1] = key;
        }
    }
    
    private static void Merge<T>(T[] array, int left, int mid, int right) where T : IComparable<T>
    {
        int leftSize = mid - left + 1;
        int rightSize = right - mid;
        
        T[] leftArray = new T[leftSize];
        T[] rightArray = new T[rightSize];
        
        // Copy data to temporary arrays
        for (int i = 0; i < leftSize; i++)
            leftArray[i] = array[left + i];
        
        for (int i = 0; i < rightSize; i++)
            rightArray[i] = array[mid + 1 + i];
        
        int iLeft = 0, iRight = 0, iMerged = left;
        
        // Merge the temporary arrays back
        while (iLeft < leftSize && iRight < rightSize)
        {
            if (leftArray[iLeft].CompareTo(rightArray[iRight]) <= 0)
            {
                array[iMerged] = leftArray[iLeft];
                iLeft++;
            }
            else
            {
                array[iMerged] = rightArray[iRight];
                iRight++;
            }
            iMerged++;
        }
        
        // Copy remaining elements
        while (iLeft < leftSize)
        {
            array[iMerged] = leftArray[iLeft];
            iLeft++;
            iMerged++;
        }
        
        while (iRight < rightSize)
        {
            array[iMerged] = rightArray[iRight];
            iRight++;
            iMerged++;
        }
    }
    
    // Helper method to print array
    public static void PrintArray<T>(T[] array)
    {
        Console.WriteLine(string.Join(", ", array));
    }
}

// Example usage
class Program
{
    static void Main()
    {
        // Example 1: Sorting integers
        int[] numbers = { 5, 2, 8, 1, 9, 3, 7, 4, 6 };
        Console.WriteLine("Original array:");
        TimSort.PrintArray(numbers);
        
        TimSort.TimSortAlgorithm(numbers);
        Console.WriteLine("Sorted array:");
        TimSort.PrintArray(numbers);
        
        Console.WriteLine();
        
        // Example 2: Sorting strings
        string[] words = { "banana", "apple", "cherry", "date", "blueberry" };
        Console.WriteLine("Original strings:");
        TimSort.PrintArray(words);
        
        TimSort.TimSortAlgorithm(words);
        Console.WriteLine("Sorted strings:");
        TimSort.PrintArray(words);
    }
}
```

## Output:
```
Original array:
5, 2, 8, 1, 9, 3, 7, 4, 6
Sorted array:
1, 2, 3, 4, 5, 6, 7, 8, 9

Original strings:
banana, apple, cherry, date, blueberry
Sorted strings:
apple, banana, blueberry, cherry, date
```

## Key Features of this TimSort Implementation:

1. **Hybrid Approach**: Combines insertion sort for small arrays with merge sort for larger arrays
2. **Adaptive**: Performs well on partially sorted data
3. **Stable**: Maintains relative order of equal elements
4. **In-place**: Sorts the array without requiring additional memory proportional to input size
5. **Time Complexity**: O(n log n) worst case, O(n) best case for nearly sorted data
6. **Space Complexity**: O(n) for temporary arrays during merging

The algorithm works by:
1. Breaking the array into small chunks (runs)
2. Sorting each run using insertion sort
3. Merging runs using a merge process similar to merge sort
4. Gradually increasing the size of merged runs until the entire array is sorted

