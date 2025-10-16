# TimSort Algorithm Implementation in C#

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class TimSort
{
    private const int MIN_MERGE = 32;

    public static void TimSortArray<T>(T[] array) where T : IComparable<T>
    {
        if (array == null || array.Length <= 1)
            return;

        int n = array.Length;
        int minRun = GetMinRun(n);
        
        // Step 1: Sort small runs using insertion sort
        for (int i = 0; i < n; i += minRun)
        {
            InsertionSort(array, i, Math.Min(i + minRun - 1, n - 1));
        }

        // Step 2: Merge runs
        for (int size = minRun; size < n; size = 2 * size)
        {
            for (int left = 0; left < n - size; left += 2 * size)
            {
                int mid = left + size - 1;
                int right = Math.Min(left + size * 2 - 1, n - 1);

                if (mid < right)
                {
                    Merge(array, left, mid, right);
                }
            }
        }
    }

    private static int GetMinRun(int n)
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

        Array.Copy(array, left, leftArray, 0, leftSize);
        Array.Copy(array, mid + 1, rightArray, 0, rightSize);

        int i = 0, j = 0, k = left;

        while (i < leftSize && j < rightSize)
        {
            if (leftArray[i].CompareTo(rightArray[j]) <= 0)
            {
                array[k] = leftArray[i];
                i++;
            }
            else
            {
                array[k] = rightArray[j];
                j++;
            }
            k++;
        }

        while (i < leftSize)
        {
            array[k] = leftArray[i];
            i++;
            k++;
        }

        while (j < rightSize)
        {
            array[k] = rightArray[j];
            j++;
            k++;
        }
    }

    // Example usage
    public static void Main()
    {
        // Example 1: Integer array
        int[] numbers = { 5, 2, 8, 1, 9, 3, 7, 4, 6 };
        Console.WriteLine("Original array: " + string.Join(", ", numbers));
        
        TimSortArray(numbers);
        Console.WriteLine("Sorted array: " + string.Join(", ", numbers));
        
        Console.WriteLine();
        
        // Example 2: String array
        string[] words = { "banana", "apple", "cherry", "date", "elderberry", "fig" };
        Console.WriteLine("Original words: " + string.Join(", ", words));
        
        TimSortArray(words);
        Console.WriteLine("Sorted words: " + string.Join(", ", words));
        
        Console.WriteLine();
        
        // Example 3: Already sorted array
        int[] sortedArray = { 1, 2, 3, 4, 5, 6, 7, 8, 9 };
        Console.WriteLine("Already sorted array: " + string.Join(", ", sortedArray));
        
        TimSortArray(sortedArray);
        Console.WriteLine("After sorting: " + string.Join(", ", sortedArray));
    }
}
```

## How TimSort Works

1. **Divide into Runs**: First, the array is divided into small runs (typically 32 elements)
2. **Insertion Sort**: Each run is sorted using insertion sort
3. **Merge Runs**: Adjacent runs are merged together using a merge process similar to merge sort
4. **Growth Strategy**: The merge process continues with increasing run sizes (32, 64, 128, etc.)

## Key Features

- **Time Complexity**: O(n log n) worst case, O(n) best case for already sorted arrays
- **Space Complexity**: O(n)
- **Stable**: Maintains relative order of equal elements
- **Adaptive**: Performs well on partially sorted data

## Output Example

```
Original array: 5, 2, 8, 1, 9, 3, 7, 4, 6
Sorted array: 1, 2, 3, 4, 5, 6, 7, 8, 9

Original words: banana, apple, cherry, date, elderberry, fig
Sorted words: apple, banana, cherry, date, elderberry, fig

Already sorted array: 1, 2, 3, 4, 5, 6, 7, 8, 9
After sorting: 1, 2, 3, 4, 5, 6, 7, 8, 9
```

