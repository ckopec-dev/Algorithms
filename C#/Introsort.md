# Introsort Algorithm in C#

Introsort is a hybrid sorting algorithm that combines quicksort, heapsort, and insertion sort to provide optimal performance. Here's an implementation in C#:

```csharp
using System;
using System.Collections.Generic;

public class Introsort
{
    private const int INSERTION_SORT_THRESHOLD = 16;
    private const int MAX_DEPTH = 2 * (int)Math.Floor(Math.Log2(Environment.ProcessorCount));

    public static void Sort<T>(T[] array) where T : IComparable<T>
    {
        if (array == null || array.Length <= 1)
            return;

        Sort(array, 0, array.Length - 1, MAX_DEPTH);
    }

    private static void Sort<T>(T[] array, int low, int high, int maxDepth) 
        where T : IComparable<T>
    {
        int size = high - low + 1;

        // Use insertion sort for small arrays
        if (size <= INSERTION_SORT_THRESHOLD)
        {
            InsertionSort(array, low, high);
            return;
        }

        // Use heapsort if maxDepth is exceeded
        if (maxDepth == 0)
        {
            HeapSort(array, low, high);
            return;
        }

        // Use quicksort and recursively sort
        int pivotIndex = Partition(array, low, high);
        Sort(array, low, pivotIndex - 1, maxDepth - 1);
        Sort(array, pivotIndex + 1, high, maxDepth - 1);
    }

    private static int Partition<T>(T[] array, int low, int high) 
        where T : IComparable<T>
    {
        T pivot = array[high];
        int i = low - 1;

        for (int j = low; j < high; j++)
        {
            if (array[j].CompareTo(pivot) <= 0)
            {
                i++;
                Swap(array, i, j);
            }
        }

        Swap(array, i + 1, high);
        return i + 1;
    }

    private static void InsertionSort<T>(T[] array, int low, int high) 
        where T : IComparable<T>
    {
        for (int i = low + 1; i <= high; i++)
        {
            T key = array[i];
            int j = i - 1;

            while (j >= low && array[j].CompareTo(key) > 0)
            {
                array[j + 1] = array[j];
                j--;
            }
            array[j + 1] = key;
        }
    }

    private static void HeapSort<T>(T[] array, int low, int high) 
        where T : IComparable<T>
    {
        int n = high - low + 1;
        int start = low;

        // Build max heap
        for (int i = n / 2 - 1; i >= 0; i--)
        {
            Heapify(array, start, n, i);
        }

        // Extract elements from heap one by one
        for (int i = n - 1; i > 0; i--)
        {
            Swap(array, start, start + i);
            Heapify(array, start, i, 0);
        }
    }

    private static void Heapify<T>(T[] array, int start, int n, int i) 
        where T : IComparable<T>
    {
        int largest = i;
        int left = 2 * i + 1;
        int right = 2 * i + 2;

        if (left < n && array[start + left].CompareTo(array[start + largest]) > 0)
            largest = left;

        if (right < n && array[start + right].CompareTo(array[start + largest]) > 0)
            largest = right;

        if (largest != i)
        {
            Swap(array, start + i, start + largest);
            Heapify(array, start, n, largest);
        }
    }

    private static void Swap<T>(T[] array, int i, int j)
    {
        T temp = array[i];
        array[i] = array[j];
        array[j] = temp;
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        // Test with integers
        int[] numbers = { 64, 34, 25, 12, 22, 11, 90, 5 };
        Console.WriteLine("Original array: " + string.Join(", ", numbers));
        
        Introsort.Sort(numbers);
        Console.WriteLine("Sorted array: " + string.Join(", ", numbers));
        
        // Test with strings
        string[] words = { "banana", "apple", "cherry", "date", "elderberry" };
        Console.WriteLine("\nOriginal words: " + string.Join(", ", words));
        
        Introsort.Sort(words);
        Console.WriteLine("Sorted words: " + string.Join(", ", words));
    }
}
```

## Key Features of this Implementation:

1. **Hybrid Approach**: Combines quicksort, heapsort, and insertion sort
2. **Depth Limitation**: Uses a maximum depth to switch to heapsort and avoid worst-case scenarios
3. **Insertion Sort Optimization**: Uses insertion sort for small subarrays (≤ 16 elements)
4. **Generic Implementation**: Works with any type that implements `IComparable<T>`
5. **Time Complexity**: O(n log n) average case, O(n log n) worst case
6. **Space Complexity**: O(log n) due to recursion

## Output:
```
Original array: 64, 34, 25, 12, 22, 11, 90, 5
Sorted array: 5, 11, 12, 22, 25, 34, 64, 90

Original words: banana, apple, cherry, date, elderberry
Sorted words: apple, banana, cherry, date, elderberry
```

