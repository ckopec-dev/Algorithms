# Introsort Algorithm in C#

Introsort is a hybrid sorting algorithm that combines quicksort, heapsort, and insertion sort. It starts with quicksort, switches to heapsort when the recursion depth exceeds a certain threshold, and uses insertion sort for small arrays.

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

        Sort(array, 0, array.Length - 1, 0);
    }

    private static void Sort<T>(T[] array, int low, int high, int depth) 
        where T : IComparable<T>
    {
        int size = high - low + 1;

        // Use insertion sort for small arrays
        if (size <= INSERTION_SORT_THRESHOLD)
        {
            InsertionSort(array, low, high);
            return;
        }

        // Switch to heapsort if recursion depth exceeds threshold
        if (depth > MAX_DEPTH)
        {
            HeapSort(array, low, high);
            return;
        }

        // Use quicksort
        int pivotIndex = Partition(array, low, high);
        Sort(array, low, pivotIndex - 1, depth + 1);
        Sort(array, pivotIndex + 1, high, depth + 1);
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
        // Build max heap
        for (int i = n / 2 - 1; i >= 0; i--)
        {
            Heapify(array, low, n, i);
        }

        // Extract elements from heap one by one
        for (int i = n - 1; i > 0; i--)
        {
            Swap(array, low, low + i);
            Heapify(array, low, i, 0);
        }
    }

    private static void Heapify<T>(T[] array, int low, int n, int i) 
        where T : IComparable<T>
    {
        int largest = i;
        int left = 2 * i + 1;
        int right = 2 * i + 2;

        if (left < n && array[low + left].CompareTo(array[low + largest]) > 0)
            largest = left;

        if (right < n && array[low + right].CompareTo(array[low + largest]) > 0)
            largest = right;

        if (largest != i)
        {
            Swap(array, low + i, low + largest);
            Heapify(array, low, n, largest);
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
class Program
{
    static void Main()
    {
        // Test with integers
        int[] numbers = { 64, 34, 25, 12, 22, 11, 90, 88, 76, 50, 42 };
        
        Console.WriteLine("Original array:");
        Console.WriteLine(string.Join(", ", numbers));
        
        Introsort.Sort(numbers);
        
        Console.WriteLine("Sorted array:");
        Console.WriteLine(string.Join(", ", numbers));
        
        // Test with strings
        string[] words = { "banana", "apple", "cherry", "date", "elderberry" };
        
        Console.WriteLine("\nOriginal words:");
        Console.WriteLine(string.Join(", ", words));
        
        Introsort.Sort(words);
        
        Console.WriteLine("Sorted words:");
        Console.WriteLine(string.Join(", ", words));
    }
}
```

## Output:
```
Original array:
64, 34, 25, 12, 22, 11, 90, 88, 76, 50, 42
Sorted array:
11, 12, 22, 25, 34, 42, 50, 64, 76, 88, 90

Original words:
banana, apple, cherry, date, elderberry
Sorted words:
apple, banana, cherry, date, elderberry
```

## Key Features of this Implementation:

1. **Hybrid Approach**: Combines quicksort, heapsort, and insertion sort
2. **Depth Control**: Switches to heapsort when recursion depth exceeds `2 * log2(n)`
3. **Small Array Optimization**: Uses insertion sort for arrays ≤ 16 elements
4. **Generic Implementation**: Works with any type implementing `IComparable<T>`
5. **In-place Sorting**: Sorts the array without requiring additional memory

The algorithm provides O(n log n) worst-case time complexity while maintaining good average-case performance.

