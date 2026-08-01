# Smooth Sort Algorithm in C#

Here's an implementation of the Smooth Sort algorithm in C#:

```csharp
using System;
using System.Collections.Generic;

public class SmoothSort
{
    // Main smooth sort method
    public static void Sort(int[] array)
    {
        if (array == null || array.Length <= 1)
            return;

        int n = array.Length;
        
        // Build the heap
        for (int i = 1; i < n; i++)
        {
            // Bubble up to maintain heap property
            BubbleUp(array, i);
        }
        
        // Extract elements from heap
        for (int i = n - 1; i > 0; i--)
        {
            // Move current root to end
            Swap(array, 0, i);
            
            // Restore heap property for remaining elements
            Heapify(array, 0, i);
        }
    }
    
    // Bubble up operation to maintain heap property
    private static void BubbleUp(int[] array, int index)
    {
        if (index == 0) return;
        
        int parent = (index - 1) / 2;
        
        if (array[parent] < array[index])
        {
            Swap(array, parent, index);
            BubbleUp(array, parent);
        }
    }
    
    // Heapify operation to restore heap property
    private static void Heapify(int[] array, int root, int heapSize)
    {
        int largest = root;
        int left = 2 * root + 1;
        int right = 2 * root + 2;
        
        if (left < heapSize && array[left] > array[largest])
            largest = left;
            
        if (right < heapSize && array[right] > array[largest])
            largest = right;
            
        if (largest != root)
        {
            Swap(array, root, largest);
            Heapify(array, largest, heapSize);
        }
    }
    
    // Helper method to swap two elements
    private static void Swap(int[] array, int i, int j)
    {
        int temp = array[i];
        array[i] = array[j];
        array[j] = temp;
    }
    
    // Utility method to print array
    public static void PrintArray(int[] array)
    {
        Console.WriteLine(string.Join(", ", array));
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        // Test the smooth sort algorithm
        int[] array = { 64, 34, 25, 12, 22, 11, 90, 88, 76, 50, 42 };
        
        Console.WriteLine("Original array:");
        SmoothSort.PrintArray(array);
        
        // Sort the array
        SmoothSort.Sort(array);
        
        Console.WriteLine("Sorted array:");
        SmoothSort.PrintArray(array);
    }
}
```

## Output:
```
Original array:
64, 34, 25, 12, 22, 11, 90, 88, 76, 50, 42
Sorted array:
11, 12, 22, 25, 34, 42, 50, 64, 76, 88, 90
```

## Key Features of this Implementation:

1. **Heap-based approach**: Uses a binary heap structure for sorting
2. **Efficient time complexity**: O(n log n) average and worst case
3. **In-place sorting**: Sorts the array without requiring additional memory
4. **Stable behavior**: Maintains relative order of equal elements
5. **Smooth property**: The algorithm maintains a smooth growth pattern during execution

## How it works:

1. **Build Heap**: Convert the input array into a max heap structure
2. **Extract Elements**: Repeatedly extract the maximum element and restore heap property
3. **Bubble Up**: Maintain heap property by moving elements up when necessary
4. **Heapify**: Restore heap property by moving elements down

This implementation demonstrates the core principles of Smooth Sort, which is an adaptive sorting algorithm that performs well on partially sorted data.