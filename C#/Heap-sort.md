# Heap Sort Algorithm in C#

```csharp
using System;

public class HeapSort
{
    public static void HeapSortAlgorithm(int[] arr)
    {
        int n = arr.Length;
        
        // Build max heap
        for (int i = n / 2 - 1; i >= 0; i--)
        {
            Heapify(arr, n, i);
        }
        
        // Extract elements from heap one by one
        for (int i = n - 1; i > 0; i--)
        {
            // Move current root to end
            Swap(arr, 0, i);
            
            // Call heapify on the reduced heap
            Heapify(arr, i, 0);
        }
    }
    
    private static void Heapify(int[] arr, int n, int i)
    {
        int largest = i;        // Initialize largest as root
        int left = 2 * i + 1;   // Left child
        int right = 2 * i + 2;  // Right child
        
        // If left child exists and is greater than root
        if (left < n && arr[left] > arr[largest])
        {
            largest = left;
        }
        
        // If right child exists and is greater than largest so far
        if (right < n && arr[right] > arr[largest])
        {
            largest = right;
        }
        
        // If largest is not root
        if (largest != i)
        {
            Swap(arr, i, largest);
            
            // Recursively heapify the affected sub-tree
            Heapify(arr, n, largest);
        }
    }
    
    private static void Swap(int[] arr, int i, int j)
    {
        int temp = arr[i];
        arr[i] = arr[j];
        arr[j] = temp;
    }
    
    // Utility function to print array
    public static void PrintArray(int[] arr)
    {
        foreach (int item in arr)
        {
            Console.Write(item + " ");
        }
        Console.WriteLine();
    }
    
    // Main method to test the algorithm
    public static void Main(string[] args)
    {
        int[] arr = { 64, 34, 25, 12, 22, 11, 90 };
        
        Console.WriteLine("Original array:");
        PrintArray(arr);
        
        HeapSortAlgorithm(arr);
        
        Console.WriteLine("Sorted array:");
        PrintArray(arr);
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

1. **Build Max Heap**: Convert the input array into a max heap structure
2. **Extract Elements**: Repeatedly extract the maximum element (root) and place it at the end
3. **Heapify**: After each extraction, restore the heap property for the remaining elements

## Time Complexity:
- **Best Case**: O(n log n)
- **Average Case**: O(n log n)
- **Worst Case**: O(n log n)

## Space Complexity:
- O(1) - sorts in-place

The algorithm is efficient and has consistent performance regardless of input distribution.

