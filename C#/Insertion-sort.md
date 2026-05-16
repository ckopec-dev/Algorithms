# Insertion Sort Algorithm in C#

Here's an implementation of the insertion sort algorithm in C#:

```csharp
using System;

public class InsertionSort
{
    public static void Sort(int[] array)
    {
        // Start from the second element (index 1)
        for (int i = 1; i < array.Length; i++)
        {
            int key = array[i];  // Current element to be inserted
            int j = i - 1;       // Index of the last element in sorted portion
            
            // Move elements greater than key one position ahead
            while (j >= 0 && array[j] > key)
            {
                array[j + 1] = array[j];
                j--;
            }
            
            // Insert key at its correct position
            array[j + 1] = key;
        }
    }
    
    // Helper method to print the array
    public static void PrintArray(int[] array)
    {
        foreach (int element in array)
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
        InsertionSort.PrintArray(arr);
        
        InsertionSort.Sort(arr);
        
        Console.WriteLine("Sorted array:");
        InsertionSort.PrintArray(arr);
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

1. **Start from index 1** - The first element is considered already sorted
2. **Compare with previous elements** - For each element, compare it with elements to its left
3. **Shift elements** - Move larger elements one position to the right
4. **Insert** - Place the current element in its correct position
5. **Repeat** - Continue until all elements are processed

## Time Complexity:
- **Best Case**: O(n) - when array is already sorted
- **Average Case**: O(n²)
- **Worst Case**: O(n²) - when array is reverse sorted

## Space Complexity:
- O(1) - sorts in place, only uses constant extra memory

