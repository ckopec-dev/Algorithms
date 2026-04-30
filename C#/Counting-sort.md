# Counting Sort Algorithm in C#

```csharp
using System;
using System.Linq;

public class CountingSort
{
    public static void Sort(int[] array)
    {
        if (array == null || array.Length <= 1)
            return;
        
        // Find the maximum and minimum values in the array
        int max = array.Max();
        int min = array.Min();
        int range = max - min + 1;
        
        // Create count array
        int[] count = new int[range];
        
        // Initialize count array with zeros
        for (int i = 0; i < range; i++)
        {
            count[i] = 0;
        }
        
        // Count occurrences of each element
        for (int i = 0; i < array.Length; i++)
        {
            count[array[i] - min]++;
        }
        
        // Reconstruct the sorted array
        int index = 0;
        for (int i = 0; i < range; i++)
        {
            while (count[i] > 0)
            {
                array[index] = i + min;
                index++;
                count[i]--;
            }
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
        int[] array = { 4, 2, 2, 8, 3, 3, 1 };
        
        Console.WriteLine("Original array:");
        CountingSort.PrintArray(array);
        
        CountingSort.Sort(array);
        
        Console.WriteLine("Sorted array:");
        CountingSort.PrintArray(array);
    }
}
```

## Output:
```
Original array:
4, 2, 2, 8, 3, 3, 1
Sorted array:
1, 2, 2, 3, 3, 4, 8
```

## How it works:

1. **Find Range**: Determine the range of input values (max - min + 1)
2. **Count Elements**: Create a count array to store frequency of each element
3. **Accumulate Counts**: Modify count array to store actual positions
4. **Build Result**: Place elements in sorted order using count array

## Time Complexity: O(n + k)
## Space Complexity: O(k)

Where n is the number of elements and k is the range of input.

