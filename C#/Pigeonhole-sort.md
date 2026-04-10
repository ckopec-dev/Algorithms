# Pigeonhole Sort Algorithm in C#

Pigeonhole sort is a sorting algorithm that is suitable for sorting arrays of elements where the number of elements and the number of possible key values are approximately the same.

## Algorithm Explanation

The algorithm works by:
1. Finding the minimum and maximum values in the array
2. Creating "pigeonholes" (buckets) for each possible value
3. Placing each element into its corresponding pigeonhole
4. Collecting elements from pigeonholes back into the array

## C# Implementation

```csharp
using System;
using System.Linq;

public class PigeonholeSort
{
    public static void Sort(int[] array)
    {
        if (array == null || array.Length <= 1)
            return;
        
        // Find minimum and maximum values
        int min = array.Min();
        int max = array.Max();
        
        // Calculate range
        int range = max - min + 1;
        
        // Create pigeonholes (buckets)
        int[] pigeonholes = new int[range];
        
        // Initialize pigeonholes to 0
        for (int i = 0; i < range; i++)
        {
            pigeonholes[i] = 0;
        }
        
        // Place elements into pigeonholes
        foreach (int num in array)
        {
            pigeonholes[num - min]++;
        }
        
        // Collect elements from pigeonholes back to array
        int index = 0;
        for (int i = 0; i < range; i++)
        {
            while (pigeonholes[i] > 0)
            {
                array[index] = i + min;
                index++;
                pigeonholes[i]--;
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
        // Example 1
        int[] array1 = { 8, 3, 2, 7, 4, 6, 1 };
        Console.WriteLine("Original array:");
        PigeonholeSort.PrintArray(array1);
        
        PigeonholeSort.Sort(array1);
        Console.WriteLine("Sorted array:");
        PigeonholeSort.PrintArray(array1);
        
        Console.WriteLine();
        
        // Example 2
        int[] array2 = { 5, 2, 8, 1, 9, 3 };
        Console.WriteLine("Original array:");
        PigeonholeSort.PrintArray(array2);
        
        PigeonholeSort.Sort(array2);
        Console.WriteLine("Sorted array:");
        PigeonholeSort.PrintArray(array2);
    }
}
```

## Output
```
Original array:
8, 3, 2, 7, 4, 6, 1
Sorted array:
1, 2, 3, 4, 6, 7, 8

Original array:
5, 2, 8, 1, 9, 3
Sorted array:
1, 2, 3, 5, 8, 9
```

## Time and Space Complexity

- **Time Complexity**: O(n + k) where n is the number of elements and k is the range of input
- **Space Complexity**: O(k) for the pigeonholes array

## Key Characteristics

- **Stable**: Maintains relative order of equal elements
- **In-place**: Requires only O(k) extra space
- **Efficient**: When range of possible values is not significantly larger than the number of elements
- **Non-comparative**: Doesn't use comparisons between elements

This algorithm is most efficient when the range of possible values (max - min) is close to the number of elements to be sorted.

