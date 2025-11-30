# Counting Sort Algorithm in C#

Here's an implementation of the counting sort algorithm in C#:

```csharp
using System;
using System.Linq;

public class CountingSort
{
    public static void Sort(int[] array)
    {
        if (array == null || array.Length <= 1)
            return;
        
        // Find the maximum and minimum values to determine the range
        int max = array.Max();
        int min = array.Min();
        int range = max - min + 1;
        
        // Create counting array
        int[] count = new int[range];
        
        // Count occurrences of each element
        foreach (int num in array)
        {
            count[num - min]++;
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

1. **Find Range**: Determine the minimum and maximum values to calculate the range
2. **Count Elements**: Create a counting array where each index represents an element value
3. **Accumulate Counts**: Count how many times each element appears
4. **Reconstruct Array**: Place elements back in sorted order based on the counts

## Time and Space Complexity:
- **Time Complexity**: O(n + k) where n is the number of elements and k is the range
- **Space Complexity**: O(k) for the counting array

## Key Characteristics:
- **Stable**: Maintains relative order of equal elements
- **Non-comparative**: Doesn't use comparisons between elements
- **Efficient**: Best when the range of possible values is not significantly larger than the number of elements

