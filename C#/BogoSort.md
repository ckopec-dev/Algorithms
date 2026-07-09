```csharp
using System;
using System.Linq;

public class BogoSort
{
    // Main BogoSort method
    public static void BogoSortAlgorithm(int[] array)
    {
        Random random = new Random();
        
        while (!IsSorted(array))
        {
            ShuffleArray(array, random);
        }
    }
    
    // Check if array is sorted
    private static bool IsSorted(int[] array)
    {
        for (int i = 1; i < array.Length; i++)
        {
            if (array[i] < array[i - 1])
                return false;
        }
        return true;
    }
    
    // Shuffle array randomly
    private static void ShuffleArray(int[] array, Random random)
    {
        for (int i = 0; i < array.Length; i++)
        {
            int j = random.Next(i, array.Length);
            Swap(array, i, j);
        }
    }
    
    // Helper method to swap two elements
    private static void Swap(int[] array, int i, int j)
    {
        int temp = array[i];
        array[i] = array[j];
        array[j] = temp;
    }
    
    // Print array helper
    public static void PrintArray(int[] array)
    {
        Console.WriteLine("[" + string.Join(", ", array) + "]");
    }
}

// Example usage
class Program
{
    static void Main()
    {
        int[] numbers = { 3, 1, 4, 1, 5, 9, 2, 6 };
        
        Console.WriteLine("Original array:");
        BogoSort.PrintArray(numbers);
        
        Console.WriteLine("Sorting...");
        BogoSort.BogoSortAlgorithm(numbers);
        
        Console.WriteLine("Sorted array:");
        BogoSort.PrintArray(numbers);
    }
}
```

**Output:**
```
Original array:
[3, 1, 4, 1, 5, 9, 2, 6]
Sorting...
Sorted array:
[1, 1, 2, 3, 4, 5, 6, 9]
```

**How it works:**
1. **IsSorted()**: Checks if the array is in ascending order
2. **ShuffleArray()**: Randomly shuffles the elements using Fisher-Yates algorithm
3. **BogoSortAlgorithm()**: Repeatedly shuffles the array until it becomes sorted

**Note:** BogoSort has an average time complexity of O(n! × n) and should only be used for educational purposes, as it's extremely inefficient for any practical application.