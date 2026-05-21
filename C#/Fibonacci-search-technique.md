# Fibonacci Search Technique in C#

```csharp
using System;

public class FibonacciSearch
{
    /// <summary>
    /// Performs Fibonacci search on a sorted array
    /// </summary>
    /// <param name="arr">Sorted array to search</param>
    /// <param name="target">Element to search for</param>
    /// <returns>Index of target element if found, -1 if not found</returns>
    public static int FibonacciSearchMethod(int[] arr, int target)
    {
        int n = arr.Length;
        
        // Generate Fibonacci numbers
        int fibM2 = 0; // (m-2)th Fibonacci number
        int fibM1 = 1; // (m-1)th Fibonacci number
        int fibM = fibM2 + fibM1; // mth Fibonacci number
        
        // Find the smallest Fibonacci number greater than or equal to n
        while (fibM < n)
        {
            fibM2 = fibM1;
            fibM1 = fibM;
            fibM = fibM2 + fibM1;
        }
        
        int offset = -1; // Marks the eliminated range from front
        
        // While there are elements to be inspected
        while (fibM > 1)
        {
            // Check if fibM2 is a valid location
            int i = Math.Min(offset + fibM2, n - 1);
            
            // If target is greater than the value at index fibM2,
            // cut the subarray from offset to i
            if (arr[i] < target)
            {
                fibM = fibM1;
                fibM1 = fibM2;
                fibM2 = fibM - fibM1;
                offset = i;
            }
            // If target is less than the value at index fibM2,
            // cut the subarray after i
            else if (arr[i] > target)
            {
                fibM = fibM2;
                fibM1 = fibM1 - fibM2;
                fibM2 = fibM - fibM1;
            }
            // Element found
            else
            {
                return i;
            }
        }
        
        // Comparing the last element with target
        if (fibM1 == 1 && arr[offset + 1] == target)
        {
            return offset + 1;
        }
        
        // Element not found
        return -1;
    }
    
    public static void Main(string[] args)
    {
        int[] arr = { 10, 22, 35, 40, 45, 50, 80, 82, 85, 90, 100 };
        int target = 85;
        
        Console.WriteLine("Array: [" + string.Join(", ", arr) + "]");
        Console.WriteLine("Searching for: " + target);
        
        int result = FibonacciSearchMethod(arr, target);
        
        if (result != -1)
        {
            Console.WriteLine("Element found at index: " + result);
        }
        else
        {
            Console.WriteLine("Element not found in array");
        }
        
        // Test with element not in array
        target = 95;
        Console.WriteLine("\nSearching for: " + target);
        result = FibonacciSearchMethod(arr, target);
        
        if (result != -1)
        {
            Console.WriteLine("Element found at index: " + result);
        }
        else
        {
            Console.WriteLine("Element not found in array");
        }
    }
}
```

## Output:
```
Array: [10, 22, 35, 40, 45, 50, 80, 82, 85, 90, 100]
Searching for: 85
Element found at index: 8

Searching for: 95
Element not found in array
```

## How it works:

1. **Fibonacci Sequence Generation**: The algorithm generates Fibonacci numbers until finding the smallest one greater than or equal to the array size
2. **Index Calculation**: Uses Fibonacci numbers to determine the next position to check
3. **Comparison Logic**: 
   - If target > element at current position: move right
   - If target < element at current position: move left
   - If target = element at current position: found!
4. **Time Complexity**: O(log n)
5. **Space Complexity**: O(1)

The Fibonacci search technique is particularly useful when the array size is a Fibonacci number, but works well for any array size. It's especially efficient when the cost of accessing elements is high, as it minimizes the number of comparisons needed.

