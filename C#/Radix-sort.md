# Radix Sort Algorithm in C#

Here's an implementation of the Radix Sort algorithm in C#:

```csharp
using System;
using System.Linq;

public class RadixSort
{
    // Main radix sort function
    public static void RadixSortAlgorithm(int[] arr)
    {
        if (arr == null || arr.Length <= 1)
            return;

        // Find the maximum number to know number of digits
        int max = arr.Max();

        // Do counting sort for every digit
        for (int exp = 1; max / exp > 0; exp *= 10)
        {
            CountingSortByDigit(arr, exp);
        }
    }

    // A function to do counting sort of arr[] according to the digit represented by exp
    private static void CountingSortByDigit(int[] arr, int exp)
    {
        int n = arr.Length;
        int[] output = new int[n]; // output array
        int[] count = new int[10]; // count array (0-9)

        // Store count of occurrences of each digit
        for (int i = 0; i < n; i++)
        {
            count[(arr[i] / exp) % 10]++;
        }

        // Change count[i] so that count[i] contains actual position of this digit in output[]
        for (int i = 1; i < 10; i++)
        {
            count[i] += count[i - 1];
        }

        // Build the output array
        for (int i = n - 1; i >= 0; i--)
        {
            output[count[(arr[i] / exp) % 10] - 1] = arr[i];
            count[(arr[i] / exp) % 10]--;
        }

        // Copy the output array to arr[], so that arr[] now contains sorted numbers according to current digit
        for (int i = 0; i < n; i++)
        {
            arr[i] = output[i];
        }
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
}

// Example usage
class Program
{
    static void Main()
    {
        int[] arr = { 170, 45, 75, 90, 2, 802, 24, 66 };

        Console.WriteLine("Original array:");
        RadixSort.PrintArray(arr);

        RadixSort.RadixSortAlgorithm(arr);

        Console.WriteLine("Sorted array:");
        RadixSort.PrintArray(arr);
    }
}
```

## Output:
```
Original array:
170 45 75 90 2 802 24 66 
Sorted array:
2 24 45 66 75 90 170 802 
```

## How it works:

1. **Find Maximum**: First, we find the maximum number to determine how many digits we need to process
2. **Process Each Digit**: Starting from the least significant digit (units place), we perform counting sort for each digit position
3. **Counting Sort**: For each digit position, we use counting sort to maintain the relative order of elements with the same digit
4. **Repeat**: Continue this process for all digit positions until we've processed the most significant digit

## Time Complexity:
- **Best Case**: O(d × n)
- **Average Case**: O(d × n)  
- **Worst Case**: O(d × n)

Where `d` is the number of digits and `n` is the number of elements.

## Space Complexity: O(n + k) where k is the range of input (10 for decimal digits)

