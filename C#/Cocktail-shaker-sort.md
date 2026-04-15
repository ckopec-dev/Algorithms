# Cocktail Shaker Sort in C#

Cocktail Shaker Sort (also known as Bidirectional Bubble Sort) is a variation of bubble sort that sorts in both directions on each pass through the list.

```csharp
using System;

public class CocktailShakerSort
{
    public static void CocktailShakerSortAlgorithm(int[] array)
    {
        int left = 0;
        int right = array.Length - 1;
        bool swapped = true;

        while (swapped)
        {
            swapped = false;

            // Forward pass (left to right)
            for (int i = left; i < right; i++)
            {
                if (array[i] > array[i + 1])
                {
                    // Swap elements
                    int temp = array[i];
                    array[i] = array[i + 1];
                    array[i + 1] = temp;
                    swapped = true;
                }
            }

            // Reduce the right boundary since the largest element is now at the end
            right--;

            if (!swapped) break;

            // Backward pass (right to left)
            for (int i = right; i > left; i--)
            {
                if (array[i] < array[i - 1])
                {
                    // Swap elements
                    int temp = array[i];
                    array[i] = array[i - 1];
                    array[i - 1] = temp;
                    swapped = true;
                }
            }

            // Increase the left boundary since the smallest element is now at the beginning
            left++;
        }
    }

    // Helper method to print array
    public static void PrintArray(int[] array)
    {
        foreach (int element in array)
        {
            Console.Write(element + " ");
        }
        Console.WriteLine();
    }

    // Main method to demonstrate the algorithm
    public static void Main(string[] args)
    {
        int[] arr = { 64, 34, 25, 12, 22, 11, 90 };
        
        Console.WriteLine("Original array:");
        PrintArray(arr);

        CocktailShakerSortAlgorithm(arr);

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

1. **Forward Pass**: Start from the left and move towards the right, comparing adjacent elements and swapping if they're in the wrong order
2. **Backward Pass**: Start from the right and move towards the left, comparing adjacent elements and swapping if they're in the wrong order
3. **Boundary Adjustment**: After each pass, adjust the boundaries since the largest/smallest elements are now in their correct positions
4. **Continue**: Repeat until no more swaps are needed

## Time Complexity:
- **Best Case**: O(n) - when array is already sorted
- **Average Case**: O(n²)
- **Worst Case**: O(n²)

## Space Complexity: O(1) - sorts in place

The cocktail shaker sort is more efficient than regular bubble sort because it reduces the number of passes needed by sorting in both directions simultaneously.

