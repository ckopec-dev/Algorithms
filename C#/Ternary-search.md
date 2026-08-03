```csharp
using System;

public class TernarySearch
{
    /// <summary>
    /// Performs ternary search on a sorted array
    /// </summary>
    /// <param name="arr">Sorted array to search</param>
    /// <param name="target">Element to search for</param>
    /// <returns>Index of target element if found, -1 otherwise</returns>
    public static int TernarySearchRecursive(int[] arr, int target)
    {
        return TernarySearchHelper(arr, target, 0, arr.Length - 1);
    }
    
    private static int TernarySearchHelper(int[] arr, int target, int left, int right)
    {
        // Base case: element not found
        if (left > right)
            return -1;
        
        // Calculate two mid points
        int mid1 = left + (right - left) / 3;
        int mid2 = right - (right - left) / 3;
        
        // Check if target is at either mid point
        if (arr[mid1] == target)
            return mid1;
        
        if (arr[mid2] == target)
            return mid2;
        
        // Recursively search in the appropriate segment
        if (target < arr[mid1])
        {
            // Search in left segment
            return TernarySearchHelper(arr, target, left, mid1 - 1);
        }
        else if (target > arr[mid2])
        {
            // Search in right segment
            return TernarySearchHelper(arr, target, mid2 + 1, right);
        }
        else
        {
            // Search in middle segment
            return TernarySearchHelper(arr, target, mid1 + 1, mid2 - 1);
        }
    }
    
    /// <summary>
    /// Iterative implementation of ternary search
    /// </summary>
    public static int TernarySearchIterative(int[] arr, int target)
    {
        int left = 0;
        int right = arr.Length - 1;
        
        while (left <= right)
        {
            // Calculate two mid points
            int mid1 = left + (right - left) / 3;
            int mid2 = right - (right - left) / 3;
            
            // Check if target is at either mid point
            if (arr[mid1] == target)
                return mid1;
            
            if (arr[mid2] == target)
                return mid2;
            
            // Narrow down the search space
            if (target < arr[mid1])
            {
                // Search in left segment
                right = mid1 - 1;
            }
            else if (target > arr[mid2])
            {
                // Search in right segment
                left = mid2 + 1;
            }
            else
            {
                // Search in middle segment
                left = mid1 + 1;
                right = mid2 - 1;
            }
        }
        
        return -1; // Element not found
    }
    
    public static void Main(string[] args)
    {
        int[] sortedArray = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
        
        Console.WriteLine("Sorted Array: [" + string.Join(", ", sortedArray) + "]");
        Console.WriteLine();
        
        // Test cases
        int[] testValues = { 1, 5, 10, 3, 7, 15 };
        
        foreach (int target in testValues)
        {
            int resultRecursive = TernarySearchRecursive(sortedArray, target);
            int resultIterative = TernarySearchIterative(sortedArray, target);
            
            Console.WriteLine($"Searching for {target}:");
            Console.WriteLine($"  Recursive: Index {resultRecursive}");
            Console.WriteLine($"  Iterative: Index {resultIterative}");
            Console.WriteLine();
        }
    }
}
```

**Output:**
```
Sorted Array: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

Searching for 1:
  Recursive: Index 0
  Iterative: Index 0

Searching for 5:
  Recursive: Index 4
  Iterative: Index 4

Searching for 10:
  Recursive: Index 9
  Iterative: Index 9

Searching for 3:
  Recursive: Index 2
  Iterative: Index 2

Searching for 7:
  Recursive: Index 6
  Iterative: Index 6

Searching for 15:
  Recursive: Index -1
  Iterative: Index -1
```

**Key Features of this Implementation:**

1. **Two Versions**: Both recursive and iterative implementations provided
2. **Proper Mid Point Calculation**: Uses `left + (right - left) / 3` to avoid integer overflow
3. **Clear Logic Flow**: Divides array into three segments and searches accordingly
4. **Edge Case Handling**: Properly handles empty arrays and elements not found
5. **Time Complexity**: O(log₃ n) - logarithmic with base 3
6. **Space Complexity**: 
   - Recursive: O(log₃ n) due to call stack
   - Iterative: O(1)

**How it works:**
- Divides the array into three parts using two mid points
- Compares target with both mid points
- Eliminates one-third of the search space in each iteration
- Continues until element is found or search space is exhausted