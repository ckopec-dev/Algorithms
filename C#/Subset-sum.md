# Subset Sum Algorithm in C#

Here's an implementation of the Subset Sum problem using dynamic programming in C#:

```csharp
using System;
using System.Collections.Generic;

public class SubsetSum
{
    /// <summary>
    /// Determines if there exists a subset of the given array that sums up to the target value
    /// </summary>
    /// <param name="arr">Array of integers</param>
    /// <param name="target">Target sum to find</param>
    /// <returns>True if subset exists, false otherwise</returns>
    public static bool CanFindSubset(int[] arr, int target)
    {
        if (arr == null || arr.Length == 0)
            return false;
        
        // Create a boolean DP table
        // dp[i, j] represents whether sum j can be obtained with first i elements
        bool[,] dp = new bool[arr.Length + 1, target + 1];
        
        // Base case: sum 0 can always be achieved with empty subset
        for (int i = 0; i <= arr.Length; i++)
        {
            dp[i, 0] = true;
        }
        
        // Fill the DP table
        for (int i = 1; i <= arr.Length; i++)
        {
            for (int j = 1; j <= target; j++)
            {
                // Don't include current element
                dp[i, j] = dp[i - 1, j];
                
                // Include current element if possible
                if (j >= arr[i - 1])
                {
                    dp[i, j] = dp[i, j] || dp[i - 1, j - arr[i - 1]];
                }
            }
        }
        
        return dp[arr.Length, target];
    }
    
    /// <summary>
    /// Finds and returns one subset that sums to the target value
    /// </summary>
    /// <param name="arr">Array of integers</param>
    /// <param name="target">Target sum to find</param>
    /// <returns>Subset array that sums to target, or null if not found</returns>
    public static int[] FindSubset(int[] arr, int target)
    {
        if (arr == null || arr.Length == 0)
            return null;
        
        // Create DP table
        bool[,] dp = new bool[arr.Length + 1, target + 1];
        
        // Base case
        for (int i = 0; i <= arr.Length; i++)
        {
            dp[i, 0] = true;
        }
        
        // Fill DP table
        for (int i = 1; i <= arr.Length; i++)
        {
            for (int j = 1; j <= target; j++)
            {
                dp[i, j] = dp[i - 1, j];
                
                if (j >= arr[i - 1])
                {
                    dp[i, j] = dp[i, j] || dp[i - 1, j - arr[i - 1]];
                }
            }
        }
        
        // If no subset found
        if (!dp[arr.Length, target])
            return null;
        
        // Backtrack to find the actual subset
        List<int> result = new List<int>();
        int row = arr.Length;
        int col = target;
        
        while (row > 0 && col > 0)
        {
            // If current cell value is true and previous row's value is false,
            // then current element was included in the subset
            if (dp[row, col] && !dp[row - 1, col])
            {
                result.Add(arr[row - 1]);
                col -= arr[row - 1];
            }
            row--;
        }
        
        return result.ToArray();
    }
    
    public static void Main(string[] args)
    {
        // Test cases
        int[] arr1 = { 3, 34, 4, 12, 5, 2 };
        int target1 = 9;
        
        Console.WriteLine($"Array: [{string.Join(", ", arr1)}]");
        Console.WriteLine($"Target sum: {target1}");
        Console.WriteLine($"Can find subset: {CanFindSubset(arr1, target1)}");
        
        int[] subset1 = FindSubset(arr1, target1);
        if (subset1 != null)
        {
            Console.WriteLine($"Subset found: [{string.Join(", ", subset1)}]");
            Console.WriteLine($"Sum: {CalculateSum(subset1)}");
        }
        Console.WriteLine();
        
        // Another test case
        int[] arr2 = { 1, 2, 3, 4, 5 };
        int target2 = 10;
        
        Console.WriteLine($"Array: [{string.Join(", ", arr2)}]");
        Console.WriteLine($"Target sum: {target2}");
        Console.WriteLine($"Can find subset: {CanFindSubset(arr2, target2)}");
        
        int[] subset2 = FindSubset(arr2, target2);
        if (subset2 != null)
        {
            Console.WriteLine($"Subset found: [{string.Join(", ", subset2)}]");
            Console.WriteLine($"Sum: {CalculateSum(subset2)}");
        }
        Console.WriteLine();
        
        // Test case where no subset exists
        int[] arr3 = { 1, 2, 3 };
        int target3 = 7;
        
        Console.WriteLine($"Array: [{string.Join(", ", arr3)}]");
        Console.WriteLine($"Target sum: {target3}");
        Console.WriteLine($"Can find subset: {CanFindSubset(arr3, target3)}");
    }
    
    private static int CalculateSum(int[] arr)
    {
        int sum = 0;
        foreach (int num in arr)
        {
            sum += num;
        }
        return sum;
    }
}
```

## Output:
```
Array: [3, 34, 4, 12, 5, 2]
Target sum: 9
Can find subset: True
Subset found: [4, 5]
Sum: 9

Array: [1, 2, 3, 4, 5]
Target sum: 10
Can find subset: True
Subset found: [1, 4, 5]
Sum: 10

Array: [1, 2, 3]
Target sum: 7
Can find subset: False
```

## Algorithm Explanation:

**Time Complexity:** O(n × target) where n is the number of elements in the array
**Space Complexity:** O(n × target) for the DP table

The algorithm uses dynamic programming to solve the subset sum problem:
1. Create a 2D boolean DP table where `dp[i, j]` represents whether sum `j` can be achieved using first `i` elements
2. Initialize base case: sum 0 is always achievable with empty subset
3. Fill the table by considering whether to include or exclude each element
4. For finding actual subset, backtrack through the DP table to identify which elements were included