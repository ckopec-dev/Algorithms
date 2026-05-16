# Longest Increasing Subsequence (LIS) Algorithm in C#

Here's an implementation of the Longest Increasing Subsequence algorithm using dynamic programming:

```csharp
using System;
using System.Linq;

public class LISAlgorithm
{
    /// <summary>
    /// Finds the length of the longest increasing subsequence
    /// </summary>
    /// <param name="arr">Input array of integers</param>
    /// <returns>Length of longest increasing subsequence</returns>
    public static int FindLISLength(int[] arr)
    {
        if (arr == null || arr.Length == 0)
            return 0;
        
        int n = arr.Length;
        // dp[i] represents the length of LIS ending at index i
        int[] dp = new int[n];
        
        // Initialize all positions with 1 (each element forms a subsequence of length 1)
        for (int i = 0; i < n; i++)
        {
            dp[i] = 1;
        }
        
        // Fill dp array using dynamic programming
        for (int i = 1; i < n; i++)
        {
            for (int j = 0; j < i; j++)
            {
                if (arr[i] > arr[j] && dp[i] < dp[j] + 1)
                {
                    dp[i] = dp[j] + 1;
                }
            }
        }
        
        // Return maximum value in dp array
        return dp.Max();
    }
    
    /// <summary>
    /// Finds the actual longest increasing subsequence
    /// </summary>
    /// <param name="arr">Input array of integers</param>
    /// <returns>Array representing the longest increasing subsequence</returns>
    public static int[] FindLIS(int[] arr)
    {
        if (arr == null || arr.Length == 0)
            return new int[0];
        
        int n = arr.Length;
        int[] dp = new int[n];
        int[] parent = new int[n]; // To track the previous element in LIS
        
        // Initialize arrays
        for (int i = 0; i < n; i++)
        {
            dp[i] = 1;
            parent[i] = -1; // -1 indicates no parent
        }
        
        // Fill dp array and track parents
        for (int i = 1; i < n; i++)
        {
            for (int j = 0; j < i; j++)
            {
                if (arr[i] > arr[j] && dp[i] < dp[j] + 1)
                {
                    dp[i] = dp[j] + 1;
                    parent[i] = j;
                }
            }
        }
        
        // Find the index with maximum LIS length
        int maxLength = dp.Max();
        int maxIndex = Array.IndexOf(dp, maxLength);
        
        // Reconstruct the LIS
        int[] result = new int[maxLength];
        int currentIndex = maxIndex;
        int resultIndex = maxLength - 1;
        
        while (currentIndex != -1)
        {
            result[resultIndex] = arr[currentIndex];
            currentIndex = parent[currentIndex];
            resultIndex--;
        }
        
        return result;
    }
    
    /// <summary>
    /// Alternative implementation using binary search for O(n log n) time complexity
    /// </summary>
    /// <param name="arr">Input array of integers</param>
    /// <returns>Length of longest increasing subsequence</returns>
    public static int FindLISLengthOptimized(int[] arr)
    {
        if (arr == null || arr.Length == 0)
            return 0;
        
        // This array will store the smallest tail element for increasing subsequences of length i+1
        int[] tails = new int[arr.Length];
        int length = 0;
        
        foreach (int num in arr)
        {
            // Binary search for the position where num should be placed
            int left = 0, right = length;
            while (left < right)
            {
                int mid = left + (right - left) / 2;
                if (tails[mid] < num)
                    left = mid + 1;
                else
                    right = mid;
            }
            
            tails[left] = num;
            
            // If we placed num at the end, increase the length
            if (left == length)
                length++;
        }
        
        return length;
    }
}

// Example usage
class Program
{
    static void Main()
    {
        int[] arr = { 10, 22, 9, 33, 21, 50, 41, 60, 80 };
        
        Console.WriteLine("Input array: [" + string.Join(", ", arr) + "]");
        
        // Find length of LIS
        int lisLength = LISAlgorithm.FindLISLength(arr);
        Console.WriteLine("Length of LIS: " + lisLength);
        
        // Find actual LIS
        int[] lis = LISAlgorithm.FindLIS(arr);
        Console.WriteLine("Longest Increasing Subsequence: [" + string.Join(", ", lis) + "]");
        
        // Optimized version
        int lisLengthOptimized = LISAlgorithm.FindLISLengthOptimized(arr);
        Console.WriteLine("Length of LIS (Optimized): " + lisLengthOptimized);
    }
}
```

## Output:
```
Input array: [10, 22, 9, 33, 21, 50, 41, 60, 80]
Length of LIS: 6
Longest Increasing Subsequence: [10, 22, 33, 50, 60, 80]
Length of LIS (Optimized): 6
```

## Algorithm Explanation:

### Basic DP Approach (O(n²)):
1. **Initialization**: Create a `dp` array where `dp[i]` represents the length of LIS ending at index `i`
2. **Fill DP Array**: For each element, check all previous elements that are smaller and update the LIS length
3. **Result**: Return the maximum value in the `dp` array

### Optimized Approach (O(n log n)):
1. **Binary Search**: Maintain an array `tails` where `tails[i]` is the smallest tail element of all increasing subsequences of length `i+1`
2. **Binary Search Placement**: For each element, find the correct position using binary search
3. **Update**: Place the element in the correct position and extend the length if needed

## Time and Space Complexity:
- **Basic DP**: O(n²) time, O(n) space
- **Optimized**: O(n log n) time, O(n) space

The algorithm correctly identifies that in the array `[10, 22, 9, 33, 21, 50, 41, 60, 80]`, the longest increasing subsequence is `[10, 22, 33, 50, 60, 80]` with length 6.

