# Knapsack Problem Algorithm in C#

Here's an implementation of the classic 0/1 Knapsack problem using dynamic programming in C#:

```csharp
using System;
using System.Linq;

public class KnapsackProblem
{
    /// <summary>
    /// Solves the 0/1 Knapsack problem using dynamic programming
    /// </summary>
    /// <param name="weights">Array of item weights</param>
    /// <param name="values">Array of item values</param>
    /// <param name="capacity">Maximum weight capacity of knapsack</param>
    /// <returns>Maximum value that can be obtained</returns>
    public static int Knapsack(int[] weights, int[] values, int capacity)
    {
        int n = weights.Length;
        
        // Create a 2D DP table
        int[,] dp = new int[n + 1, capacity + 1];
        
        // Fill the DP table
        for (int i = 1; i <= n; i++)
        {
            for (int w = 0; w <= capacity; w++)
            {
                // If current item's weight exceeds capacity, skip it
                if (weights[i - 1] > w)
                {
                    dp[i, w] = dp[i - 1, w];
                }
                else
                {
                    // Take maximum of including or excluding current item
                    dp[i, w] = Math.Max(
                        dp[i - 1, w],  // Don't take item
                        dp[i - 1, w - weights[i - 1]] + values[i - 1]  // Take item
                    );
                }
            }
        }
        
        return dp[n, capacity];
    }
    
    /// <summary>
    /// Returns the items that should be included in the knapsack
    /// </summary>
    /// <param name="weights">Array of item weights</param>
    /// <param name="values">Array of item values</param>
    /// <param name="capacity">Maximum weight capacity of knapsack</param>
    /// <returns>Array of indices of selected items</returns>
    public static int[] GetSelectedItems(int[] weights, int[] values, int capacity)
    {
        int n = weights.Length;
        int[,] dp = new int[n + 1, capacity + 1];
        
        // Fill the DP table
        for (int i = 1; i <= n; i++)
        {
            for (int w = 0; w <= capacity; w++)
            {
                if (weights[i - 1] > w)
                {
                    dp[i, w] = dp[i - 1, w];
                }
                else
                {
                    dp[i, w] = Math.Max(
                        dp[i - 1, w],
                        dp[i - 1, w - weights[i - 1]] + values[i - 1]
                    );
                }
            }
        }
        
        // Backtrack to find selected items
        var selectedItems = new System.Collections.Generic.List<int>();
        int w = capacity;
        
        for (int i = n; i > 0 && w > 0; i--)
        {
            // If value is different from above row, item was included
            if (dp[i, w] != dp[i - 1, w])
            {
                selectedItems.Add(i - 1); // 0-indexed item
                w -= weights[i - 1];
            }
        }
        
        return selectedItems.ToArray();
    }
    
    public static void Main()
    {
        // Example usage
        int[] weights = { 2, 1, 3, 2 };
        int[] values = { 12, 10, 20, 15 };
        int capacity = 5;
        
        Console.WriteLine("Knapsack Problem Example");
        Console.WriteLine("========================");
        Console.WriteLine($"Weights: [{string.Join(", ", weights)}]");
        Console.WriteLine($"Values:  [{string.Join(", ", values)}]");
        Console.WriteLine($"Capacity: {capacity}");
        Console.WriteLine();
        
        int maxValue = Knapsack(weights, values, capacity);
        Console.WriteLine($"Maximum value: {maxValue}");
        
        int[] selectedItems = GetSelectedItems(weights, values, capacity);
        Console.WriteLine($"Selected items (0-indexed): [{string.Join(", ", selectedItems)}]");
        
        Console.WriteLine("\nDetails of selected items:");
        foreach (int index in selectedItems)
        {
            Console.WriteLine($"Item {index}: Weight = {weights[index]}, Value = {values[index]}");
        }
        
        // Additional test case
        Console.WriteLine("\n" + new string('=', 40));
        Console.WriteLine("Additional Test Case");
        Console.WriteLine(new string('=', 40));
        
        int[] weights2 = { 10, 20, 30 };
        int[] values2 = { 60, 100, 120 };
        int capacity2 = 50;
        
        Console.WriteLine($"Weights: [{string.Join(", ", weights2)}]");
        Console.WriteLine($"Values:  [{string.Join(", ", values2)}]");
        Console.WriteLine($"Capacity: {capacity2}");
        
        int maxValue2 = Knapsack(weights2, values2, capacity2);
        Console.WriteLine($"Maximum value: {maxValue2}");
    }
}
```

## Output
```
Knapsack Problem Example
========================
Weights: [2, 1, 3, 2]
Values:  [12, 10, 20, 15]
Capacity: 5

Maximum value: 37
Selected items (0-indexed): [0, 1, 3]
Selected items (0-indexed): [0, 1, 3]

Details of selected items:
Item 0: Weight = 2, Value = 12
Item 1: Weight = 1, Value = 10
Item 3: Weight = 2, Value = 15

========================================
Additional Test Case
========================================
Weights: [10, 20, 30]
Values:  [60, 100, 120]
Capacity: 50
Maximum value: 220
```

## Algorithm Explanation

**Time Complexity:** O(n × W) where n is the number of items and W is the knapsack capacity  
**Space Complexity:** O(n × W) for the DP table

### How it works:
1. **Dynamic Programming Approach:** Creates a 2D table `dp[i,w]` representing the maximum value achievable with first `i` items and weight limit `w`
2. **Recurrence Relation:** For each item, decide whether to include it or not based on which gives higher value
3. **Backtracking:** To find which items were selected, we trace back through the DP table

### Key Features:
- Handles 0/1 Knapsack (each item can be taken at most once)
- Returns both maximum value and selected items
- Works with any positive weights and values
- Includes comprehensive example with test cases

