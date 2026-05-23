# Knapsack Problem - Dynamic Programming Solution

Here's a Python implementation of the 0/1 Knapsack problem using dynamic programming:

```python
def knapsack_01(weights, values, capacity):
    """
    Solve the 0/1 Knapsack problem using dynamic programming
    
    Args:
        weights: List of item weights
        values: List of item values
        capacity: Maximum weight capacity of knapsack
    
    Returns:
        Maximum value that can be obtained
    """
    n = len(weights)
    
    # Create a 2D DP table
    # dp[i][w] represents maximum value with first i items and weight limit w
    dp = [[0 for _ in range(capacity + 1)] for _ in range(n + 1)]
    
    # Fill the DP table
    for i in range(1, n + 1):
        for w in range(capacity + 1):
            # If current item's weight exceeds capacity, skip it
            if weights[i-1] > w:
                dp[i][w] = dp[i-1][w]
            else:
                # Choose maximum between including and excluding current item
                dp[i][w] = max(
                    dp[i-1][w],  # Don't take current item
                    dp[i-1][w - weights[i-1]] + values[i-1]  # Take current item
                )
    
    return dp[n][capacity]

def knapsack_with_items(weights, values, capacity):
    """
    Solve knapsack problem and return both maximum value and selected items
    
    Returns:
        Tuple of (maximum_value, selected_items_indices)
    """
    n = len(weights)
    dp = [[0 for _ in range(capacity + 1)] for _ in range(n + 1)]
    
    # Fill the DP table
    for i in range(1, n + 1):
        for w in range(capacity + 1):
            if weights[i-1] > w:
                dp[i][w] = dp[i-1][w]
            else:
                dp[i][w] = max(
                    dp[i-1][w],
                    dp[i-1][w - weights[i-1]] + values[i-1]
                )
    
    # Backtrack to find selected items
    selected_items = []
    w = capacity
    for i in range(n, 0, -1):
        if dp[i][w] != dp[i-1][w]:
            selected_items.append(i-1)  # Item index (0-based)
            w -= weights[i-1]
    
    selected_items.reverse()
    return dp[n][capacity], selected_items

# Example usage
if __name__ == "__main__":
    # Example 1: Basic knapsack problem
    weights = [2, 1, 3, 2]
    values = [12, 10, 20, 15]
    capacity = 5
    
    max_value = knapsack_01(weights, values, capacity)
    print("Example 1:")
    print(f"Items: weights={weights}, values={values}")
    print(f"Capacity: {capacity}")
    print(f"Maximum value: {max_value}")
    print()
    
    # Example 2: With item tracking
    max_value, items = knapsack_with_items(weights, values, capacity)
    print("Example 2 (with item tracking):")
    print(f"Maximum value: {max_value}")
    print(f"Selected items (indices): {items}")
    print(f"Selected weights: {[weights[i] for i in items]}")
    print(f"Selected values: {[values[i] for i in items]}")
    print()
    
    # Example 3: Another test case
    weights2 = [10, 20, 30]
    values2 = [60, 100, 120]
    capacity2 = 50
    
    max_value2 = knapsack_01(weights2, values2, capacity2)
    print("Example 3:")
    print(f"Items: weights={weights2}, values={values2}")
    print(f"Capacity: {capacity2}")
    print(f"Maximum value: {max_value2}")
```

## Output:
```
Example 1:
Items: weights=[2, 1, 3, 2], values=[12, 10, 20, 15]
Capacity: 5
Maximum value: 37

Example 2 (with item tracking):
Maximum value: 37
Selected items (indices): [0, 1, 3]
Selected weights: [2, 1, 2]
Selected values: [12, 10, 15]

Example 3:
Items: weights=[10, 20, 30], values=[60, 100, 120]
Capacity: 50
Maximum value: 220
```

## Algorithm Explanation:

1. **Dynamic Programming Approach**: Uses a 2D table where `dp[i][w]` represents the maximum value achievable using first `i` items with weight limit `w`.

2. **Recurrence Relation**: 
   ```
   dp[i][w] = max(dp[i-1][w], dp[i-1][w-weight[i]] + value[i])
   ```

3. **Time Complexity**: O(n × W) where n is number of items and W is capacity
4. **Space Complexity**: O(n × W)

This solution handles the classic 0/1 Knapsack problem where each item can be taken at most once.

