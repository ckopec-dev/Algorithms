# Knapsack Problem - Dynamic Programming Solution in C++

Here's a complete implementation of the 0/1 Knapsack problem using dynamic programming:

```cpp
#include <iostream>
#include <vector>
#include <algorithm>
using namespace std;

class Knapsack {
public:
    // 0/1 Knapsack problem using dynamic programming
    static int knapsack(int capacity, vector<int>& weights, vector<int>& values) {
        int n = weights.size();
        
        // Create a 2D DP table
        vector<vector<int>> dp(n + 1, vector<int>(capacity + 1, 0));
        
        // Fill the DP table
        for (int i = 1; i <= n; i++) {
            for (int w = 0; w <= capacity; w++) {
                // If current item's weight exceeds capacity, skip it
                if (weights[i-1] > w) {
                    dp[i][w] = dp[i-1][w];
                } else {
                    // Max of: not taking item OR taking item + best solution for remaining capacity
                    dp[i][w] = max(dp[i-1][w], 
                                  values[i-1] + dp[i-1][w - weights[i-1]]);
                }
            }
        }
        
        return dp[n][capacity];
    }
    
    // Function to find which items were selected
    static vector<int> getSelectedItems(int capacity, vector<int>& weights, vector<int>& values) {
        int n = weights.size();
        vector<vector<int>> dp(n + 1, vector<int>(capacity + 1, 0));
        
        // Fill DP table
        for (int i = 1; i <= n; i++) {
            for (int w = 0; w <= capacity; w++) {
                if (weights[i-1] > w) {
                    dp[i][w] = dp[i-1][w];
                } else {
                    dp[i][w] = max(dp[i-1][w], 
                                  values[i-1] + dp[i-1][w - weights[i-1]]);
                }
            }
        }
        
        // Backtrack to find selected items
        vector<int> selectedItems;
        int w = capacity;
        for (int i = n; i > 0; i--) {
            // If value differs from above row, item was included
            if (dp[i][w] != dp[i-1][w]) {
                selectedItems.push_back(i-1); // 0-indexed item number
                w -= weights[i-1];
            }
        }
        
        reverse(selectedItems.begin(), selectedItems.end());
        return selectedItems;
    }
};

int main() {
    // Example: Items with weights and values
    vector<int> weights = {2, 1, 3, 2};
    vector<int> values = {12, 10, 20, 15};
    int capacity = 5;
    
    cout << "Knapsack Problem Example" << endl;
    cout << "========================" << endl;
    cout << "Capacity: " << capacity << endl;
    cout << "Items:" << endl;
    
    for (int i = 0; i < weights.size(); i++) {
        cout << "Item " << i << ": Weight = " << weights[i] 
             << ", Value = " << values[i] << endl;
    }
    
    // Solve knapsack problem
    int maxProfit = Knapsack::knapsack(capacity, weights, values);
    cout << "\nMaximum profit: " << maxProfit << endl;
    
    // Find selected items
    vector<int> selected = Knapsack::getSelectedItems(capacity, weights, values);
    cout << "Selected items (0-indexed): ";
    for (int item : selected) {
        cout << item << " ";
    }
    cout << endl;
    
    // Show total weight and value of selected items
    int totalWeight = 0, totalValue = 0;
    for (int item : selected) {
        totalWeight += weights[item];
        totalValue += values[item];
    }
    cout << "Total weight of selected items: " << totalWeight << endl;
    cout << "Total value of selected items: " << totalValue << endl;
    
    return 0;
}
```

## Output:
```
Knapsack Problem Example
========================
Capacity: 5
Items:
Item 0: Weight = 2, Value = 12
Item 1: Weight = 1, Value = 10
Item 2: Weight = 3, Value = 20
Item 3: Weight = 2, Value = 15

Maximum profit: 37
Selected items (0-indexed): 0 1 3 
Total weight of selected items: 5
Total value of selected items: 37
```

## Algorithm Explanation:

1. **Dynamic Programming Approach**: Uses a 2D table `dp[i][w]` where:
   - `i` represents the first `i` items
   - `w` represents the current capacity
   - `dp[i][w]` stores the maximum value achievable with first `i` items and capacity `w`

2. **Recurrence Relation**:
   ```
   dp[i][w] = max(dp[i-1][w], values[i-1] + dp[i-1][w - weights[i-1]])
   ```

3. **Time Complexity**: O(n × W) where n is number of items and W is capacity
4. **Space Complexity**: O(n × W)

## Key Features:
- Solves the classic 0/1 Knapsack problem
- Returns both maximum profit and selected items
- Handles edge cases (empty knapsack, items too heavy)
- Efficient dynamic programming solution

This implementation demonstrates the fundamental approach to solving knapsack problems using dynamic programming, which is a classic optimization technique in computer science.

