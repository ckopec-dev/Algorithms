# Subset Sum Algorithm in C++

Here's a complete implementation of the subset sum problem using dynamic programming:

```cpp
#include <iostream>
#include <vector>
using namespace std;

class SubsetSum {
public:
    // Method 1: Using Dynamic Programming (Bottom-up approach)
    bool isSubsetSum(vector<int>& arr, int target) {
        int n = arr.size();
        
        // Create a 2D DP table
        vector<vector<bool>> dp(n + 1, vector<bool>(target + 1, false));
        
        // Base case: sum of 0 is always possible (empty subset)
        for (int i = 0; i <= n; i++) {
            dp[i][0] = true;
        }
        
        // Fill the DP table
        for (int i = 1; i <= n; i++) {
            for (int j = 1; j <= target; j++) {
                // If current element is greater than target sum, skip it
                if (arr[i-1] > j) {
                    dp[i][j] = dp[i-1][j];
                } else {
                    // Either include the element or exclude it
                    dp[i][j] = dp[i-1][j] || dp[i-1][j - arr[i-1]];
                }
            }
        }
        
        return dp[n][target];
    }
    
    // Method 2: Space optimized version using 1D array
    bool isSubsetSumOptimized(vector<int>& arr, int target) {
        vector<bool> dp(target + 1, false);
        dp[0] = true; // Sum of 0 is always possible
        
        for (int num : arr) {
            // Traverse backwards to avoid using updated values
            for (int j = target; j >= num; j--) {
                dp[j] = dp[j] || dp[j - num];
            }
        }
        
        return dp[target];
    }
    
    // Method 3: Print all possible subsets that sum to target
    void printAllSubsets(vector<int>& arr, int target) {
        vector<vector<int>> result;
        vector<int> current;
        
        findSubsets(arr, target, 0, current, result);
        
        cout << "All subsets with sum " << target << ": " << endl;
        for (const auto& subset : result) {
            cout << "{ ";
            for (int num : subset) {
                cout << num << " ";
            }
            cout << "}" << endl;
        }
    }
    
private:
    void findSubsets(vector<int>& arr, int target, int index, 
                     vector<int>& current, vector<vector<int>>& result) {
        if (target == 0) {
            result.push_back(current);
            return;
        }
        
        if (index >= arr.size() || target < 0) {
            return;
        }
        
        // Include current element
        current.push_back(arr[index]);
        findSubsets(arr, target - arr[index], index + 1, current, result);
        current.pop_back();
        
        // Exclude current element
        findSubsets(arr, target, index + 1, current, result);
    }
};

int main() {
    SubsetSum ss;
    
    // Test case 1
    vector<int> arr1 = {3, 34, 4, 12, 5, 2};
    int target1 = 9;
    
    cout << "Array: ";
    for (int num : arr1) {
        cout << num << " ";
    }
    cout << endl;
    cout << "Target sum: " << target1 << endl;
    
    bool result1 = ss.isSubsetSum(arr1, target1);
    cout << "Can subset sum " << target1 << " be achieved? " 
         << (result1 ? "Yes" : "No") << endl;
    
    // Test case 2 - Space optimized version
    bool result2 = ss.isSubsetSumOptimized(arr1, target1);
    cout << "Space optimized result: " << (result2 ? "Yes" : "No") << endl;
    
    cout << "\n" << string(40, '-') << "\n" << endl;
    
    // Test case 3 - Find all subsets
    vector<int> arr2 = {1, 2, 3, 4, 5};
    int target2 = 5;
    
    cout << "Array: ";
    for (int num : arr2) {
        cout << num << " ";
    }
    cout << endl;
    cout << "Target sum: " << target2 << endl;
    
    ss.printAllSubsets(arr2, target2);
    
    return 0;
}
```

## Output:
```
Array: 3 34 4 12 5 2 
Target sum: 9
Can subset sum 9 be achieved? Yes
Space optimized result: Yes

----------------------------------------

Array: 1 2 3 4 5 
Target sum: 5
All subsets with sum 5: 
{ 1 4 }
{ 2 3 }
{ 5 }
```

## Key Features:

1. **Dynamic Programming Approach**: Uses a 2D table to store intermediate results
2. **Space Optimization**: Includes a 1D array version that reduces space complexity from O(n×target) to O(target)
3. **Subset Enumeration**: Can find and print all possible subsets that sum to the target
4. **Time Complexity**: O(n × target) where n is the number of elements
5. **Space Complexity**: O(target) for optimized version

## How it works:

1. **Base Case**: Sum of 0 is always achievable with an empty subset
2. **Recurrence Relation**: For each element, we can either include it or exclude it
3. **DP Transition**: `dp[i][j] = dp[i-1][j] || dp[i-1][j - arr[i-1]]`

This algorithm efficiently solves the classic subset sum problem and can be easily modified for various related problems.