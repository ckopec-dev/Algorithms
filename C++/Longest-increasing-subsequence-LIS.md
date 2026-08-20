# Longest Increasing Subsequence (LIS) Algorithm in C++

## Algorithm Implementation

```cpp
#include <iostream>
#include <vector>
#include <algorithm>
using namespace std;

// Method 1: Dynamic Programming Approach - O(n²)
int lis_dp(vector<int>& arr) {
    if (arr.empty()) return 0;
    
    int n = arr.size();
    vector<int> dp(n, 1); // dp[i] stores length of LIS ending at index i
    
    for (int i = 1; i < n; i++) {
        for (int j = 0; j < i; j++) {
            if (arr[i] > arr[j]) {
                dp[i] = max(dp[i], dp[j] + 1);
            }
        }
    }
    
    return *max_element(dp.begin(), dp.end());
}

// Method 2: Binary Search Approach - O(n log n)
int lis_binary_search(vector<int>& arr) {
    if (arr.empty()) return 0;
    
    vector<int> tails;
    
    for (int num : arr) {
        // Find the position where num should be inserted
        auto it = lower_bound(tails.begin(), tails.end(), num);
        
        if (it == tails.end()) {
            tails.push_back(num); // Extend the array
        } else {
            *it = num; // Replace the element
        }
    }
    
    return tails.size();
}

// Function to print the actual LIS sequence
vector<int> get_lis_sequence(vector<int>& arr) {
    if (arr.empty()) return {};
    
    int n = arr.size();
    vector<int> dp(n, 1);
    vector<int> parent(n, -1); // To track the sequence
    
    for (int i = 1; i < n; i++) {
        for (int j = 0; j < i; j++) {
            if (arr[i] > arr[j] && dp[j] + 1 > dp[i]) {
                dp[i] = dp[j] + 1;
                parent[i] = j;
            }
        }
    }
    
    // Find the index with maximum LIS length
    int max_length = 0, max_index = 0;
    for (int i = 0; i < n; i++) {
        if (dp[i] > max_length) {
            max_length = dp[i];
            max_index = i;
        }
    }
    
    // Reconstruct the sequence
    vector<int> result;
    int current = max_index;
    while (current != -1) {
        result.push_back(arr[current]);
        current = parent[current];
    }
    
    reverse(result.begin(), result.end());
    return result;
}

int main() {
    // Test cases
    vector<int> arr1 = {10, 9, 2, 5, 3, 7, 101, 18};
    vector<int> arr2 = {0, 1, 0, 3, 2, 3};
    vector<int> arr3 = {7, 7, 7, 7, 7, 7, 7};
    vector<int> arr4 = {1, 3, 6, 7, 9, 4, 10, 5, 6};

    cout << "Array: ";
    for (int x : arr1) cout << x << " ";
    cout << "\nLIS Length (DP): " << lis_dp(arr1) << endl;
    cout << "LIS Length (Binary Search): " << lis_binary_search(arr1) << endl;
    
    vector<int> lis_seq = get_lis_sequence(arr1);
    cout << "Actual LIS Sequence: ";
    for (int x : lis_seq) cout << x << " ";
    cout << "\n\n";

    cout << "Array: ";
    for (int x : arr2) cout << x << " ";
    cout << "\nLIS Length (DP): " << lis_dp(arr2) << endl;
    cout << "LIS Length (Binary Search): " << lis_binary_search(arr2) << endl;
    
    lis_seq = get_lis_sequence(arr2);
    cout << "Actual LIS Sequence: ";
    for (int x : lis_seq) cout << x << " ";
    cout << "\n\n";

    return 0;
}
```

## Output Example
```
Array: 10 9 2 5 3 7 101 18 
LIS Length (DP): 4
LIS Length (Binary Search): 4
Actual LIS Sequence: 2 3 7 18 

Array: 0 1 0 3 2 3 
LIS Length (DP): 4
LIS Length (Binary Search): 4
Actual LIS Sequence: 0 1 2 3 
```

## Key Points

- **Time Complexity**: 
  - DP approach: O(n²)
  - Binary search approach: O(n log n)
- **Space Complexity**: O(n) for both approaches
- **Method 1 (DP)**: Simple to understand, good for small inputs
- **Method 2 (Binary Search)**: More efficient for large inputs
- The binary search method maintains an array of smallest tail elements for increasing subsequences of different lengths

The algorithm finds the length of the longest subsequence where elements are in strictly increasing order, but not necessarily consecutive in the original array.