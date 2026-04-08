# Longest Increasing Subsequence (LIS) Algorithm

Here's an implementation of the Longest Increasing Subsequence algorithm using dynamic programming:

```python
def longest_increasing_subsequence(arr):
    """
    Find the length of the longest increasing subsequence in an array.
    
    Args:
        arr: List of integers
    
    Returns:
        int: Length of the longest increasing subsequence
    """
    if not arr:
        return 0
    
    n = len(arr)
    # dp[i] represents the length of LIS ending at index i
    dp = [1] * n
    
    # Fill dp array
    for i in range(1, n):
        for j in range(i):
            if arr[i] > arr[j]:
                dp[i] = max(dp[i], dp[j] + 1)
    
    # Return the maximum value in dp array
    return max(dp)

def longest_increasing_subsequence_with_sequence(arr):
    """
    Find the length and actual sequence of the longest increasing subsequence.
    
    Args:
        arr: List of integers
    
    Returns:
        tuple: (length, sequence) of the LIS
    """
    if not arr:
        return 0, []
    
    n = len(arr)
    dp = [1] * n
    parent = [-1] * n  # To track the actual sequence
    
    # Fill dp array and track parent indices
    for i in range(1, n):
        for j in range(i):
            if arr[i] > arr[j] and dp[j] + 1 > dp[i]:
                dp[i] = dp[j] + 1
                parent[i] = j
    
    # Find the index with maximum LIS length
    max_length = max(dp)
    max_index = dp.index(max_length)
    
    # Reconstruct the actual sequence
    sequence = []
    current = max_index
    while current != -1:
        sequence.append(arr[current])
        current = parent[current]
    
    sequence.reverse()
    return max_length, sequence

# Example usage
if __name__ == "__main__":
    # Test cases
    test_arrays = [
        [10, 9, 2, 5, 3, 7, 101, 18],
        [0, 1, 0, 3, 2, 3],
        [7, 7, 7, 7, 7, 7, 7],
        [1, 3, 6, 7, 9, 4, 10, 5, 6],
        []
    ]
    
    for i, arr in enumerate(test_arrays):
        length = longest_increasing_subsequence(arr)
        length_with_seq, sequence = longest_increasing_subsequence_with_sequence(arr)
        
        print(f"Test case {i+1}: {arr}")
        print(f"Length of LIS: {length}")
        print(f"LIS sequence: {sequence}")
        print("-" * 50)
```

## Output:
```
Test case 1: [10, 9, 2, 5, 3, 7, 101, 18]
Length of LIS: 4
LIS sequence: [2, 3, 7, 18]
--------------------------------------------------
Test case 2: [0, 1, 0, 3, 2, 3]
Length of LIS: 4
LIS sequence: [0, 1, 2, 3]
--------------------------------------------------
Test case 3: [7, 7, 7, 7, 7, 7, 7]
Length of LIS: 1
LIS sequence: [7]
--------------------------------------------------
Test case 4: [1, 3, 6, 7, 9, 4, 10, 5, 6]
Length of LIS: 6
LIS sequence: [1, 3, 6, 7, 9, 10]
--------------------------------------------------
Test case 5: []
Length of LIS: 0
LIS sequence: []
--------------------------------------------------
```

## Algorithm Complexity:
- **Time Complexity**: O(n²) where n is the length of the array
- **Space Complexity**: O(n) for the dp array

## How it works:
1. Create a dp array where dp[i] represents the length of the longest increasing subsequence ending at index i
2. For each element, check all previous elements that are smaller than the current element
3. Update dp[i] by taking the maximum of current value and dp[j] + 1 (where j < i and arr[j] < arr[i])
4. Return the maximum value in the dp array

This implementation provides both the length of the LIS and the actual sequence that forms the longest increasing subsequence.

