```python
def boyer_moore_majority_vote(nums):
    """
    Find the majority element in an array using Boyer-Moore Majority Vote Algorithm.
    The majority element appears more than n/2 times where n is the array length.
    
    Args:
        nums: List of integers
    
    Returns:
        The majority element if exists, otherwise None
    """
    # Phase 1: Find candidate
    candidate = None
    count = 0
    
    for num in nums:
        if count == 0:
            candidate = num
        if num == candidate:
            count += 1
        else:
            count -= 1
    
    # Phase 2: Verify candidate is actually majority
    if candidate is not None:
        actual_count = sum(1 for num in nums if num == candidate)
        if actual_count > len(nums) // 2:
            return candidate
    
    return None

# Example usage
if __name__ == "__main__":
    # Test case 1: Array with majority element
    arr1 = [3, 2, 3]
    result1 = boyer_moore_majority_vote(arr1)
    print(f"Array: {arr1}")
    print(f"Majority element: {result1}")
    print()
    
    # Test case 2: Array with majority element
    arr2 = [2, 2, 1, 1, 1, 2, 2]
    result2 = boyer_moore_majority_vote(arr2)
    print(f"Array: {arr2}")
    print(f"Majority element: {result2}")
    print()
    
    # Test case 3: Array without majority element
    arr3 = [1, 2, 3, 4]
    result3 = boyer_moore_majority_vote(arr3)
    print(f"Array: {arr3}")
    print(f"Majority element: {result3}")
    print()
    
    # Test case 4: Single element
    arr4 = [5]
    result4 = boyer_moore_majority_vote(arr4)
    print(f"Array: {arr4}")
    print(f"Majority element: {result4}")
```

This implementation follows the Boyer-Moore Majority Vote Algorithm which works in two phases:

1. **Phase 1 - Finding Candidate**: 
   - Maintain a candidate and count
   - If count is 0, set current element as candidate
   - If current element equals candidate, increment count
   - Otherwise, decrement count

2. **Phase 2 - Verification**:
   - Count actual occurrences of candidate
   - Return candidate only if it appears more than n/2 times

**Time Complexity**: O(n)  
**Space Complexity**: O(1)

The algorithm is optimal for finding majority elements in a single pass with constant extra space.

