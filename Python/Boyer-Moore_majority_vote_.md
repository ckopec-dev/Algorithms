# Boyer-Moore Majority Vote Algorithm in Python

The Boyer-Moore Majority Vote Algorithm is used to find the majority element in an array (an element that appears more than n/2 times, where n is the array length).

## Implementation

```python
def boyer_moore_majority_vote(nums):
    """
    Find the majority element in an array using Boyer-Moore Majority Vote Algorithm.
    
    Args:
        nums: List of integers
    
    Returns:
        The majority element if it exists, None otherwise
    """
    # Phase 1: Find candidate
    candidate = None
    count = 0
    
    for num in nums:
        if count == 0:
            candidate = num
        count += (1 if num == candidate else -1)
    
    # Phase 2: Verify candidate is actually majority
    if candidate is not None:
        actual_count = sum(1 for num in nums if num == candidate)
        if actual_count > len(nums) // 2:
            return candidate
    
    return None

# Example usage
if __name__ == "__main__":
    # Example 1: Array with majority element
    arr1 = [3, 2, 3]
    result1 = boyer_moore_majority_vote(arr1)
    print(f"Array: {arr1}")
    print(f"Majority element: {result1}")
    print()
    
    # Example 2: Array with majority element
    arr2 = [2, 2, 1, 1, 1, 2, 2]
    result2 = boyer_moore_majority_vote(arr2)
    print(f"Array: {arr2}")
    print(f"Majority element: {result2}")
    print()
    
    # Example 3: Array without majority element
    arr3 = [1, 2, 3, 4, 5]
    result3 = boyer_moore_majority_vote(arr3)
    print(f"Array: {arr3}")
    print(f"Majority element: {result3}")
    print()
    
    # Example 4: Single element array
    arr4 = [5]
    result4 = boyer_moore_majority_vote(arr4)
    print(f"Array: {arr4}")
    print(f"Majority element: {result4}")
```

## Output
```
Array: [3, 2, 3]
Majority element: 3

Array: [2, 2, 1, 1, 1, 2, 2]
Majority element: 2

Array: [1, 2, 3, 4, 5]
Majority element: None

Array: [5]
Majority element: 5
```

## How it works:

1. **Phase 1 - Candidate Selection**: 
   - Initialize `candidate` and `count` to 0
   - For each element:
     - If `count` is 0, set current element as `candidate`
     - If current element equals `candidate`, increment `count`
     - Otherwise, decrement `count`

2. **Phase 2 - Verification**:
   - Count actual occurrences of the candidate
   - Return candidate only if it appears more than n/2 times

## Time and Space Complexity:
- **Time Complexity**: O(n)
- **Space Complexity**: O(1)

The algorithm is efficient because it only requires a single pass through the array and uses constant extra space.

