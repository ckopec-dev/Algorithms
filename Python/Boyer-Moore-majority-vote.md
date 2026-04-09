# Boyer-Moore Majority Vote Algorithm

The Boyer-Moore Majority Vote Algorithm is an efficient algorithm to find the majority element in an array (an element that appears more than n/2 times).

## Python Implementation

```python
def boyer_moore_majority_vote(nums):
    """
    Find the majority element in an array using Boyer-Moore Majority Vote Algorithm.
    
    Args:
        nums: List of integers
    
    Returns:
        The majority element if exists, None otherwise
    """
    if not nums:
        return None
    
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
    actual_count = 0
    for num in nums:
        if num == candidate:
            actual_count += 1
    
    # Return candidate only if it appears more than n/2 times
    return candidate if actual_count > len(nums) // 2 else None

# Example usage
def main():
    # Test case 1: Array with majority element
    nums1 = [3, 2, 3]
    result1 = boyer_moore_majority_vote(nums1)
    print(f"Array: {nums1}")
    print(f"Majority element: {result1}")
    print()
    
    # Test case 2: Array with majority element
    nums2 = [2, 2, 1, 1, 1, 2, 2]
    result2 = boyer_moore_majority_vote(nums2)
    print(f"Array: {nums2}")
    print(f"Majority element: {result2}")
    print()
    
    # Test case 3: Array without majority element
    nums3 = [1, 2, 3, 4, 5]
    result3 = boyer_moore_majority_vote(nums3)
    print(f"Array: {nums3}")
    print(f"Majority element: {result3}")
    print()
    
    # Test case 4: Array with single element
    nums4 = [5]
    result4 = boyer_moore_majority_vote(nums4)
    print(f"Array: {nums4}")
    print(f"Majority element: {result4}")

if __name__ == "__main__":
    main()
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
   - For each element in the array:
     - If `count` is 0, set current element as `candidate`
     - If current element equals `candidate`, increment `count`
     - Otherwise, decrement `count`

2. **Phase 2 - Verification**:
   - Count actual occurrences of the candidate
   - Return candidate only if it appears more than n/2 times

## Time and Space Complexity:
- **Time Complexity**: O(n) - two passes through the array
- **Space Complexity**: O(1) - only using constant extra space

## Key Properties:
- Works only when a majority element exists (appears more than n/2 times)
- Efficient and optimal for this specific problem
- The algorithm is based on the principle that majority elements can "cancel out" minority elements

