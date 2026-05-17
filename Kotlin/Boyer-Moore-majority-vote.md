# Boyer-Moore Majority Vote Algorithm in Kotlin

The Boyer-Moore Majority Vote Algorithm is used to find the majority element in an array (an element that appears more than n/2 times).

```kotlin
fun majorityElement(nums: IntArray): Int {
    var candidate = 0
    var count = 0
    
    // Phase 1: Find candidate
    for (num in nums) {
        if (count == 0) {
            candidate = num
        }
        count += if (num == candidate) 1 else -1
    }
    
    // Phase 2: Verify candidate is actually majority
    count = 0
    for (num in nums) {
        if (num == candidate) {
            count++
        }
    }
    
    // Return candidate if it's actually majority
    return if (count > nums.size / 2) candidate else -1
}

// Alternative implementation with single pass
fun majorityElementSinglePass(nums: IntArray): Int {
    var candidate = 0
    var count = 0
    
    for (num in nums) {
        if (count == 0) {
            candidate = num
        }
        count += if (num == candidate) 1 else -1
    }
    
    return candidate
}

// Example usage
fun main() {
    // Test case 1: Array with majority element
    val nums1 = intArrayOf(3, 2, 3)
    println("Array: ${nums1.contentToString()}")
    println("Majority element: ${majorityElement(nums1)}")
    
    // Test case 2: Array with majority element
    val nums2 = intArrayOf(2, 2, 1, 1, 1, 2, 2)
    println("Array: ${nums2.contentToString()}")
    println("Majority element: ${majorityElement(nums2)}")
    
    // Test case 3: Array without majority element
    val nums3 = intArrayOf(1, 2, 3, 4, 5)
    println("Array: ${nums3.contentToString()}")
    println("Majority element: ${majorityElement(nums3)}")
    
    // Test case 4: Single element
    val nums4 = intArrayOf(5)
    println("Array: ${nums4.contentToString()}")
    println("Majority element: ${majorityElement(nums4)}")
}
```

## How it works:

1. **Phase 1 - Finding Candidate**: 
   - Initialize `candidate` and `count` to 0
   - For each element:
     - If `count` is 0, set current element as `candidate`
     - If current element equals `candidate`, increment `count`
     - Otherwise, decrement `count`

2. **Phase 2 - Verification**:
   - Count occurrences of the candidate
   - Return candidate if it appears more than n/2 times

## Output:
```
Array: [3, 2, 3]
Majority element: 3
Array: [2, 2, 1, 1, 1, 2, 2]
Majority element: 2
Array: [1, 2, 3, 4, 5]
Majority element: -1
Array: [5]
Majority element: 5
```

## Time and Space Complexity:
- **Time Complexity**: O(n) - two passes through the array
- **Space Complexity**: O(1) - only using constant extra space

The algorithm is optimal for finding majority elements and works correctly even when there's no majority element in the array.

