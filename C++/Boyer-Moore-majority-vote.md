# Boyer-Moore Majority Vote Algorithm in C++

The Boyer-Moore Majority Vote Algorithm is used to find the majority element in an array (an element that appears more than n/2 times).

## Implementation

```cpp
#include <iostream>
#include <vector>
using namespace std;

int boyerMooreMajorityVote(vector<int>& nums) {
    // Phase 1: Find candidate
    int candidate = 0;
    int count = 0;
    
    for (int num : nums) {
        if (count == 0) {
            candidate = num;
        }
        
        if (num == candidate) {
            count++;
        } else {
            count--;
        }
    }
    
    return candidate;
}

// Function to verify if the candidate is actually majority element
bool isMajority(vector<int>& nums, int candidate) {
    int count = 0;
    for (int num : nums) {
        if (num == candidate) {
            count++;
        }
    }
    return count > nums.size() / 2;
}

int main() {
    // Example 1
    vector<int> nums1 = {3, 2, 3};
    int result1 = boyerMooreMajorityVote(nums1);
    
    cout << "Array: [3, 2, 3]" << endl;
    cout << "Majority element: " << result1 << endl;
    cout << "Is valid majority: " << (isMajority(nums1, result1) ? "Yes" : "No") << endl << endl;
    
    // Example 2
    vector<int> nums2 = {2, 2, 1, 1, 1, 2, 2};
    int result2 = boyerMooreMajorityVote(nums2);
    
    cout << "Array: [2, 2, 1, 1, 1, 2, 2]" << endl;
    cout << "Majority element: " << result2 << endl;
    cout << "Is valid majority: " << (isMajority(nums2, result2) ? "Yes" : "No") << endl << endl;
    
    // Example 3
    vector<int> nums3 = {1};
    int result3 = boyerMooreMajorityVote(nums3);
    
    cout << "Array: [1]" << endl;
    cout << "Majority element: " << result3 << endl;
    cout << "Is valid majority: " << (isMajority(nums3, result3) ? "Yes" : "No") << endl;
    
    return 0;
}
```

## Output
```
Array: [3, 2, 3]
Majority element: 3
Is valid majority: Yes

Array: [2, 2, 1, 1, 1, 2, 2]
Majority element: 2
Is valid majority: Yes

Array: [1]
Majority element: 1
Is valid majority: Yes
```

## How it works:

1. **Phase 1 - Finding Candidate**: 
   - Initialize `candidate` and `count` to 0
   - For each element in array:
     - If `count` is 0, set current element as candidate
     - If current element equals candidate, increment count
     - Otherwise, decrement count

2. **Phase 2 - Verification**: 
   - Count occurrences of the candidate
   - Verify it appears more than n/2 times

## Time and Space Complexity:
- **Time Complexity**: O(n)
- **Space Complexity**: O(1)

The algorithm works because the majority element will always "win" the vote count process due to its frequency being greater than half the array size.