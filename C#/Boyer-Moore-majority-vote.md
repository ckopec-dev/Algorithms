# Boyer-Moore Majority Vote Algorithm in C#

The Boyer-Moore Majority Vote Algorithm is used to find the majority element in an array (an element that appears more than n/2 times).

```csharp
using System;
using System.Linq;

public class BoyerMooreMajorityVote
{
    /// <summary>
    /// Finds the majority element in an array using Boyer-Moore Majority Vote Algorithm
    /// </summary>
    /// <param name="nums">Input array of integers</param>
    /// <returns>The majority element, or null if no majority exists</returns>
    public static int? FindMajorityElement(int[] nums)
    {
        if (nums == null || nums.Length == 0)
            return null;
        
        // Phase 1: Find candidate
        int candidate = 0;
        int count = 0;
        
        foreach (int num in nums)
        {
            if (count == 0)
            {
                candidate = num;
            }
            
            count += (num == candidate) ? 1 : -1;
        }
        
        // Phase 2: Verify candidate is actually majority
        int candidateCount = nums.Count(x => x == candidate);
        
        return candidateCount > nums.Length / 2 ? candidate : null;
    }
    
    /// <summary>
    /// Alternative implementation that returns the candidate directly
    /// (useful when you're sure a majority exists)
    /// </summary>
    /// <param name="nums">Input array of integers</param>
    /// <returns>The candidate element</returns>
    public static int FindCandidate(int[] nums)
    {
        if (nums == null || nums.Length == 0)
            throw new ArgumentException("Array cannot be null or empty");
        
        int candidate = 0;
        int count = 0;
        
        foreach (int num in nums)
        {
            if (count == 0)
            {
                candidate = num;
            }
            
            count += (num == candidate) ? 1 : -1;
        }
        
        return candidate;
    }
}

// Example usage
public class Program
{
    public static void Main()
    {
        // Test case 1: Array with majority element
        int[] array1 = {3, 2, 3};
        int? result1 = BoyerMooreMajorityVote.FindMajorityElement(array1);
        Console.WriteLine($"Array: [{string.Join(", ", array1)}]");
        Console.WriteLine($"Majority element: {result1}");
        Console.WriteLine();
        
        // Test case 2: Array with majority element
        int[] array2 = {2, 2, 1, 1, 1, 2, 2};
        int? result2 = BoyerMooreMajorityVote.FindMajorityElement(array2);
        Console.WriteLine($"Array: [{string.Join(", ", array2)}]");
        Console.WriteLine($"Majority element: {result2}");
        Console.WriteLine();
        
        // Test case 3: Array without majority element
        int[] array3 = {1, 2, 3, 4, 5};
        int? result3 = BoyerMooreMajorityVote.FindMajorityElement(array3);
        Console.WriteLine($"Array: [{string.Join(", ", array3)}]");
        Console.WriteLine($"Majority element: {result3}");
        Console.WriteLine();
        
        // Test case 4: Single element array
        int[] array4 = {5};
        int? result4 = BoyerMooreMajorityVote.FindMajorityElement(array4);
        Console.WriteLine($"Array: [{string.Join(", ", array4)}]");
        Console.WriteLine($"Majority element: {result4}");
        Console.WriteLine();
        
        // Test case 5: Empty array
        int[] array5 = {};
        int? result5 = BoyerMooreMajorityVote.FindMajorityElement(array5);
        Console.WriteLine($"Array: [{string.Join(", ", array5)}]");
        Console.WriteLine($"Majority element: {result5}");
    }
}
```

## Output:
```
Array: [3, 2, 3]
Majority element: 3

Array: [2, 2, 1, 1, 1, 2, 2]
Majority element: 2

Array: [1, 2, 3, 4, 5]
Majority element: 

Array: [5]
Majority element: 5

Array: []
Majority element: 
```

## How it works:

1. **Phase 1 - Voting**: 
   - Initialize candidate and count variables
   - For each element in the array:
     - If count is 0, set current element as candidate
     - If current element equals candidate, increment count
     - Otherwise, decrement count

2. **Phase 2 - Verification**:
   - Count occurrences of the candidate
   - Return candidate only if it appears more than n/2 times

## Time and Space Complexity:
- **Time Complexity**: O(n) - single pass through the array
- **Space Complexity**: O(1) - only using constant extra space

## Key Properties:
- Works only when a majority element exists (appears more than n/2 times)
- Efficient for finding candidates in linear time
- The algorithm is optimal for this specific problem

