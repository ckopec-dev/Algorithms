# Exponential Search Algorithm in F#

Exponential search is a searching algorithm that first finds the range where the target element might be located, then performs binary search within that range.

```fsharp
// Exponential search implementation in F#
let exponentialSearch (arr: int[]) (target: int) : int =
    // Handle empty array
    if arr.Length = 0 then
        -1
    else
        // If target is at the first position
        if arr.[0] = target then
            0
        else
            // Find range where target may be present
            let rec findRange index =
                if index >= arr.Length || arr.[index] >= target then
                    index
                else
                    findRange (index * 2)
            
            let bound = findRange 1
            
            // Perform binary search in the range [bound/2, min(bound, length-1)]
            let rec binarySearch left right =
                if left > right then
                    -1
                else
                    let mid = left + (right - left) / 2
                    match arr.[mid] with
                    | x when x = target -> mid
                    | x when x > target -> binarySearch left (mid - 1)
                    | _ -> binarySearch (mid + 1) right
            
            binarySearch (bound / 2) (min bound (arr.Length - 1))

// Example usage
let numbers = [|1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15|]

// Test cases
let result1 = exponentialSearch numbers 7    // Should return 6
let result2 = exponentialSearch numbers 1    // Should return 0
let result3 = exponentialSearch numbers 15   // Should return 14
let result4 = exponentialSearch numbers 20   // Should return -1 (not found)

// Print results
printfn "Searching for 7: Index %d" result1
printfn "Searching for 1: Index %d" result2
printfn "Searching for 15: Index %d" result3
printfn "Searching for 20: Index %d" result4

// Alternative implementation using built-in binary search
let exponentialSearchWithBinary (arr: int[]) (target: int) : int =
    if arr.Length = 0 then
        -1
    elif arr.[0] = target then
        0
    else
        let rec findBound index =
            if index >= arr.Length || arr.[index] >= target then
                index
            else
                findBound (index * 2)
        
        let bound = findBound 1
        let left = bound / 2
        let right = min bound (arr.Length - 1)
        
        // Binary search using List.binarySearch (if available) or custom implementation
        let rec binarySearch left right =
            if left > right then
                -1
            else
                let mid = left + (right - left) / 2
                match arr.[mid] with
                | x when x = target -> mid
                | x when x > target -> binarySearch left (mid - 1)
                | _ -> binarySearch (mid + 1) right
        
        binarySearch left right

// Test the alternative implementation
let result5 = exponentialSearchWithBinary numbers 8
printfn "Searching for 8 (alternative): Index %d" result5
```

## Output:
```
Searching for 7: Index 6
Searching for 1: Index 0
Searching for 15: Index 14
Searching for 20: Index -1
Searching for 8 (alternative): Index 7
```

## Algorithm Complexity:
- **Time Complexity**: O(log n) - The exponential search takes O(log n) to find the range, and binary search takes O(log n) to find the element
- **Space Complexity**: O(1) - Only a constant amount of extra space is used

## How it works:
1. First, check if the target is at index 0
2. If not, exponentially increase the index (1, 2, 4, 8, 16, ...) until we find an element greater than or equal to the target
3. Perform binary search in the range [prevIndex/2, current_index]
4. Return the index if found, otherwise return -1

This algorithm is particularly useful when dealing with unbounded or infinite arrays where we don't know the size beforehand.

