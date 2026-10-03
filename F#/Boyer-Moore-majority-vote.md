```fsharp
// Boyer-Moore Majority Vote Algorithm Implementation in F#

let boyerMooreMajorityVote (nums: int[]) : int option =
    match nums with
    | [||] -> None  // Empty array case
    | _ ->
        let mutable candidate = 0
        let mutable count = 0
        
        // Phase 1: Find candidate
        for num in nums do
            if count = 0 then
                candidate <- num
            if num = candidate then
                count <- count + 1
            else
                count <- count - 1
        
        // Phase 2: Verify candidate is actually majority
        let mutable verifyCount = 0
        for num in nums do
            if num = candidate then
                verifyCount <- verifyCount + 1
        
        if verifyCount > nums.Length / 2 then
            Some candidate
        else
            None

// Alternative implementation using List.fold
let boyerMooreMajorityVoteFold (nums: int list) : int option =
    match nums with
    | [] -> None
    | _ ->
        let (candidate, count) = 
            nums
            |> List.fold (fun (cand, cnt) num ->
                if cnt = 0 then
                    (num, 1)
                elif num = cand then
                    (cand, cnt + 1)
                else
                    (cand, cnt - 1)
            ) (0, 0)
        
        // Verify the candidate
        let verifyCount = 
            nums 
            |> List.filter (fun x -> x = candidate) 
            |> List.length
        
        if verifyCount > List.length nums / 2 then
            Some candidate
        else
            None

// Example usage
let example1 = [|3; 2; 3|]
let result1 = boyerMooreMajorityVote example1
printfn "Example 1: %A" result1  // Output: Some 3

let example2 = [|2; 2; 1; 1; 1; 2; 2|]
let result2 = boyerMooreMajorityVote example2
printfn "Example 2: %A" result2  // Output: Some 2

let example3 = [|1; 2; 3|]
let result3 = boyerMooreMajorityVote example3
printfn "Example 3: %A" result3  // Output: None

// Using the fold version
let listExample = [3; 2; 3]
let result4 = boyerMooreMajorityVoteFold listExample
printfn "List Example: %A" result4  // Output: Some 3
```

The Boyer-Moore Majority Vote algorithm finds the majority element in an array (an element that appears more than n/2 times) in O(n) time and O(1) space.

**How it works:**
1. **Phase 1**: Find a candidate by maintaining a count. If the current element equals the candidate, increment count; otherwise decrement it.
2. **Phase 2**: Verify that the candidate actually appears more than n/2 times.

**Key features of this F# implementation:**
- Uses mutable variables for the traditional approach
- Includes a functional fold-based alternative
- Handles edge cases like empty arrays
- Returns `option` type to properly handle cases where no majority exists
- Works with both arrays and lists