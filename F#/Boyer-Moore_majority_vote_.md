```fsharp
// Boyer-Moore Majority Vote Algorithm in F#

let boyerMooreMajorityVote (nums: int[]) : int option =
    match nums with
    | [||] -> None  // Empty array
    | _ ->
        let mutable candidate = 0
        let mutable count = 0
        
        // Phase 1: Find candidate
        for num in nums do
            if count = 0 then
                candidate <- num
            count <- if num = candidate then count + 1 else count - 1
        
        // Phase 2: Verify candidate is actually majority
        let mutable candidateCount = 0
        for num in nums do
            if num = candidate then
                candidateCount <- candidateCount + 1
        
        if candidateCount > nums.Length / 2 then
            Some candidate
        else
            None

// Example usage
let example1 = [|1; 2; 3; 2; 2|]
let result1 = boyerMooreMajorityVote example1
printfn "Majority element in %A: %A" example1 result1  // Output: Some 2

let example2 = [|1; 1; 1; 2; 2|]
let result2 = boyerMooreMajorityVote example2
printfn "Majority element in %A: %A" example2 result2  // Output: Some 1

let example3 = [|1; 2; 3; 4; 5|]
let result3 = boyerMooreMajorityVote example3
printfn "Majority element in %A: %A" example3 result3  // Output: None

// Alternative functional implementation using List.fold
let boyerMooreMajorityVoteFunctional (nums: int list) : int option =
    match nums with
    | [] -> None
    | head :: tail ->
        let (candidate, count) = 
            tail
            |> List.fold (fun (cand, cnt) num ->
                if cnt = 0 then
                    (num, 1)
                elif num = cand then
                    (cand, cnt + 1)
                else
                    (cand, cnt - 1)
            ) (head, 1)
        
        // Verify the candidate
        let actualCount = nums |> List.filter (fun x -> x = candidate) |> List.length
        if actualCount > nums.Length / 2 then
            Some candidate
        else
            None

// Functional example
let functionalExample = [1; 2; 3; 2; 2]
let functionalResult = boyerMooreMajorityVoteFunctional functionalExample
printfn "Functional result for %A: %A" functionalExample functionalResult  // Output: Some 2
```

This implementation shows two versions of the Boyer-Moore Majority Vote Algorithm in F#:

1. **Imperative version** using mutable variables and loops
2. **Functional version** using `List.fold` for a more functional approach

**How it works:**
- **Phase 1**: Find a candidate element that could be the majority
- **Phase 2**: Verify that the candidate actually appears more than n/2 times
- **Time Complexity**: O(n)
- **Space Complexity**: O(1)

The algorithm works by maintaining a candidate and a counter, incrementing the counter when we see the candidate and decrementing it otherwise. When the counter reaches zero, we pick a new candidate.

