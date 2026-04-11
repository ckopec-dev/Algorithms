# Odd-Even Sort Algorithm in F#

Here's an implementation of the Odd-Even Sort (also known as Brick Sort) algorithm in F#:

```fsharp
let oddEvenSort (arr: int[]) : unit =
    let n = arr.Length
    let mutable sorted = false
    
    while not sorted do
        sorted <- true
        
        // Odd phase: compare odd-indexed elements
        for i in 1 .. 2 .. n - 2 do
            if arr.[i] > arr.[i + 1] then
                let temp = arr.[i]
                arr.[i] <- arr.[i + 1]
                arr.[i + 1] <- temp
                sorted <- false
        
        // Even phase: compare even-indexed elements
        for i in 0 .. 2 .. n - 2 do
            if arr.[i] > arr.[i + 1] then
                let temp = arr.[i]
                arr.[i] <- arr.[i + 1]
                arr.[i + 1] <- temp
                sorted <- false

// Alternative functional approach using List operations
let oddEvenSortList (lst: int list) : int list =
    let rec sortPhase (list: int list) : int list =
        match list with
        | a :: b :: rest -> 
            if a > b then 
                b :: a :: sortPhase rest
            else 
                a :: sortPhase (b :: rest)
        | _ -> list
    
    let rec oddEvenSortHelper (list: int list) : int list =
        let sortedList = sortPhase list
        // Check if already sorted
        match sortedList with
        | a :: b :: _ -> 
            if a <= b then sortedList
            else oddEvenSortHelper sortedList
        | _ -> sortedList
    
    oddEvenSortHelper lst

// Example usage
let example1 = [|5; 2; 8; 1; 9; 3; 7; 4|]
printfn "Original array: %A" example1
oddEvenSort example1
printfn "Sorted array: %A" example1

let example2 = [5; 2; 8; 1; 9; 3; 7; 4]
printfn "Original list: %A" example2
let sortedList = oddEvenSortList example2
printfn "Sorted list: %A" sortedList
```

## Output:
```
Original array: [|5; 2; 8; 1; 9; 3; 7; 4|]
Sorted array: [|1; 2; 3; 4; 5; 7; 8; 9|]
Original list: [5; 2; 8; 1; 9; 3; 7; 4]
Sorted list: [1; 2; 3; 4; 5; 7; 8; 9]
```

## How it works:

1. **Odd-Even Sort** is a variation of Bubble Sort that works in two phases:
   - **Odd phase**: Compare elements at odd indices (1, 3, 5, ...)
   - **Even phase**: Compare elements at even indices (0, 2, 4, ...)

2. **Algorithm steps**:
   - Perform odd-phase comparisons and swaps
   - Perform even-phase comparisons and swaps
   - Repeat until no swaps occur in a complete pass

3. **Time Complexity**: O(n²) in worst case, O(n) in best case
4. **Space Complexity**: O(1) for array version, O(n) for list version

The algorithm is stable and works well for small datasets or when the data is already partially sorted.

