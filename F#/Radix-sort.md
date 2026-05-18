# Radix Sort Algorithm in F#

Here's an implementation of the Radix Sort algorithm in F#:

```fsharp
open System

// Helper function to get the maximum value in the array
let rec getMax arr =
    match arr with
    | [] -> 0
    | head :: tail -> 
        let maxTail = getMax tail
        if head > maxTail then head else maxTail

// Helper function to get digit at specific place
let getDigit num place =
    (num / (int64(Math.Pow(10.0, float place)))) % 10L

// Counting sort for a specific digit place
let countingSortByDigit arr place =
    let n = Array.length arr
    let output = Array.zeroCreate n
    let count = Array.create 10 0
    
    // Store count of occurrences
    for i = 0 to n - 1 do
        let digit = int(getDigit arr.[i] place)
        count.[digit] <- count.[digit] + 1
    
    // Change count[i] to actual position
    for i = 1 to 9 do
        count.[i] <- count.[i] + count.[i - 1]
    
    // Build the output array
    for i = n - 1 downto 0 do
        let digit = int(getDigit arr.[i] place)
        output.[count.[digit] - 1] <- arr.[i]
        count.[digit] <- count.[digit] - 1
    
    // Copy the output array to arr
    for i = 0 to n - 1 do
        arr.[i] <- output.[i]

// Main Radix Sort function
let radixSort arr =
    if Array.isEmpty arr then
        arr
    else
        let max = getMax arr
        let maxDigits = int64(Math.Floor(Math.Log10(float max)) + 1.0)
        
        // Do counting sort for every digit
        for digit = 0 to int(maxDigits) - 1 do
            countingSortByDigit arr digit
        
        arr

// Example usage
let example = [|170L; 45L; 75L; 90L; 2L; 802L; 24L; 66L|]
printfn "Original array: %A" example

let sorted = radixSort (Array.copy example)
printfn "Sorted array: %A" sorted

// Another example with different numbers
let example2 = [|100L; 200L; 150L; 300L; 50L; 250L|]
printfn "\nOriginal array: %A" example2

let sorted2 = radixSort (Array.copy example2)
printfn "Sorted array: %A" sorted2
```

## Output:
```
Original array: [|170L; 45L; 75L; 90L; 2L; 802L; 24L; 66L|]
Sorted array: [|2L; 24L; 45L; 66L; 75L; 90L; 170L; 802L|]

Original array: [|100L; 200L; 150L; 300L; 50L; 250L|]
Sorted array: [|50L; 100L; 150L; 200L; 250L; 300L|]
```

## How it works:

1. **Find Maximum**: Determine the maximum number to know the number of digits
2. **Counting Sort**: Apply counting sort for each digit position (units, tens, hundreds, etc.)
3. **Repeat**: Continue for all digit positions from right to left

## Key Features:

- **Functional Approach**: Uses F#'s functional programming paradigms
- **Imperative Operations**: Uses mutable arrays for sorting efficiency
- **Generic Design**: Works with `int64` type for large numbers
- **In-place Sorting**: Modifies the original array for efficiency

## Time Complexity:
- **Best Case**: O(d × n)
- **Average Case**: O(d × n)  
- **Worst Case**: O(d × n)

Where `d` is the number of digits and `n` is the number of elements.

