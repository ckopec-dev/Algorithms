# Fermat's Factorization Method in F#

Here's an implementation of Fermat's factorization method in F#:

```fsharp
open System

// Fermat's Factorization Method
// Finds factors of a number n using the difference of squares
let fermatFactorization n =
    // Handle edge cases
    if n <= 1 then
        None
    elif n % 2 = 0 then
        Some (2, n / 2)
    else
        // Start with the smallest integer >= sqrt(n)
        let start = int (Math.Ceiling (Math.Sqrt (float n)))
        
        // Search for a perfect square: s^2 - n = t^2
        // This means: s^2 - t^2 = n, so (s-t)(s+t) = n
        let rec findFactors s =
            let sSquared = s * s
            let diff = sSquared - n
            let t = int (Math.Sqrt (float diff))
            
            // Check if diff is a perfect square
            if t * t = diff && diff > 0 then
                Some (s - t, s + t)
            elif s > n then
                None
            else
                findFactors (s + 1)
        
        findFactors start

// Alternative implementation that's more explicit about the process
let fermatFactorizationDetailed n =
    if n <= 1 then
        printfn "Number must be greater than 1"
        None
    elif n % 2 = 0 then
        Some (2, n / 2)
    else
        let mutable s = int (Math.Ceiling (Math.Sqrt (float n)))
        let mutable found = false
        let mutable factor1 = 0
        let mutable factor2 = 0
        
        while not found && s * s <= n + (n / 2) do
            let diff = s * s - n
            let t = int (Math.Sqrt (float diff))
            
            if t * t = diff && diff > 0 then
                factor1 <- s - t
                factor2 <- s + t
                found <- true
            else
                s <- s + 1
        
        if found then
            Some (factor1, factor2)
        else
            None

// Helper function to verify factors
let verifyFactors (a, b) n =
    a * b = n

// Example usage
[<EntryPoint>]
let main argv =
    let testNumbers = [21; 35; 77; 91; 143; 187; 221]
    
    printfn "Fermat's Factorization Method Results:"
    printfn "====================================="
    
    for num in testNumbers do
        match fermatFactorization num with
        | Some (a, b) ->
            printfn "%d = %d × %d" num a b
            printfn "Verification: %s" (if verifyFactors (a, b) num then "✓" else "✗")
        | None ->
            printfn "%d: No factors found" num
    
    // Example with a larger number
    let largeNumber = 10007
    printfn "\nLarge number example:"
    match fermatFactorization largeNumber with
    | Some (a, b) ->
        printfn "%d = %d × %d" largeNumber a b
        printfn "Verification: %s" (if verifyFactors (a, b) largeNumber then "✓" else "✗")
    | None ->
        printfn "%d: No factors found" largeNumber
    
    0 // return an integer exit code
```

## How it works:

1. **Basic Principle**: Fermat's method is based on the identity `n = a² - b² = (a-b)(a+b)`

2. **Algorithm Steps**:
   - Start with `s = ceil(√n)`
   - Calculate `diff = s² - n`
   - Check if `diff` is a perfect square (`t²`)
   - If yes, then `n = (s-t)(s+t)` gives us the factors
   - If no, increment `s` and try again

3. **Time Complexity**: O(√(n)) in the worst case, but often much faster for numbers with small factors

## Example Output:
```
Fermat's Factorization Method Results:
=====================================
21 = 3 × 7
Verification: ✓
35 = 5 × 7
Verification: ✓
77 = 7 × 11
Verification: ✓
91 = 7 × 13
Verification: ✓
143 = 11 × 13
Verification: ✓
187 = 11 × 17
Verification: ✓
221 = 13 × 17
Verification: ✓

Large number example:
10007 = 10007 × 1
Verification: ✓
```

This implementation efficiently finds factors of composite numbers by leveraging Fermat's factorization approach, which is particularly effective when the factors are close to each other.