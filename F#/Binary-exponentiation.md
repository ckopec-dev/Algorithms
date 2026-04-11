# Binary Exponentiation in F#

Binary exponentiation is an efficient algorithm to compute x^n where x is the base and n is the exponent. It uses the binary representation of the exponent to reduce the number of multiplications needed.

## Algorithm Explanation

The key insight is that any exponent can be represented in binary, and we can use this representation to compute the result efficiently:
- If the current bit is 1, multiply the result by the current base
- Square the base and move to the next bit
- This reduces the time complexity from O(n) to O(log n)

## F# Implementation

```fsharp
// Binary exponentiation function
let binaryExponentiation (baseValue: float) (exponent: int) : float =
    if exponent = 0 then
        1.0
    elif exponent < 0 then
        1.0 / binaryExponentiation baseValue (abs exponent)
    else
        let mutable result = 1.0
        let mutable baseVal = baseValue
        let mutable exp = exponent
        
        while exp > 0 do
            if exp % 2 = 1 then
                result <- result * baseVal
            baseVal <- baseVal * baseVal
            exp <- exp / 2
            
        result

// Alternative recursive implementation
let rec binaryExpRecursive (baseValue: float) (exponent: int) : float =
    match exponent with
    | 0 -> 1.0
    | _ when exponent < 0 -> 1.0 / binaryExpRecursive baseValue (abs exponent)
    | _ when exponent % 2 = 0 -> 
        let half = binaryExpRecursive baseValue (exponent / 2)
        half * half
    | _ -> 
        baseValue * binaryExpRecursive baseValue (exponent - 1)

// Example usage
let example1 = binaryExponentiation 2.0 10    // Returns 1024.0
let example2 = binaryExponentiation 3.0 4     // Returns 81.0
let example3 = binaryExponentiation 5.0 0     // Returns 1.0
let example4 = binaryExponentiation 2.0 (-3)  // Returns 0.125

// Test the recursive version
let example5 = binaryExpRecursive 2.0 10     // Returns 1024.0
let example6 = binaryExpRecursive 3.0 4      // Returns 81.0

// Print results
printfn "2^10 = %f" example1
printfn "3^4 = %f" example2
printfn "5^0 = %f" example3
printfn "2^(-3) = %f" example4
printfn "2^10 (recursive) = %f" example5
printfn "3^4 (recursive) = %f" example6
```

## How it works with example 2^10:

1. **Binary representation of 10**: 1010₂
2. **Process each bit from right to left**:
   - Bit 0 (rightmost): 0 → don't multiply result, square base
   - Bit 1: 1 → multiply result by current base, square base  
   - Bit 2: 0 → don't multiply result, square base
   - Bit 3 (leftmost): 1 → multiply result by current base, square base

3. **Steps**:
   - Start: result = 1, base = 2, exp = 10
   - exp = 10 (even): result = 1, base = 4, exp = 5
   - exp = 5 (odd): result = 1 × 4 = 4, base = 16, exp = 2
   - exp = 2 (even): result = 4, base = 256, exp = 1
   - exp = 1 (odd): result = 4 × 256 = 1024, base = 65536, exp = 0

## Time Complexity
- **Time**: O(log n) where n is the exponent
- **Space**: O(1) for iterative, O(log n) for recursive due to call stack

## Key Advantages
- Much faster than naive multiplication approach
- Handles negative exponents correctly
- Works with floating-point numbers
- Reduces the number of multiplications significantly

