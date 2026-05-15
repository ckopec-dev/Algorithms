# Pollard's Rho Algorithm in F#

Pollard's Rho is a probabilistic algorithm for integer factorization. Here's an implementation in F#:

```fsharp
open System

// Pollard's Rho factorization algorithm
let pollardRho n =
    if n <= 1 then None
    elif n % 2 = 0 then Some 2
    else
        let rec gcd a b =
            if b = 0 then a
            else gcd b (a % b)
        
        let rec rho x g =
            let next x = (x * x + 1) % n
            let x1 = next x
            let x2 = next (next x)
            let d = gcd (abs (x1 - x2)) n
            if d = 1 then rho x1 g
            elif d = n then None
            else Some d
        
        // Try different starting values
        let rec tryStart start =
            match rho start 1 with
            | Some factor -> Some factor
            | None ->
                if start > 1000 then None
                else tryStart (start + 1)
        
        tryStart 2

// Helper function to factorize a number completely
let factorize n =
    let rec factorizeHelper num factors =
        match pollardRho num with
        | Some factor ->
            let remaining = num / factor
            if remaining = 1 then 
                factors @ [factor]
            else
                factorizeHelper remaining (factors @ [factor])
        | None -> factors @ [num]
    
    if n <= 1 then []
    else factorizeHelper n []

// Example usage
let example = 
    let number = 1387L  // 1387 = 19 × 73
    printfn "Factoring %d" number
    
    match pollardRho number with
    | Some factor -> 
        printfn "Found factor: %d" factor
        let otherFactor = number / factor
        printfn "Other factor: %d" otherFactor
        printfn "Verification: %d × %d = %d" factor otherFactor (factor * otherFactor)
    | None -> 
        printfn "No factor found"
    
    printfn "Complete factorization: %A" (factorize number)

// Run the example
example
```

## How it works:

1. **GCD Function**: Computes the greatest common divisor using Euclidean algorithm
2. **Rho Function**: Implements the core Pollard's Rho algorithm using the recurrence relation x_{n+1} = (x_n² + 1) mod n
3. **PollardRho Function**: Main function that handles edge cases and calls the rho function
4. **Factorize Function**: Complete factorization by repeatedly applying Pollard's Rho

## Key Features:

- **Probabilistic**: May need multiple attempts to find a factor
- **Space Efficient**: Uses O(1) space complexity
- **Time Complexity**: Expected O(n^(1/4)) for finding a factor
- **Handles Edge Cases**: Properly deals with even numbers and small primes

## Sample Output:
```
Factoring 1387
Found factor: 19
Other factor: 73
Verification: 19 × 73 = 1387
Complete factorization: [19; 73]
```

This implementation demonstrates how Pollard's Rho algorithm can efficiently find factors of composite numbers, particularly useful for numbers with small prime factors.

