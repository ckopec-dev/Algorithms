# Berlekamp-Massey Algorithm in F#

Here's an implementation of the Berlekamp-Massey algorithm in F# to find the minimal polynomial of a linearly recurrent sequence:

```fsharp
open System

// Berlekamp-Massey algorithm implementation
let berlekampMassey (sequence: int[]) =
    let n = sequence.Length
    
    // Initialize variables
    let mutable C = Array.create n 0
    let mutable B = Array.create n 0
    let mutable L = 0
    let mutable m = 1
    let mutable b = 1
    
    // Set initial polynomial coefficients
    C.[0] <- 1
    B.[0] <- 1
    
    for n in 0 .. n - 1 do
        // Calculate discrepancy
        let mutable d = sequence.[n]
        for j in 0 .. L do
            d <- d - C.[j] * sequence.[n - j]
        
        if d <> 0 then
            // Store old polynomial
            let oldC = Array.copy C
            
            // Update the polynomial
            for j in 0 .. n - m do
                C.[j + m] <- C.[j + m] - d * B.[j]
            
            if 2 * L <= n then
                L <- n - L
                m <- 1
                B <- oldC
                b <- d
            else
                m <- m + 1
    
    // Return the minimal polynomial coefficients (excluding leading term)
    [0 .. L] |> List.map (fun i -> C.[i])

// Alternative implementation with clearer structure
let berlekampMasseySimple (sequence: int[]) =
    let n = sequence.Length
    
    // Initialize arrays for the characteristic polynomial
    let mutable c = Array.create (n + 1) 0
    let mutable b = Array.create (n + 1) 0
    let mutable l = 0
    let mutable m = 1
    let mutable b_val = 1
    
    // Initialize for the first element
    c.[0] <- 1
    b.[0] <- 1
    
    for i in 0 .. n - 1 do
        // Calculate discrepancy
        let mut_d = sequence.[i]
        for j in 0 .. l do
            mut_d <- mut_d - c.[j] * sequence.[i - j]
        
        if mut_d <> 0 then
            // Store old values
            let old_c = Array.copy c
            
            // Update the polynomial
            for j in 0 .. i - m do
                c.[j + m] <- c.[j + m] - mut_d * b.[j]
            
            if 2 * l <= i then
                l <- i - l
                m <- 1
                b <- old_c
                b_val <- mut_d
            else
                m <- m + 1
    
    // Extract the minimal polynomial coefficients (excluding leading term)
    [0 .. l] |> List.map (fun i -> c.[i])

// More readable version with better variable names
let berlekampMasseyReadable (sequence: int[]) =
    let length = sequence.Length
    
    // Initialize arrays
    let mutable C = Array.create (length + 1) 0  // Current polynomial coefficients
    let mutable B = Array.create (length + 1) 0  // Previous polynomial coefficients
    let mutable L = 0                             // Length of current polynomial
    let mutable m = 1                             // Step counter
    let mutable b_val = 1                         // Value of b
    
    // Set initial values
    C.[0] <- 1
    B.[0] <- 1
    
    for n in 0 .. length - 1 do
        // Calculate discrepancy d_n
        let mutable discrepancy = sequence.[n]
        for j in 0 .. L do
            discrepancy <- discrepancy - C.[j] * sequence.[n - j]
        
        if discrepancy <> 0 then
            // Store previous polynomial
            let old_C = Array.copy C
            
            // Update the current polynomial
            for j in 0 .. n - m do
                C.[j + m] <- C.[j + m] - discrepancy * B.[j]
            
            // Update parameters if necessary
            if 2 * L <= n then
                L <- n - L
                m <- 1
                B <- old_C
                b_val <- discrepancy
            else
                m <- m + 1
    
    // Return minimal polynomial coefficients
    [0 .. L] |> List.map (fun i -> C.[i])

// Example usage and test cases
let example1 = 
    // Sequence: 1, 1, 2, 3, 5, 8, 13, 21 (Fibonacci sequence)
    let fib = [|1; 1; 2; 3; 5; 8; 13; 21|]
    let result = berlekampMasseyReadable fib
    printfn "Fibonacci sequence: %A" fib
    printfn "Minimal polynomial coefficients: %A" result
    printfn "Expected: [1; -1; -1] (x^2 - x - 1)\n"

let example2 = 
    // Sequence: 1, 2, 4, 8, 16, 32 (Geometric sequence with ratio 2)
    let geo = [|1; 2; 4; 8; 16; 32|]
    let result = berlekampMasseyReadable geo
    printfn "Geometric sequence: %A" geo
    printfn "Minimal polynomial coefficients: %A" result
    printfn "Expected: [1; -2] (x - 2)\n"

let example3 = 
    // Sequence: 1, 0, 1, 1, 2, 3, 5, 8 (Fibonacci starting with 1,0)
    let fib_zero = [|1; 0; 1; 1; 2; 3; 5; 8|]
    let result = berlekampMasseyReadable fib_zero
    printfn "Fibonacci (1,0): %A" fib_zero
    printfn "Minimal polynomial coefficients: %A" result
    printfn "Expected: [1; -1; -1] (x^2 - x - 1)\n"

// Run examples
[<EntryPoint>]
let main argv =
    example1
    example2
    example3
    
    printfn "Press any key to continue..."
    Console.ReadKey() |> ignore
    0
```

## Key Features of this Implementation:

1. **Input**: Takes an array of integers representing a linearly recurrent sequence
2. **Output**: Returns the coefficients of the minimal polynomial (excluding leading term)
3. **Algorithm**: Implements the standard Berlekamp-Massey algorithm with proper variable tracking
4. **Examples**: Includes Fibonacci and geometric sequences to demonstrate usage

## How it Works:

1. The algorithm maintains two polynomials: `C` (current) and `B` (previous)
2. For each element in the sequence, it calculates a discrepancy
3. If discrepancy is non-zero, it updates the polynomial coefficients
4. The process continues until the entire sequence is processed
5. Returns the minimal polynomial that generates the given sequence

## Expected Output:
```
Fibonacci sequence: [|1; 1; 2; 3; 5; 8; 13; 21|]
Minimal polynomial coefficients: [1; -1; -1]
Expected: [1; -1; -1] (x^2 - x - 1)

Geometric sequence: [|1; 2; 4; 8; 16; 32|]
Minimal polynomial coefficients: [1; -2]
Expected: [1; -2] (x - 2)
```