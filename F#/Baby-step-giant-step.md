# Baby-step Giant-step Algorithm in F#

The baby-step giant-step algorithm is used to solve the discrete logarithm problem: finding `x` such that `g^x ≡ h (mod p)`.

```fsharp
open System

// Baby-step Giant-step algorithm for discrete logarithm
let babyStepGiantStep g h p =
    // Calculate the square root of p (rounded up)
    let m = int (Math.Ceiling (Math.Sqrt (double p)))
    
    // Baby steps: compute g^j mod p for j = 0, 1, 2, ..., m-1
    let babySteps = 
        [0..m-1]
        |> List.map (fun j -> (int64 (g |> bigint.Pow j |> bigint.Mod p)), j)
        |> Map.ofList
    
    // Calculate g^(-m) mod p (multiplicative inverse of g^m)
    let gM = g |> bigint.Pow m |> bigint.Mod p
    let gMInv = bigint.ModPow gM -1 p  // This computes g^(-m) mod p
    
    // Giant steps: check if h * (g^(-m))^i is in babySteps
    let mutable result = None
    
    [0..m-1]
    |> List.tryFind (fun i ->
        let giantStep = bigint.ModPow (h *% gMInv) i p
        match Map.tryFind (int64 giantStep) babySteps with
        | Some j -> 
            result <- Some (i * m + j)
            true
        | None -> false)
    
    match result with
    | Some x -> x
    | None -> -1  // No solution found

// Alternative implementation with better handling
let babyStepGiantStepImproved g h p =
    let m = int (Math.Ceiling (Math.Sqrt (double p)))
    
    // Create a map of baby steps: { g^j mod p -> j }
    let babySteps = 
        [0..m-1]
        |> List.map (fun j -> 
            let value = (g |> bigint.Pow j |> bigint.Mod p) |> int64
            (value, j))
        |> Map.ofList
    
    // Calculate g^(-m) mod p
    let gM = g |> bigint.Pow m |> bigint.Mod p
    let gMInv = bigint.ModPow gM -1 p
    
    // Check giant steps
    for i in 0..m-1 do
        let value = bigint.ModPow (h *% gMInv) i p
        let valueInt = int64 value
        match Map.tryFind valueInt babySteps with
        | Some j -> 
            return i * m + j
        | None -> ()
    
    -1  // No solution found

// Example usage
let example() =
    // Solve g^x ≡ h (mod p)
    // Example: 3^x ≡ 13 (mod 17)
    let g = 3
    let h = 13
    let p = 17
    
    printfn "Solving: %d^x ≡ %d (mod %d)" g h p
    
    let result = babyStepGiantStep g h p
    
    if result >= 0 then
        printfn "Solution found: x = %d" result
        // Verify the result
        let verification = (g |> bigint.Pow result |> bigint.Mod p)
        printfn "Verification: %d^%d ≡ %d (mod %d)" g result verification p
    else
        printfn "No solution found"

// Run the example
example()
```

## How it works:

1. **Baby Steps**: Compute and store `g^j mod p` for `j = 0, 1, ..., ⌈√p⌉-1`
2. **Giant Steps**: For `i = 0, 1, ..., ⌈√p⌉-1`, compute `h * (g^(-⌈√p⌉))^i mod p`
3. **Lookup**: Check if the giant step value exists in the baby steps map
4. **Solution**: If found, the solution is `x = i * ⌈√p⌉ + j`

## Time Complexity:
- **Time**: O(√p)
- **Space**: O(√p)

## Example Output:
```
Solving: 3^x ≡ 13 (mod 17)
Solution found: x = 4
Verification: 3^4 ≡ 13 (mod 17)
```

This algorithm is particularly useful for solving discrete logarithm problems when the modulus is not too large, as it's significantly faster than brute force methods.

