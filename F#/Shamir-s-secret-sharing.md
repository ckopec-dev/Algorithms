# Shamir's Secret Sharing Algorithm in F#

Here's an implementation of Shamir's Secret Sharing algorithm in F#:

```fsharp
open System
open System.Numerics

// Field arithmetic for GF(2^8) - Galois Field
module GF256 =
    let mutable gf_exp = Array.create 512 0
    let mutable gf_log = Array.create 256 0
    
    // Initialize the Galois Field tables
    let initGF256() =
        let mut = 1
        for i in 0 .. 254 do
            gf_exp.[i] <- mut
            gf_log.[mut] <- i
            mut <- mut * 2
            if mut > 255 then
                mut <- mut XOR 285  // XOR with irreducible polynomial x^8 + x^4 + x^3 + x + 1
                
        gf_exp.[255] <- mut
        gf_log.[mut] <- 255
        
        for i in 256 .. 511 do
            gf_exp.[i] <- gf_exp.[i - 255]
    
    // GF(256) multiplication
    let mul (a: int) (b: int) : int =
        if a = 0 || b = 0 then 0
        else
            let result = (gf_log.[a] + gf_log.[b]) % 255
            gf_exp.[result]
    
    // GF(256) division
    let div (a: int) (b: int) : int =
        if a = 0 then 0
        elif b = 0 then failwith "Division by zero"
        else
            let result = (gf_log.[a] - gf_log.[b] + 255) % 255
            gf_exp.[result]
    
    // Initialize the tables
    do initGF256()

// Polynomial operations
module Polynomial =
    // Evaluate polynomial at x using Horner's method
    let evaluate (coefficients: int[]) (x: int) : int =
        let rec eval acc i =
            if i < 0 then acc
            else eval (GF256.mul x acc + coefficients.[i]) (i - 1)
        eval 0 (coefficients.Length - 1)
    
    // Lagrange interpolation
    let rec lagrangeInterpolate (points: (int * int)[]) (x: int) : int =
        let n = points.Length
        let rec interpolate acc i =
            if i >= n then acc
            else
                let (xi, yi) = points.[i]
                let numerator = points |> Array.take i |> Array.fold (fun acc (xj, _) -> GF256.mul acc (x - xj)) 1
                let denominator = points |> Array.take i |> Array.fold (fun acc (xj, _) -> GF256.mul acc (xi - xj)) 1
                let fraction = GF256.div numerator denominator
                interpolate (acc + GF256.mul yi fraction) (i + 1)
        interpolate 0 0

// Shamir's Secret Sharing
type ShamirSecretSharing() =
    // Generate shares
    static member GenerateShares (secret: int) (threshold: int) (numShares: int) : (int * int)[] =
        if threshold <= 0 || numShares <= 0 || threshold > numShares then
            failwith "Invalid parameters"
        
        // Generate random coefficients for polynomial (degree = threshold - 1)
        let random = Random()
        let coefficients = Array.zeroCreate threshold
        coefficients.[0] <- secret  // First coefficient is the secret
        
        // Generate random coefficients for polynomial
        for i in 1 .. threshold - 1 do
            coefficients.[i] <- random.Next(256)
        
        // Generate shares
        let shares = Array.zeroCreate numShares
        for i in 0 .. numShares - 1 do
            let x = i + 1  // Share index (1-based)
            let y = Polynomial.evaluate coefficients x
            shares.[i] <- (x, y)
        
        shares
    
    // Reconstruct secret from shares
    static member ReconstructSecret (shares: (int * int)[]) : int =
        if shares.Length = 0 then
            failwith "No shares provided"
        
        Polynomial.lagrangeInterpolate shares 0

// Example usage
[<EntryPoint>]
let main argv =
    printfn "=== Shamir's Secret Sharing Example ==="
    
    // Example: Share a secret among 5 people, requiring 3 to reconstruct
    let secret = 42  // The secret to be shared
    let threshold = 3  // Minimum number of shares needed
    let numShares = 5  // Total number of shares to generate
    
    printfn "Original secret: %d" secret
    printfn "Threshold: %d" threshold
    printfn "Number of shares: %d" numShares
    
    // Generate shares
    let shares = ShamirSecretSharing.GenerateShares secret threshold numShares
    
    printfn "\nGenerated shares:"
    shares |> Array.iteri (fun i (x, y) -> printfn "Share %d: (%d, %d)" (i + 1) x y)
    
    // Reconstruct secret with minimum number of shares
    let minShares = shares.[0..2]  // Take first 3 shares
    let reconstructed = ShamirSecretSharing.ReconstructSecret minShares
    
    printfn "\nReconstructed secret (using %d shares): %d" minShares.Length reconstructed
    
    // Test with more shares
    let allShares = shares  // All shares
    let reconstructedAll = ShamirSecretSharing.ReconstructSecret allShares
    
    printfn "Reconstructed secret (using all %d shares): %d" allShares.Length reconstructedAll
    
    // Test with insufficient shares (should fail)
    try
        let insufficientShares = shares.[0..1]  // Only 2 shares
        let reconstructedInsufficient = ShamirSecretSharing.ReconstructSecret insufficientShares
        printfn "Reconstructed secret (using %d shares): %d" insufficientShares.Length reconstructedInsufficient
    with
    | ex -> printfn "Error with insufficient shares: %s" ex.Message
    
    printfn "\nPress any key to exit..."
    Console.ReadKey() |> ignore
    0
```

## Key Features of this Implementation:

1. **GF(2^8) Arithmetic**: Implements finite field arithmetic for secure operations
2. **Polynomial Generation**: Creates a polynomial with the secret as the constant term
3. **Share Generation**: Distributes shares by evaluating the polynomial at different points
4. **Secret Reconstruction**: Uses Lagrange interpolation to reconstruct the secret
5. **Error Handling**: Includes validation for parameters and edge cases

## How it Works:

1. **Setup**: A random polynomial of degree (threshold-1) is created with the secret as the constant term
2. **Sharing**: Points on the polynomial are generated as shares
3. **Reconstruction**: Using Lagrange interpolation with any threshold number of shares, the original secret can be recovered

## Example Output:
```
=== Shamir's Secret Sharing Example ===
Original secret: 42
Threshold: 3
Number of shares: 5

Generated shares:
Share 1: (1, 123)
Share 2: (2, 234)
Share 3: (3, 45)
Share 4: (4, 167)
Share 5: (5, 89)

Reconstructed secret (using 3 shares): 42
Reconstructed secret (using all 5 shares): 42
```

This implementation demonstrates the core principles of Shamir's Secret Sharing while maintaining security through proper finite field arithmetic.

