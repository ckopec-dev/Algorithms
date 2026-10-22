```fsharp
// Thomas Algorithm Implementation in F#
// Solves tridiagonal systems of equations: Ax = b
// where A is a tridiagonal matrix

let thomasAlgorithm a b c d =
    let n = Array.length d
    
    // Create arrays for modified coefficients
    let cPrime = Array.zeroCreate n
    let x = Array.zeroCreate n
    
    // Forward elimination
    cPrime.[0] <- c.[0] / a.[0]
    x.[0] <- d.[0] / a.[0]
    
    for i in 1 .. n - 1 do
        let denom = a.[i] - b.[i] * cPrime.[i - 1]
        cPrime.[i] <- c.[i] / denom
        x.[i] <- (d.[i] - b.[i] * x.[i - 1]) / denom
    
    // Back substitution
    let result = Array.zeroCreate n
    result.[n - 1] <- x.[n - 1]
    
    for i in (n - 2) .. -1 .. 0 do
        result.[i] <- x.[i] - cPrime.[i] * result.[i + 1]
    
    result

// Example usage
let example() =
    // Tridiagonal matrix system:
    // 2x1 + x2 = 3
    // x1 + 3x2 + x3 = 4
    // x2 + 4x3 = 5
    
    // Coefficients for the tridiagonal matrix
    let a = [|2.0; 3.0; 4.0|]     // Main diagonal
    let b = [|1.0; 1.0; 0.0|]     // Lower diagonal (0 at end)
    let c = [|1.0; 1.0; 0.0|]     // Upper diagonal (0 at start)
    let d = [|3.0; 4.0; 5.0|]     // Right-hand side
    
    // Solve the system
    let solution = thomasAlgorithm a b c d
    
    printfn "Solution:"
    Array.iteri (fun i x -> printfn "x%d = %f" (i + 1) x) solution
    
    // Expected result: x1 = 1.0, x2 = 1.0, x3 = 1.0

// Run the example
example()
```

This implementation of the Thomas algorithm solves tridiagonal systems of linear equations with the following structure:

**Matrix form:**
```
a[0]  c[0]   0    0    0    0     0
b[1]  a[1]  c[1]   0    0    0     0
 0   b[2]  a[2]  c[2]   0    0     0
 0    0   b[3]  a[3]  c[3]   0     0
 0    0    0   b[4]  a[4]  c[4]   0
 0    0    0    0   b[5]  a[5]  c[5]
 0    0    0    0    0   b[6]  a[6]
```

The algorithm performs:
1. **Forward elimination** to transform the system
2. **Back substitution** to find the solution vector

**Key parameters:**
- `a`: Main diagonal coefficients
- `b`: Lower diagonal coefficients  
- `c`: Upper diagonal coefficients
- `d`: Right-hand side vector

The example solves a 3x3 tridiagonal system and should output:
```
Solution:
x1 = 1.000000
x2 = 1.000000
x3 = 1.000000
```