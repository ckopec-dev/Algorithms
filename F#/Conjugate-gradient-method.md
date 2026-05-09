# Conjugate Gradient Method in F#

Here's an implementation of the conjugate gradient method algorithm in F# to solve linear systems of equations Ax = b:

```fsharp
open System

// Conjugate Gradient Method Implementation
let conjugateGradient (A: float[,]) (b: float[]) (maxIterations: int) (tolerance: float) =
    let n = Array2D.length1 A
    
    // Initialize
    let x = Array.create n 0.0
    let r = Array.copy b
    let p = Array.copy b
    let rsold = ref (Array.zip r r |> Array.map (fun (a,b) -> a * b) |> Array.sum)
    
    // Main iteration loop
    let mutable iteration = 0
    let mutable converged = false
    
    while not converged && iteration < maxIterations do
        // Compute Ap
        let Ap = Array.create n 0.0
        for i in 0 .. n - 1 do
            for j in 0 .. n - 1 do
                Ap.[i] <- Ap.[i] + A.[i,j] * p.[j]
        
        // Compute alpha
        let alpha = !rsold / (Array.zip p Ap |> Array.map (fun (a,b) -> a * b) |> Array.sum)
        
        // Update x
        for i in 0 .. n - 1 do
            x.[i] <- x.[i] + alpha * p.[i]
        
        // Update r
        for i in 0 .. n - 1 do
            r.[i] <- r.[i] - alpha * Ap.[i]
        
        // Compute new residual norm
        let rsnew = Array.zip r r |> Array.map (fun (a,b) -> a * b) |> Array.sum
        
        // Check for convergence
        if sqrt rsnew < tolerance then
            converged <- true
        
        // Compute beta
        let beta = rsnew / !rsold
        
        // Update p
        for i in 0 .. n - 1 do
            p.[i] <- r.[i] + beta * p.[i]
        
        rsold := rsnew
        iteration <- iteration + 1
    
    (x, iteration, converged)

// Helper function to create a test matrix and vector
let createTestSystem () =
    // Example: 3x3 system
    let A = array2D [
        [2.0; 1.0; 1.0]
        [4.0; 1.0; 3.0]
        [1.0; 2.0; 1.0]
    ]
    
    let b = [| 4.0; 12.0; 6.0 |]
    (A, b)

// Example usage
let example () =
    let (A, b) = createTestSystem()
    let maxIter = 100
    let tolerance = 1e-6
    
    printfn "Solving system Ax = b"
    printfn "A = %A" A
    printfn "b = %A" b
    
    let (solution, iterations, converged) = 
        conjugateGradient A b maxIter tolerance
    
    printfn "Solution: %A" solution
    printfn "Iterations: %d" iterations
    printfn "Converged: %b" converged
    
    // Verify solution
    let verify = Array.create (Array2D.length1 A) 0.0
    for i in 0 .. Array2D.length1 A - 1 do
        for j in 0 .. Array2D.length2 A - 1 do
            verify.[i] <- verify.[i] + A.[i,j] * solution.[j]
    
    printfn "Verification (Ax): %A" verify
    printfn "Original b: %A" b

// Run the example
example()
```

## Key Features of this Implementation:

1. **Matrix-Vector Operations**: Properly handles matrix-vector multiplication
2. **Convergence Check**: Uses residual norm to determine convergence
3. **Iteration Limit**: Prevents infinite loops with maximum iterations
4. **Error Handling**: Returns iteration count and convergence status
5. **Verification**: Includes solution verification

## Algorithm Steps:

1. Initialize solution vector x, residual r, and search direction p
2. Compute initial residual r = b - Ax
3. For each iteration:
   - Compute Ap (matrix-vector product)
   - Calculate step size α
   - Update solution x = x + αp
   - Update residual r = r - αAp
   - Check convergence
   - Compute new search direction p = r + βp

## Usage:
The function returns the solution vector, number of iterations, and convergence status, making it easy to integrate into larger applications.

This implementation is suitable for symmetric positive definite matrices and will converge to the exact solution in at most n iterations for such systems.

