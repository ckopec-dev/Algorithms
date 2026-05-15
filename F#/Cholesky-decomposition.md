# Cholesky Decomposition in F#

Here's an implementation of the Cholesky decomposition algorithm in F#:

```fsharp
open System

// Cholesky decomposition for a symmetric positive definite matrix
let choleskyDecomposition (matrix: float[,]) : float[,] =
    let n = matrix.GetLength(0)
    
    // Create result matrix
    let L = Array2D.zeroCreate n n
    
    // Main Cholesky decomposition algorithm
    for i in 0 .. n - 1 do
        for j in 0 .. i do
            let mutable sum = 0.0
            
            // Compute sum of products of elements
            for k in 0 .. j - 1 do
                sum <- sum + L.[i, k] * L.[j, k]
            
            // Compute diagonal elements
            if i = j then
                let diagonal = matrix.[i, i] - sum
                if diagonal < 0.0 then
                    failwith "Matrix is not positive definite"
                L.[i, j] <- sqrt diagonal
            else
                // Compute off-diagonal elements
                L.[i, j] <- (matrix.[i, j] - sum) / L.[j, j]
    
    L

// Helper function to print matrix
let printMatrix (matrix: float[,]) =
    let rows = matrix.GetLength(0)
    let cols = matrix.GetLength(1)
    
    for i in 0 .. rows - 1 do
        for j in 0 .. cols - 1 do
            printf "%10.4f " matrix.[i, j]
        printfn ""

// Example usage
let main() =
    // Example symmetric positive definite matrix
    let A = array2D [
        [ 4.0; 12.0; -16.0 ]
        [ 12.0; 37.0; -43.0 ]
        [ -16.0; -43.0; 98.0 ]
    ]
    
    printfn "Original matrix A:"
    printMatrix A
    
    try
        let L = choleskyDecomposition A
        printfn "\nCholesky decomposition L:"
        printMatrix L
        
        // Verify that L * L^T = A
        let L_transpose = Array2D.init (L.GetLength(0)) (L.GetLength(1)) 
                          (fun i j -> L.[j, i])
        
        let LLt = Array2D.init (L.GetLength(0)) (L.GetLength(1)) 
                  (fun i j -> 
                      let mutable sum = 0.0
                      for k in 0 .. L.GetLength(1) - 1 do
                          sum <- sum + L.[i, k] * L_transpose.[k, j]
                      sum)
        
        printfn "\nVerification L * L^T:"
        printMatrix LLt
        
    with
    | ex -> printfn "Error: %s" ex.Message

// Run the example
main()
```

## How it works:

1. **Input**: A symmetric positive definite matrix (n×n)
2. **Output**: Lower triangular matrix L such that A = L × L^T
3. **Algorithm**:
   - For diagonal elements (i = j): L[i,i] = √(A[i,i] - Σ(L[i,k]²))
   - For off-diagonal elements (i > j): L[i,j] = (A[i,j] - Σ(L[i,k] × L[j,k])) / L[j,j]
4. **Error handling**: Checks for positive definiteness

## Sample Output:
```
Original matrix A:
    4.0000   12.0000  -16.0000 
   12.0000   37.0000  -43.0000 
  -16.0000  -43.0000   98.0000 

Cholesky decomposition L:
    2.0000    0.0000    0.0000 
    6.0000    1.0000    0.0000 
   -8.0000    5.0000    3.0000 

Verification L * L^T:
    4.0000   12.0000  -16.0000 
   12.0000   37.0000  -43.0000 
  -16.0000  -43.0000   98.0000 
```

This implementation provides a clean, functional approach to Cholesky decomposition in F# with proper error handling and verification.

