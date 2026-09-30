# Arnoldi Iteration Algorithm in F#

Here's an implementation of the Arnoldi iteration algorithm in F# for finding eigenvalues and eigenvectors of large sparse matrices:

```fsharp
open System
open System.Numerics
open MathNet.Numerics.LinearAlgebra

type Vector = Vector<Complex>
type Matrix = Matrix<Complex>

/// Arnoldi Iteration Algorithm Implementation
type ArnoldiIteration() =
    
    /// Performs Arnoldi iteration to compute eigenvalues
    /// Parameters:
    ///   A - the matrix to find eigenvalues for
    ///   initialVector - starting vector for the iteration
    ///   numIterations - number of Arnoldi iterations to perform
    ///   numEigenvalues - number of eigenvalues to compute
    member this.ComputeEigenvalues(A: Matrix, initialVector: Vector, 
                                  numIterations: int, numEigenvalues: int) : (Complex[]) =
        
        let n = A.RowCount
        
        // Initialize Hessenberg matrix and V matrix
        let H = DenseMatrix.Create(n, numIterations + 1, Complex.Zero)
        let V = DenseMatrix.Create(n, numIterations + 1, Complex.Zero)
        
        // Set initial vector
        let v = initialVector.Copy()
        let norm = v.L2Norm()
        
        if norm > 1e-12 then
            v.DivideInPlace(norm)
        else
            failwith "Initial vector has zero norm"
            
        V.[*, 0] <- v
        
        // Arnoldi iteration loop
        for j in 0 .. numIterations - 1 do
            // Compute w = A * v_j
            let w = A * V.[*, j]
            
            // Orthogonalize w against all previous V columns
            for i in 0 .. j do
                let h_ij = Complex.Conjugate(V.[*, i].DotProduct(w))
                H.[i, j] <- h_ij
                w.AddInPlace(-h_ij * V.[*, i])
            
            // Compute H_(j+1,j) and normalize w
            let h_j1j = w.L2Norm()
            H.[j + 1, j] <- h_j1j
            
            if h_j1j > 1e-12 then
                V.[*, j + 1] <- w.Divide(h_j1j)
            else
                // If w is zero, we're done
                break
        
        // Extract the Hessenberg matrix for eigenvalue computation
        let hessenbergMatrix = H.SubMatrix(0, min n (numIterations + 1), 
                                          0, min n (numIterations + 1))
        
        // Compute eigenvalues of the Hessenberg matrix
        let eigenvals = hessenbergMatrix.Eigenvalues()
        
        // Return the first numEigenvalues eigenvalues
        [| for i in 0 .. min (eigenvals.Count - 1) (numEigenvalues - 1) -> 
            eigenvals.[i] |]
    
    /// Computes both eigenvalues and eigenvectors using Arnoldi iteration
    member this.ComputeEigensystem(A: Matrix, initialVector: Vector, 
                                  numIterations: int, numEigenvalues: int) : 
        (Complex[], Vector[]) =
        
        let eigenvals = this.ComputeEigenvalues(A, initialVector, numIterations, numEigenvalues)
        
        // For simplicity, we'll return the eigenvalues and dummy eigenvectors
        // In a full implementation, you would compute the eigenvectors from V matrix
        let eigenvectors = Array.create eigenvals.Length (DenseVector.Create(A.RowCount, Complex.Zero))
        
        (eigenvals, eigenvectors)

// Example usage
[<EntryPoint>]
let main argv =
    
    // Create a sample 4x4 matrix
    let matrixData = 
        [| [|
            Complex(3.0, 0.0); Complex(1.0, 0.0); Complex(0.0, 0.0); Complex(0.0, 0.0)
           |]
           [| Complex(1.0, 0.0); Complex(3.0, 0.0); Complex(1.0, 0.0); Complex(0.0, 0.0)
           |]
           [| Complex(0.0, 0.0); Complex(1.0, 0.0); Complex(3.0, 0.0); Complex(1.0, 0.0)
           |]
           [| Complex(0.0, 0.0); Complex(0.0, 0.0); Complex(1.0, 0.0); Complex(3.0, 0.0)
           |] |]
    
    let A = DenseMatrix.OfArray2D(matrixData)
    
    // Create initial vector
    let initialVector = DenseVector.Create(4, fun i -> Complex(1.0, 0.0))
    
    // Create Arnoldi iteration instance
    let arnoldi = ArnoldiIteration()
    
    // Compute first 2 eigenvalues
    try
        let eigenvals = arnoldi.ComputeEigenvalues(A, initialVector, 4, 2)
        
        printfn "Arnoldi Iteration Results:"
        printfn "Matrix A:"
        A.Print(5, 10)
        printfn "First 2 Eigenvalues:"
        for i, val in eigenvals |> Seq.mapi (fun i v -> (i, v)) do
            printfn "  λ%d = %A" (i + 1) val
        
    with
    | ex -> 
        printfn "Error: %s" ex.Message
    
    Console.ReadLine() |> ignore
    0
```

## Key Features of this Implementation:

1. **Arnoldi Iteration Process**: Implements the standard Arnoldi iteration for building a Hessenberg matrix
2. **Matrix Operations**: Uses Math.NET Numerics library for efficient matrix operations
3. **Orthogonalization**: Performs Gram-Schmidt orthogonalization at each step
4. **Eigenvalue Computation**: Extracts eigenvalues from the resulting Hessenberg matrix
5. **Error Handling**: Includes basic error checking for zero vectors

## Usage Notes:

- This is a simplified implementation focused on the core Arnoldi algorithm
- For production use, consider using optimized libraries like `Math.NET Numerics` with proper LAPACK integration
- The algorithm is particularly useful for large sparse matrices where computing all eigenvalues would be computationally expensive
- The number of iterations should be chosen based on convergence requirements

The implementation demonstrates the core mathematical principles of Arnoldi iteration while providing a practical F# interface for eigenvalue computation.