```fsharp
// Matrix Exponentiation Algorithm in F#

// Define a matrix type
type Matrix = float[,]

// Helper function to create a 2x2 identity matrix
let identityMatrix () : Matrix =
    let mat = Array2D.zeroCreate 2 2
    mat.[0, 0] <- 1.0
    mat.[1, 1] <- 1.0
    mat

// Helper function to create a 2x2 matrix
let makeMatrix a b c d : Matrix =
    let mat = Array2D.zeroCreate 2 2
    mat.[0, 0] <- a
    mat.[0, 1] <- b
    mat.[1, 0] <- c
    mat.[1, 1] <- d
    mat

// Matrix multiplication function
let multiplyMatrices (a: Matrix) (b: Matrix) : Matrix =
    let result = Array2D.zeroCreate 2 2
    for i in 0 .. 1 do
        for j in 0 .. 1 do
            result.[i, j] <- 
                a.[i, 0] * b.[0, j] + 
                a.[i, 1] * b.[1, j]
    result

// Fast matrix exponentiation using binary exponentiation
let matrixPower (matrix: Matrix) (n: int) : Matrix =
    if n = 0 then
        identityMatrix()
    elif n = 1 then
        matrix
    else
        let rec fastPow mat power acc =
            match power with
            | 0 -> acc
            | _ when power % 2 = 0 -> 
                fastPow (multiplyMatrices mat mat) (power / 2) acc
            | _ -> 
                fastPow mat (power - 1) (multiplyMatrices acc mat)
        fastPow matrix n (identityMatrix())

// Example usage: Calculate Fibonacci numbers using matrix exponentiation
// Fibonacci transition matrix [[1; 1]; [1; 0]]
let fibonacciMatrix = makeMatrix 1.0 1.0 1.0 0.0

// Get nth Fibonacci number
let fibonacci n =
    if n <= 0 then 0.0
    elif n = 1 then 1.0
    else
        let resultMatrix = matrixPower fibonacciMatrix (n - 1)
        resultMatrix.[0, 0]

// Example usage
let example () =
    printfn "Fibonacci numbers using matrix exponentiation:"
    for i in 0 .. 10 do
        printfn "F(%d) = %f" i (fibonacci i)
    
    // Example with a general 2x2 matrix
    let testMatrix = makeMatrix 2.0 1.0 1.0 1.0
    printfn "\nMatrix power 3 of [[2; 1]; [1; 1]]:"
    let result = matrixPower testMatrix 3
    printfn "Result: [[%f, %f]; [%f, %f]]" 
        result.[0, 0] result.[0, 1] 
        result.[1, 0] result.[1, 1]

// Run the example
example()
```

This F# implementation demonstrates:

1. **Matrix Type Definition**: Uses `float[,]` for 2D arrays representing matrices
2. **Helper Functions**: 
   - `identityMatrix()` creates a 2x2 identity matrix
   - `makeMatrix()` creates custom 2x2 matrices
3. **Matrix Operations**:
   - `multiplyMatrices()` performs matrix multiplication
   - `matrixPower()` implements fast exponentiation using binary exponentiation
4. **Practical Application**: Calculates Fibonacci numbers using matrix exponentiation (more efficient than naive recursion)
5. **Binary Exponentiation**: The core optimization that reduces time complexity from O(n) to O(log n)

The algorithm uses the property that Fibonacci numbers can be computed using matrix exponentiation:
```
[F(n+1)]   [1 1] [F(n)  ]
[F(n)  ] = [1 0] [F(n-1)]
```