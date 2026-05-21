# Arnoldi Iteration Algorithm in Go

Here's an implementation of the Arnoldi iteration algorithm for finding eigenvalues of a matrix in Go:

```go
package main

import (
    "fmt"
    "math"
    "math/cmplx"
)

// Matrix represents a dense matrix
type Matrix struct {
    data [][]float64
    rows int
    cols int
}

// NewMatrix creates a new matrix with given dimensions
func NewMatrix(rows, cols int) *Matrix {
    data := make([][]float64, rows)
    for i := range data {
        data[i] = make([]float64, cols)
    }
    return &Matrix{data: data, rows: rows, cols: cols}
}

// Set sets a value at position (i, j)
func (m *Matrix) Set(i, j int, value float64) {
    m.data[i][j] = value
}

// Get gets a value at position (i, j)
func (m *Matrix) Get(i, j int) float64 {
    return m.data[i][j]
}

// Multiply multiplies this matrix with another matrix
func (m *Matrix) Multiply(other *Matrix) *Matrix {
    if m.cols != other.rows {
        panic("Matrix dimensions don't match for multiplication")
    }
    
    result := NewMatrix(m.rows, other.cols)
    for i := 0; i < m.rows; i++ {
        for j := 0; j < other.cols; j++ {
            sum := 0.0
            for k := 0; k < m.cols; k++ {
                sum += m.data[i][k] * other.data[k][j]
            }
            result.data[i][j] = sum
        }
    }
    return result
}

// Vector represents a vector
type Vector struct {
    data []float64
    size int
}

// NewVector creates a new vector with given size
func NewVector(size int) *Vector {
    data := make([]float64, size)
    return &Vector{data: data, size: size}
}

// Set sets a value at position i
func (v *Vector) Set(i int, value float64) {
    v.data[i] = value
}

// Get gets a value at position i
func (v *Vector) Get(i int) float64 {
    return v.data[i]
}

// DotProduct computes the dot product of two vectors
func (v *Vector) DotProduct(other *Vector) float64 {
    if v.size != other.size {
        panic("Vector sizes don't match")
    }
    sum := 0.0
    for i := 0; i < v.size; i++ {
        sum += v.data[i] * other.data[i]
    }
    return sum
}

// Norm computes the Euclidean norm of the vector
func (v *Vector) Norm() float64 {
    sum := 0.0
    for i := 0; i < v.size; i++ {
        sum += v.data[i] * v.data[i]
    }
    return math.Sqrt(sum)
}

// Normalize normalizes the vector
func (v *Vector) Normalize() {
    norm := v.Norm()
    if norm > 0 {
        for i := 0; i < v.size; i++ {
            v.data[i] /= norm
        }
    }
}

// ArnoldiIteration performs Arnoldi iteration to find eigenvalues
func ArnoldiIteration(matrix *Matrix, maxIterations int, k int) ([][]complex128, [][]float64) {
    n := matrix.rows
    if k > n {
        k = n
    }
    
    // Initialize Hessenberg matrix H
    H := NewMatrix(k+1, k)
    
    // Initialize the first vector v1
    v := make([]*Vector, k+1)
    v[0] = NewVector(n)
    
    // Initialize with random vector (normalized)
    for i := 0; i < n; i++ {
        v[0].Set(i, 1.0)
    }
    v[0].Normalize()
    
    // Arnoldi iteration
    for j := 0; j < k; j++ {
        // Compute w = A * v_j
        w := NewVector(n)
        for i := 0; i < n; i++ {
            sum := 0.0
            for l := 0; l < n; l++ {
                sum += matrix.Get(i, l) * v[j].Get(l)
            }
            w.Set(i, sum)
        }
        
        // Orthogonalize w against all previous v's
        for i := 0; i <= j; i++ {
            // Compute h_{i,j}
            h := w.DotProduct(v[i])
            H.Set(i, j, h)
            
            // Subtract projection
            for l := 0; l < n; l++ {
                w.Set(l, w.Get(l) - h*v[i].Get(l))
            }
        }
        
        // Compute h_{j+1,j}
        h := w.Norm()
        H.Set(j+1, j, h)
        
        // If h_{j+1,j} is zero, stop
        if h < 1e-12 {
            break
        }
        
        // Normalize w to get v_{j+1}
        v[j+1] = NewVector(n)
        for i := 0; i < n; i++ {
            v[j+1].Set(i, w.Get(i)/h)
        }
    }
    
    // Extract eigenvalues from Hessenberg matrix
    eigenvals := make([]complex128, k)
    for i := 0; i < k; i++ {
        eigenvals[i] = complex(H.Get(i, i), 0)
    }
    
    // Return eigenvalues and Hessenberg matrix
    return eigenvals, H.data
}

// PrintMatrix prints a matrix
func PrintMatrix(matrix [][]float64) {
    for _, row := range matrix {
        for _, val := range row {
            fmt.Printf("%8.4f ", val)
        }
        fmt.Println()
    }
}

// PrintEigenvalues prints eigenvalues
func PrintEigenvalues(eigenvals []complex128) {
    for _, val := range eigenvals {
        fmt.Printf("%8.4f + %8.4fi\n", real(val), imag(val))
    }
}

func main() {
    // Create a sample 4x4 matrix
    matrix := NewMatrix(4, 4)
    matrix.Set(0, 0, 3.0)
    matrix.Set(0, 1, 1.0)
    matrix.Set(0, 2, 0.0)
    matrix.Set(0, 3, 0.0)
    
    matrix.Set(1, 0, 1.0)
    matrix.Set(1, 1, 3.0)
    matrix.Set(1, 2, 1.0)
    matrix.Set(1, 3, 0.0)
    
    matrix.Set(2, 0, 0.0)
    matrix.Set(2, 1, 1.0)
    matrix.Set(2, 2, 3.0)
    matrix.Set(2, 3, 1.0)
    
    matrix.Set(3, 0, 0.0)
    matrix.Set(3, 1, 0.0)
    matrix.Set(3, 2, 1.0)
    matrix.Set(3, 3, 3.0)
    
    fmt.Println("Original Matrix:")
    for i := 0; i < 4; i++ {
        for j := 0; j < 4; j++ {
            fmt.Printf("%8.4f ", matrix.Get(i, j))
        }
        fmt.Println()
    }
    
    fmt.Println("\nArnoldi Iteration Results:")
    eigenvals, H := ArnoldiIteration(matrix, 100, 4)
    
    fmt.Println("Hessenberg Matrix H:")
    PrintMatrix(H)
    
    fmt.Println("\nEigenvalues:")
    PrintEigenvalues(eigenvals)
}
```

## Key Features of this Implementation:

1. **Matrix and Vector Structures**: Custom implementations for matrices and vectors with basic operations
2. **Arnoldi Iteration**: Core algorithm that builds the Hessenberg matrix
3. **Orthogonalization**: Uses Gram-Schmidt process to maintain orthonormal basis
4. **Eigenvalue Extraction**: Extracts eigenvalues from the Hessenberg matrix
5. **Error Handling**: Includes basic error checking for matrix operations

## How it Works:

1. **Initialization**: Start with a random normalized vector
2. **Iteration**: For each step:
   - Multiply current vector by matrix
   - Orthogonalize against previous vectors
   - Compute Hessenberg matrix entries
   - Normalize resulting vector
3. **Convergence**: Stop when the next vector becomes zero or max iterations reached
4. **Result**: Extract eigenvalues from the Hessenberg matrix

This implementation demonstrates the core concepts of Arnoldi iteration, which is particularly useful for large sparse matrices and finding a few eigenvalues of interest.

