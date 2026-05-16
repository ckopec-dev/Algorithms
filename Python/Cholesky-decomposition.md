# Cholesky Decomposition in Python

## What is Cholesky Decomposition?

Cholesky decomposition is a method for decomposing a Hermitian, positive-definite matrix into the product of a lower triangular matrix and its conjugate transpose.

## Implementation Example

```python
import numpy as np

def cholesky_decomposition(A):
    """
    Perform Cholesky decomposition on a symmetric positive-definite matrix.
    
    Parameters:
    A (numpy.ndarray): Symmetric positive-definite matrix
    
    Returns:
    L (numpy.ndarray): Lower triangular matrix such that A = L @ L.T
    """
    n = A.shape[0]
    L = np.zeros((n, n))
    
    for i in range(n):
        for j in range(i + 1):
            if i == j:
                # Diagonal elements
                sum_val = sum(L[i][k]**2 for k in range(j))
                L[i][j] = np.sqrt(A[i][i] - sum_val)
            else:
                # Off-diagonal elements
                sum_val = sum(L[i][k] * L[j][k] for k in range(j))
                L[i][j] = (A[i][j] - sum_val) / L[j][j]
    
    return L

# Example usage
if __name__ == "__main__":
    # Create a symmetric positive-definite matrix
    A = np.array([[4, 12, -16],
                  [12, 37, -43],
                  [-16, -43, 98]], dtype=float)
    
    print("Original matrix A:")
    print(A)
    print()
    
    # Perform Cholesky decomposition
    L = cholesky_decomposition(A)
    
    print("Lower triangular matrix L:")
    print(L)
    print()
    
    # Verify the decomposition: L @ L.T should equal A
    L_transpose = L.T
    reconstructed = L @ L_transpose
    
    print("Reconstructed matrix L @ L.T:")
    print(reconstructed)
    print()
    
    # Check if decomposition is correct
    print("Is A equal to L @ L.T? ", np.allclose(A, reconstructed))
    print()
    
    # Compare with NumPy's built-in function
    print("Using NumPy's cholesky decomposition:")
    L_numpy = np.linalg.cholesky(A)
    print(L_numpy)
    print("Are results the same? ", np.allclose(L, L_numpy))
```

## Output
```
Original matrix A:
[[  4.  12. -16.]
 [ 12.  37. -43.]
 [-16. -43.  98.]]

Lower triangular matrix L:
[[ 2.  0.  0.]
 [ 6.  1.  0.]
 [-8.  5.  3.]]

Reconstructed matrix L @ L.T:
[[  4.  12. -16.]
 [ 12.  37. -43.]
 [-16. -43.  98.]]

Is A equal to L @ L.T?  True

Using NumPy's cholesky decomposition:
[[ 2.  0.  0.]
 [ 6.  1.  0.]
 [-8.  5.  3.]]
Are results the same?  True
```

## Key Points

1. **Input Requirements**: The matrix must be symmetric and positive-definite
2. **Output**: Lower triangular matrix L such that A = L × L^T
3. **Applications**: Solving linear systems, Monte Carlo simulations, optimization
4. **Advantages**: More efficient than general matrix decomposition methods
5. **Limitations**: Only works for symmetric positive-definite matrices

## Alternative: Using NumPy

For practical applications, you can use NumPy's built-in function:

```python
# Simple approach using NumPy
A = np.array([[4, 12, -16],
              [12, 37, -43],
              [-16, -43, 98]])

L = np.linalg.cholesky(A)
print(L)
```

This implementation provides both a manual understanding of the algorithm and a practical approach using existing libraries.

