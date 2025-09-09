# Cholesky Decomposition in Python

## Algorithm Implementation

```python
import numpy as np

def cholesky_decomposition(A):
    """
    Compute the Cholesky decomposition of a symmetric positive definite matrix A
    such that A = L * L.T where L is lower triangular
    
    Parameters:
    A : numpy array, symmetric positive definite matrix
    
    Returns:
    L : numpy array, lower triangular matrix
    """
    n = A.shape[0]
    L = np.zeros((n, n))
    
    for i in range(n):
        for j in range(i + 1):
            if i == j:
                # Compute diagonal elements
                sum_diag = sum(L[i][k]**2 for k in range(j))
                L[i][j] = np.sqrt(A[i][i] - sum_diag)
            else:
                # Compute off-diagonal elements
                sum_off = sum(L[i][k] * L[j][k] for k in range(j))
                L[i][j] = (A[i][j] - sum_off) / L[j][j]
    
    return L

def verify_cholesky(A, L):
    """
    Verify that A = L * L.T
    """
    LT = L.T
    reconstruction = np.dot(L, LT)
    return np.allclose(A, reconstruction)

# Example usage
if __name__ == "__main__":
    # Create a symmetric positive definite matrix
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
    
    print("Upper triangular matrix U (L.T):")
    U = L.T
    print(U)
    print()
    
    # Verify the decomposition
    is_valid = verify_cholesky(A, L)
    print(f"Decomposition valid: {is_valid}")
    print()
    
    # Show that A = L * L.T
    reconstruction = np.dot(L, U)
    print("Reconstruction A = L * L.T:")
    print(reconstruction)
    print()
    
    # Compare with numpy's built-in function
    print("Using NumPy's cholesky decomposition:")
    L_numpy = np.linalg.cholesky(A)
    print(L_numpy)
    print()
    
    print("Are results equal?", np.allclose(L, L_numpy))
```

## Output

```
Original matrix A:
[[  4.  12. -16.]
 [ 12.  37. -43.]
 [-16. -43.  98.]]

Lower triangular matrix L:
[[ 2.          0.          0.        ]
 [ 6.          1.          0.        ]
 [-8.         -5.          3.        ]]

Upper triangular matrix U (L.T):
[[ 2.          6.         -8.        ]
 [ 0.          1.         -5.        ]
 [ 0.          0.          3.        ]]

Decomposition valid: True

Reconstruction A = L * L.T:
[[  4.  12. -16.]
 [ 12.  37. -43.]
 [-16. -43.  98.]]

Using NumPy's cholesky decomposition:
[[ 2.          0.          0.        ]
 [ 6.          1.          0.        ]
 [-8.         -5.          3.        ]]

Are results equal? True
```

## Algorithm Explanation

The Cholesky decomposition algorithm works as follows:

1. **Input**: A symmetric positive definite matrix A
2. **Output**: A lower triangular matrix L such that A = L × L^T
3. **Process**:
   - For diagonal elements (i = j): 
     L[i][i] = √(A[i][i] - Σₖ₌₀ⁱ⁻¹ L[i][k]²)
   - For off-diagonal elements (i > j):
     L[i][j] = (A[i][j] - Σₖ₌₀ʲ⁻¹ L[i][k] × L[j][k]) / L[j][j]

The algorithm has O(n³) time complexity and is numerically stable for well-conditioned matrices.

