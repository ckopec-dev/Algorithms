# Arnoldi Iteration Algorithm Example

Here's a Python implementation of the Arnoldi iteration algorithm for finding eigenvalues and eigenvectors:

```python
import numpy as np
from scipy.linalg import qr
import matplotlib.pyplot as plt

def arnoldi_iteration(A, k, initial_vector=None):
    """
    Perform Arnoldi iteration to find eigenvalues of matrix A
    
    Parameters:
    A: square matrix
    k: number of iterations (dimension of Krylov subspace)
    initial_vector: starting vector (optional)
    
    Returns:
    H: Hessenberg matrix
    Q: orthonormal basis vectors
    """
    
    n = A.shape[0]
    
    # Initialize
    if initial_vector is None:
        v = np.random.rand(n)
    else:
        v = initial_vector.copy()
    
    # Normalize initial vector
    v = v / np.linalg.norm(v)
    
    # Initialize matrices
    Q = np.zeros((n, k+1))
    H = np.zeros((k+1, k))
    
    Q[:, 0] = v
    
    for j in range(k):
        # Arnoldi step: w = A * v_j
        w = A @ Q[:, j]
        
        # Orthogonalize against all previous vectors
        for i in range(j+1):
            H[i, j] = np.dot(Q[:, i], w)
            w = w - H[i, j] * Q[:, i]
        
        # Normalize the new vector
        H[j+1, j] = np.linalg.norm(w)
        
        if H[j+1, j] > 1e-12:  # Avoid division by zero
            Q[:, j+1] = w / H[j+1, j]
        else:
            # If we get a zero vector, stop the iteration
            break
    
    return H, Q

def arnoldi_eigenvalues(A, k=10, initial_vector=None):
    """
    Find eigenvalues using Arnoldi iteration
    """
    H, Q = arnoldi_iteration(A, k, initial_vector)
    
    # Extract the upper triangular part of H
    H_reduced = H[:k, :k]
    
    # Compute eigenvalues of H
    eigenvals = np.linalg.eigvals(H_reduced)
    
    return eigenvals

# Example usage
if __name__ == "__main__":
    # Create a test matrix (symmetric matrix)
    np.random.seed(42)
    n = 100
    A = np.random.rand(n, n)
    A = (A + A.T) / 2  # Make it symmetric
    
    print("Matrix shape:", A.shape)
    print("First few eigenvalues of original matrix:")
    eigenvals_original = np.linalg.eigvals(A)
    print(eigenvals_original[:5])
    
    # Apply Arnoldi iteration
    k = 20  # Number of iterations
    eigenvals_arnoldi = arnoldi_eigenvalues(A, k)
    
    print(f"\nEigenvalues from Arnoldi iteration (k={k}):")
    print(eigenvals_arnoldi[:5])
    
    # Compare with actual eigenvalues
    print(f"\nComparison of first 5 eigenvalues:")
    print("Original:", eigenvals_original[:5])
    print("Arnoldi: ", eigenvals_arnoldi[:5])
    
    # Plot the eigenvalues
    plt.figure(figsize=(10, 6))
    
    plt.subplot(1, 2, 1)
    plt.scatter(np.real(eigenvals_original), np.imag(eigenvals_original), 
               alpha=0.7, label='Original')
    plt.xlabel('Real part')
    plt.ylabel('Imaginary part')
    plt.title('Eigenvalues of Original Matrix')
    plt.grid(True, alpha=0.3)
    plt.legend()
    
    plt.subplot(1, 2, 2)
    plt.scatter(np.real(eigenvals_arnoldi), np.imag(eigenvals_arnoldi), 
               alpha=0.7, color='red', label='Arnoldi')
    plt.xlabel('Real part')
    plt.ylabel('Imaginary part')
    plt.title('Eigenvalues from Arnoldi Iteration')
    plt.grid(True, alpha=0.3)
    plt.legend()
    
    plt.tight_layout()
    plt.show()
```

## Key Features of this Implementation:

1. **Arnoldi Iteration Process**:
   - Builds an orthonormal basis for the Krylov subspace
   - Constructs a Hessenberg matrix H that approximates the original matrix A

2. **Algorithm Steps**:
   - Initialize with a random vector
   - For each iteration, compute A*v_j and orthogonalize against previous vectors
   - Maintain orthonormal basis Q and Hessenberg matrix H

3. **Eigenvalue Computation**:
   - Uses eigenvalues of the smaller Hessenberg matrix as approximations
   - More accurate for the largest eigenvalues

4. **Applications**:
   - Finding dominant eigenvalues
   - Large sparse matrix eigenvalue problems
   - Preconditioning for iterative solvers

The algorithm is particularly useful when you need only a few eigenvalues (especially the largest or smallest) of very large matrices where computing all eigenvalues would be computationally prohibitive.

