# Arnoldi Iteration Algorithm Example

The Arnoldi iteration is an iterative method for finding eigenvalues and eigenvectors of large sparse matrices. Here's a Python implementation:

```python
import numpy as np
from scipy.linalg import qr
import matplotlib.pyplot as plt

def arnoldi_iteration(A, k, initial_vector=None):
    """
    Perform Arnoldi iteration to find eigenvalues of matrix A
    
    Parameters:
    A: square matrix
    k: number of iterations (or desired number of eigenvalues)
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
        # Arnoldi step
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
            # If we get a zero vector, we're done
            break
    
    return H, Q

def find_eigenvalues(H):
    """
    Find eigenvalues from Hessenberg matrix
    """
    return np.linalg.eigvals(H)

# Example usage
if __name__ == "__main__":
    # Create a test matrix (random symmetric matrix)
    np.random.seed(42)
    n = 100
    A = np.random.rand(n, n)
    A = (A + A.T) / 2  # Make it symmetric
    
    print("Matrix shape:", A.shape)
    print("First few eigenvalues of original matrix:")
    eigenvals_full = np.linalg.eigvalsh(A)
    print(eigenvals_full[:10])
    
    # Perform Arnoldi iteration
    k = 20  # Number of iterations
    H, Q = arnoldi_iteration(A, k)
    
    print("\nHessenberg matrix H (first 10x10):")
    print(H[:10, :10])
    
    # Find eigenvalues from Hessenberg matrix
    eigenvals_arnoldi = find_eigenvalues(H)
    print("\nEigenvalues from Arnoldi iteration:")
    print(np.sort(eigenvals_arnoldi))
    
    # Compare with exact eigenvalues (first 10)
    print("\nComparison with exact eigenvalues (first 10):")
    print("Exact:", np.sort(eigenvals_full)[:10])
    print("Arnoldi:", np.sort(eigenvals_arnoldi)[:10])
    
    # Plot convergence
    plt.figure(figsize=(10, 6))
    plt.subplot(1, 2, 1)
    plt.scatter(range(len(eigenvals_full)), eigenvals_full, alpha=0.7, label='Exact')
    plt.scatter(range(len(eigenvals_arnoldi)), eigenvals_arnoldi, alpha=0.7, label='Arnoldi')
    plt.xlabel('Eigenvalue index')
    plt.ylabel('Eigenvalue')
    plt.title('Eigenvalue Comparison')
    plt.legend()
    
    # Plot the Hessenberg matrix
    plt.subplot(1, 2, 2)
    plt.imshow(H[:15, :15], cmap='viridis')
    plt.colorbar()
    plt.title('Hessenberg Matrix H (first 15x15)')
    plt.xlabel('Column')
    plt.ylabel('Row')
    
    plt.tight_layout()
    plt.show()
```

## Key Features of This Implementation:

1. **Basic Arnoldi Iteration**: Constructs a Hessenberg matrix that approximates the original matrix
2. **Orthonormal Basis**: Maintains orthonormal basis vectors using Gram-Schmidt process
3. **Eigenvalue Extraction**: Uses the Hessenberg matrix to find eigenvalues
4. **Convergence Analysis**: Shows how Arnoldi iteration converges to eigenvalues

## Algorithm Steps:

1. Start with a random vector and normalize it
2. Apply the matrix A to the current vector
3. Orthogonalize against all previous vectors using Gram-Schmidt
4. Normalize the resulting vector and store in Hessenberg matrix
5. Repeat for desired number of iterations

## Advantages:

- Works well for large sparse matrices
- Can find a few extreme eigenvalues efficiently
- Basis vectors form an orthonormal set
- Can be easily modified for specific eigenvalue problems

## Applications:

- Large-scale eigenvalue problems
- Quantum mechanics calculations
- Structural analysis
- Data analysis and dimensionality reduction

