# Arnoldi Iteration Algorithm Example

Here's a Python implementation of the Arnoldi iteration algorithm to compute eigenvalues and eigenvectors of a matrix:

```python
import numpy as np
from scipy.linalg import qr
import matplotlib.pyplot as plt

def arnoldi_iteration(A, k, max_iter=1000, tol=1e-10):
    """
    Arnoldi iteration algorithm for finding eigenvalues of a matrix
    
    Parameters:
    A: square matrix
    k: number of eigenvalues to compute
    max_iter: maximum number of iterations
    tol: convergence tolerance
    
    Returns:
    H: Hessenberg matrix
    Q: orthogonal matrix
    eigenvals: computed eigenvalues
    """
    
    m, n = A.shape
    if m != n:
        raise ValueError("Matrix must be square")
    
    # Initialize matrices
    Q = np.zeros((n, max_iter))
    H = np.zeros((max_iter, max_iter))
    
    # Start with random vector
    v = np.random.rand(n)
    v = v / np.linalg.norm(v)
    Q[:, 0] = v
    
    for j in range(max_iter):
        # Arnoldi step
        w = A @ Q[:, j]
        
        # Orthogonalize w against all previous Q columns
        for i in range(j + 1):
            H[i, j] = np.dot(Q[:, i], w)
            w = w - H[i, j] * Q[:, i]
        
        # Normalize the new vector
        H[j + 1, j] = np.linalg.norm(w)
        
        if H[j + 1, j] < tol:
            break
            
        Q[:, j + 1] = w / H[j + 1, j]
    
    # Extract the first k columns for computation
    Q_k = Q[:, :j + 1]
    H_k = H[:j + 1, :j + 1]
    
    # Compute eigenvalues of Hessenberg matrix
    eigenvals = np.linalg.eigvals(H_k)
    
    return H_k, Q_k, eigenvals

def arnoldi_eigenvalues(A, k=5, max_iter=1000):
    """
    Compute k largest eigenvalues using Arnoldi iteration
    """
    H, Q, eigenvals = arnoldi_iteration(A, k, max_iter)
    
    # Sort eigenvalues by magnitude (largest first)
    idx = np.argsort(np.abs(eigenvals))[::-1]
    eigenvals = eigenvals[idx][:k]
    
    return eigenvals

# Example usage
if __name__ == "__main__":
    # Create a test matrix
    np.random.seed(42)
    n = 10
    A = np.random.rand(n, n)
    A = A + A.T  # Make it symmetric
    A = A - np.mean(A) * np.eye(n)  # Center the matrix
    
    print("Original matrix A:")
    print(A)
    print("\nMatrix shape:", A.shape)
    
    # Compute eigenvalues using Arnoldi iteration
    k = 5  # Number of eigenvalues to compute
    eigenvals = arnoldi_eigenvalues(A, k)
    
    print(f"\nArnoldi iteration - First {k} eigenvalues:")
    for i, val in enumerate(eigenvals):
        print(f"Eigenvalue {i+1}: {val:.6f}")
    
    # Compare with direct computation
    direct_eigenvals = np.linalg.eigvals(A)
    direct_eigenvals = np.sort(direct_eigenvals)[::-1][:k]
    
    print(f"\nDirect computation - First {k} eigenvalues:")
    for i, val in enumerate(direct_eigenvals):
        print(f"Eigenvalue {i+1}: {val:.6f}")
    
    # Plot comparison
    plt.figure(figsize=(10, 6))
    plt.subplot(1, 2, 1)
    plt.scatter(range(len(direct_eigenvals)), direct_eigenvals, 
               label='Direct computation', alpha=0.7)
    plt.scatter(range(len(eigenvals)), eigenvals, 
               label='Arnoldi iteration', alpha=0.7)
    plt.xlabel('Eigenvalue index')
    plt.ylabel('Eigenvalue')
    plt.title('Eigenvalue Comparison')
    plt.legend()
    plt.grid(True, alpha=0.3)
    
    plt.subplot(1, 2, 2)
    plt.scatter(direct_eigenvals, eigenvals, alpha=0.7)
    plt.plot([direct_eigenvals.min(), direct_eigenvals.max()], 
             [direct_eigenvals.min(), direct_eigenvals.max()], 'r--')
    plt.xlabel('Direct eigenvalues')
    plt.ylabel('Arnoldi eigenvalues')
    plt.title('Comparison: Direct vs Arnoldi')
    plt.grid(True, alpha=0.3)
    
    plt.tight_layout()
    plt.show()
    
    # Test with a larger matrix
    print("\n" + "="*50)
    print("Testing with larger matrix (20x20):")
    
    # Create a larger test matrix
    A_large = np.random.rand(20, 20)
    A_large = A_large + A_large.T
    A_large = A_large - np.mean(A_large) * np.eye(20)
    
    eigenvals_large = arnoldi_eigenvalues(A_large, k=5)
    print(f"First 5 eigenvalues (20x20 matrix):")
    for i, val in enumerate(eigenvals_large):
        print(f"Eigenvalue {i+1}: {val:.6f}")
```

## Key Features of this Implementation:

1. **Arnoldi Iteration Process**:
   - Constructs a Hessenberg matrix H
   - Builds an orthogonal basis Q
   - Uses QR decomposition for convergence

2. **Algorithm Steps**:
   - Start with a random vector
   - Apply matrix A to current vector
   - Orthogonalize against previous vectors
   - Normalize and continue

3. **Output**:
   - Hessenberg matrix H
   - Orthogonal matrix Q
   - Computed eigenvalues

4. **Comparison**:
   - Compares Arnoldi results with direct eigenvalue computation
   - Visualizes the accuracy of the approximation

## Usage Notes:

- The algorithm is particularly useful for large sparse matrices
- Convergence depends on the matrix properties and number of iterations
- For symmetric matrices, the Hessenberg matrix becomes tridiagonal
- The method can be extended to compute eigenvectors as well

This implementation demonstrates the core principles of Arnoldi iteration while providing practical examples and validation through comparison with direct methods.

