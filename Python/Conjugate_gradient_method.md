```python
import numpy as np

def conjugate_gradient(A, b, x0=None, max_iter=None, tol=1e-10):
    """
    Conjugate Gradient Method for solving Ax = b
    
    Parameters:
    A : numpy array, symmetric positive definite matrix
    b : numpy array, right-hand side vector
    x0 : numpy array, initial guess (optional)
    max_iter : int, maximum number of iterations (optional)
    tol : float, tolerance for convergence
    
    Returns:
    x : numpy array, solution vector
    """
    
    # Initialize
    if x0 is None:
        x = np.zeros_like(b)
    else:
        x = x0.copy()
    
    if max_iter is None:
        max_iter = len(b)
    
    # Initial residual
    r = b - A @ x
    p = r.copy()
    rsold = r @ r
    
    # Conjugate gradient iterations
    for i in range(max_iter):
        # Compute Ap
        Ap = A @ p
        
        # Compute alpha
        alpha = rsold / (p @ Ap)
        
        # Update solution
        x = x + alpha * p
        
        # Update residual
        r = r - alpha * Ap
        
        # Check for convergence
        rsnew = r @ r
        if np.sqrt(rsnew) < tol:
            print(f"Converged after {i+1} iterations")
            break
            
        # Compute beta
        beta = rsnew / rsold
        
        # Update search direction
        p = r + beta * p
        
        rsold = rsnew
    
    return x

# Example usage
if __name__ == "__main__":
    # Create a test matrix (symmetric positive definite)
    np.random.seed(42)
    n = 100
    A = np.random.rand(n, n)
    A = A.T @ A + np.eye(n)  # Make it SPD
    
    # Create solution vector
    x_true = np.random.rand(n)
    
    # Generate right-hand side
    b = A @ x_true
    
    # Solve using conjugate gradient
    x_solution = conjugate_gradient(A, b, max_iter=1000, tol=1e-8)
    
    # Check solution
    print(f"True solution norm: {np.linalg.norm(x_true):.6f}")
    print(f"Solution norm: {np.linalg.norm(x_solution):.6f}")
    print(f"Error norm: {np.linalg.norm(x_solution - x_true):.6f}")
    
    # Verify Ax = b
    residual = np.linalg.norm(A @ x_solution - b)
    print(f"Residual norm: {residual:.6f}")
```


