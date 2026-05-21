# Conjugate Gradient Method Implementation

Here's a complete implementation of the conjugate gradient method in Python:

```python
import numpy as np
import matplotlib.pyplot as plt

def conjugate_gradient(A, b, x0=None, max_iter=None, tol=1e-8):
    """
    Solve linear system Ax = b using conjugate gradient method
    
    Parameters:
    A : numpy array, symmetric positive definite matrix
    b : numpy array, right-hand side vector
    x0 : numpy array, initial guess (optional)
    max_iter : int, maximum number of iterations (optional)
    tol : float, tolerance for convergence
    
    Returns:
    x : numpy array, solution vector
    num_iter : int, number of iterations performed
    """
    
    # Initialize
    n = len(b)
    if x0 is None:
        x = np.zeros(n)
    else:
        x = x0.copy()
    
    if max_iter is None:
        max_iter = n
    
    # Initialize residual
    r = b - A @ x
    p = r.copy()
    
    # Store convergence history
    residuals = [np.linalg.norm(r)]
    
    # Conjugate gradient iterations
    for i in range(max_iter):
        # Compute alpha
        Ap = A @ p
        alpha = (r.T @ r) / (p.T @ Ap)
        
        # Update solution
        x = x + alpha * p
        
        # Update residual
        r_new = r - alpha * Ap
        
        # Check for convergence
        norm_r = np.linalg.norm(r_new)
        residuals.append(norm_r)
        
        if norm_r < tol:
            break
            
        # Compute beta (Polak-Ribiere formula)
        beta = (r_new.T @ r_new) / (r.T @ r)
        
        # Update search direction
        p = r_new + beta * p
        
        # Update residual for next iteration
        r = r_new
    
    return x, i + 1, residuals

def solve_with_cg_example():
    """
    Example usage of conjugate gradient method
    """
    # Example 1: Simple 2x2 system
    print("Example 1: 2x2 System")
    A1 = np.array([[4.0, 1.0],
                   [1.0, 3.0]])
    b1 = np.array([1.0, 2.0])
    x1, iter1, res1 = conjugate_gradient(A1, b1)
    
    print(f"Solution: {x1}")
    print(f"Iterations: {iter1}")
    print(f"Residual norm: {np.linalg.norm(b1 - A1 @ x1):.2e}")
    
    # Example 2: Larger 5x5 system
    print("\nExample 2: 5x5 System")
    np.random.seed(42)
    n = 5
    A2 = np.random.rand(n, n)
    A2 = A2.T @ A2 + np.eye(n)  # Make it positive definite
    b2 = np.random.rand(n)
    x2, iter2, res2 = conjugate_gradient(A2, b2)
    
    print(f"Solution: {x2}")
    print(f"Iterations: {iter2}")
    print(f"Residual norm: {np.linalg.norm(b2 - A2 @ x2):.2e}")
    
    # Plot convergence
    plt.figure(figsize=(10, 6))
    plt.semilogy(res2, 'b-o', linewidth=2, markersize=6)
    plt.xlabel('Iteration')
    plt.ylabel('Residual Norm')
    plt.title('Conjugate Gradient Convergence')
    plt.grid(True, alpha=0.3)
    plt.show()

def test_convergence():
    """
    Test convergence properties with different systems
    """
    print("Testing convergence properties:")
    
    # Create a well-conditioned system
    A = np.array([[10.0, 1.0],
                  [1.0, 1.0]])
    b = np.array([11.0, 2.0])
    
    x, iterations, residuals = conjugate_gradient(A, b)
    
    print(f"Well-conditioned system:")
    print(f"Solution: {x}")
    print(f"Iterations: {iterations}")
    print(f"Final residual: {residuals[-1]:.2e}")
    
    # Create a poorly conditioned system
    A_bad = np.array([[1000.0, 1.0],
                      [1.0, 1.0]])
    b_bad = np.array([1001.0, 2.0])
    
    x_bad, iterations_bad, residuals_bad = conjugate_gradient(A_bad, b_bad)
    
    print(f"\nPoorly conditioned system:")
    print(f"Solution: {x_bad}")
    print(f"Iterations: {iterations_bad}")
    print(f"Final residual: {residuals_bad[-1]:.2e}")

# Run examples
if __name__ == "__main__":
    solve_with_cg_example()
    print("\n" + "="*50)
    test_convergence()
```

## Key Features of the Implementation:

### 1. **Core Algorithm Components:**
- **Initialization**: Sets up initial solution and residual
- **Search Direction**: Updates conjugate directions using Polak-Ribiere formula
- **Step Size**: Computes optimal step size using dot products
- **Convergence Check**: Stops when residual norm falls below tolerance

### 2. **Algorithm Steps:**
1. Initialize solution and residual
2. Set initial search direction (same as residual)
3. Compute step size α
4. Update solution: x = x + αp
5. Update residual: r = r - αAp
6. Compute β using Polak-Ribiere formula
7. Update search direction: p = r + βp
8. Check convergence criteria

### 3. **Parameters:**
- **A**: Symmetric positive definite matrix
- **b**: Right-hand side vector
- **x0**: Initial guess (optional)
- **max_iter**: Maximum iterations (optional)
- **tol**: Convergence tolerance

### 4. **Output:**
- **x**: Solution vector
- **num_iter**: Number of iterations performed
- **residuals**: Convergence history for plotting

This implementation is efficient for large sparse systems and converges in at most n iterations for n×n systems.

